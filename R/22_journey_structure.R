# 22_journey_structure.R - What each journey to school is made of
# ---------------------------------------------------------------------------
# For each Output Area's journey to its quickest school (from R/20), on the
# networks R/20 built:
#   A. Structure: how many buses, where children would board, change and get off,
#      waits and walking, at departures of 07:45, 08:00 and 08:15. A change of
#      bus is a place a child gets off and could stay off.
#   B. Missing it: every departure minute from 07:00. The last departure that
#      still arrives by the bell, then how late a child is if they miss it. High
#      where the bus is the only sensible way in and runs rarely.
#   C. Hills: the climb along the walking route, from OS Terrain 50, to school and
#      home separately, and walking time by Tobler's function each way. r5r's own
#      elevation option averages both directions, and did not change walking times
#      in our tests (r5r 2.3.0, R5 7.4), so it is not used.
#
# Sources and formulas: method.qmd#sec-journey-method. Run R/20 first.
# Output: data/cache/journey/struct_<network>.rds (not committed).
# Rscript R/22_journey_structure.R ; NETS=bh Rscript R/22_journey_structure.R
# ---------------------------------------------------------------------------

JH <- Sys.getenv("JAVA_HOME")
if (!nzchar(JH) || !dir.exists(JH)) { JH <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "rJavaEnv", "installed", "windows", "x64", "21"); Sys.setenv(JAVA_HOME = JH) }
options(java.parameters = paste0("-Xmx", Sys.getenv("R5R_HEAP", "40G")))
proj_db <- system.file("proj", package = "terra"); if (nzchar(proj_db)) Sys.setenv(PROJ_LIB = proj_db, PROJ_DATA = proj_db)
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(sf); library(r5r); library(data.table) })

J <- here::here("data", "cache", "journey"); SRC <- here::here("data-raw", "r5", "src"); NET <- here::here("data-raw", "r5", "net")
ROUTE_DATE <- as.Date("2026-11-17"); BELLS <- c(bell_0845 = "08:45:00", bell_0830 = "08:30:00")
FR <- readRDS(file.path(J, "frame.rds"))
ids <- sub("^flat_(.*)\\.rds$", "\\1", list.files(J, pattern = "^flat_"))
want <- strsplit(Sys.getenv("NETS"), ",")[[1]]; if (length(want)) ids <- intersect(ids, want)
N_THREADS <- as.numeric(Sys.getenv("R5R_THREADS", "Inf"))   # fewer threads for London: each holds its own search state
at <- function(clock) as.POSIXct(paste(ROUTE_DATE, clock), tz = "Europe/London")

# OS Terrain 50 for the network, kept on the National Grid
terrain_for <- function(box) {
  z <- file.path(SRC, "terr50_gagg_gb.zip"); stopifnot(file.exists(z))
  fl <- unzip(z, list = TRUE)$Name; fl <- fl[grepl("OST50GRID.*\\.zip$", fl)]
  ng <- function(f) { s <- toupper(substr(basename(f), 1, 4)); l <- function(ch) { i <- match(ch, setdiff(LETTERS, "I")) - 1; c(i %% 5, 4 - i %/% 5) }
    a <- l(substr(s, 1, 1)); b <- l(substr(s, 2, 2)); c(e = ((a[1] - 2) * 5 + b[1]) * 1e5 + as.numeric(substr(s, 3, 3)) * 1e4, n = ((a[2] - 1) * 5 + b[2]) * 1e5 + as.numeric(substr(s, 4, 4)) * 1e4) }
  en <- t(vapply(fl, ng, c(e = 0, n = 0)))
  keep <- fl[en[, "e"] + 1e4 >= box[1] - 2e4 & en[, "e"] <= box[3] + 2e4 & en[, "n"] + 1e4 >= box[2] - 2e4 & en[, "n"] <= box[4] + 2e4]
  td <- tempfile(); dir.create(td)
  rr <- lapply(keep, function(f) { unzip(z, files = f, exdir = td, junkpaths = TRUE); got <- unzip(file.path(td, basename(f)), exdir = td); r <- terra::rast(got[grepl("\\.asc$", got)]); names(r) <- "z"; r })
  r <- do.call(terra::merge, unname(rr)); terra::crs(r) <- "EPSG:27700"; r
}

# Tobler's hiking function, scaled so that flat ground is walked at the routing speed
SPEED <- 3.6
tobler_kmh <- function(s) SPEED * exp(-3.5 * abs(s + 0.05)) / exp(-3.5 * 0.05)

#' Climb and walking time along lines, in each direction, sampling every 50 m
climb <- function(lines, id, dem) {
  l <- st_transform(st_geometry(lines), 27700); ok <- as.numeric(st_length(l)) >= 25
  res <- data.table(k = seq_along(l), id = id, metres = as.numeric(st_length(l)), up_out = 0, up_back = 0, t_flat = as.numeric(st_length(l)) / (SPEED * 1000 / 60), t_out = NA_real_, t_back = NA_real_)
  res[, `:=`(t_out = t_flat, t_back = t_flat)]
  if (!any(ok)) return(res)
  pts <- st_line_sample(l[ok], density = units::set_units(1 / 50, "1/m"), type = "regular"); xy <- st_coordinates(pts); key <- which(ok)[xy[, "L1"]]
  zz <- terra::extract(dem, xy[, c("X", "Y")])[[1]]
  d <- data.table(k = key, x = xy[, "X"], y = xy[, "Y"], z = zz)[!is.na(z)]
  a <- d[, { dz <- diff(z); run <- sqrt(diff(x)^2 + diff(y)^2); s <- ifelse(run > 0, dz / run, 0)
             .(up_out = sum(pmax(dz, 0)), up_back = sum(pmax(-dz, 0)), t_out = sum(run / (tobler_kmh(s) * 1000 / 60)), t_back = sum(run / (tobler_kmh(-s) * 1000 / 60)), run = sum(run)) }, by = k][run > 0]
  # keep each line's own length: sampling stops short of the end, so scale the time to it
  m <- match(a$k, res$k); sc <- res$metres[m] / a$run
  res[m, `:=`(up_out = a$up_out, up_back = a$up_back, t_out = a$t_out * sc, t_back = a$t_back * sc)]; res
}

run_net <- function(id) {
  f_out <- file.path(J, paste0("struct_", id, ".rds")); if (file.exists(f_out)) { message(id, ": done already"); return(invisible()) }
  t0 <- Sys.time(); fl <- readRDS(file.path(J, paste0("flat_", id, ".rds"))); message("\n=== ", id)
  tm <- as.data.table(fl$times)[order(scenario, from_id, t)][, .(school = to_id[1], t1 = as.numeric(t[1])), by = .(scenario, from_id)]
  oa <- as.data.table(FR$oa); sc <- as.data.table(FR$sch)[, .(id = as.character(URN), lon, lat)]
  net <- build_network(file.path(NET, id), verbose = FALSE)
  pairs <- function(s) { p <- tm[scenario == s]; list(or = oa[match(p$from_id, oa21cd), .(id = oa21cd, lon, lat)], de = sc[match(p$school, id), .(id, lon, lat)], p = p) }
  bb <- oa[oa21cd %in% tm$from_id, c(min(e), min(n), max(e), max(n))]; dem <- terrain_for(bb)

  # ---- A. Structure ---------------------------------------------------------
  P <- pairs("walk_bus"); str_ <- list(); stops <- list(); walk_legs <- NULL
  for (clk in c("07:45:00", "08:00:00", "08:15:00")) {
    message("  itineraries ", clk)
    x <- detailed_itineraries(net, origins = P$or, destinations = P$de, mode = c("WALK", "TRANSIT"), departure_datetime = at(clk), max_walk_time = 60, max_trip_duration = 120, walk_speed = SPEED, shortest_path = TRUE, all_to_all = FALSE, n_threads = N_THREADS, progress = FALSE)
    stopifnot(all(x$from_id %in% P$or$id))
    co <- do.call(rbind, lapply(st_geometry(x), function(g) { m <- st_coordinates(g); c(m[1, 1:2], m[nrow(m), 1:2]) })); colnames(co) <- c("lon0", "lat0", "lon1", "lat1")
    d <- as.data.table(st_drop_geometry(x)); d <- cbind(d, co); d[, is_bus := mode != "WALK"]; d[, `:=`(distance = as.numeric(distance), wait = as.numeric(wait), segment_duration = as.numeric(segment_duration), total_duration = as.numeric(total_duration))]; d <- d[order(from_id, segment)]
    str_[[clk]] <- d[, .(school = to_id[1], total = total_duration[1], n_bus = sum(is_bus), wait = sum(wait), ride = sum(segment_duration[is_bus]), walk_min = sum(segment_duration[!is_bus]), walk_m = sum(distance[!is_bus]),
                         first_walk_m = if (any(is_bus)) sum(distance[seq_len(which(is_bus)[1] - 1)]) else NA_real_, routes = paste(route[is_bus], collapse = " > ")), by = from_id][, dep := clk]
    stops[[clk]] <- d[is_bus == TRUE][, .(from_id, to_id, i = seq_len(.N), n = .N, route, mode, board_lon = lon0, board_lat = lat0, off_lon = lon1, off_lat = lat1), by = from_id][, from_id := NULL][, dep := clk]
    if (clk == "08:00:00") { w <- x[x$mode == "WALK", ]; cl <- climb(w, w$from_id, dem); cl[, `:=`(segment = w$segment)]; walk_legs <- cl }
    rm(x); gc(FALSE)
  }

  # ---- B. Missing it --------------------------------------------------------
  message("  every departure from 07:00")
  mins <- function(tt) { h <- as.numeric(substr(tt, 1, 2)); m <- as.numeric(substr(tt, 4, 5)); h * 60 + m }
  miss <- rbindlist(lapply(split(seq_len(nrow(P$p)), P$p$school), function(i) {
    e <- as.data.table(expanded_travel_time_matrix(net, origins = P$or[i, ], destinations = P$de[i[1], ], mode = c("WALK", "TRANSIT"), departure_datetime = at("07:00:00"), time_window = 110, max_walk_time = 60, max_trip_duration = 120, walk_speed = SPEED, n_threads = N_THREADS, progress = FALSE))
    stopifnot(all(e$from_id %in% P$or$id[i]))
    e[, `:=`(d = mins(departure_time), a = mins(departure_time) + total_time)]
    rbindlist(lapply(names(BELLS), function(b) { bell <- mins(BELLS[[b]])
      e[order(from_id, d)][, { ok <- which(a <= bell)
        if (length(ok)) { L <- d[max(ok)]; nxt <- a[d > L]; .(last_leave = L, late = if (length(nxt)) min(nxt) - bell else NA_real_, options = uniqueN(a[a <= bell & d >= bell - 75]), routes_last = routes[max(ok)]) } else .(last_leave = NA_real_, late = NA_real_, options = 0L, routes_last = NA_character_) }, by = from_id][, bell := b] }))
  }))

  # ---- C. Hills on foot ------------------------------------------------------
  message("  walking routes")
  W <- pairs("walk")
  x <- detailed_itineraries(net, origins = W$or, destinations = W$de, mode = "WALK", departure_datetime = at("08:00:00"), max_trip_duration = 120, walk_speed = SPEED, shortest_path = TRUE, all_to_all = FALSE, n_threads = N_THREADS, progress = FALSE)
  cw <- climb(x, x$from_id, dem); stop_r5(net); rJava::.jgc(R.gc = TRUE)
  saveRDS(list(id = id, structure = rbindlist(str_), stops = rbindlist(stops), walk_legs = walk_legs, walk_route = cw, miss = miss, built = Sys.time()), f_out)
  message(sprintf("  done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
for (id in ids) run_net(id)
message("Structure complete")
