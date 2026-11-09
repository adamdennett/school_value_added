# 27_la_routing.R - Journeys to school from every Output Area in the 109 authorities
# ---------------------------------------------------------------------------
# The same questions as R/20 and R/22, on the networks of R/26. Each network is run in
# its own process, so that Java's memory is returned, and each step is saved as it ends:
#
#   flat    Walking and bus, leaving 07:45 to 08:15 (the median of those departures), walking
#           alone, and walking and bus at 4.8 km/h as a check (SCENARIOS to drop it for
#           London). 60 minute walk limit, 120 minute trip limit. For each Output Area: the
#           five quickest schools, the quickest neither selective nor faith, and how many
#           schools can be reached in 30 and 45 minutes.
#   struct  The journey to the quickest school by walking and bus, at 07:45, 08:00, 08:15:
#           buses, changes, waits, stops. Walking legs at 08:00 are kept as lines, for R/28.
#   walk    The walking route to the quickest school on foot, kept as a line, with the climb
#           each way from OS Terrain 50 and walking time by Tobler's function (as R/22).
#   miss    Every departure minute from 07:00: the last that arrives by the bell, then how
#           late a child is if they miss it (as R/22).
#
# Run R/26 first, and R/25 for the elevation file. Sources and formulas:
# method.qmd#sec-la-routing-method. Output: data/cache/journey_la/<step>_<network>.rds
# (not committed). Uses only street, timetable, Census and school location data.
# NETS=devon R5R_HEAP=18G R5R_THREADS=5 Rscript R/27_la_routing.R
# ---------------------------------------------------------------------------

JH <- Sys.getenv("JAVA_HOME")
if (!nzchar(JH) || !dir.exists(JH)) { JH <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "rJavaEnv", "installed", "windows", "x64", "21"); Sys.setenv(JAVA_HOME = JH) }
options(java.parameters = paste0("-Xmx", Sys.getenv("R5R_HEAP", "18G")))
proj_db <- system.file("proj", package = "terra"); if (nzchar(proj_db)) Sys.setenv(PROJ_LIB = proj_db, PROJ_DATA = proj_db)
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(sf); library(r5r); library(data.table) })
OUT <- here::here("data", "cache", "journey_la"); NET <- here::here("data-raw", "r5", "net_la")
FR <- readRDS(file.path(OUT, "frame.rds")); oa_all <- FR$oa; sch_all <- FR$sch
ROUTE_DATE <- as.Date("2026-11-17"); BELLS <- c(bell_0845 = "08:45:00", bell_0830 = "08:30:00"); SPEED <- 3.6
N_THREADS <- as.numeric(Sys.getenv("R5R_THREADS", "5"))
at <- function(clock) as.POSIXct(paste(ROUTE_DATE, clock), tz = "Europe/London")
ALL_SCEN <- list(walk_bus = list(mode = c("WALK", "TRANSIT"), window = 30, speed = 3.6), walk = list(mode = "WALK", window = 1, speed = 3.6), walk_bus_f = list(mode = c("WALK", "TRANSIT"), window = 30, speed = 4.8))
want_s <- strsplit(Sys.getenv("SCENARIOS"), ",")[[1]]; SCEN <- if (length(want_s)) ALL_SCEN[want_s] else ALL_SCEN
STEPS <- strsplit(Sys.getenv("STEPS", "flat,struct,walk,miss"), ",")[[1]]
dem <- terra::rast(here::here("data", "cache", "journey", "terrain50_gb.vrt"))

tobler_kmh <- function(s) SPEED * exp(-3.5 * abs(s + 0.05)) / exp(-3.5 * 0.05)
climb <- function(lines, id) {
  l <- st_transform(st_geometry(lines), 27700); ok <- as.numeric(st_length(l)) >= 25
  res <- data.table(k = seq_along(l), id = id, metres = as.numeric(st_length(l)), up_out = 0, up_back = 0, t_flat = as.numeric(st_length(l)) / (SPEED * 1000 / 60), t_out = NA_real_, t_back = NA_real_)
  res[, `:=`(t_out = t_flat, t_back = t_flat)]
  if (!any(ok)) return(res)
  pts <- st_line_sample(l[ok], density = units::set_units(1 / 50, "1/m"), type = "regular"); xy <- st_coordinates(pts); key <- which(ok)[xy[, "L1"]]
  d <- data.table(k = key, x = xy[, "X"], y = xy[, "Y"], z = terra::extract(dem, xy[, c("X", "Y")])[[1]])[!is.na(z)]
  a <- d[, { dz <- diff(z); run <- sqrt(diff(x)^2 + diff(y)^2); s <- ifelse(run > 0, dz / run, 0)
             .(up_out = sum(pmax(dz, 0)), up_back = sum(pmax(-dz, 0)), t_out = sum(run / (tobler_kmh(s) * 1000 / 60)), t_back = sum(run / (tobler_kmh(-s) * 1000 / 60)), run = sum(run)) }, by = k][run > 0]
  m <- match(a$k, res$k); sc <- res$metres[m] / a$run
  res[m, `:=`(up_out = a$up_out, up_back = a$up_back, t_out = a$t_out * sc, t_back = a$t_back * sc)]; res
}
# Lines for R/28, on the National Grid, simplified to 5 m: enough for counting what is within 50 m
keep_lines <- function(x, cols) { g <- st_simplify(st_transform(st_geometry(x), 27700), dTolerance = 5); st_sf(as.data.frame(st_drop_geometry(x))[cols], geometry = g) }

run_net <- function(id) {
  t00 <- Sys.time(); f <- function(step) file.path(OUT, paste0(step, "_", id, ".rds")); done <- function(step) !(step %in% STEPS) || file.exists(f(step))
  if (all(vapply(c("flat", "struct", "walk", "miss"), done, TRUE))) { message(id, ": done already"); return(invisible()) }
  o <- oa_all[net == id]; box <- o[, c(min(e) - 15000, min(n) - 15000, max(e) + 15000, max(n) + 15000)]
  s <- sch_all[e >= box[1] & n >= box[2] & e <= box[3] & n <= box[4]]
  or <- o[, .(id = oa21cd, lon, lat)]; de <- s[, .(id = as.character(URN), lon, lat)]
  dir_ <- file.path(NET, id); nd <- file.path(dir_, "network.dat")
  if (file.exists(nd) && max(file.mtime(list.files(dir_, pattern = "\\.(pbf|zip)$", full.names = TRUE))) > file.mtime(nd)) file.remove(nd)
  message("\n=== ", id, ": ", nrow(o), " Output Areas, ", nrow(s), " schools"); net <- build_network(dir_, verbose = FALSE)
  message(sprintf("  network ready, %.1f min", as.numeric(difftime(Sys.time(), t00, units = "mins"))))

  # ---- Times --------------------------------------------------------------------
  if (!done("flat")) {
    open_ <- s[open_to_all == TRUE, as.character(URN)]
    mats <- lapply(names(SCEN), function(k) { p <- SCEN[[k]]; t0 <- Sys.time()
      tt <- function(orig) as.data.table(travel_time_matrix(net, origins = orig, destinations = de, mode = p$mode, departure_datetime = at("07:45:00"), time_window = p$window, percentiles = 50, max_walk_time = if (identical(p$mode, "WALK")) 120 else 60, max_trip_duration = 120, walk_speed = p$speed, n_threads = N_THREADS, progress = FALSE))
      # With no transit and more origins than destinations, r5r searches backwards and returns the ids swapped (see R/20)
      x <- if (identical(p$mode, "WALK")) rbindlist(lapply(split(seq_len(nrow(or)), ceiling(seq_len(nrow(or)) / nrow(de))), function(i) tt(or[i, ]))) else tt(or)
      stopifnot(all(x$from_id %in% or$id), all(x$to_id %in% de$id))
      setnames(x, grep("travel_time", names(x), value = TRUE)[1], "t"); x[, t := as.numeric(t)]; x[, open := to_id %in% open_]; setorder(x, from_id, t)
      a <- x[, .(n30 = sum(t <= 30), n45 = sum(t <= 45), t_open = if (any(open)) t[open][1] else NA_real_, n_open30 = sum(open & t <= 30)), by = from_id]
      top <- x[, .(to_id = to_id[seq_len(min(.N, 5))], t = t[seq_len(min(.N, 5))]), by = from_id]
      message(sprintf("  %s: %.1f min", k, as.numeric(difftime(Sys.time(), t0, units = "mins")))); list(top = top[, scenario := k], reach = a[, scenario := k]) })
    saveRDS(list(id = id, times = rbindlist(lapply(mats, `[[`, "top")), reach = rbindlist(lapply(mats, `[[`, "reach")), date = ROUTE_DATE, n_origins = nrow(o), n_schools = nrow(s), feeds = list.files(dir_, pattern = "^gtfs_"), built = Sys.time()), f("flat"))
  }
  fl <- as.data.table(readRDS(f("flat"))$times)[order(scenario, from_id, t)][, .(school = to_id[1], t1 = t[1]), by = .(scenario, from_id)]
  pairs <- function(sc) { p <- fl[scenario == sc]; list(or = or[match(p$from_id, id)], de = de[match(p$school, id)], p = p) }

  # ---- Structure ------------------------------------------------------------------
  P <- pairs("walk_bus")
  if (!done("struct")) { str_ <- list(); stops <- list(); legs <- NULL; lg <- NULL
    for (clk in c("07:45:00", "08:00:00", "08:15:00")) { t0 <- Sys.time()
      x <- detailed_itineraries(net, origins = P$or, destinations = P$de, mode = c("WALK", "TRANSIT"), departure_datetime = at(clk), max_walk_time = 60, max_trip_duration = 120, walk_speed = SPEED, shortest_path = TRUE, all_to_all = FALSE, n_threads = N_THREADS, progress = FALSE)
      stopifnot(all(x$from_id %in% P$or$id))
      co <- do.call(rbind, lapply(st_geometry(x), function(g) { m <- st_coordinates(g); c(m[1, 1:2], m[nrow(m), 1:2]) })); colnames(co) <- c("lon0", "lat0", "lon1", "lat1")
      d <- as.data.table(st_drop_geometry(x)); d <- cbind(d, co); d[, is_bus := mode != "WALK"]; d[, `:=`(distance = as.numeric(distance), wait = as.numeric(wait), segment_duration = as.numeric(segment_duration), total_duration = as.numeric(total_duration))]; d <- d[order(from_id, segment)]
      str_[[clk]] <- d[, .(school = to_id[1], total = total_duration[1], n_bus = sum(is_bus), wait = sum(wait), ride = sum(segment_duration[is_bus]), walk_min = sum(segment_duration[!is_bus]), walk_m = sum(distance[!is_bus]),
                           first_walk_m = if (any(is_bus)) sum(distance[seq_len(which(is_bus)[1] - 1)]) else NA_real_, routes = paste(route[is_bus], collapse = " > ")), by = from_id][, dep := clk]
      stops[[clk]] <- d[is_bus == TRUE][, .(to_id = to_id[1], i = seq_len(.N), n = .N, route, mode, board_lon = lon0, board_lat = lat0, off_lon = lon1, off_lat = lat1), by = from_id][, dep := clk]
      if (clk == "08:00:00") { w <- x[x$mode == "WALK", ]; cl <- climb(w, w$from_id); legs <- cl[, `:=`(segment = w$segment)]; lg <- keep_lines(w, c("from_id", "segment")) }
      message(sprintf("  itineraries %s: %.1f min", clk, as.numeric(difftime(Sys.time(), t0, units = "mins")))); rm(x); gc(FALSE) }
    saveRDS(list(id = id, structure = rbindlist(str_), stops = rbindlist(stops), walk_legs = legs, leg_lines = lg), f("struct")) }

  # ---- Walking routes ---------------------------------------------------------------
  if (!done("walk")) { t0 <- Sys.time(); W <- pairs("walk")
    x <- detailed_itineraries(net, origins = W$or, destinations = W$de, mode = "WALK", departure_datetime = at("08:00:00"), max_trip_duration = 120, walk_speed = SPEED, shortest_path = TRUE, all_to_all = FALSE, n_threads = N_THREADS, progress = FALSE)
    stopifnot(all(x$from_id %in% W$or$id)); cw <- climb(x, x$from_id)
    saveRDS(list(id = id, walk_route = cw, route_lines = keep_lines(x, "from_id")), f("walk")); message(sprintf("  walking routes: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))); rm(x); gc(FALSE) }

  # ---- Missing it -------------------------------------------------------------------
  if (!done("miss")) { t0 <- Sys.time(); mins <- function(tt) { h <- as.numeric(substr(tt, 1, 2)); m <- as.numeric(substr(tt, 4, 5)); h * 60 + m }
    miss <- rbindlist(lapply(split(seq_len(nrow(P$p)), P$p$school), function(i) {
      e <- as.data.table(expanded_travel_time_matrix(net, origins = P$or[i, ], destinations = P$de[i[1], ], mode = c("WALK", "TRANSIT"), departure_datetime = at("07:00:00"), time_window = 110, max_walk_time = 60, max_trip_duration = 120, walk_speed = SPEED, n_threads = N_THREADS, progress = FALSE))
      stopifnot(all(e$from_id %in% P$or$id[i])); e[, `:=`(d = mins(departure_time), a = mins(departure_time) + total_time)]
      rbindlist(lapply(names(BELLS), function(b) { bell <- mins(BELLS[[b]])
        e[order(from_id, d)][, { ok <- which(a <= bell)
          if (length(ok)) { L <- d[max(ok)]; nxt <- a[d > L]; .(last_leave = L, late = if (length(nxt)) min(nxt) - bell else NA_real_, options = uniqueN(a[a <= bell & d >= bell - 75])) } else .(last_leave = NA_real_, late = NA_real_, options = 0L) }, by = from_id][, bell := b] })) }))
    saveRDS(list(id = id, miss = miss), f("miss")); message(sprintf("  missing it: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))) }
  stop_r5(net); rJava::.jgc(R.gc = TRUE)
  message(sprintf("  %s done in %.1f min", id, as.numeric(difftime(Sys.time(), t00, units = "mins"))))
}
for (id in strsplit(Sys.getenv("NETS"), ",")[[1]]) run_net(id)
message("Routing complete")
