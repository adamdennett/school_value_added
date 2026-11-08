# 24_school_bus_penalty.R - What it costs to miss the school bus
# ---------------------------------------------------------------------------
# R/23 finds school buses. Here, for each one in the places routed by R/20: the Output
# Areas within 400 m of a stop on its morning trips are routed to that bus's school at
# every departure minute from 07:00, with all buses and walking available. So for each:
#   - the last time a child can leave and still arrive by the bell;
#   - whether that journey uses a school bus;
#   - how late they are if they miss it, by the next best journey, whatever that is;
#   - how long walking the whole way takes.
# School bells are not published in the data, so the bell is taken as 15 minutes after
# the last school bus arrives, with a check 10 minutes later. All pupils; nothing here is split by disadvantage.
#
# Sources and formulas: method.qmd#sec-journey-method. Run R/20 and R/23 first.
# Output: data/cache/journey/penalty_<network>.rds. Rscript R/24_school_bus_penalty.R
# ---------------------------------------------------------------------------

JH <- Sys.getenv("JAVA_HOME")
if (!nzchar(JH) || !dir.exists(JH)) { JH <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "rJavaEnv", "installed", "windows", "x64", "21"); Sys.setenv(JAVA_HOME = JH) }
options(java.parameters = paste0("-Xmx", Sys.getenv("R5R_HEAP", "24G")))
proj_db <- system.file("proj", package = "terra"); if (nzchar(proj_db)) Sys.setenv(PROJ_LIB = proj_db, PROJ_DATA = proj_db)
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(sf); library(r5r); library(data.table) })

J <- here::here("data", "cache", "journey"); NET <- here::here("data-raw", "r5", "net"); ROUTE_DATE <- as.Date("2026-11-17"); SPEED <- 3.6
FR <- readRDS(file.path(J, "frame.rds")); SB <- readRDS(here::here("data", "school_buses.rds"))
oa <- as.data.table(FR$oa); sch <- as.data.table(FR$sch)
ids <- sub("^flat_(.*)\\.rds$", "\\1", list.files(J, pattern = "^flat_")); want <- strsplit(Sys.getenv("NETS"), ",")[[1]]; if (length(want)) ids <- intersect(ids, want)
mm <- function(x) { p <- tstrsplit(x, ":", fixed = TRUE); as.numeric(p[[1]]) * 60 + as.numeric(p[[2]]) }

run_net <- function(id) {
  f_out <- file.path(J, paste0("penalty_", id, ".rds")); if (file.exists(f_out)) return(invisible())
  fl <- readRDS(file.path(J, paste0("flat_", id, ".rds"))); o <- oa[lad %in% fl$places]; dir_ <- file.path(NET, id)
  gz <- list.files(dir_, pattern = "^gtfs.*\\.zip$", full.names = TRUE)
  rd_g <- function(z, f, ...) { td <- tempfile(); dir.create(td); unzip(z, files = f, exdir = td); fread(file.path(td, f), colClasses = "character", ...) }
  tr <- rbindlist(lapply(gz, rd_g, f = "trips.txt", select = c("route_id", "trip_id"))); stt <- rbindlist(lapply(gz, rd_g, f = "stop_times.txt", select = c("trip_id", "stop_id", "stop_sequence", "arrival_time")))
  sp <- rbindlist(lapply(gz, rd_g, f = "stops.txt", select = c("stop_id", "stop_lat", "stop_lon")))[!duplicated(stop_id)]
  xy <- st_coordinates(st_transform(st_as_sf(sp[!is.na(as.numeric(stop_lat))], coords = c("stop_lon", "stop_lat"), crs = 4326), 27700)); sp <- sp[!is.na(as.numeric(stop_lat))][, `:=`(e = xy[, 1], n = xy[, 2])]
  rs <- as.data.table(SB$routes)[route_id %in% tr$route_id & am_trips > 0 & URN %in% fl$schools]
  if (!nrow(rs)) { message(id, ": no school buses"); saveRDS(list(id = id, pairs = NULL), f_out); return(invisible()) }
  # Stops on each route's morning trips, before it reaches school
  am <- merge(tr[route_id %in% rs$route_id], stt, by = "trip_id")[, .(trip_id, route_id, stop_id, seq = as.integer(stop_sequence), arr = mm(arrival_time))]
  am <- am[, if (max(arr) >= 6.5 * 60 & max(arr) <= 9.5 * 60) .SD, by = trip_id]; am <- am[am[, .I[seq < max(seq)], by = trip_id]$V1]
  pts <- merge(unique(am[, .(route_id, stop_id)]), sp[, .(stop_id, e, n)], by = "stop_id")
  link <- rbindlist(lapply(split(pts, pts$route_id), function(p) { nn <- nabor::knn(as.matrix(p[, .(e, n)]), as.matrix(o[, .(e, n)]), k = 1); o[nn$nn.dists[, 1] <= 400, .(oa21cd, kids, lad, walk_to_stop = nn$nn.dists[nn$nn.dists[, 1] <= 400, 1])][, route_id := p$route_id[1]] }))
  link <- merge(link, rs[, .(route_id, URN, route, am_trips, pm_trips, last_arr)], by = "route_id", allow.cartesian = TRUE)
  # Routes can serve schools near a place without stopping in it (London's 600 series, for Newham and Tower Hamlets)
  if (!nrow(link)) { message(id, ": school buses nearby, none stopping in the place"); saveRDS(list(id = id, pairs = NULL, routes_nearby = rs), f_out); return(invisible()) }
  bells <- link[, .(bell = max(last_arr, na.rm = TRUE) + 15), by = URN]; link <- merge(link, bells, by = "URN")
  message("\n=== ", id, ": ", uniqueN(rs$route_id), " school routes, ", uniqueN(link$URN), " schools, ", uniqueN(link$oa21cd), " Output Areas near a stop")
  net <- build_network(dir_, elevation = "NONE", verbose = FALSE)
  res <- rbindlist(lapply(unique(link$URN), function(u) { lk <- link[URN == u]; or <- oa[oa21cd %in% lk$oa21cd, .(id = oa21cd, lon, lat)]; de <- sch[URN == u, .(id = as.character(URN), lon, lat)]; bell <- lk$bell[1]
    e <- as.data.table(expanded_travel_time_matrix(net, origins = or, destinations = de, mode = c("WALK", "TRANSIT"), departure_datetime = as.POSIXct(paste(ROUTE_DATE, "07:00:00"), tz = "Europe/London"), time_window = 110, max_walk_time = 60, max_trip_duration = 120, walk_speed = SPEED, progress = FALSE))
    stopifnot(all(e$from_id %in% or$id)); e[, `:=`(d = mm(departure_time), a = mm(departure_time) + total_time)]
    # Walking is the same both ways on a flat network, so route from the school: one search, not hundreds
    w1 <- as.data.table(travel_time_matrix(net, origins = de, destinations = or, mode = "WALK", departure_datetime = as.POSIXct(paste(ROUTE_DATE, "08:00:00"), tz = "Europe/London"), max_trip_duration = 120, walk_speed = SPEED, progress = FALSE))
    data.table::setnames(w1, grep("travel_time", names(w1), value = TRUE)[1], "walk_t"); w1[, from_id := to_id]
    a <- e[order(from_id, d)][, { ok <- which(a <= bell)
      if (length(ok)) { L <- d[max(ok)]; nxt <- a[d > L]; .(last_leave = L, arrive = a[max(ok)], routes_last = routes[max(ok)], late = if (length(nxt)) min(nxt) - bell else NA_real_, n_arrivals = uniqueN(a[a <= bell])) } else .(last_leave = NA_real_, arrive = NA_real_, routes_last = NA_character_, late = NA_real_, n_arrivals = 0L) }, by = from_id]
    # The same with a bell 10 minutes later, since bells are inferred
    b2 <- bell + 10; a25 <- e[order(from_id, d)][, { ok <- which(a <= b2); if (length(ok)) { L <- d[max(ok)]; nxt <- a[d > L]; .(late25 = if (length(nxt)) min(nxt) - b2 else NA_real_) } else .(late25 = NA_real_) }, by = from_id]
    a <- merge(merge(a, a25, by = "from_id"), w1[, .(from_id, walk_t = as.numeric(walk_t))], by = "from_id", all.x = TRUE); a[, `:=`(URN = u, bell = bell)]; a }))
  stop_r5(net); rJava::.jgc(R.gc = TRUE)
  # Does the last workable journey use one of this school's buses?
  route_ids <- rs[, .(ids = list(unique(route_id))), by = URN]
  res <- merge(res, route_ids, by = "URN"); res[, uses_sb := mapply(function(s, i) !is.na(s) && any(vapply(i, function(x) grepl(paste0("\\b", x, "\\b"), s), TRUE)), routes_last, ids)]; res[, ids := NULL]
  saveRDS(list(id = id, pairs = res, link = link, routes = rs), f_out); message("  done")
}
for (id in ids) run_net(id)
message("Penalty complete")
