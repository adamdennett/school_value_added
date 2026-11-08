# 20_journey_times.R - Journey times to school on the real networks (routing)
# ---------------------------------------------------------------------------
# R/18 and R/19 measure distance in straight lines. This routes the same
# question on real streets and bus timetables with r5r, for Brighton and Hove
# and its comparator places, so that hills, the sea, street layouts and bus
# services are no longer missing. R/21_journey_analysis.R does the analysis.
#
#   Places: Brighton and Hove, Hastings, Portsmouth, Southampton, Bristol,
#     Plymouth, Blackpool, Newham, Tower Hamlets (the districts of distance.qmd).
#   Origins: every Census 2021 Output Area in each place, at its population-
#     weighted centroid.
#   Destinations: every open state-funded secondary school (the definition of
#     R/18) within 15 km of the place, so that schools over the boundary count.
#   Scenarios:
#     walk_bus   walking and bus, leaving 07:45 to 08:15, the median of those
#                departures (time_window 30); walking at r5r's default 3.6 km/h
#     walk       walking alone, same speed
#     walk_bus_f walk_bus at 4.8 km/h, as a check on walking speed
#     bh_model   the Brighton and Hove model's settings (one departure, 08:00),
#                to compare with it
#     All with a 60 minute walk limit and 120 minute trip limit.
#   Hills are measured along the routes in R/22_journey_structure.R. r5r's own
#     elevation option averages both directions and did not change walking times
#     in our tests (r5r 2.3.0, R5 7.4), so networks here are built without it.
#
# Sources (method.qmd#sec-journey-method documents each step):
#   OpenStreetMap county extracts, Geofabrik
#   Bus Open Data Service GTFS regional files, Department for Transport
#   Get Information About Schools, June 2026; Census 2021; ONS lookups (as R/18)
#
# Traps inherited from bh_school_admissions_open/public/R/00b_build_travel_matrix.R:
#   a network folder must hold exactly one .pbf; Brighton and Hove is in the West
#   Sussex extract; a stale network.dat keeps routing on the old data. And one found
#   here: walk-only matrices can come back with origins and destinations swapped.
#
# Requires Java 21 (rJavaEnv::java_quick_install(version = 21)) and ~40 GB heap
# for all networks. data-raw/r5 is not committed; outputs go to data/cache.
#
# Rscript R/20_journey_times.R                 route every network
# NETS=hastings Rscript R/20_journey_times.R   route one
# ---------------------------------------------------------------------------

JH <- Sys.getenv("JAVA_HOME")
if (!nzchar(JH) || !dir.exists(JH)) {
  JH <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "rJavaEnv", "installed", "windows", "x64", "21")
  Sys.setenv(JAVA_HOME = JH)
}
options(java.parameters = paste0("-Xmx", Sys.getenv("R5R_HEAP", "40G")))

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(sf); library(r5r) })
num <- function(x) suppressWarnings(as.numeric(x))
wm <- function(x, w) sum(x * w) / sum(w)

SRC <- here::here("data-raw", "r5", "src")
NET_ROOT <- here::here("data-raw", "r5", "net")
OUT <- here::here("data", "cache", "journey"); dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(NET_ROOT, showWarnings = FALSE, recursive = TRUE)
# A school-term Tuesday: after half term and before Christmas 2026
TARGET_DATE <- as.Date(Sys.getenv("R5R_DATE", "2026-11-17"))

NETS <- tibble::tribble(
  ~id,         ~pbfs,                                    ~gtfs,        ~places,
  "bh",        c("west-sussex", "east-sussex"),          "south_east", "Brighton and Hove",
  "hastings",  "east-sussex",                            "south_east", "Hastings",
  "hampshire", "hampshire",                              c("south_west", "south_east"), c("Portsmouth", "Southampton"),
  "bristol",   c("bristol", "gloucestershire", "somerset"), "south_west", "Bristol",
  "plymouth",  c("devon", "cornwall"),                   "south_west", "Plymouth",
  "blackpool", "lancashire",                             "north_west", "Blackpool",
  "london",    "greater-london",                         "london",     c("Newham", "Tower Hamlets"))
want <- strsplit(Sys.getenv("NETS"), ",")[[1]]
if (length(want)) NETS <- NETS[NETS$id %in% want, ]
ALL_PLACES <- c("Brighton and Hove", "Hastings", "Portsmouth", "Southampton", "Bristol", "Plymouth", "Blackpool", "Newham", "Tower Hamlets")

# ---- 1. Frame: Output Areas and schools (shared with R/21) ----------------------
frame_f <- file.path(OUT, "frame.rds")
if (!file.exists(frame_f)) {
  ons <- here::here("data-raw", "census2021", "ons_custom")
  oa <- readRDS(here::here("data", "cache", "oa_pwc_2021.rds")) |> mutate(e = num(easting), n = num(northing)) |>
    inner_join(readr::read_csv(file.path(ons, "oa_lsoa_msoa_2021.csv"), show_col_types = FALSE) |> transmute(oa21cd = OA21CD, lsoa = LSOA21CD), by = "oa21cd") |>
    inner_join(readr::read_csv(file.path(ons, "lsoa_lad23.csv"), show_col_types = FALSE) |> transmute(lsoa = LSOA21CD, lad = LAD23NM), by = "lsoa")
  k <- readr::read_csv(here::here("data-raw", "census2021", "ts007a", "census2021-ts007a-oa.csv"), show_col_types = FALSE)
  oa <- oa |> inner_join(tibble(oa21cd = k$`geography code`, kids = k$`Age: Aged 10 to 14 years`), by = "oa21cd") |> filter(lad %in% ALL_PLACES) |> select(oa21cd, lsoa, lad, e, n, kids)
  g <- readr::read_csv(here::here("data-raw", "gias", "edubasealldata20260610.csv"), col_types = readr::cols(.default = "c"), progress = FALSE, name_repair = "unique_quiet")
  sch <- g |> filter(`EstablishmentStatus (name)` == "Open", `EstablishmentTypeGroup (name)` %in% c("Academies", "Free Schools", "Local authority maintained schools"),
                     `PhaseOfEducation (name)` %in% c("Secondary", "All-through", "Middle deemed secondary")) |>
    transmute(URN = as.integer(URN), name = EstablishmentName, la = `LA (name)`, low = num(StatutoryLowAge), high = num(StatutoryHighAge), admissions = `AdmissionsPolicy (name)`,
              religion = `ReligiousCharacter (name)`, e = num(Easting), n = num(Northing)) |>
    filter(high >= 16, low <= 13, !is.na(e), e > 0) |>
    mutate(selective = grepl("^Selective", admissions), faith = !is.na(religion) & !religion %in% c("None", "Does not apply", "Not applicable"), open_to_all = !selective & !faith)
  to_ll <- function(d) { x <- st_coordinates(st_transform(st_as_sf(d, coords = c("e", "n"), crs = 27700, remove = FALSE), 4326)); d$lon <- x[, 1]; d$lat <- x[, 2]; d }
  saveRDS(list(oa = to_ll(oa), sch = to_ll(sch)), frame_f)
}
FR <- readRDS(frame_f)
message(nrow(FR$oa), " Output Areas in ", n_distinct(FR$oa$lad), " places; ", nrow(FR$sch), " schools in England")

# ---- 2. Inputs for each network --------------------------------------------------
merge_pbf <- function(inputs, output) {
  tool <- Sys.getenv("OSM_MERGE_TOOL", file.path(dirname(here::here()), "osmosis-0.49.2", "bin", "osmosis.bat"))
  stopifnot(file.exists(tool))
  args <- c(as.vector(rbind("--read-pbf", paste0("file=", normalizePath(inputs, winslash = "/")))), rep("--merge", length(inputs) - 1), "--write-pbf", paste0("file=", normalizePath(output, winslash = "/", mustWork = FALSE)))
  status <- system2(tool, args, stdout = FALSE, stderr = FALSE)
  if (status != 0 || !file.exists(output)) stop("osmosis merge failed: ", output)
}

#' Trips running on a date, from calendar and calendar_dates
trips_on <- function(z, dates) {
  td <- tempfile(); dir.create(td); fl <- unzip(z, list = TRUE)$Name
  unzip(z, files = intersect(c("calendar.txt", "calendar_dates.txt", "trips.txt"), fl), exdir = td)
  rd <- function(f) if (f %in% fl) data.table::fread(file.path(td, f), colClasses = "character") else NULL
  cal <- rd("calendar.txt"); cd <- rd("calendar_dates.txt"); tr <- rd("trips.txt")
  WD <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
  ymd <- function(x) as.Date(x, "%Y%m%d")
  res <- vapply(dates, function(d) {
    base <- character(0)
    if (!is.null(cal)) base <- cal$service_id[cal[[WD[as.integer(format(d, "%u"))]]] == "1" & ymd(cal$start_date) <= d & ymd(cal$end_date) >= d]
    if (!is.null(cd)) { dd <- cd[ymd(cd$date) == d, ]; base <- union(setdiff(base, dd$service_id[dd$exception_type == "2"]), dd$service_id[dd$exception_type == "1"]) }
    sum(tr$service_id %in% base)
  }, 1L)
  tibble(date = dates, trips = res)
}

#' Keep only trips that serve the area, so networks build quickly
gtfs_for <- function(region, ids_bbox, out) {
  if (file.exists(out)) return(out)
  g <- gtfstools::read_gtfs(file.path(SRC, paste0("gtfs_", region, ".zip")))
  bb <- st_as_sfc(st_bbox(ids_bbox)); f <- gtfstools::filter_by_spatial_extent(g, bb)
  gtfstools::write_gtfs(f, out); message("  GTFS ", region, ": ", format(nrow(g$trips), big.mark = ","), " trips, kept ", format(nrow(f$trips), big.mark = ","))
  out
}

# ---- 3. Route ---------------------------------------------------------------------
SCEN <- list(
  walk_bus = list(mode = c("WALK", "TRANSIT"), clock = "07:45:00", window = 30, speed = 3.6),
  walk = list(mode = "WALK", clock = "07:45:00", window = 1, speed = 3.6),
  walk_bus_f = list(mode = c("WALK", "TRANSIT"), clock = "07:45:00", window = 30, speed = 4.8),
  bh_model = list(mode = c("WALK", "TRANSIT"), clock = "08:00:00", window = 1, speed = 3.6))

route_net <- function(id, pbfs, gtfs, places) {
  res_f <- file.path(OUT, paste0("flat_", id, ".rds"))
  if (file.exists(res_f)) { message(id, ": already routed"); return(invisible()) }
  t0 <- Sys.time(); message("\n=== ", id, ": ", paste(places, collapse = ", "))
  dir_ <- file.path(NET_ROOT, id); dir.create(dir_, showWarnings = FALSE, recursive = TRUE)
  o <- FR$oa |> filter(lad %in% places)
  box <- c(xmin = min(o$e) - 15000, ymin = min(o$n) - 15000, xmax = max(o$e) + 15000, ymax = max(o$n) + 15000)
  s <- FR$sch |> filter(e >= box[["xmin"]], e <= box[["xmax"]], n >= box[["ymin"]], n <= box[["ymax"]])
  message("  ", nrow(o), " origins, ", nrow(s), " schools within 15 km")
  # one pbf only
  pbf <- file.path(dir_, "network.osm.pbf")
  if (!file.exists(pbf)) {
    inp <- file.path(SRC, paste0(pbfs, ".osm.pbf"))
    if (length(inp) == 1) file.copy(inp, pbf) else merge_pbf(inp, pbf)
    message("  OSM: ", paste(pbfs, collapse = " + "), sprintf(" (%.0f MB)", file.size(pbf) / 1024^2))
  }
  bb30 <- st_transform(st_as_sfc(st_bbox(c(box["xmin"] - 15000, box["ymin"] - 15000, box["xmax"] + 15000, box["ymax"] + 15000), crs = 27700)), 4326)
  # BODS regions follow operators, not places: Portsmouth and Southampton's buses are in the South West
  # file (342 and 355 stops within 3 km of the centre, against 1 and 76 in the South East file).
  gz <- vapply(gtfs, function(r) {
    gtfs_for(r, bb30, file.path(dir_, paste0("gtfs_", r, ".zip")))
  }, "")
  cand <- seq(TARGET_DATE - 14, TARGET_DATE + 14, by = "day"); cand <- cand[format(cand, "%u") %in% c("2", "3", "4")]
  svc <- Reduce(function(x, y) transform(x, trips = x$trips + y$trips), lapply(gz, trips_on, dates = cand)); n_day <- svc$trips[svc$date == TARGET_DATE]
  message("  trips on ", TARGET_DATE, ": ", format(n_day, big.mark = ","), "; range over nearby school days ", paste(range(svc$trips), collapse = " to "))
  if (n_day == 0) stop("No bus trips on ", TARGET_DATE, " in ", id)
  # stale network trap
  nd <- file.path(dir_, "network.dat")
  if (file.exists(nd) && max(file.mtime(list.files(dir_, pattern = "\\.(pbf|zip|tif)$", full.names = TRUE))) > file.mtime(nd)) file.remove(nd)
  net <- build_network(dir_, elevation = "NONE", verbose = FALSE)
  or <- o |> transmute(id = oa21cd, lon, lat); de <- s |> transmute(id = as.character(URN), lon, lat)
  mats <- lapply(names(SCEN), function(k) {
    p <- SCEN[[k]]; message("  routing ", k)
    tt <- function(orig) data.table::as.data.table(travel_time_matrix(net, origins = orig, destinations = de, mode = p$mode, departure_datetime = as.POSIXct(paste(TARGET_DATE, p$clock), tz = "Europe/London"),
                            time_window = p$window, percentiles = 50, max_walk_time = if (identical(p$mode, "WALK")) 120 else 60, max_trip_duration = 120, walk_speed = p$speed, progress = FALSE))
    # With no transit and more origins than destinations, r5r searches backwards from the
    # destinations and returns the ids swapped. Walking uphill is not walking downhill, so
    # keep batches no larger than the number of schools and check the ids that come back.
    x <- if (identical(p$mode, "WALK")) data.table::rbindlist(lapply(split(seq_len(nrow(or)), ceiling(seq_len(nrow(or)) / nrow(de))), function(i) tt(or[i, ]))) else tt(or)
    stopifnot(all(x$from_id %in% or$id), all(x$to_id %in% de$id))
    data.table::setnames(x, grep("travel_time", names(x), value = TRUE)[1], "t")
    x <- x[order(from_id, t)][, .(to_id = to_id[seq_len(min(.N, 5))], t = t[seq_len(min(.N, 5))]), by = from_id]; x[, scenario := k]; x
  })
  stop_r5(net); rJava::.jgc(R.gc = TRUE)
  saveRDS(list(id = id, places = places, times = data.table::rbindlist(mats), date = TARGET_DATE, trips = n_day, trips_range = range(svc$trips), n_origins = nrow(o), n_schools = nrow(s), schools = s$URN,
               pbfs = pbfs, gtfs = gtfs, osm_date = file.mtime(file.path(SRC, paste0(pbfs[1], ".osm.pbf"))), built = Sys.time()), res_f)
  message(sprintf("  done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
for (i in seq_len(nrow(NETS))) route_net(NETS$id[i], NETS$pbfs[[i]], NETS$gtfs[[i]], NETS$places[[i]])
message("Routing complete")
