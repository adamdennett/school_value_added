# 23_school_buses.R - Dedicated school buses, from the timetables
# ---------------------------------------------------------------------------
# Some routes exist only for the school day: a few trips in the morning that end
# at a school and a few back in the afternoon, like the 72 from Whitehawk to Longhill.
# Miss one and there may be no other sensible way in. This finds them from the Bus
# Open Data Service GTFS files, for every region of England, and relates them to
# school absence.
#
# A route is a school bus on the routing date if it:
#   - has at most 8 trips that day and none outside 06:30 to 09:30 and 14:00 to 17:30;
#   - and has a morning trip whose last stop, or an afternoon trip whose first stop,
#     is within 1 km of a state secondary school, and no nearer a college.
# Headsigns naming a school are recorded as a stricter check. Timetables do not say
# who a bus is for, so this is a pattern and can catch a commuter extra near a school.
#
# Absence: DfE school level, autumn and spring 2025/26, for all pupils (no split by
# disadvantage is published). Same model as R/18, within local authority, with the
# distance of each school's nearest children and its White British share.
#
# Sources and formulas: method.qmd#sec-journey-method.
# Output: data/school_buses.rds (committed). Rscript R/23_school_buses.R
# ---------------------------------------------------------------------------

proj_db <- system.file("proj", package = "terra"); if (nzchar(proj_db)) Sys.setenv(PROJ_LIB = proj_db, PROJ_DATA = proj_db)
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(sf); library(fixest) })
setFixest_notes(FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
SRC <- here::here("data-raw", "r5", "src"); ROUTE_DATE <- as.Date("2026-11-17")
REGIONS <- sub("^gtfs_(.*)\\.zip$", "\\1", list.files(SRC, pattern = "^gtfs_.*\\.zip$"))
MAX_TRIPS <- 8; AM <- c(6.5, 9.5) * 60; PM <- c(14, 17.5) * 60; NEAR_M <- 1000
FR <- readRDS(here::here("data", "cache", "journey", "frame.rds")); sch <- as.data.table(FR$sch)
# Sixth form and further education colleges: a bus that ends nearer one of these is not a school's bus
g <- fread(here::here("data-raw", "gias", "edubasealldata20260610.csv"), select = c("EstablishmentStatus (name)", "PhaseOfEducation (name)", "EstablishmentTypeGroup (name)", "Easting", "Northing"), colClasses = "character", encoding = "Latin-1")
setnames(g, c("status", "phase", "group", "e", "n"))
colleges <- g[status == "Open" & (phase == "16 plus" | group == "Colleges"), .(e = num(e), n = num(n))][e > 0 & !is.na(e)]
mm <- function(x) { p <- tstrsplit(x, ":", fixed = TRUE); as.numeric(p[[1]]) * 60 + as.numeric(p[[2]]) }
WD <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

one_region <- function(region) {
  z <- file.path(SRC, paste0("gtfs_", region, ".zip")); td <- tempfile(); dir.create(td); fl <- unzip(z, list = TRUE)$Name
  unzip(z, files = intersect(c("routes.txt", "trips.txt", "stop_times.txt", "stops.txt", "agency.txt", "calendar.txt", "calendar_dates.txt"), fl), exdir = td)
  rd <- function(f, ...) if (f %in% fl) fread(file.path(td, f), colClasses = "character", ...) else NULL
  ro <- rd("routes.txt"); tr <- rd("trips.txt"); ag <- rd("agency.txt"); cal <- rd("calendar.txt"); cd <- rd("calendar_dates.txt"); sp <- rd("stops.txt")
  ymd <- function(x) as.Date(x, "%Y%m%d"); w <- WD[as.integer(format(ROUTE_DATE, "%u"))]
  on <- cal$service_id[cal[[w]] == "1" & ymd(cal$start_date) <= ROUTE_DATE & ymd(cal$end_date) >= ROUTE_DATE]
  if (!is.null(cd)) { dd <- cd[ymd(date) == ROUTE_DATE]; on <- union(setdiff(on, dd$service_id[dd$exception_type == "2"]), dd$service_id[dd$exception_type == "1"]) }
  tt <- tr[service_id %in% on]; st <- rd("stop_times.txt", select = c("trip_id", "stop_id", "stop_sequence", "departure_time", "arrival_time"))[trip_id %in% tt$trip_id]
  st[, seq := as.integer(stop_sequence)]; setorder(st, trip_id, seq)
  ends <- st[, .(first_stop = stop_id[1], last_stop = stop_id[.N], dep = mm(departure_time[1]), arr = mm(arrival_time[.N]), n_stops = .N), by = trip_id]
  x <- merge(tt[, .(trip_id, route_id, trip_headsign)], ends, by = "trip_id")
  x <- merge(x, ro[, .(route_id, agency_id, route_short_name)], by = "route_id"); x <- merge(x, ag[, .(agency_id, agency_name)], by = "agency_id", all.x = TRUE)
  x[, slot := fcase(dep >= AM[1] & dep <= AM[2], "am", dep >= PM[1] & dep <= PM[2], "pm", default = "other")]
  routes_all <- x[, .(trips = .N, other = sum(slot == "other")), by = route_id]; cand <- routes_all[trips <= MAX_TRIPS & other == 0, route_id]
  y <- x[route_id %in% cand]
  # nearest school to the last stop of morning trips, and the first stop of afternoon trips
  sp <- sp[, .(stop_id, stop_name, lat = num(stop_lat), lon = num(stop_lon))][!is.na(lat)]
  xy <- st_coordinates(st_transform(st_as_sf(sp, coords = c("lon", "lat"), crs = 4326), 27700)); sp[, `:=`(e = xy[, 1], n = xy[, 2])]
  y[, end_stop := ifelse(slot == "am", last_stop, first_stop)]
  y <- merge(y, sp[, .(end_stop = stop_id, stop_name, e, n)], by = "end_stop"); y <- y[!is.na(e)]
  nn <- nabor::knn(as.matrix(sch[, .(e, n)]), as.matrix(y[, .(e, n)]), k = 1)
  nc <- nabor::knn(as.matrix(colleges), as.matrix(y[, .(e, n)]), k = 1)
  y[, `:=`(URN = sch$URN[nn$nn.idx[, 1]], metres = nn$nn.dists[, 1], college_m = nc$nn.dists[, 1])]
  n_college <- y[metres <= NEAR_M & college_m < metres, uniqueN(route_id)]; y <- y[metres <= NEAR_M & metres <= college_m]
  named <- "school|academy|college|high$|grammar|grounds|sixth"
  y[, named := grepl(named, trip_headsign, ignore.case = TRUE) | grepl(named, stop_name, ignore.case = TRUE)]
  res <- y[, .(region = region, agency = agency_name[1], route = route_short_name[1], am_trips = sum(slot == "am"), pm_trips = sum(slot == "pm"), first_arr = if (any(slot == "am")) min(arr[slot == "am"]) else NA_real_, last_arr = if (any(slot == "am")) max(arr[slot == "am"]) else NA_real_,
               first_dep_pm = if (any(slot == "pm")) min(dep[slot == "pm"]) else NA_real_, last_dep_pm = if (any(slot == "pm")) max(dep[slot == "pm"]) else NA_real_, metres = min(metres), named = any(named), head = trip_headsign[1]), by = .(route_id, URN)]
  message(region, ": ", format(nrow(tt), big.mark = ","), " trips, ", length(cand), " candidate routes, ", uniqueN(res$route_id), " school routes, ", uniqueN(res$URN), " schools")
  list(res = res, summary = data.table(region = region, trips = nrow(tt), routes = uniqueN(tt$route_id), candidates = length(cand), college_routes = n_college, school_routes = uniqueN(res$route_id), schools = uniqueN(res$URN)))
}
R <- lapply(REGIONS, one_region)
routes <- rbindlist(lapply(R, `[[`, "res")); regions <- rbindlist(lapply(R, `[[`, "summary"))
# A route in two regions' files (operators can appear in both) counts once
routes <- routes[order(-am_trips)][, .SD[1], by = .(agency, route, URN)]

by_school <- routes[, .(routes = .N, am_trips = sum(am_trips), pm_trips = sum(pm_trips), named_routes = sum(named), first_arr = min(first_arr, na.rm = TRUE), last_arr = max(last_arr, na.rm = TRUE), route_list = paste(sort(unique(route)), collapse = ", ")), by = URN]
by_school[!is.finite(first_arr), first_arr := NA]; by_school[!is.finite(last_arr), last_arr := NA]

# ---- Against absence: every state secondary, all pupils ---------------------------
frame <- as.data.table(readRDS(here::here("data", "absence_school_frame.rds")))[, URN := as.integer(URN)]
# Each school's nearest children, as R/18, on the current network
oa <- as.data.table(readRDS(here::here("data", "cache", "oa_pwc_2021.rds")))[, `:=`(e = num(easting), n = num(northing))][!is.na(e)]
k <- fread(here::here("data-raw", "census2021", "ts007a", "census2021-ts007a-oa.csv"), select = c("geography code", "Age: Aged 10 to 14 years")); setnames(k, c("oa21cd", "kids"))
oa <- merge(oa[startsWith(oa21cd, "E")], k, by = "oa21cd"); nn <- nabor::knn(as.matrix(sch[, .(e, n)]), as.matrix(oa[, .(e, n)]), k = 1)
oa[, `:=`(URN = sch$URN[nn$nn.idx[, 1]], d = nn$nn.dists[, 1] / 1000)]
catch <- oa[, .(km = weighted.mean(d, pmax(kids, 1)), kids = sum(kids)), by = URN]
d <- merge(frame, catch, by = "URN"); d <- merge(d, by_school[, .(URN, routes, am_trips, pm_trips, named_routes, route_list)], by = "URN", all.x = TRUE)
d[, `:=`(has_bus = !is.na(routes) & am_trips > 0, trips_am = fifelse(is.na(am_trips), 0, am_trips), routes_n = fifelse(is.na(routes), 0, routes), l2 = log2(km))]
d[, selective := grepl("Selective", ADMPOL_PT)]
d <- d[kids >= 100 & !selective & !is.na(overall)][, fsm_c := fsm - mean(fsm, na.rm = TRUE)]
CTRL <- "fsm + eal + wbri + ks2_c + z_l4 + sen_sup + ehcp + mobile + girls + age16"
OUT <- c(overall = "Overall absence", illness = "Illness", unauth = "Unauthorised", persistent = "Persistent absence (% of pupils)")
fit <- function(y, x, dat, fe = "la", extra = "l2 + place") { f <- as.formula(paste0(y, " ~ ", x, " + ", extra, " + ", CTRL, " | ", fe)); m <- feols(f, dat, cluster = ~la); nm <- names(coef(m))[startsWith(names(coef(m)), strsplit(x, " ")[[1]][1])]
  data.table(term = nm, b = coef(m)[nm], se = se(m)[nm], n = m$nobs) }
samples <- list(`All schools` = d, `Urban` = d[grepl("^Urban", place)], `Rural` = d[!grepl("^Urban", place)], `With high FSM` = d[fsm >= quantile(fsm, 0.75, na.rm = TRUE)])
models <- rbindlist(lapply(names(OUT), function(y) rbindlist(lapply(names(samples), function(s) { dd <- samples[[s]][!is.na(get(y))]
  rbind(fit(y, "has_bus", dd)[, spec := "Has a school bus"], fit(y, "has_bus", dd, extra = "place")[, spec := "Has a school bus, without distance"], fit(y, "log1p(trips_am)", dd)[, spec := "Morning school bus trips (log)"]) [, `:=`(outcome = y, sample = s)] }))))
inter <- rbindlist(lapply(names(OUT), function(y) { dd <- d[!is.na(get(y))]; m <- feols(as.formula(paste0(y, " ~ has_bus * fsm_c + l2 + place + eal + wbri + ks2_c + z_l4 + sen_sup + ehcp + mobile + girls + age16 | la")), dd, cluster = ~la)
  data.table(outcome = y, b_bus = coef(m)[["has_busTRUE"]], se_bus = se(m)[["has_busTRUE"]], b_int = coef(m)[["has_busTRUE:fsm_c"]], se_int = se(m)[["has_busTRUE:fsm_c"]], n = m$nobs) }))
desc <- d[, .(schools = .N, overall = mean(overall), unauth = mean(unauth), persistent = mean(persistent, na.rm = TRUE), km = mean(km), fsm = mean(fsm, na.rm = TRUE)), by = has_bus]

out <- list(built = Sys.Date(), date = ROUTE_DATE, regions = regions, routes = routes, by_school = by_school, models = models, inter = inter, desc = desc, n_schools = nrow(d), rules = list(max_trips = MAX_TRIPS, am = AM, pm = PM, near_m = NEAR_M))
saveRDS(out, here::here("data", "school_buses.rds")); message("Saved data/school_buses.rds")
