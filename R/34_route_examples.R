# 34_route_examples.R - What the journeys look like: example routes for the maps
# ---------------------------------------------------------------------------
# The journey sections report averages. This picks example journeys, by rule, and routes
# each again with r5r to keep the full path of every walking and bus leg, for maps:
#
#   Whitehawk      the Output Area in Whitehawk (House of Commons Library MSOA name) whose
#                  walking and bus time is nearest the child-weighted mean of Whitehawk's
#                  Output Areas, to its quickest school and to Dorothy Stringer, where the
#                  2024 map sends more of its children.
#   Comparators    in each comparator city, the Output Area in the most deprived third of its
#                  neighbourhoods whose time is nearest that third's child-weighted mean: a
#                  typical journey for a deprived neighbourhood there.
#
# Each journey is routed every five minutes from 07:45 to 08:15 on the routing date of R/27,
# on the same networks and settings, and the departure with the median time is kept.
# Climb along each walking leg is read from OS Terrain 50 every 25 m.
#
# Sources and formulas: method.qmd#sec-route-examples-method. Needs R/26 and R/27 run first
# (networks and journey times). Output: data/route_examples.rds (committed).
# R5R_HEAP=18G Rscript R/34_route_examples.R
# ---------------------------------------------------------------------------

JH <- Sys.getenv("JAVA_HOME")
if (!nzchar(JH) || !dir.exists(JH)) { JH <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "rJavaEnv", "installed", "windows", "x64", "21"); Sys.setenv(JAVA_HOME = JH) }
options(java.parameters = paste0("-Xmx", Sys.getenv("R5R_HEAP", "18G")))
proj_db <- system.file("proj", package = "terra"); if (nzchar(proj_db)) Sys.setenv(PROJ_LIB = proj_db, PROJ_DATA = proj_db)
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(sf); library(r5r); library(data.table) })
J <- here::here("data", "cache", "journey_la"); NET <- here::here("data-raw", "r5", "net_la"); raw <- here::here("data-raw", "census2021")
FR <- readRDS(file.path(J, "frame.rds")); oa <- as.data.table(FR$oa); sch <- as.data.table(FR$sch)
ROUTE_DATE <- as.Date("2026-11-17"); SPEED <- 3.6
dem <- terra::rast(here::here("data", "cache", "journey", "terrain50_gb.vrt"))
CITIES <- c(`Brighton and Hove` = "sussex", Bristol = "bristol", Southampton = "solent", Plymouth = "devon")
DS <- sch[name == "Dorothy Stringer School", URN]; stopifnot(length(DS) == 1)

# ---- The most deprived third, as R/29 (Census 2021 TS011, households deprived in 2+ dimensions) ----
t11 <- fread(file.path(raw, "ts011", "census2021-ts011-lsoa.csv")); setnames(t11, sub("; measures: Value$", "", names(t11)))
dc <- grep("deprived in (two|three|four)", names(t11), value = TRUE)
dep <- data.table(lsoa = t11$`geography code`, dep2 = rowSums(t11[, ..dc]) / t11[[grep("Total: All households", names(t11), value = TRUE)]])
oa <- merge(oa, dep, by = "lsoa")
th <- unique(oa[, .(la, lsoa, dep2)])[, third := dep2 >= quantile(dep2, 2 / 3), by = la]; oa <- merge(oa, th[, .(lsoa, third)], by = "lsoa")
# Neighbourhood names
hcl <- fread(here::here("data-raw", "msoa_names", "MSOA-Names-2.2.csv"), encoding = "UTF-8")[, .(MSOA21CD = msoa21cd, area = msoa21hclnm)]
lk <- unique(fread(file.path(raw, "ons_custom", "oa_lsoa_msoa_2021.csv"))[, .(oa21cd = OA21CD, MSOA21CD)])
oa <- merge(oa, merge(lk, hcl, by = "MSOA21CD")[, .(oa21cd, area)], by = "oa21cd", all.x = TRUE)

# ---- Journey times already routed (R/27): quickest school by walking and bus ---------------------
quick <- function(net) { f <- readRDS(file.path(J, paste0("flat_", net, ".rds")))$times
  as.data.table(f)[scenario == "walk_bus"][order(from_id, t)][, .(t_bus = t[1], school = to_id[1]), by = from_id] }
# The example is the Output Area nearest the group's mean, among those whose quickest school is
# open to all (neither faith nor selective), so that it is a school any child could attend
open_urn <- sch[open_to_all == TRUE, as.character(URN)]
pick <- function(d) { m <- weighted.mean(d$t_bus, d$kids); d <- d[school %in% open_urn]; d[which.min(abs(t_bus - m))] }
ex <- list()
for (city in names(CITIES)) {
  net <- CITIES[[city]]; q <- quick(net); d <- merge(oa[la == city], q, by.x = "oa21cd", by.y = "from_id")[!is.na(t_bus)]
  if (city == "Brighton and Hove") {
    w <- pick(d[area == "Whitehawk"])
    ex[[length(ex) + 1]] <- data.table(id = "wh_quick", label = "Whitehawk to its quickest school", city, net, oa21cd = w$oa21cd, area = w$area, URN = as.integer(w$school), t_mean = d[area == "Whitehawk", weighted.mean(t_bus, kids)], t_bus = w$t_bus)
    ex[[length(ex) + 1]] <- data.table(id = "wh_ds", label = "Whitehawk to Dorothy Stringer", city, net, oa21cd = w$oa21cd, area = w$area, URN = DS, t_mean = NA_real_, t_bus = NA_real_)
  }
  p <- pick(d[third == TRUE])
  ex[[length(ex) + 1]] <- data.table(id = paste0("typ_", net), label = paste0("A typical deprived journey, ", city), city, net, oa21cd = p$oa21cd, area = p$area, URN = as.integer(p$school),
                                     t_mean = d[third == TRUE, weighted.mean(t_bus, kids)], t_bus = p$t_bus)
}
EX <- rbindlist(ex); EX <- merge(EX, oa[, .(oa21cd, lon, lat, kids)], by = "oa21cd"); EX <- merge(EX, sch[, .(URN, school = name, s_lon = lon, s_lat = lat)], by = "URN")
print(EX[, .(id, city, area, oa21cd, school, t_mean, t_bus)])

# ---- Route each, keeping every leg ---------------------------------------------------------------
climb <- function(g) { # metres up along a line, in its own direction, from Terrain 50 every 25 m
  g <- st_transform(st_geometry(g), 27700)
  pts <- st_cast(st_segmentize(g, 25), "POINT")   # every vertex, and points no more than 25 m apart
  z <- terra::extract(dem, terra::vect(pts))[[2]]; z <- z[!is.na(z)]
  if (length(z) < 2) return(NA_real_); sum(pmax(diff(z), 0)) }
# Bus route numbers as passengers know them, from the network's own timetable files
route_names <- function(nw) rbindlist(lapply(list.files(file.path(NET, nw), pattern = "\\.zip$", full.names = TRUE), function(z) {
  r <- fread(text = readLines(unz(z, "routes.txt"), warn = FALSE), colClasses = "character")
  r[, .(route = route_id, short = route_short_name)] }), fill = TRUE)
# Every five minutes from 07:45 to 08:15, as the headline times' window; the example keeps the
# departure with the median total time, so it is neither a lucky nor an unlucky connection
DEPS <- sprintf("%s:00", c("07:45", "07:50", "07:55", "08:00", "08:05", "08:10", "08:15"))
# The methodology's worked example: Whitehawk to its quickest school, which is the school every
# journey measure (structure, missing a journey, points of interest) is calculated for
WX <- "wh_quick"; WX_NET <- EX[id == WX, net]
legs <- list(); deps <- list()
for (nw in unique(EX$net)) {
  r5 <- build_network(file.path(NET, nw), verbose = FALSE); rn <- unique(route_names(nw), by = "route")
  E <- EX[net == nw]
  for (i in seq_len(nrow(E))) {
    e <- E[i]
    tries <- lapply(DEPS, function(dp) {
      x <- detailed_itineraries(r5, origins = data.frame(id = e$oa21cd, lon = e$lon, lat = e$lat), destinations = data.frame(id = as.character(e$URN), lon = e$s_lon, lat = e$s_lat),
                                mode = c("WALK", "TRANSIT"), departure_datetime = as.POSIXct(paste(ROUTE_DATE, dp), tz = "Europe/London"),
                                max_walk_time = 60, max_trip_duration = 120, walk_speed = SPEED, shortest_path = TRUE, drop_geometry = FALSE, progress = FALSE)
      x <- st_as_sf(x); x$dep <- substr(dp, 1, 5); x })
    tot <- vapply(tries, function(x) x$total_duration[1], 0)
    k <- order(tot)[ceiling(length(tot) / 2)]; x <- tries[[k]]; x$id <- e$id
    deps[[e$id]] <- data.table(id = e$id, dep = substr(DEPS, 1, 5), total = tot, chosen = seq_along(tot) == k)
    x$route_no <- rn$short[match(x$route, rn$route)]
    x$climb <- vapply(seq_len(nrow(x)), function(j) if (x$mode[j] == "WALK") climb(x[j, ]) else NA_real_, 0)
    legs[[e$id]] <- x[, c("id", "dep", "segment", "mode", "segment_duration", "wait", "distance", "route", "route_no", "total_duration", "climb")]
  }
  # The worked example: every departure minute from 07:00, as R/27's missed-journey step
  if (nw == WX_NET) {
    e <- EX[id == WX]
    mm <- as.data.table(expanded_travel_time_matrix(r5, origins = data.frame(id = e$oa21cd, lon = e$lon, lat = e$lat), destinations = data.frame(id = as.character(e$URN), lon = e$s_lon, lat = e$s_lat),
                                                    mode = c("WALK", "TRANSIT"), departure_datetime = as.POSIXct(paste(ROUTE_DATE, "07:00:00"), tz = "Europe/London"), time_window = 110,
                                                    max_walk_time = 60, max_trip_duration = 120, walk_speed = SPEED, progress = FALSE))
    mins <- function(tt) as.numeric(substr(tt, 1, 2)) * 60 + as.numeric(substr(tt, 4, 5))   # as R/27
    by_minute <- mm[, .(leave = mins(departure_time), t = total_time)][, .(t = min(t)), by = leave][, arrive := leave + t][order(leave)]
  }
  stop_r5(r5); rJava::.jgc(R.gc = TRUE)
}
L <- do.call(rbind, legs); L <- st_transform(L, 4326)
# Each journey's parts, in minutes
S <- as.data.table(st_drop_geometry(L))[, .(total = total_duration[1], walk = sum(segment_duration[mode == "WALK"]), wait = sum(wait), bus = sum(segment_duration[mode != "WALK"]),
                                            buses = sum(mode != "WALK"), walk_m = sum(distance[mode == "WALK"]), climb = sum(climb, na.rm = TRUE),
                                            routes = paste(route_no[mode != "WALK"], collapse = " then "), dep = dep[1]), by = id]
S <- merge(EX[, .(id, label, city, area, oa21cd, school, t_mean, t_bus, kids)], S, by = "id")
S[, km_line := as.numeric(st_distance(st_as_sf(EX[match(S$id, EX$id)], coords = c("lon", "lat"), crs = 4326), st_as_sf(EX[match(S$id, EX$id)], coords = c("s_lon", "s_lat"), crs = 4326), by_element = TRUE)) / 1000]
ends <- EX[, .(id, lon, lat, s_lon, s_lat)]

# ---- The worked example, for the methodology ----------------------------------------------------
e <- EX[id == WX]; home <- st_transform(st_as_sf(e, coords = c("lon", "lat"), crs = 4326), 27700)
# 1. The walking route to the quickest school on foot (R/27's walk step), and its height profile
wk <- readRDS(file.path(J, paste0("walk_", WX_NET, ".rds")))
wroute <- wk$route_lines[wk$route_lines$from_id == e$oa21cd, ]; wstat <- as.data.table(wk$walk_route)[id == e$oa21cd]
profile <- function(g) { g <- st_transform(st_geometry(g), 27700); pts <- st_cast(st_segmentize(g, 25), "POINT"); cc <- st_coordinates(pts)
  data.table(d = c(0, cumsum(sqrt(diff(cc[, 1])^2 + diff(cc[, 2])^2))), z = terra::extract(dem, terra::vect(pts))[[2]]) }
wprof <- profile(wroute)
# 2. Stops on the itinerary: where the child boards, changes and gets off
lx <- st_transform(L[L$id == WX, ], 27700); bx <- lx[lx$mode != "WALK", ]
ends_of <- function(k, w) { cc <- st_coordinates(bx[k, ]); cc[if (w == "on") 1 else nrow(cc), 1:2] }
st_tab <- rbindlist(lapply(seq_len(nrow(bx)), function(k) data.table(k = k, role = c(if (k == 1) "board" else "change", if (k == nrow(bx)) "alight" else "change"),
                                                                    x = c(ends_of(k, "on")[1], ends_of(k, "off")[1]), y = c(ends_of(k, "on")[2], ends_of(k, "off")[2]))))
stops_x <- st_as_sf(st_tab, coords = c("x", "y"), crs = 27700)
# 3. Points of interest near any of it (R/28's extract), flagged by which count they enter
PO <- readRDS(file.path(J, "pois.rds")); area <- st_buffer(st_as_sfc(st_bbox(st_union(c(st_geometry(lx), st_geometry(wroute), st_geometry(home))))), 450)
near <- lengths(st_intersects(PO$geom, area)) > 0
px <- st_sf(PO$tb[near], geometry = PO$geom[near]); px <- px[!duplicated(st_coordinates(st_centroid(px))), ]
within <- function(g, d) lengths(st_is_within_distance(px, g, d)) > 0
px$at_stop <- within(st_geometry(stops_x), 200); px$walk_legs <- within(st_geometry(lx[lx$mode == "WALK", ]), 50)
px$walk_route <- within(st_geometry(wroute), 50); px$home <- within(st_geometry(home), 400); rm(PO); gc(FALSE)
px <- px[px$at_stop | px$walk_legs | px$walk_route | px$home, ]
# 4. Missing a journey: the published measure for this Output Area, beside the curve above
ms <- as.data.table(readRDS(file.path(J, paste0("miss_", WX_NET, ".rds")))$miss)[from_id == e$oa21cd]
# 5. Every Output Area in Brighton and Hove: time to its quickest school, and the deprived third
oa_bh <- merge(oa[la == "Brighton and Hove", .(oa21cd, lsoa, lon, lat, kids, third, area)], quick(WX_NET), by.x = "oa21cd", by.y = "from_id")
worked <- list(id = WX, oa21cd = e$oa21cd, school = e$school, walk_route = st_transform(wroute, 4326), walk_stats = wstat, walk_profile = wprof,
               stops = st_transform(stops_x, 4326), pois = st_transform(px, 4326), by_minute = by_minute, miss = ms, oa_bh = oa_bh)
out <- list(built = Sys.Date(), date = ROUTE_DATE, departures = rbindlist(deps), legs = L, summary = S, ends = ends, worked = worked)
saveRDS(out, here::here("data", "route_examples.rds")); message("Saved data/route_examples.rds")
print(S, digits = 3)
