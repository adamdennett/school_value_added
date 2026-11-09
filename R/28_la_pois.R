# 28_la_pois.R - Places a child passes on the way to school
# ---------------------------------------------------------------------------
# A direct test of "intervening opportunities": points of interest (POIs) from
# OpenStreetMap, around the stops where a child gets on and off a bus, and along the
# walking routes of R/27. General, not shops alone, because many shops may not sell to
# children in school hours.
#
#   STEP=extract  England's POIs by category, from the same OSM file as the networks.
#   STEP=count    For each routed journey:
#       Around stops, within 200 m (400 m as a check): the first stop, each change and
#       the last stop, averaged over the 07:45, 08:00 and 08:15 departures.
#       Along walking routes, within 50 m (100 m as a check): the whole walking route to
#       the quickest school on foot, and the walking legs of the journey by walking and
#       bus at 08:00. Each POI counts once per journey.
#       Around home, within 400 m: a control, because a child in a busy place passes
#       busy places whatever their journey.
#
# Categories (tags in method.qmd#sec-poi-method):
#   food_drink   cafes, fast food, restaurants, pubs, bars, ice cream
#   shops        every shop=* except vacant
#   leisure      cinemas, arcades, betting, bowling, sports centres, museums and the like
#   open_space   parks, playgrounds, recreation grounds, commons, nature reserves, beaches
#   civic        libraries, community centres, worship, banks, pharmacies, doctors and the like
# Schools, transport, parking and street furniture are left out. POIs mapped as areas are
# represented by a point inside them, except open spaces, which keep their edges, because
# what matters is walking past the park, not its middle. OSM completeness varies by place.
#
# Output: data/cache/journey_la/pois.rds, poi_<network>.rds (not committed).
# Rscript R/28_la_pois.R with STEP=extract, then STEP=count
# ---------------------------------------------------------------------------

proj_db <- system.file("proj", package = "terra"); if (nzchar(proj_db)) Sys.setenv(PROJ_LIB = proj_db, PROJ_DATA = proj_db)
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(sf); library(data.table) })
OUT <- here::here("data", "cache", "journey_la"); PBF <- here::here("data-raw", "r5", "src", "england-latest.osm.pbf")
STEP <- Sys.getenv("STEP", "extract"); poi_f <- file.path(OUT, "pois.rds")

FOOD <- c("cafe", "fast_food", "restaurant", "pub", "bar", "ice_cream", "food_court", "biergarten")
LEIS_AM <- c("cinema", "theatre", "arts_centre", "nightclub", "casino", "gambling", "events_venue", "music_venue")
LEIS_LE <- c("amusement_arcade", "adult_gaming_centre", "bowling_alley", "sports_centre", "fitness_centre", "ice_rink", "trampoline_park", "water_park", "miniature_golf", "dance", "escape_game", "swimming_pool")
LEIS_TO <- c("attraction", "museum", "gallery", "theme_park", "zoo", "aquarium", "viewpoint")
OPEN_LE <- c("park", "playground", "recreation_ground", "common", "nature_reserve", "dog_park")
CIVIC <- c("library", "community_centre", "social_centre", "place_of_worship", "townhall", "post_office", "bank", "pharmacy", "doctors", "dentist", "clinic", "hospital", "police", "fire_station", "courthouse", "marketplace", "youth_centre")
classify <- function(d) fcase(
  d$amenity %in% FOOD, "food_drink",
  !is.na(d$shop) & !d$shop %in% c("vacant", "no", "disused"), "shops",
  d$leisure %in% OPEN_LE | d$landuse %in% "recreation_ground" | d$natural %in% "beach", "open_space",
  d$amenity %in% LEIS_AM | d$leisure %in% LEIS_LE | d$tourism %in% LEIS_TO, "leisure",
  d$amenity %in% CIVIC, "civic", default = NA_character_)

if (STEP == "extract") {
  tags <- c("shop", "amenity", "leisure", "tourism", "natural", "landuse")
  W <- "shop IS NOT NULL OR amenity IS NOT NULL OR leisure IS NOT NULL OR tourism IS NOT NULL OR \"natural\" = 'beach' OR landuse = 'recreation_ground'"
  read_layer <- function(layer) { t0 <- Sys.time()
    g <- osmextract::oe_read(PBF, layer = layer, extra_tags = tags, vectortranslate_options = c("-t_srs", "EPSG:27700", "-where", W, "-select", paste(c("name", tags), collapse = ",")), force_vectortranslate = TRUE, quiet = TRUE, download_directory = OUT)
    message(layer, ": ", nrow(g), sprintf(" features, %.0f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))); for (k in setdiff(tags, names(g))) g[[k]] <- NA_character_; g }
  pt <- read_layer("points"); pl <- read_layer("multipolygons")
  pt$cat <- classify(pt); pl$cat <- classify(pl); pt <- pt[!is.na(pt$cat), ]; pl <- pl[!is.na(pl$cat), ]
  open_ <- pl$cat == "open_space"; pl_open <- st_make_valid(st_simplify(st_geometry(pl)[open_], dTolerance = 5)); pl_pts <- st_point_on_surface(st_make_valid(st_geometry(pl)[!open_]))
  G <- c(st_geometry(pt), pl_pts, pl_open)
  tb <- data.table(cat = c(pt$cat, pl$cat[!open_], pl$cat[open_]), tag = c(fcoalesce(pt$shop, pt$amenity, pt$leisure, pt$tourism, pt$natural, pt$landuse), fcoalesce(pl$shop, pl$amenity, pl$leisure, pl$tourism, pl$natural, pl$landuse)[!open_], fcoalesce(pl$shop, pl$amenity, pl$leisure, pl$tourism, pl$natural, pl$landuse)[open_]),
                   mapped = c(rep("point", nrow(pt)), rep("area", sum(!open_)), rep("area", sum(open_))))
  saveRDS(list(geom = G, tb = tb, by_cat = tb[, .(n = .N), by = .(cat, mapped)], top_tags = tb[, .(n = .N), by = .(cat, tag)][order(cat, -n)][, .SD[seq_len(min(.N, 8))], by = cat], osm_date = file.mtime(PBF)), poi_f)
  print(tb[, .(n = .N), by = .(cat, mapped)])
}

if (STEP == "count") {
  P <- readRDS(poi_f); FR <- readRDS(file.path(OUT, "frame.rds"))
  ids <- sub("^struct_(.*)\\.rds$", "\\1", list.files(OUT, pattern = "^struct_")); want <- strsplit(Sys.getenv("NETS"), ",")[[1]]; if (length(want)) ids <- intersect(ids, want)
  cats <- c("food_drink", "shops", "leisure", "open_space", "civic")
  # Counts of POIs within d of each feature; `key` groups features into one journey, a POI counted once per key
  near <- function(geom, key, d, pg, ptb, prefix) {
    hit <- st_is_within_distance(geom, pg, dist = d)
    long <- data.table(k_id = rep(key, lengths(hit)), j = unlist(hit))
    long <- unique(long); long[, cat := ptb$cat[j]]
    w <- dcast(long, k_id ~ cat, fun.aggregate = length, value.var = "j")
    for (k in setdiff(cats, names(w))) w[[k]] <- 0L
    w[, any := rowSums(.SD), .SDcols = cats]
    setnames(w, c(cats, "any"), paste0(prefix, "_", c(cats, "any"))); w[]
  }
  for (id in ids) { fo <- file.path(OUT, paste0("poi_", id, ".rds")); if (file.exists(fo)) next
    t0 <- Sys.time(); s <- readRDS(file.path(OUT, paste0("struct_", id, ".rds"))); w <- readRDS(file.path(OUT, paste0("walk_", id, ".rds")))
    o <- FR$oa[net == id]; b <- o[, c(min(e) - 32000, min(n) - 32000, max(e) + 32000, max(n) + 32000)]
    keep <- which(lengths(st_intersects(P$geom, st_as_sfc(st_bbox(c(xmin = b[1], ymin = b[2], xmax = b[3], ymax = b[4]), crs = 27700)))) > 0); pg <- P$geom[keep]; ptb <- P$tb[keep]
    # Along walking routes
    r50 <- near(st_geometry(w$route_lines), w$route_lines$from_id, 50, pg, ptb, "walk50"); r100 <- near(st_geometry(w$route_lines), w$route_lines$from_id, 100, pg, ptb, "walk100")
    l50 <- near(st_geometry(s$leg_lines), s$leg_lines$from_id, 50, pg, ptb, "leg50")
    # Around stops: every distinct stop once, then joined to the journeys that use it
    st <- as.data.table(s$stops); lst <- rbind(st[i == 1, .(from_id, dep, role = "board", lon = board_lon, lat = board_lat)], st[i < n, .(from_id, dep, role = "change", lon = off_lon, lat = off_lat)], st[i == n, .(from_id, dep, role = "alight", lon = off_lon, lat = off_lat)])
    lst[, `:=`(lon = round(lon, 4), lat = round(lat, 4))]; us <- unique(lst[, .(lon, lat)])[, sid := .I]
    if (nrow(us)) { sg <- st_transform(st_as_sfc(sprintf("POINT(%f %f)", us$lon, us$lat), crs = 4326), 27700)
      s200 <- near(sg, us$sid, 200, pg, ptb, "p"); s400 <- near(sg, us$sid, 400, pg, ptb, "q")
      u2 <- merge(merge(us, s200, by.x = "sid", by.y = "k_id", all.x = TRUE), s400[, .(k_id, q_any)], by.x = "sid", by.y = "k_id", all.x = TRUE); nz <- setdiff(names(u2), c("sid", "lon", "lat")); for (k in nz) set(u2, which(is.na(u2[[k]])), k, 0L)
      lj <- merge(lst, u2, by = c("lon", "lat"), allow.cartesian = TRUE)
      # journeys: over three departures, the mean for each role; a journey with no change scores 0 on changes, and the mean per change is kept apart
      pv <- c(paste0("p_", c(cats, "any")), "q_any")
      per <- lj[, c(.(events = .N), lapply(.SD, sum)), by = .(from_id, dep, role), .SDcols = pv]
      deps <- unique(st[, .(from_id, dep)])[, .(ndep = .N), by = from_id]
      stops_w <- per[, c(.(events = sum(events)), lapply(.SD, sum)), by = .(from_id, role), .SDcols = pv]; stops_w <- merge(stops_w, deps, by = "from_id")
      for (k in c("events", pv)) stops_w[, (k) := get(k) / 3]; stops_w[, ndep := NULL]
      stops_stop <- dcast(stops_w, from_id ~ role, value.var = c("events", pv), fill = 0)
    } else { u2 <- NULL; stops_stop <- data.table(from_id = character()) }
    # Around home, within 400 m
    hg <- st_as_sf(o, coords = c("e", "n"), crs = 27700); h <- near(st_geometry(hg), o$oa21cd, 400, pg, ptb, "home400")
    fix <- function(x) { x <- copy(x); setnames(x, "k_id", "from_id"); x }
    res <- Reduce(function(a, b) merge(a, b, by = "from_id", all.x = TRUE), list(data.table(from_id = o$oa21cd), fix(r50), fix(r100), fix(l50), fix(h), stops_stop))
    nz <- setdiff(names(res), "from_id"); for (k in nz) set(res, which(is.na(res[[k]])), k, 0)
    saveRDS(list(id = id, oa = res, stop_table = u2, n_poi = table(ptb$cat)), fo); message(id, sprintf(": %d Output Areas, %d POIs, %.1f min", nrow(res), length(keep), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
}
message("Step ", STEP, " complete")
