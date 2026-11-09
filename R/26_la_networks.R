# 26_la_networks.R - Networks for the 109 mostly urban authorities
# ---------------------------------------------------------------------------
# R/20 routed nine places. To test journey times against absence, and to see whether
# Brighton and Hove is unusual, this prepares the 109 authorities in R/19's "mostly
# urban" sample (at least 80% of children aged 10 to 14 in urban LSOAs, 30 or more
# LSOAs), with Hastings kept as a district because the earlier work compared it.
#
#   STEP=frame  Output Areas, networks (neighbouring authorities share one, so a child
#               can cross a boundary), and which bus files serve each network. BODS
#               regional files follow operators, not places, so each network takes
#               every file with a real presence in it, not its own region's alone.
#   STEP=osm    Clips the England OpenStreetMap extract to each network, in one pass.
#   STEP=gtfs   Clips each regional bus file to each network. REGIONS=a,b for some.
#   STEP=freq   Turns frequency-based trips in clips already written into timetabled ones.
#
# Sources (method.qmd#sec-la-routing-method):
#   OpenStreetMap, england-latest.osm.pbf, Geofabrik
#   Bus Open Data Service GTFS regional files, Department for Transport (all nine)
#   Census 2021 Output Areas and children aged 10 to 14 (TS007a), ONS centroids and
#   lookups; schools as R/20. Outputs in data/cache/journey_la and data-raw/r5/net_la
#   (neither committed).
# ---------------------------------------------------------------------------

JH <- Sys.getenv("JAVA_HOME")
if (!nzchar(JH) || !dir.exists(JH)) { JH <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "rJavaEnv", "installed", "windows", "x64", "21"); Sys.setenv(JAVA_HOME = JH) }
proj_db <- system.file("proj", package = "terra"); if (nzchar(proj_db)) Sys.setenv(PROJ_LIB = proj_db, PROJ_DATA = proj_db)
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(sf); library(data.table) })
num <- function(x) suppressWarnings(as.numeric(x))
STEP <- Sys.getenv("STEP", "frame")
SRC <- here::here("data-raw", "r5", "src"); NET <- here::here("data-raw", "r5", "net_la"); OUT <- here::here("data", "cache", "journey_la")
ons <- here::here("data-raw", "census2021", "ons_custom"); raw <- here::here("data-raw", "census2021")
dir.create(NET, showWarnings = FALSE, recursive = TRUE); dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
REGIONS_ALL <- c("east_anglia", "east_midlands", "london", "north_east", "north_west", "south_east", "south_west", "west_midlands", "yorkshire")

NETS <- list(
  tyne_wear = c("Newcastle upon Tyne", "Gateshead", "North Tyneside", "South Tyneside", "Sunderland"),
  tees = c("Darlington", "Hartlepool", "Middlesbrough", "Stockton-on-Tees"),
  west_yorks = c("Leeds", "Bradford", "Kirklees", "Calderdale", "Wakefield"),
  south_yorks = c("Sheffield", "Rotherham", "Barnsley", "Doncaster"),
  humber = c("Kingston upon Hull", "North East Lincolnshire"),
  york = "York",
  gmcr = c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan"),
  merseyside = c("Liverpool", "Knowsley", "Sefton", "St. Helens", "Wirral", "Halton", "Warrington"),
  lancashire = c("Lancashire", "Blackpool", "Blackburn with Darwen"),
  cheshire_east = "Cheshire East",
  west_mids = c("Birmingham", "Coventry", "Dudley", "Sandwell", "Solihull", "Walsall", "Wolverhampton"),
  stoke = "Stoke-on-Trent",
  telford = "Telford and Wrekin",
  east_mids = c("Derby", "Leicester", "Nottingham"),
  peterborough = "Peterborough",
  herts_luton = c("Hertfordshire", "Luton"),
  mk = "Milton Keynes",
  south_essex = c("Southend-on-Sea", "Thurrock"),
  medway = "Medway",
  thames_valley = c("Reading", "Slough", "Bracknell Forest", "Windsor and Maidenhead", "Wokingham"),
  surrey = "Surrey",
  sussex = c("Brighton and Hove", "West Sussex", "Hastings"),
  solent = c("Portsmouth", "Southampton"),
  bcp = "Bournemouth, Christchurch and Poole",
  bristol = c("Bristol", "South Gloucestershire"),
  swindon = "Swindon",
  devon = c("Plymouth", "Torbay"),
  london = c("Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney", "Hammersmith and Fulham", "Haringey", "Harrow", "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea", "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest", "Wandsworth", "Westminster"))

frame_f <- file.path(OUT, "frame.rds"); nets_f <- file.path(OUT, "nets.rds")

if (STEP == "frame") {
  ruc <- fread(file.path(ons, "lsoa_ruc21.csv")); utla <- fread(file.path(ons, "lsoa_utla23.csv")); lad <- fread(file.path(ons, "lsoa_lad23.csv"))
  lk <- fread(file.path(ons, "oa_lsoa_msoa_2021.csv"), select = c("OA21CD", "LSOA21CD")); setnames(lk, c("oa21cd", "lsoa"))
  k <- fread(file.path(raw, "ts007a", "census2021-ts007a-oa.csv"), select = c("geography code", "Age: Aged 10 to 14 years")); setnames(k, c("oa21cd", "kids"))
  oa <- as.data.table(readRDS(here::here("data", "cache", "oa_pwc_2021.rds")))[startsWith(oa21cd, "E"), .(oa21cd, e = num(easting), n = num(northing))][!is.na(e)]
  oa <- merge(merge(oa, lk, by = "oa21cd"), k, by = "oa21cd")
  oa <- merge(oa, ruc[, .(lsoa = LSOA21CD, urban = startsWith(RUC21NM, "Urban"))], by = "lsoa")
  oa <- merge(oa, utla[, .(lsoa = LSOA21CD, la = UTLA23NM)], by = "lsoa"); oa <- merge(oa, lad[, .(lsoa = LSOA21CD, lad = LAD23NM)], by = "lsoa")
  # The sample, by the same rule as R/19: at least 80% urban by children, at least 30 LSOAs
  rule <- oa[, .(oas = .N, lsoas = uniqueN(lsoa), urban_share = 100 * sum(urban * kids) / sum(kids)), by = la][urban_share >= 80 & lsoas >= 30]
  stopifnot(nrow(rule) == 109, setequal(rule$la, setdiff(unlist(NETS), "Hastings")))
  oa[lad == "Hastings", la := "Hastings"]   # a district of East Sussex: only for comparison, not in the models
  fr <- oa[la %in% unlist(NETS)]; fr[, net := names(NETS)[vapply(la, function(l) which(vapply(NETS, function(v) l %in% v, TRUE)), 1L)]]
  xy <- st_coordinates(st_transform(st_as_sf(fr, coords = c("e", "n"), crs = 27700), 4326)); fr[, `:=`(lon = xy[, 1], lat = xy[, 2])]
  sch <- as.data.table(readRDS(here::here("data", "cache", "journey", "frame.rds"))$sch)
  saveRDS(list(oa = fr, sch = sch, rule = rule), frame_f)
  # Which bus files serve each network: stops within 5 km of its Output Areas' box
  boxes <- fr[, .(oas = .N, las = uniqueN(la), xmin = min(e), xmax = max(e), ymin = min(n), ymax = max(n)), by = net]
  ll_box <- function(b, buf) { p <- st_transform(st_as_sfc(st_bbox(c(xmin = b$xmin - buf, ymin = b$ymin - buf, xmax = b$xmax + buf, ymax = b$ymax + buf), crs = 27700)), 4326); as.list(st_bbox(p)) }
  stops <- rbindlist(lapply(REGIONS_ALL, function(r) { td <- tempfile(); dir.create(td); unzip(file.path(SRC, paste0("gtfs_", r, ".zip")), files = "stops.txt", exdir = td); s <- fread(file.path(td, "stops.txt"), select = c("stop_lat", "stop_lon")); s[, region := r][!is.na(stop_lat)] }))
  audit <- rbindlist(lapply(seq_len(nrow(boxes)), function(i) { b <- ll_box(boxes[i], 5000); stops[stop_lon >= b$xmin & stop_lon <= b$xmax & stop_lat >= b$ymin & stop_lat <= b$ymax, .(stops = .N), by = region][, net := boxes$net[i]] }))
  audit[, keep := stops >= 50 & stops >= 0.02 * max(stops), by = net]
  boxes[, feeds := vapply(net, function(id) paste(audit[net == id & keep == TRUE, region], collapse = ","), "")]
  saveRDS(list(boxes = boxes, audit = audit, members = NETS), nets_f)
  print(boxes[, .(net, oas, las, feeds)]); message(nrow(fr), " Output Areas in ", uniqueN(fr$la), " places, ", nrow(boxes), " networks")
}

if (STEP %in% c("osm", "gtfs")) { N <- readRDS(nets_f); boxes <- N$boxes
  ll_box <- function(b, buf) { p <- st_transform(st_as_sfc(st_bbox(c(xmin = b$xmin - buf, ymin = b$ymin - buf, xmax = b$xmax + buf, ymax = b$ymax + buf), crs = 27700)), 4326); st_bbox(p) } }

if (STEP == "osm") {
  # One pass of the England file, teed to a bounding box for each network: routes run 30 km
  # around the Output Areas (15 km to the schools R/20 considers, and 15 km beyond)
  tool <- Sys.getenv("OSM_MERGE_TOOL", file.path(dirname(here::here()), "osmosis-0.49.2", "bin", "osmosis.bat")); stopifnot(file.exists(tool))
  Sys.setenv(JAVA_OPTS = "-Xmx16G")
  todo <- boxes[!file.exists(file.path(NET, net, "network.osm.pbf"))]
  args <- c("--read-pbf", paste0("file=", normalizePath(file.path(SRC, "england-latest.osm.pbf"), winslash = "/")), "--tee", paste0("outputCount=", nrow(todo)))
  # Milton Keynes takes 31 km: at 30 km the box cut a car park way to a single node, and R5
  # stops on a one-point line when it builds its park-and-ride areas (no other effect)
  OSM_BUF <- c(mk = 31000)
  for (i in seq_len(nrow(todo))) { b <- ll_box(todo[i], if (todo$net[i] %in% names(OSM_BUF)) OSM_BUF[[todo$net[i]]] else 30000); d <- file.path(NET, todo$net[i]); dir.create(d, showWarnings = FALSE)
    args <- c(args, "--buffer", "--bounding-box", sprintf("left=%.4f", b$xmin), sprintf("right=%.4f", b$xmax), sprintf("top=%.4f", b$ymax), sprintf("bottom=%.4f", b$ymin), "clipIncompleteEntities=true", "--write-pbf", paste0("file=", normalizePath(file.path(d, "network.osm.pbf"), winslash = "/", mustWork = FALSE))) }
  t0 <- Sys.time(); st <- system2(tool, args); message(sprintf("osmosis exit %s in %.0f min", st, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  if (st != 0) stop("osmosis failed")
  print(data.table(net = boxes$net, mb = round(file.size(file.path(NET, boxes$net, "network.osm.pbf")) / 1024^2)))
}

if (STEP == "gtfs") {
  want <- strsplit(Sys.getenv("REGIONS"), ",")[[1]]; if (!length(want)) want <- REGIONS_ALL
  for (r in want) {
    ids <- boxes[vapply(strsplit(feeds, ","), function(f) r %in% f, TRUE), net]
    ids <- ids[!file.exists(file.path(NET, ids, paste0("gtfs_", r, ".zip")))]
    if (!length(ids)) next
    t0 <- Sys.time(); g <- gtfstools::read_gtfs(file.path(SRC, paste0("gtfs_", r, ".zip")))
    for (id in ids) { b <- st_as_sfc(ll_box(boxes[net == id], 30000)); f <- gtfstools::filter_by_spatial_extent(g, b); dir.create(file.path(NET, id), showWarnings = FALSE)
      if (NROW(f$frequencies)) f <- gtfstools::frequencies_to_stop_times(f)
      gtfstools::write_gtfs(f, file.path(NET, id, paste0("gtfs_", r, ".zip"))); message(r, " > ", id, ": kept ", format(nrow(f$trips), big.mark = ","), " of ", format(nrow(g$trips), big.mark = ","), " trips") }
    message(r, sprintf(" done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))); rm(g); gc(FALSE)
  }
}

if (STEP == "freq") {
  # r5r's detailed_itineraries() refuses a feed with a frequency-based timetable. BODS South
  # East has one: Sullivan Buses route 84, two trips every 20 min from 09:25 to 13:05. Each
  # such trip becomes timetabled trips, so the service stays. STEP=gtfs now does this as it
  # clips; this step applies it to clips already written. Clips with no rows are left alone.
  for (z in list.files(NET, pattern = "^gtfs_.*\\.zip$", recursive = TRUE, full.names = TRUE)) {
    g <- gtfstools::read_gtfs(z); if (!NROW(g$frequencies)) next
    n0 <- nrow(g$trips); g <- gtfstools::frequencies_to_stop_times(g); stopifnot(!NROW(g$frequencies))
    gtfstools::write_gtfs(g, z); message(basename(dirname(z)), "/", basename(z), sprintf(": trips %s to %s", format(n0, big.mark = ","), format(nrow(g$trips), big.mark = ",")))
  }
}
message("Step ", STEP, " complete")
