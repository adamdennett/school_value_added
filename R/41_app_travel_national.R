# 41_app_travel_national.R - National walking and cycling routes, summarised by school for the app
# ---------------------------------------------------------------------------
# The sibling project school_active_travel routes every Census 2021 Output Area in England
# to every school within 120 minutes' walk (r5r 2.5 on OpenStreetMap, OS Terrain 50
# heights every 50 m, cycling times at level of traffic stress 2 and 4). It runs a tile
# at a time over several nights; this summarises whatever has finished, and is rerun as
# more does. Output: app/data/travel_national.rds, which the app shows when present.
#
# For each Output Area routed so far, its quickest walk (walk_min_r5) to a state-funded
# school teaching secondary ages. Each such school then gets, over the children aged 10 to
# 14 (Census 2021 TS007a) in the Output Areas for which it is the quickest walk:
#   kids       their number
#   walk_med   their median walking time, minutes, at 4.8 km/h on the flat
#   within20   the % within a 20-minute walk
#   climb_med  the median metres climbed on the walk to school
#   lts2_15    the % with a quiet-street (LTS 2) cycle route of 15 minutes or less
# A school counts only when every Output Area within 10 km of it has been routed (a
# 9.6 km walk is the most the routing searches), so its catchment is complete.
# Schools are matched to Value Added identities through data/lookups/urn_family.csv.
#
# Rscript R/41_app_travel_national.R   (a few minutes; reads ../school_active_travel/data/out)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(arrow); library(data.table) })
SAT_DIR <- normalizePath(Sys.getenv("SAT_DIR", file.path(dirname(here::here()), "school_active_travel")), winslash = "/")
OUT <- file.path(SAT_DIR, "data", "out")
stopifnot(dir.exists(file.path(OUT, "od")))

o <- as.data.table(read_parquet(file.path(OUT, "origins.parquet"), col_select = c("oa21cd", "e", "n", "age_10_14", "la", "tile")))
s <- as.data.table(read_parquet(file.path(OUT, "schools.parquet"), col_select = c("urn", "e", "n", "serves_secondary", "independent")))
sec <- s[serves_secondary == TRUE & independent == FALSE]
tiles_done <- basename(dirname(Sys.glob(file.path(SAT_DIR, "data", "cache", "tiles", "*", "gathered.csv"))))
o[, done := tile %in% tiles_done]
message(sprintf("%d of %d tiles finished: %d of %d Output Areas", length(tiles_done), uniqueN(o$tile), sum(o$done), nrow(o)))

# Every pair from a finished Output Area to a state secondary
od <- open_dataset(file.path(OUT, "od")) |>
  dplyr::filter(la %in% !!unique(o$la[o$done]), oa21cd %in% !!o$oa21cd[o$done], urn %in% !!sec$urn) |>
  dplyr::select(oa21cd, urn, walk_min_r5, route_m, climb_to_school_m, bike_min_lts2) |> dplyr::collect() |> as.data.table()
pairs <- nrow(od)
q <- od[order(walk_min_r5, route_m)][, .SD[1], by = oa21cd]           # each Output Area's quickest walk
q <- merge(q, o[, .(oa21cd, kids = age_10_14)], by = "oa21cd")
wmed <- function(x, w) { ok <- !is.na(x) & w > 0; if (!any(ok)) return(NA_real_); x <- x[ok]; w <- w[ok]; i <- order(x)
  x[i][which(cumsum(w[i]) >= sum(w) / 2)[1]] }
by_school <- q[, .(kids = sum(kids), walk_med = wmed(walk_min_r5, kids),
                   within20 = 100 * sum(kids[walk_min_r5 <= 20]) / sum(kids), climb_med = wmed(climb_to_school_m, kids),
                   lts2_15 = 100 * sum(kids[!is.na(bike_min_lts2) & bike_min_lts2 <= 15]) / sum(kids), oas = .N), by = urn]

# Complete catchments only: no unrouted Output Area within 10 km of the school
left <- o[done == FALSE]
far <- if (nrow(left)) nabor::knn(as.matrix(left[, .(e, n)]), as.matrix(sec[, .(e, n)]), k = 1)$nn.dists[, 1] else rep(Inf, nrow(sec))
complete <- sec$urn[far > 10000]
by_school <- by_school[urn %in% complete & kids > 0]

# Value Added identities
fam <- fread(here::here("data", "lookups", "urn_family.csv"))
by_school <- merge(by_school, unique(fam[, .(URN, urn)]), by = "urn", all.x = TRUE)[, URN := fifelse(is.na(URN), urn, URN)]
by_school <- by_school[, .(kids = sum(kids), walk_med = walk_med[which.max(kids)], within20 = weighted.mean(within20, kids),
                           climb_med = climb_med[which.max(kids)], lts2_15 = weighted.mean(lts2_15, kids), oas = sum(oas)), by = URN]

# Authorities whose Output Areas are all routed (the app lists these)
la_done <- o[, .(all = all(done)), by = la][all == TRUE, la]
res <- list(schools = as.data.frame(by_school[, lapply(.SD, function(x) if (is.double(x)) round(x, 1) else x)]),
            coverage = list(las = length(la_done), las_total = uniqueN(o$la), oas = sum(o$done), oas_total = nrow(o),
                            tiles = length(tiles_done), tiles_total = uniqueN(o$tile), pairs = pairs, built = Sys.Date()))
dir.create(here::here("app", "data"), showWarnings = FALSE)
saveRDS(res, here::here("app", "data", "travel_national.rds"))
message(sprintf("travel_national.rds: %d schools with complete walking catchments; %d of %d authorities routed",
                nrow(res$schools), res$coverage$las, res$coverage$las_total))
