# 31_admissions_journeys.R - What Brighton and Hove's admissions changes did to journeys and mixing
# ---------------------------------------------------------------------------
# R/30 puts a cost on distance (through absence, in Attainment 8 points) and Gorard and
# Siddiqui's mixing estimate on the same scale. This asks what the city's admissions changes
# did on both counts, from the published runs of the city's school system model
# (adamdennett/bh-school-system, data/council_options.rds):
#
#   today     the 2024 catchment map and today's priorities, priority 6 at 5% (2026/27 on)
#   pre2024   the pre-2024 catchments under today's priorities
#   p6_off    today's map without priority 6
#   whitehawk today's map with Whitehawk back in Longhill's catchment
#   comart    CoMArt reopened, Brighton Aldridge and Longhill at 150
#
# The map's effect is today minus pre2024; priority 6's is today minus p6_off; together they
# are the sum, an approximation because the model has no run with both undone. Journeys are
# the model's walking and bus minutes from home to the school offered, for the city's schools;
# mixing is its Gorard index over the modelled intakes of disadvantaged children.
#
# Sources and formulas: method.qmd#sec-admissions-method. Inputs: the published model file
# (downloaded to data-raw/bh_system, with its commit), data/journey_la.rds (R/29),
# data/distance_gcse.rds (R/30). Output: data/admissions_journeys.rds (committed).
# Rscript R/31_admissions_journeys.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(data.table))
BH <- "Brighton and Hove"; REPO <- "adamdennett/bh-school-system"
dir.create(here::here("data-raw", "bh_system"), showWarnings = FALSE, recursive = TRUE)
f <- here::here("data-raw", "bh_system", "council_options.rds")
# The commit of the published file, so the figures can be traced to a version of the model
sha <- tryCatch(jsonlite::fromJSON(sprintf("https://api.github.com/repos/%s/commits?path=data/council_options.rds&per_page=1", REPO))$sha[1], error = function(e) NA_character_)
if (!file.exists(f) || !is.na(sha)) download.file(sprintf("https://raw.githubusercontent.com/%s/%s/data/council_options.rds", REPO, if (is.na(sha)) "main" else sha), f, mode = "wb", quiet = TRUE)
CO <- readRDS(f)
IDS <- c(today = "Today", pre2024 = "The pre-2024 catchments", p6_off = "Today, without priority 6", whitehawk = "Whitehawk back in Longhill's catchment", comart = "CoMArt reopened")
runs <- as.data.table(CO$runs)[id %in% names(IDS), .(id, label = IDS[id], year, gorard, dep_min, dep_gap, mean_min, p90_min, over_40, displaced)]

# ---- Effects of the changes -----------------------------------------------------------------
eff <- function(y) { r <- runs[year == y]; v <- function(i, col) r[[col]][r$id == i]
  rbindlist(lapply(c("gorard", "dep_min", "dep_gap", "mean_min"), function(col) data.table(year = y, measure = col,
    map = v("today", col) - v("pre2024", col), p6 = v("today", col) - v("p6_off", col),
    both = (v("today", col) - v("pre2024", col)) + (v("today", col) - v("p6_off", col)), before = v("pre2024", col), today = v("today", col)))) }
effects <- rbindlist(lapply(c(2026, 2030), eff))

# ---- On the scales of R/30 ------------------------------------------------------------------
J <- readRDS(here::here("data", "journey_la.rds")); G <- readRDS(here::here("data", "distance_gcse.rds"))
P <- J$places[in_models == TRUE]
mpk <- P[la == BH, mpk_dep]                               # minutes per straight-line km, BH deprived third
la <- J$la_models[term == "t_bus_dep" & group == "dis" & ctrl == "And every control" & phase == "sec" & outcome == "overall"]
per_min <- setNames(la$b, la$sample)                       # points of absence per minute, disadvantaged pupils
chain_km <- setNames(G$chain[group == "dis" & ctrl == "And every control"]$chain_va, G$chain[group == "dis" & ctrl == "And every control"]$sample)
pp <- G$means[g == "dis", per_point_va]
cost <- function(dmin) list(
  via_km = dmin / mpk * chain_km[c("All 109", "Without London")],                    # minutes -> km -> Attainment 8
  via_minutes = -dmin * per_min[c("All 109", "Without London")] * pp)               # minutes -> absence -> Attainment 8
gain <- function(dg, base) -dg / base * G$gorard$att8                               # share of segregation removed x Gorard's range
e26 <- effects[year == 2026]
ev <- function(m, col) e26[[col]][e26$measure == m]
summ <- rbindlist(lapply(c("map", "both"), function(k) {
  c1 <- cost(ev("dep_min", k)); g1 <- gain(ev("gorard", k), ev("gorard", "before"))
  data.table(change = k, dmin = ev("dep_min", k), dindex = ev("gorard", k), share_removed = -ev("gorard", k) / ev("gorard", "before"),
             cost_lo = min(unlist(c1)), cost_hi = max(unlist(c1)), gain_lo = g1[["lo"]], gain_hi = g1[["hi"]]) }))

out <- list(built = Sys.Date(), source = list(repo = REPO, file = "data/council_options.rds", commit = sha, model_built = CO$built_at, dis_source = CO$dis_source),
            runs = runs, effects = effects, mpk = mpk, per_min = per_min, chain_km = chain_km, per_point = pp, summary = summ,
            bh_gap = P[la == BH, .(t_bus_gap, t_bus_dep, km1_gap)])
saveRDS(out, here::here("data", "admissions_journeys.rds")); message("Saved data/admissions_journeys.rds")
print(runs[year == 2026], digits = 3); print(effects, digits = 3); print(summ, digits = 3)
