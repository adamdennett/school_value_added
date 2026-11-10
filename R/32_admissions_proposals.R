# 32_admissions_proposals.R - What the proposals that did not go ahead would have done
# ---------------------------------------------------------------------------
# R/31 used the published runs of the city's school system model for the changes that were
# made. Two proposals were not made, so the model has no published run of them:
#
#   council_2024  The council's 2024 proposal: priority 6 (places at the community schools for
#                 children from the single-school catchments) at 20%, with Dorothy Stringer
#                 and Blatchington Mill each cut by 30 places to 300.
#   p6_30_cuts    Priority 6 at 30% with the same cuts, as Class Divide argued for
#                 (p6_30, without the cuts, for comparison).
#
# Both are run here, with the parts of the council's proposal separately, on the published
# model itself: its app code and inputs (adamdennett/bh-school-system, app/R/model.R,
# app/R/outcomes.R, app/data/sim_inputs.rds) are downloaded at a fixed commit and run as
# they are, under today's map and priorities. Before anything else the script reproduces
# the model's own published "Today" run, and stops if it does not.
#
# Sources and formulas: method.qmd#sec-proposals-method. Output: data/admissions_proposals.rds
# (committed). Rscript R/32_admissions_proposals.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(dplyr) })
REPO <- "adamdennett/bh-school-system"; SHA <- "40a055afdbd14033f6b9bc1579f3728aa3507c0d"   # as R/31
D <- here::here("data-raw", "bh_system", "app"); dir.create(file.path(D, "R"), showWarnings = FALSE, recursive = TRUE); dir.create(file.path(D, "data"), showWarnings = FALSE)
for (p in c("app/R/model.R", "app/R/outcomes.R", "app/data/sim_inputs.rds")) {
  f <- file.path(D, sub("^app/", "", p))
  if (!file.exists(f)) download.file(sprintf("https://raw.githubusercontent.com/%s/%s/%s", REPO, SHA, p), f, mode = "wb", quiet = TRUE) }
sim <- new.env(); sys.source(file.path(D, "R", "model.R"), envir = sim); sys.source(file.path(D, "R", "outcomes.R"), envir = sim)
inp <- readRDS(file.path(D, "data", "sim_inputs.rds"))
pan_now <- setNames(inp$schools$pan, inp$schools$name)
DS <- "Dorothy Stringer School"; BMS <- "Blatchington Mill School"
cut30 <- c(setNames(pan_now[[DS]] - 30, DS), setNames(pan_now[[BMS]] - 30, BMS))

# The same measures as the model's own option runs (bh-school-system R/05, co_metrics)
metrics <- function(r) {
  m <- sim$outcomes(inp, r)
  f <- r$flows %>% mutate(d = sim$dis_flows(inp, r$flows)) %>% filter(name %in% r$schools$name[r$schools$city])
  s <- r$schools %>% filter(city)
  data.table(gorard = m$gorard, dep_min = sum(f$d * f$cij) / sum(f$d), rest_min = sum((f$flow - f$d) * f$cij) / sum(f$flow - f$d),
             dep_gap = m$dep_gap, mean_min = m$mean_min, p90_min = m$p90_min, over_40 = m$over_40,
             displaced = m$catchment$displaced, in_catchment = 1 - m$catchment$outside_share, empty = m$empty,
             p6_places = sum(r$flows$p6), intake_ds = s$intake[s$name == DS], intake_bms = s$intake[s$name == BMS],
             intake_lh = s$intake[s$name == "Longhill High School"])
}
rules <- function(p6) list(rule = "priorities", p6_share = p6)
OPT <- list(
  today        = list(label = "Today: priority 6 at 5%", p6 = 0.05, pans = NULL),
  p6_20        = list(label = "Priority 6 at 20%", p6 = 0.20, pans = NULL),
  cuts         = list(label = "Stringer and Blatchington Mill cut to 300", p6 = 0.05, pans = cut30),
  council_2024 = list(label = "The council's 2024 proposal: priority 6 at 20%, Stringer and Blatchington Mill at 300", p6 = 0.20, pans = cut30),
  p6_30        = list(label = "Priority 6 at 30%", p6 = 0.30, pans = NULL),
  p6_30_cuts   = list(label = "Priority 6 at 30%, Stringer and Blatchington Mill at 300", p6 = 0.30, pans = cut30))
# Today is priority 6 at 5%: the check below confirms it reproduces the published Today run
runs <- rbindlist(lapply(names(OPT), function(id) rbindlist(lapply(c(2026, 2030), function(y) {
  o <- OPT[[id]]; r <- sim$run_sim(inp, year = y, pans = o$pans, rules = rules(o$p6))
  cbind(data.table(id = id, label = o$label, year = y), metrics(r)) }))))

# ---- Where priority 6 stops binding: places taken under it, by share, with and without the cuts --
sweep <- rbindlist(lapply(c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40), function(p) rbindlist(lapply(list(none = NULL, cuts = cut30), function(pn) {
  r <- sim$run_sim(inp, year = 2026, pans = pn, rules = rules(p)); m <- metrics(r)
  m[, .(p6 = p, cuts = !is.null(pn), p6_places, gorard, dep_min, rest_min, displaced)] }))))

# ---- Check: the published Today run ---------------------------------------------------------
pub <- as.data.table(readRDS(here::here("data-raw", "bh_system", "council_options.rds"))$runs)[id == "today"]
chk <- merge(runs[id == "today", .(year, gorard, dep_min, mean_min)], pub[, .(year, g_pub = gorard, d_pub = dep_min, m_pub = mean_min)], by = "year")
print(chk)
stopifnot(chk[, max(abs(gorard - g_pub), abs(dep_min - d_pub), abs(mean_min - m_pub))] < 1e-6)

# ---- Effects against today, and on the scales of R/30 and R/31 --------------------------------
AJ <- readRDS(here::here("data", "admissions_journeys.rds")); G <- readRDS(here::here("data", "distance_gcse.rds"))
eff <- runs[, .(id, label, d_gorard = gorard - gorard[id == "today"], d_dep_min = dep_min - dep_min[id == "today"],
                d_rest_min = rest_min - rest_min[id == "today"], d_mean_min = mean_min - mean_min[id == "today"],
                d_displaced = displaced - displaced[id == "today"], d_in_catchment = in_catchment - in_catchment[id == "today"]), by = year]
g_today <- runs[id == "today" & year == 2026, gorard]
cost_rng <- function(dmin) { x <- c(dmin / AJ$mpk * AJ$chain_km[c("All 109", "Without London")], -dmin * AJ$per_min[c("All 109", "Without London")] * AJ$per_point); range(x) }
eff[, `:=`(cost_lo = sapply(d_dep_min, function(v) cost_rng(v)[1]), cost_hi = sapply(d_dep_min, function(v) cost_rng(v)[2]),
           gain_lo = -d_gorard / g_today * G$gorard$att8[["lo"]], gain_hi = -d_gorard / g_today * G$gorard$att8[["hi"]])]

out <- list(built = Sys.Date(), source = list(repo = REPO, commit = SHA, files = c("app/R/model.R", "app/R/outcomes.R", "app/data/sim_inputs.rds")),
            pan_now = pan_now[c(DS, BMS)], options = OPT, runs = runs, effects = eff, sweep = sweep, check = chk)
saveRDS(out, here::here("data", "admissions_proposals.rds")); message("Saved data/admissions_proposals.rds")
print(runs[, .(id, year, gorard, dep_min, rest_min, dep_gap, mean_min, p90_min, displaced, in_catchment, p6_places, intake_ds, intake_bms, intake_lh)], digits = 3)
print(eff, digits = 3); print(sweep, digits = 3)
