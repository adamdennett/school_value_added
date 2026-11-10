# 33_admissions_neighbourhoods.R - Whose journeys the admissions changes moved most
# ---------------------------------------------------------------------------
# R/31 and R/32 give city averages. Averages hide the neighbourhoods a change is aimed at,
# such as Whitehawk, which the 2024 map moved out of Longhill's catchment. This runs the same
# published school system model (code and inputs at the commit R/32 uses) and reports, for
# each of its zones (an LSOA, or the part of one on one side of a catchment boundary):
#
#   the 2024 map         today against the pre-2024 catchments, both under today's priorities
#   the 2024 proposal    priority 6 at 20% with Stringer and Blatchington Mill at 300, against
#                        today (Class Divide's 30% with the same cuts gives the same, R/32)
#
# children, modelled disadvantaged children, mean walking and bus minutes to the school
# offered at the city's schools (all children, disadvantaged children), and where they go.
# Zones are grouped by how the 2024 map moved them, and named by the House of Commons
# Library's MSOA names.
#
# Sources and formulas: method.qmd#sec-neighbourhoods-method. Output:
# data/admissions_neighbourhoods.rds (committed). Rscript R/33_admissions_neighbourhoods.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(dplyr) })
REPO <- "adamdennett/bh-school-system"; SHA <- "40a055afdbd14033f6b9bc1579f3728aa3507c0d"   # as R/31 and R/32
D <- here::here("data-raw", "bh_system", "app"); dir.create(file.path(D, "R"), showWarnings = FALSE, recursive = TRUE); dir.create(file.path(D, "data"), showWarnings = FALSE)
for (p in c("app/R/model.R", "app/R/outcomes.R", "app/data/sim_inputs.rds")) {
  f <- file.path(D, sub("^app/", "", p))
  if (!file.exists(f)) download.file(sprintf("https://raw.githubusercontent.com/%s/%s/%s", REPO, SHA, p), f, mode = "wb", quiet = TRUE) }
sim <- new.env(); sys.source(file.path(D, "R", "model.R"), envir = sim); sys.source(file.path(D, "R", "outcomes.R"), envir = sim)
inp <- readRDS(file.path(D, "data", "sim_inputs.rds"))
LH <- "Longhill High School"; DS <- "Dorothy Stringer School"; V <- "Varndean School"; BMS <- "Blatchington Mill School"
cut30 <- setNames(inp$schools$pan[match(c(DS, BMS), inp$schools$name)] - 30, c(DS, BMS))

# ---- Names: LSOA 2021 -> MSOA 2021 (ONS) -> House of Commons Library MSOA names ------------------
nm_f <- here::here("data-raw", "msoa_names", "MSOA-Names-2.2.csv"); dir.create(dirname(nm_f), showWarnings = FALSE)
if (!file.exists(nm_f)) download.file("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv", nm_f, mode = "wb", quiet = TRUE)
hcl <- fread(nm_f, encoding = "UTF-8")[, .(MSOA21CD = msoa21cd, area = msoa21hclnm)]
lk <- unique(fread(here::here("data-raw", "census2021", "ons_custom", "oa_lsoa_msoa_2021.csv"))[, .(LSOA21CD, MSOA21CD)])
nm <- merge(lk, hcl, by = "MSOA21CD")[, .(lsoa = LSOA21CD, msoa = MSOA21CD, area)]

# ---- The runs, by zone -------------------------------------------------------------------------
RUNS <- list(pre2024 = list(design = "Pre-2024 catchments", p6 = 0.05, pans = NULL), today = list(design = "Current catchments", p6 = 0.05, pans = NULL),
             proposal = list(design = "Current catchments", p6 = 0.20, pans = cut30))
by_zone <- function(id) { o <- RUNS[[id]]
  r <- sim$run_sim(inp, year = 2026, pans = o$pans, design = o$design, rules = list(rule = "priorities", p6_share = o$p6))
  f <- as.data.table(r$flows); f[, dis := sim$dis_flows(inp, r$flows)]; city <- r$schools$name[r$schools$city]
  f[, .(children = sum(flow), dis = sum(dis),
        min_all = sum((flow * cij)[name %in% city]) / sum(flow[name %in% city]), min_dis = sum((dis * cij)[name %in% city]) / sum(dis[name %in% city]),
        to_lh = sum(flow[name == LH]) / sum(flow), to_dsv = sum(flow[name %in% c(DS, V)]) / sum(flow),
        main = names(which.max(tapply(flow, name, sum)))), by = .(zone, lsoa)][, run := id][] }
Z <- rbindlist(lapply(names(RUNS), by_zone))
W <- dcast(Z, zone + lsoa ~ run, value.var = c("children", "dis", "min_all", "min_dis", "to_lh", "to_dsv", "main"))
# Catchment of each zone on each map, from the model's own designs
cat_now <- inp$designs[["Current catchments"]]$zone; cat_pre <- inp$designs[["Pre-2024 catchments"]]$zone
W[, `:=`(c_pre = cat_pre[zone], c_now = cat_now[zone])]
W[, group := fcase(c_pre == "Longhill" & c_now == "DS_Varndean", "Out of Longhill's catchment, into Stringer and Varndean's",
                   c_pre == "DS_Varndean" & c_now == "Longhill", "Into Longhill's catchment, from Stringer and Varndean's",
                   c_pre != c_now, "Other catchment changes", default = "Unchanged")]
W <- merge(W, as.data.table(inp$zones)[, .(zone, idaci = idaci_score, fsm)], by = "zone")
W <- merge(W, nm, by = "lsoa", all.x = TRUE)
W[, `:=`(d_map = min_all_today - min_all_pre2024, d_map_dis = min_dis_today - min_dis_pre2024,
         d_prop = min_all_proposal - min_all_today, d_prop_dis = min_dis_proposal - min_dis_today)]

# ---- Summaries --------------------------------------------------------------------------------
wmean <- function(x, w) sum(x * w) / sum(w)
grp <- W[, .(zones = .N, areas = paste(sort(unique(na.omit(area))), collapse = ", "), children = sum(children_today), dis = sum(dis_today), idaci = wmean(idaci, children_today),
             min_pre = wmean(min_all_pre2024, children_today), min_now = wmean(min_all_today, children_today),
             dis_pre = wmean(min_dis_pre2024, dis_today), dis_now = wmean(min_dis_today, dis_today),
             lh_pre = wmean(to_lh_pre2024, children_today), lh_now = wmean(to_lh_today, children_today),
             dsv_pre = wmean(to_dsv_pre2024, children_today), dsv_now = wmean(to_dsv_today, children_today)), by = group]
grp[, `:=`(d_all = min_now - min_pre, d_dis = dis_now - dis_pre)]
# The council's proposal, by today's catchment
pgrp <- W[, .(children = sum(children_today), idaci = wmean(idaci, children_today), min_now = wmean(min_all_today, children_today), min_prop = wmean(min_all_proposal, children_today),
              dis_now = wmean(min_dis_today, dis_today), dis_prop = wmean(min_dis_proposal, dis_today)), by = .(catchment = c_now)][, `:=`(d_all = min_prop - min_now, d_dis = dis_prop - dis_now)][]
# How many children's journeys moved by more than 2 and 5 minutes, either way
spread <- rbindlist(lapply(c("d_map", "d_prop"), function(v) W[, .(change = v, up5 = sum(children_today[get(v) > 5]), up2 = sum(children_today[get(v) > 2]),
  down2 = sum(children_today[get(v) < -2]), all = sum(children_today))]))

# Whitehawk on the Attainment 8 scales of R/30 and R/31, as an "if", for its disadvantaged children
AJ <- readRDS(here::here("data", "admissions_journeys.rds"))
wh <- grp[group == "Out of Longhill's catchment, into Stringer and Varndean's"]
cost_rng <- function(dmin) range(c(dmin / AJ$mpk * AJ$chain_km[c("All 109", "Without London")], -dmin * AJ$per_min[c("All 109", "Without London")] * AJ$per_point))
wh_cost <- cost_rng(wh$d_dis)

out <- list(built = Sys.Date(), source = list(repo = REPO, commit = SHA, names = "House of Commons Library MSOA Names 2.2"),
            zones = W, groups = grp, proposal_groups = pgrp, spread = spread, whitehawk_cost = wh_cost)
saveRDS(out, here::here("data", "admissions_neighbourhoods.rds")); message("Saved data/admissions_neighbourhoods.rds")
print(grp[, .(group, zones, children, dis, idaci, min_pre, min_now, d_all, dis_pre, dis_now, d_dis, lh_pre, lh_now, dsv_pre, dsv_now)], digits = 3)
print(grp[, .(group, areas)]); print(pgrp, digits = 3); print(spread); print(wh_cost)
print(W[group != "Unchanged"][order(-d_map_dis)][, .(zone, area, idaci, kids = children_today, d_map, d_map_dis, main_pre2024, main_today)], digits = 3)
print(W[order(-d_prop)][1:10, .(zone, area, c_now, idaci, kids = children_today, d_prop, d_prop_dis, main_today, main_proposal)], digits = 3)
