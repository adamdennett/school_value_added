# 47_briefing_checks.R - Pupil forecasts: authorities', DfE's and ours, with births added
# ---------------------------------------------------------------------------
# Follows up the House of Commons Library briefing "Declining pupil numbers in England"
# (CBP-12207, 22 September 2026), which reports that most local authorities over-forecast
# their pupil numbers, and that DfE's national projections fall faster than the
# authorities' own. Four checks:
#
#  A. Authorities' forecasts made in 2024/25 (base January 2025) against what the
#     January 2026 census found, for primary (Reception to Year 6), Year 7 and Years 7
#     to 11, by authority. Then the same one-year test for this project's cohort
#     roll-forward, rerun as if from January 2025 (ratios from the three transitions to
#     2024/25; Reception from births, below), so the two methods face the same test.
#  B. DfE's national pupil projections (July 2026) against our roll-forward for England:
#     ages 5 to 10 (Years 1 to 6) and 11 to 15 (Years 7 to 11) in state primary and
#     secondary schools, and DfE's all-age totals, which the briefing quotes.
#  C. Births: Reception pupils in each authority as a share of the births that feed them,
#     then Reception projected from births registered to 2025, so primary totals run to
#     2029/30 and Year 7 to 2036/37.
#  D. Looking ahead: the authorities' forecasts for 2029/30 (and Year 7 to 2031/32)
#     against our projections from January 2026.
#  E. London: the boroughs' forecasts as London Councils reports them (January 2026:
#     Reception -2.5%, Year 7 -3.8%, 2025/26 to 2029/30, forecast to forecast), against
#     the January 2026 census and our roll-forward from it.
#
# Sources (all published):
#   Census year groups: as R/42 (data-raw/ees/spc_2526/yeargroup_totals.csv), state-funded
#     primary and secondary schools, January 2016 to January 2026.
#   Authority forecasts: DfE "School capacity" 2024/25 (released 2026), file
#     forecasts-pupils-hdc-hif_national-regional-la-pa_202425.csv: pupils on roll in
#     January 2025 and authorities' forecasts for 2025/26 onwards (primary to 2029/30,
#     secondary to 2031/32), state-funded mainstream schools. Downloaded from
#     https://content.explore-education-statistics.service.gov.uk/api/releases/960750dd-6ed7-4e32-a9df-3cc9132bb1a4/files
#     to data-raw/ees/school_capacity_2425/.
#   National projections: DfE "National pupil projections", reporting year 2026 (July
#     2026), pupilprojprincipal_2026.csv, from
#     https://content.explore-education-statistics.service.gov.uk/api/releases/46ea1775-0174-4fb9-a045-e2a615f9764b/files
#     to data-raw/ees/npp_2026/. Its model_year Y is the January census of year Y.
#   Births: ONS live births by local authority of usual residence of the mother,
#     calendar years 2013 to 2025, county and unitary authorities (April 2023), Nomis
#     data set NM_206_1, saved as data-raw/ons/births_la_2013_2025_nomis_NM_206_1.csv.
#
# Calculations:
#   Areas as R/42: three reorganised councils combined (DEM$combine).
#   A child in Reception in academic year t/t+1 was born between September of t-5 and
#     August of t-4, so the births behind a Reception year are 4/12 of calendar year t-5
#     and 8/12 of t-4. Reception / those births is the "births ratio", which takes in
#     moves in and out, independent schools and special schools. It is averaged over the
#     last three years, as the progression ratios are.
#   Errors are (forecast - actual) / actual, in %.
#
# Output: data/briefing_checks.rds.   Rscript R/47_briefing_checks.R   (under a minute)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(data.table))
DEM <- readRDS(here::here("data", "demographics.rds"))
area_of <- function(la) ifelse(la %in% names(DEM$combine), DEM$combine[la], la)
canon <- function(x) gsub("[^a-z]", "", tolower(gsub("&", "and", x)))   # match names across publishers

# ---- census year groups (as R/42) -------------------------------------------------------
Y <- fread(here::here("data-raw", "ees", "spc_2526", "yeargroup_totals.csv"))[phase_type_grouping %in% c("State-funded primary", "State-funded secondary")]
ycols <- c(R = "year_group_reception_pupil_count", setNames(paste0("year_group_", 1:11, "_pupil_count"), paste0("Y", 1:11)))
Y <- Y[, c(list(year = paste0(substr(time_period, 1, 4), "-", substr(time_period, 5, 6)), level = geographic_level, la = la_name),
           lapply(.SD, as.numeric)), .SDcols = unname(ycols)]
setnames(Y, unname(ycols), names(ycols))
Y[level == "Local authority", area := area_of(la)][level == "National", area := "England"]
Y <- Y[!is.na(area) & la != "Isles of Scilly"]
steps <- c("R", paste0("Y", 1:11))
yg <- Y[, lapply(.SD, sum, na.rm = TRUE), by = .(level, area, year), .SDcols = steps]
YEARS <- sort(unique(yg$year))
nxt_year <- function(y) paste0(as.integer(substr(y, 1, 4)) + 1, "-", sprintf("%02d", (as.integer(substr(y, 6, 7)) + 1) %% 100))
tot <- function(d) d[, `:=`(primary = R + Y1 + Y2 + Y3 + Y4 + Y5 + Y6, sec = Y7 + Y8 + Y9 + Y10 + Y11)][]

# ---- births and the births ratio -------------------------------------------------------
B <- fread(here::here("data-raw", "ons", "births_la_2013_2025_nomis_NM_206_1.csv"))
setnames(B, tolower(names(B)))
B <- B[geography_type %like% "county / unitary" | geography_name == "England", .(cal = as.integer(date_name), name = geography_name, births = obs_value)]
B[, area := ifelse(name == "England", "England", area_of(name))]
B <- B[, .(births = sum(births)), by = .(area, cal)]
# the births behind Reception in academic year starting t: 4/12 of t-5 and 8/12 of t-4
beff <- function(area_, t) { b <- B[area == area_]; (4 * b$births[match(t - 5, b$cal)] + 8 * b$births[match(t - 4, b$cal)]) / 12 }
yg_la <- yg[level %in% c("Local authority", "National")]
yg_la[, t := as.integer(substr(year, 1, 4))]
yg_la[, key := canon(area)]; B[, key := canon(area)]
bkey <- unique(B[, .(key, barea = area)])
yg_la <- merge(yg_la, bkey, by = "key", all.x = TRUE)
unmatched <- unique(yg_la[is.na(barea), area])
message("Authorities with no births match: ", if (length(unmatched)) paste(unmatched, collapse = ", ") else "none")
yg_la[!is.na(barea), b_eff := mapply(beff, barea, t)]
yg_la[, br := R / b_eff]

# ---- the roll-forward, from any base year -------------------------------------------------
# progression ratios from the three transitions ending at `base`, the births ratio over the
# three Reception years ending at `base`; h years ahead
roll_from <- function(base, h) {
  bi <- match(base, YEARS); use <- YEARS[(bi - 3):(bi - 1)]           # transitions use -> use+1
  rat <- rbindlist(lapply(2:length(steps), function(k) {
    a <- steps[k - 1]; b <- steps[k]
    x <- merge(yg_la[year %in% use, .(area, year, from = get(a))],
               yg_la[, .(area, year_prev = c(NA, YEARS)[match(year, YEARS)], to = get(b))], by.x = c("area", "year"), by.y = c("area", "year_prev"))
    x[, .(step = b, r = sum(to) / sum(from)), by = area]
  }))
  brr <- yg_la[year %in% YEARS[(bi - 2):bi], .(br = mean(br)), by = area]
  now <- yg_la[year == base]
  out <- list(); cur <- now[, c("area", "barea", steps), with = FALSE]; yr <- base
  for (j in 1:h) {
    yr <- nxt_year(yr); t <- as.integer(substr(yr, 1, 4))
    n2 <- copy(cur)
    for (k in length(steps):2) { s <- steps[k]; p <- steps[k - 1]
      n2[[s]] <- cur[[p]] * rat[step == s][match(cur$area, area), r] }
    n2$R <- brr[match(cur$area, area), br] * mapply(function(a, tt) if (is.na(a)) NA_real_ else beff(a, tt), cur$barea, t)
    out[[j]] <- tot(copy(n2))[, year := yr]
    cur <- n2
  }
  rbindlist(out)
}

# ---- A. authorities' forecasts, scored against January 2026 -------------------------------
FC <- fread(here::here("data-raw", "ees", "school_capacity_2425", "data", "forecasts-pupils-hdc-hif_national-regional-la-pa_202425.csv"),
            select = c("time_period", "geographic_level", "la_name", "nc_year_group", "pupil_forecast_and_202425_actuals"))
FC <- FC[geographic_level %in% c("Local authority", "National")]
FC[, n := as.numeric(pupil_forecast_and_202425_actuals)]
FC[, year := paste0(substr(time_period, 1, 4), "-", substr(time_period, 5, 6))]
FC[, area := ifelse(geographic_level == "National", "England", area_of(la_name))]
gmap <- c(Reception = "R", setNames(paste0("Y", 1:11), sprintf("Year %02d", 1:11)))
FC <- FC[nc_year_group %in% names(gmap)][, g := gmap[nc_year_group]]
fc <- dcast(FC[, .(n = sum(n)), by = .(area, year, g)], area + year ~ g, value.var = "n")
fc <- tot(fc[, c("area", "year", steps), with = FALSE])
fc[, key := canon(area)]
actual <- tot(copy(yg_la[, c("area", "key", "year", steps), with = FALSE]))
fkeys <- setdiff(unique(fc$key), unique(actual$key)); message("Forecast areas with no census match: ", paste(fkeys, collapse = ", "))

ours25 <- roll_from("2024-25", 7)                              # as if made in 2024/25
ours25[, key := canon(area)]
score <- function(pred, who) {
  p <- pred[year == "2025-26", .(key, primary, Y7, sec)]
  a <- actual[year == "2025-26", .(key, area, a_primary = primary, a_Y7 = Y7, a_sec = sec)]
  m <- merge(p, a, by = "key")
  m[, .(area, who, primary = 100 * (primary / a_primary - 1), Y7 = 100 * (Y7 / a_Y7 - 1), sec = 100 * (sec / a_sec - 1))]
}
errs <- rbind(score(fc, "Authorities' forecasts"), score(ours25, "Cohort roll-forward"))
summ_err <- function(e) e[area != "England", .(authorities = .N,
  over_primary = sum(primary > 0, na.rm = TRUE), over_sec = sum(sec > 0, na.rm = TRUE), over_Y7 = sum(Y7 > 0, na.rm = TRUE),
  med_primary = median(primary, na.rm = TRUE), med_sec = median(sec, na.rm = TRUE), med_Y7 = median(Y7, na.rm = TRUE),
  mae_primary = mean(abs(primary), na.rm = TRUE), mae_sec = mean(abs(sec), na.rm = TRUE), mae_Y7 = mean(abs(Y7), na.rm = TRUE)), by = who]
A_summary <- summ_err(errs)
A_england <- errs[area == "England"]
# base check: the forecast file's January 2025 actuals against the census
base_chk <- merge(fc[year == "2024-25", .(key, f_primary = primary, f_sec = sec)], actual[year == "2024-25", .(key, area, primary, sec)], by = "key")
base_chk[, `:=`(d_primary = 100 * (f_primary / primary - 1), d_sec = 100 * (f_sec / sec - 1))]

# ---- B. DfE national projections (July 2026) --------------------------------------------
NP <- fread(here::here("data-raw", "ees", "npp_2026", "data", "pupilprojprincipal_2026.csv"))
# Ages 5 and over are given as FTE only (the same as headcount at compulsory ages). Middle
# and all-through schools appear as ranges ("11 to 15" in state primary), so both phases count.
NP <- NP[pupil_status == "FTE"][, n := as.numeric(pupil_count)]
np_age <- function(ages) NP[establishment_type_group %in% c("State primary", "State secondary") & pupil_age %in% ages, .(n = sum(n), measure = population_measure[1]), by = model_year]
dfe <- rbind(np_age(c(as.character(5:10), "5 to 10"))[, what := "DfE: state primary and secondary, ages 5 to 10"],
             np_age(c(as.character(11:15), "11 to 15"))[, what := "DfE: state primary and secondary, ages 11 to 15"],
             NP[establishment_type_group == "State secondary" & pupil_age == "All", .(n, measure = population_measure, what = "DfE: state secondary, all ages"), by = model_year][, .(model_year, n, measure, what)],
             NP[establishment_type_group == "State nursery and primary" & pupil_age == "All", .(n, measure = population_measure, what = "DfE: state nursery and primary, all ages"), by = model_year][, .(model_year, n, measure, what)])
dfe[, year := paste0(model_year - 1, "-", sprintf("%02d", model_year %% 100))]
ours26 <- roll_from("2025-26", 11)                            # from January 2026
eng_ours <- rbind(actual[area == "England", .(year, y1_6 = Y1 + Y2 + Y3 + Y4 + Y5 + Y6, sec, kind = "Census")],
                  ours26[area == "England", .(year, y1_6 = Y1 + Y2 + Y3 + Y4 + Y5 + Y6, sec, kind = "Roll-forward")])
B_compare <- merge(eng_ours, dcast(dfe, year ~ what, value.var = "n"), by = "year", all = TRUE)[order(year)]
pk <- function(x, from) 100 * (x / from - 1)
B_headline <- list(
  dfe_sec_all_peak = dfe[what == "DfE: state secondary, all ages"][which.max(n)],
  dfe_sec_all_2031 = dfe[what == "DfE: state secondary, all ages" & model_year == 2031, n],
  dfe_sec_11_15_2026 = dfe[what == "DfE: state primary and secondary, ages 11 to 15" & model_year == 2026, n],
  dfe_sec_11_15_2031 = dfe[what == "DfE: state primary and secondary, ages 11 to 15" & model_year == 2031, n],
  ours_sec_2026 = actual[area == "England" & year == "2025-26", sec],
  ours_sec_2031 = ours26[area == "England" & year == "2030-31", sec],
  dfe_prim_5_10_2026 = dfe[what == "DfE: state primary and secondary, ages 5 to 10" & model_year == 2026, n],
  dfe_prim_5_10_2030 = dfe[what == "DfE: state primary and secondary, ages 5 to 10" & model_year == 2030, n],
  ours_y1_6_2026 = eng_ours[kind == "Census" & year == "2025-26", y1_6],
  ours_y1_6_2030 = eng_ours[kind == "Roll-forward" & year == "2029-30", y1_6])

# ---- C. births ---------------------------------------------------------------------------
births_eng <- B[area == "England"][order(cal)]
br_now <- yg_la[year %in% tail(YEARS, 3), .(births_ratio = mean(br)), by = area]
births_chg <- B[cal %in% c(2015, 2021, 2025), .(area, cal, births)] |> dcast(area ~ cal, value.var = "births")
setnames(births_chg, c("2015", "2021", "2025"), c("b2015", "b2021", "b2025"))
births_chg[, `:=`(chg_2015_2025 = 100 * (b2025 / b2015 - 1), chg_2021_2025 = 100 * (b2025 / b2021 - 1))]

# ---- D. looking ahead: authorities against ours ------------------------------------------
la_ahead <- merge(fc[year %in% c("2029-30", "2031-32"), .(key, year, f_primary = primary, f_Y7 = Y7, f_sec = sec)],
                  ours26[, .(key = canon(area), area, year, o_primary = primary, o_Y7 = Y7, o_sec = sec)], by = c("key", "year"))
la_ahead[, `:=`(gap_primary = 100 * (f_primary / o_primary - 1), gap_Y7 = 100 * (f_Y7 / o_Y7 - 1), gap_sec = 100 * (f_sec / o_sec - 1))]
D_summary <- la_ahead[area != "England", .(authorities = .N, above_primary = sum(gap_primary > 0, na.rm = TRUE),
  above_Y7 = sum(gap_Y7 > 0, na.rm = TRUE), above_sec = sum(gap_sec > 0, na.rm = TRUE),
  med_gap_primary = median(gap_primary, na.rm = TRUE), med_gap_Y7 = median(gap_Y7, na.rm = TRUE), med_gap_sec = median(gap_sec, na.rm = TRUE)), by = year]
eng_ahead <- la_ahead[area == "England"]
bh <- list(errors = errs[area == "Brighton and Hove"], ahead = la_ahead[area == "Brighton and Hove"],
           births = births_chg[area == "Brighton and Hove"], ours = ours26[area == "Brighton and Hove", .(year, R, primary, Y7, sec)],
           forecast = fc[area == "Brighton and Hove", .(year, R, primary, Y7, sec)], census = actual[area == "Brighton and Hove", .(year, R, primary, Y7, sec)])

# ---- E. London: the boroughs' forecasts against the census and ours -----------------------
# London Councils (Managing school rolls and maintaining educational standards in London,
# January 2026) reports the boroughs' SCAP forecasts as Reception -2.5% and Year 7 -3.8%
# from 2025/26 to 2029/30, measured forecast to forecast. Here the same forecasts are set
# against the January 2026 census, and our roll-forward from that census.
lon <- unique(as.data.table(DEM$trend)[region == "London" & level == "Local authority" & area != "City of London", area])
lon_sum <- function(d, yr) d[area %in% lon & year == yr, .(R = sum(R), Y7 = sum(Y7))]
london <- rbind(cbind(what = "Census 2025/26", lon_sum(actual, "2025-26")),
                cbind(what = "Boroughs' forecast for 2025/26", lon_sum(fc, "2025-26")),
                cbind(what = "Boroughs' forecast for 2029/30", lon_sum(fc, "2029-30")),
                cbind(what = "Roll-forward for 2029/30", lon_sum(ours26, "2029-30")))
london_chg <- data.table(measure = c("Boroughs' forecast to forecast, 2025/26 to 2029/30", "Boroughs' 2029/30 forecast against the 2025/26 census",
                                     "Roll-forward from the 2025/26 census", "Boroughs' 2025/26 forecast against the census"),
  R = 100 * c(london$R[3] / london$R[2], london$R[3] / london$R[1], london$R[4] / london$R[1], london$R[2] / london$R[1]) - 100,
  Y7 = 100 * c(london$Y7[3] / london$Y7[2], london$Y7[3] / london$Y7[1], london$Y7[4] / london$Y7[1], london$Y7[2] / london$Y7[1]) - 100)

res <- list(london = london, london_chg = london_chg, A_summary = A_summary, A_england = A_england, A_errors = errs, base_chk = base_chk,
            B_compare = B_compare, B_headline = B_headline, dfe = dfe,
            births_eng = births_eng, births_chg = births_chg, br_now = br_now, ours26 = ours26,
            D_summary = D_summary, eng_ahead = eng_ahead, la_ahead = la_ahead, bh = bh, built = Sys.Date())
saveRDS(res, here::here("data", "briefing_checks.rds"))

options(width = 160)
cat("\n== Base check: forecast file's January 2025 actuals against the census (median % difference)\n"); print(base_chk[, .(n = .N, d_primary = median(d_primary, na.rm = TRUE), d_sec = median(d_sec, na.rm = TRUE))])
cat("\n== A. One year ahead, 2025/26, by authority\n"); print(A_summary, digits = 3)
cat("\n== A. England\n"); print(A_england, digits = 3)
cat("\n== B. England: census, our roll-forward and DfE (July 2026)\n"); print(B_compare, digits = 7)
cat("\n== D. Authorities' forecasts against ours\n"); print(D_summary, digits = 3); print(eng_ahead, digits = 4)
cat("\n== Brighton and Hove\n"); print(bh, digits = 4)
cat("\n== Births, England\n"); print(births_eng)
cat("\n== E. London (32 boroughs, not the City)\n"); print(london); print(london_chg, digits = 3)
