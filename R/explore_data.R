# explore_data.R - The data behind the Explore by area map, shared with the trust pages
# ---------------------------------------------------------------------------
# Every school in England with the map's measures, value added year by year for the moving
# averages, the measure definitions, colour caps and level definitions. Sourced by explore.qmd
# and trust_*.qmd after site_helpers.R and explore_js.R. dat keeps URN so a page can subset it.
# ---------------------------------------------------------------------------

V <- load_va()
P <- readRDS(here::here("data", "place_absence.rds"))

# Combined and mayoral authorities: the ONS lookup (May 2025) for those
# established, by district; the Devolution Priority Programme areas,
# announced February 2025, by upper-tier council. A council area takes the
# authority of its districts, so schools without a district still get one.
ons <- read.csv(here::here("data", "lookups", "lad_cauth_2025.csv"))
new_ma <- read.csv(here::here("data", "lookups", "new_mayoral_authorities.csv"))
w <- va_wide(V) |> left_join(P$abs_schools |> select(URN, abs_excess), by = "URN")
la_ca <- w |> filter(!is.na(lad)) |> distinct(LANAME, lad) |>
  inner_join(ons, by = c("lad" = "LAD25NM")) |> distinct(LANAME, ca = CAUTH25NM)
la_ca <- bind_rows(la_ca, new_ma |> transmute(LANAME, ca = paste0(authority, " (new)"))) |>
  distinct(LANAME, .keep_all = TRUE)
w <- w |> left_join(la_ca, by = "LANAME")

# The measures the Sussex maps offer, for every school in England: value
# added in its forms, attendance, results, intake and the school itself
# (R/02b_school_context.R builds the national attendance and governance
# measures the Sussex pages take from the original project's area files)
CTX <- readRDS(here::here("data", "school_context.rds"))
w <- w |>
  left_join(CTX |> select(URN, persistent, illness, unauth, reduced, susp_rate, gov_nat), by = "URN") |>
  left_join(P$va_m2 |> filter(group == "all") |> select(URN, va_m2), by = "URN") |>
  mutate(religion = case_when(religion %in% c("Does not apply", "None") | is.na(religion) ~ "None",
                              religion %in% c("Church of England", "Roman Catholic", "Christian") ~ religion,
                              TRUE ~ "Other faith"))

dat <- w |>
  transmute(URN, SCHNAME, LANAME, region = gor_name, ca, la = LANAME, lat, lon, pupils,
            va_shrunk_all, va_lo_all, va_hi_all, va_shrunk_disadvantaged, va_shrunk_non_disadvantaged,
            va_shrunk_girls, va_shrunk_boys, va_kind_all, va_kind_disadvantaged, va_kind_non_disadvantaged, va_m2, va_2526_all, v1_all,
            abs_excess, absence, persistent, illness, unauth, reduced, susp_rate,
            att8, att8_dis, att8_non, fsm, ks2_c, eal, sen_support, sen_ehcp, mobile, l4,
            gov_nat, ofsted_combined, ofsted_rc, ofsted = ofsted_model, religion, ofsted_latest,
            up = va_lo_all > 0, down = va_hi_all < 0) |>   # flags before rounding
  mutate(across(where(is.double) & !c(lat, lon), ~round(.x, 2)))

# Value added year by year, one array per group in YEARS order (null where
# the school had no result), for the trajectories and moving averages
yrs <- va_years(V)
YEARS <- sort(unique(yrs$year_label))
for (g in names(GROUPS)) {
  yg <- yrs |> filter(group == g) |>
    mutate(year_label = factor(year_label, levels = YEARS), va = round(va, 2)) |>
    tidyr::complete(URN, year_label) |> arrange(URN, year_label) |>
    group_by(URN) |> summarise(ys = list(va), .groups = "drop")
  dat[[paste0("y_", g)]] <- yg$ys[match(dat$URN, yg$URN)]
}
# dat keeps URN: pages subset on it (the explorer ignores fields it does not use)

# Colour caps for moving averages: the 97.5th percentile of |average| over
# every centre year, by group and window, as colour_caps() does for the rest
caps_ma <- lapply(setNames(names(GROUPS), names(GROUPS)), function(g) {
  yg <- yrs |> filter(group == g)
  lapply(setNames(c(1, 3, 5), c(1, 3, 5)), function(n) {
    ma <- bind_rows(lapply(YEARS, function(cc) centred_ma(yg, cc, n)))$ma
    round(unname(quantile(abs(ma), 0.975, na.rm = TRUE)), 1) })
})
pv <- load_prov()
prov_year <- if (is.null(pv)) "" else pv$year

gy <- yl(max(V$spec$years))
measures <- tibble::tribble(
  ~var, ~label, ~short, ~grp, ~scale, ~good, ~dp, ~unit, ~group,
  "va_shrunk_all", "Value added v2.1, all pupils", NA, "Value added", "div", "high", 1, " pts", "all",
  "va_shrunk_disadvantaged", "Value added v2.1, disadvantaged", NA, "Value added", "div", "high", 1, " pts", "disadvantaged",
  "va_shrunk_non_disadvantaged", "Value added v2.1, non-disadvantaged", NA, "Value added", "div", "high", 1, " pts", "non_disadvantaged",
  "va_shrunk_girls", "Value added v2.1, girls", NA, "Value added", "div", "high", 1, " pts", "girls",
  "va_shrunk_boys", "Value added v2.1, boys", NA, "Value added", "div", "high", 1, " pts", "boys",
  "va_kind_all", "Value added allowing for the kind of absence, all pupils", "Kind of absence", "Value added", "div", "high", 1, " pts", NA,
  "va_kind_disadvantaged", "Value added allowing for the kind of absence, disadvantaged", "Kind of absence, disadv.", "Value added", "div", "high", 1, " pts", NA,
  "va_kind_non_disadvantaged", "Value added allowing for the kind of absence, non-disadvantaged", "Kind of absence, non-disadv.", "Value added", "div", "high", 1, " pts", NA,
  "va_m2", "Value added with no allowance for absence, all pupils", "No allowance for absence", "Value added", "div", "high", 1, " pts", NA,
  "va_2526_all", "Value added 2025/26 PROVISIONAL, all pupils", "2025/26 (prov.)", "Value added", "div", "high", 1, " pts", NA,
  "v1_all", "Value added v1 (published, within the LA), all pupils", "v1", "Value added", "div", "high", 1, " pts", NA,
  "abs_excess", "Absence against intake, 2021/22 to 2024/25", NA, "Attendance", "div", "low", 1, " pp", NA,
  "absence", paste0("Overall absence, ", gy), "Absence", "Attendance", "seq", "low", 1, "%", NA,
  "persistent", paste0("Persistent absence, ", gy), "Persistent absence", "Attendance", "seq", "low", 1, "%", NA,
  "illness", paste0("Illness absence, ", gy, ", full year"), "Illness absence", "Attendance", "seq", "low", 1, "%", NA,
  "unauth", paste0("Unauthorised absence, ", gy, ", full year"), "Unauthorised", "Attendance", "seq", "low", 1, "%", NA,
  "reduced", paste0("Reduced timetables, ", gy, ", full year"), "Reduced timetables", "Attendance", "seq", "low", 2, "%", NA,
  "susp_rate", paste0("Suspensions per 100 pupils, ", gy), "Suspensions /100", "Attendance", "seq", "low", 1, "", NA,
  "att8", paste0("Attainment 8, all pupils, ", gy), "Attainment 8", "Results", "seq", "high", 1, "", NA,
  "att8_dis", paste0("Attainment 8, disadvantaged pupils, ", gy), "Attainment 8, disadv.", "Results", "seq", "high", 1, "", NA,
  "att8_non", paste0("Attainment 8, non-disadvantaged pupils, ", gy), "Attainment 8, non-disadv.", "Results", "seq", "high", 1, "", NA,
  "fsm", "Disadvantaged pupils (FSM6) %", "FSM6 %", "Intake", "seq", "", 1, "%", NA,
  "ks2_c", "Prior attainment at KS2 (centred)", "KS2 (centred)", "Intake", "seq", "", 1, "", NA,
  "eal", "English as an additional language %", "EAL %", "Intake", "seq", "", 1, "%", NA,
  "sen_support", "SEN support %", "SEN support %", "Intake", "seq", "", 1, "%", NA,
  "sen_ehcp", "Education, health and care plans %", "EHCP %", "Intake", "seq", "", 1, "%", NA,
  "mobile", "Pupil mobility %", "Mobility %", "Intake", "seq", "", 1, "%", NA,
  "l4", "Neighbourhood education (% level 4+)", "Neighbourhood L4+ %", "Intake", "seq", "", 1, "%", NA,
  "pupils", "Pupils on roll", "Pupils", "Intake", "seq", "", 0, "", NA,
  "gov_nat", paste0("Who runs it (GIAS, ", format(attr(CTX, "gias_date"), "%B %Y"), ")"), "Run by", "The school", "cat", "", 0, "", NA,
  "ofsted_combined", "Ofsted, report card where there is one, else the latest graded judgements", "Ofsted", "The school", "cat", "", 0, "", NA,
  "ofsted_rc", "Ofsted report card, achievement (since November 2025)", "Report card", "The school", "cat", "", 0, "", NA,
  "ofsted", "Ofsted rating, last graded inspection before September 2024", "Ofsted (to Aug 2024)", "The school", "cat", "", 0, "", NA,
  "religion", "Religious character", "Religious character", "The school", "cat", "", 0, "", NA)
CATS <- c(MAP_CATS, list(gov_nat = MAP_CATS$gov))
NONE <- c(as.list(MAP_NONE), list(gov_nat = "not recorded"))
levels <- tibble::tibble(key = c("region", "ca", "la"),
                         label = c("Region", "Combined or mayoral authority", "Local authority (upper tier)"))
notes <- setNames(as.list(rep("New mayoral strategic authority, from the Devolution Priority Programme announced in February 2025; not yet in the ONS lookup.",
                                n_distinct(new_ma$authority))), paste0(unique(new_ma$authority), " (new)"))
