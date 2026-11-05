# 15_graduate_deprivation.R - Two kinds of deprived neighbourhood?
# ---------------------------------------------------------------------------
# The covariates experiment (R/13) and the v2.2 candidates (R/14) found that,
# at the same share of graduates and with the same pupils, schools in more
# deprived neighbourhoods do a little better. The hypothesis tested here:
# that this is inner-city, near-university and London neighbourhoods full of
# recent graduates, culturally and educationally rich but not yet financially
# so, and that it is geographically different from deprived towns without
# graduate jobs. If so:
#   1. the effect should depend on neighbourhood education (an interaction);
#   2. it should sit in London and near universities, not in low-graduate
#      places (the effect estimated separately by kind of place);
#   3. it should go when early-career measures are added (aged 25-34, private
#      renting, students);
#   4. of the parts of deprivation, overcrowding should carry it, not ill
#      health, long-term sickness or unemployment.
#
# All fits: v2.1 plus the school's own White British share (each January, as
# R/14), maximum likelihood, identical rows within each group, measures over
# the 150 Output Areas nearest each school.
#
# Output: data/graduate_deprivation.rds (committed).
# Rscript R/15_graduate_deprivation.R   (several minutes)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
source(here::here("pipeline", "08_census.R"))
suppressPackageStartupMessages(library(lme4))
num <- function(x) suppressWarnings(as.numeric(x))
K <- 150; UNI_KM <- 5; BH <- "Brighton and Hove"
pc <- function(a, b) 100 * a / b

# ---- 1. Output Area measures ------------------------------------------------------
t11 <- fetch_census_table("ts011"); t07 <- fetch_census_table("ts007a"); t52 <- fetch_census_table("ts052")
t37 <- fetch_census_table("ts037"); t66 <- fetch_census_table("ts066"); t54 <- fetch_census_table("ts054")
oa <- tibble(oa21cd = t11[["geography code"]],
  dep = pc(t11[["Household deprivation: Household is deprived in two dimensions; measures: Value"]] +
           t11[["Household deprivation: Household is deprived in three dimensions; measures: Value"]] +
           t11[["Household deprivation: Household is deprived in four dimensions; measures: Value"]],
           t11[["Household deprivation: Total: All households; measures: Value"]])) |>
  left_join(tibble(oa21cd = t07[["geography code"]],
                   young = pc(t07[["Age: Aged 25 to 29 years"]] + t07[["Age: Aged 30 to 34 years"]], t07[["Age: Total"]])), by = "oa21cd") |>
  left_join(tibble(oa21cd = t52[["geography code"]],
                   overcrowd = pc(t52[["Occupancy rating for bedrooms: Occupancy rating of bedrooms: -1"]] +
                                  t52[["Occupancy rating for bedrooms: Occupancy rating of bedrooms: -2 or less"]],
                                  t52[["Occupancy rating for bedrooms: Total: All households"]])), by = "oa21cd") |>
  left_join(tibble(oa21cd = t37[["geography code"]],
                   bad_health = pc(t37[["General health: Bad health"]] + t37[["General health: Very bad health"]],
                                   t37[["General health: Total: All usual residents"]])), by = "oa21cd") |>
  left_join({ tot <- t66[["Economic activity status: Total: All usual residents aged 16 years and over"]]
    tibble(oa21cd = t66[["geography code"]],
           lt_sick = pc(t66[["Economic activity status: Economically inactive: Long-term sick or disabled"]], tot),
           unemployed = pc(t66[["Economic activity status: Economically active (excluding full-time students): Unemployed"]], tot),
           student = pc(t66[["Economic activity status: Economically active and a full-time student"]] +
                        t66[["Economic activity status: Economically inactive: Student"]], tot)) }, by = "oa21cd") |>
  left_join(tibble(oa21cd = t54[["geography code"]],
                   private_rent = pc(t54[["Tenure of household: Private rented"]], t54[["Tenure of household: Total: All households"]])), by = "oa21cd")
MEAS <- c(dep = "Households deprived in 2+ dimensions (%)", young = "Aged 25 to 34 (%)", private_rent = "Private renting (%)",
          student = "Full-time students (%)", overcrowd = "Overcrowded households (%)", bad_health = "Bad or very bad health (%)",
          lt_sick = "Long-term sick or disabled (%)", unemployed = "Unemployed (%)")

# ---- 2. schools: neighbourhoods, universities, ethnicity ---------------------------
cent <- readRDS(here::here("data", "cache", "oa_pwc_2021.rds")) |> inner_join(oa, by = "oa21cd")
d <- readRDS(here::here("data", "inputs.rds"))
g0 <- readr::read_csv(here::here("data-raw", "gias", "edubasealldata20260610.csv"), col_types = readr::cols(.default = "c"), progress = FALSE)
gias <- g0 |> transmute(URN = as.integer(URN), easting = num(Easting), northing = num(Northing))
uni <- g0 |> filter(`EstablishmentStatus (name)` == "Open", grepl("Higher education", `TypeOfEstablishment (name)`) |
                      `EstablishmentTypeGroup (name)` %in% c("Universities", "Higher education institutions")) |>
  transmute(easting = num(Easting), northing = num(Northing)) |> filter(!is.na(easting), easting > 0)
sch <- d |> distinct(URN) |> inner_join(gias, by = "URN") |> filter(!is.na(easting), easting > 0)
nn <- nabor::knn(as.matrix(cent[, c("easting", "northing")]), as.matrix(sch[, c("easting", "northing")]), k = K)
M <- as.matrix(cent[, names(MEAS)])
for (v in names(MEAS)) sch[[v]] <- rowMeans(matrix(M[as.vector(nn$nn.idx), v], nrow = nrow(sch)), na.rm = TRUE)
sch$uni_km <- as.vector(nabor::knn(as.matrix(uni), as.matrix(sch[, c("easting", "northing")]), k = 1)$nn.dists) / 1000

read_wbri <- function(yr) {
  z <- here::here("data-raw", "ees", "spc_all", paste0("spc_", yr, ".zip"))
  f <- grep("supporting-files/spc_school_level_underlying_data", unzip(z, list = TRUE)$Name, value = TRUE)
  x <- readr::read_csv(unz(z, f[1]), col_types = readr::cols(.default = "c"), progress = FALSE, name_repair = "unique_quiet")
  tibble(URN = as.integer(x[[grep("^urn$", names(x), ignore.case = TRUE, value = TRUE)[1]]]), year_label = yr,
         wbri_sch = num(x[[grep("^% of pupils classified as white British", names(x), ignore.case = TRUE, value = TRUE)[1]]]))
}
eth <- bind_rows(lapply(c("2021-22", "2022-23", "2023-24", "2024-25"), read_wbri)) |>
  filter(!is.na(URN), !is.na(wbri_sch)) |> distinct(URN, year_label, .keep_all = TRUE)
zsc <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
d <- d |> left_join(eth, by = c("URN", "year_label")) |> left_join(sch |> select(-easting, -northing), by = "URN")
if ("urn_published" %in% names(d)) d <- d |>
  left_join(eth |> rename(urn_published = URN, wbri_pub = wbri_sch), by = c("urn_published", "year_label")) |>
  mutate(wbri_sch = coalesce(wbri_sch, wbri_pub)) |> select(-wbri_pub)
d <- d |> mutate(z_eth = zsc(wbri_sch), across(all_of(names(MEAS)), zsc, .names = "z_{.col}"))

# kinds of place. "More" and "fewer" graduates split the schools outside London
# and away from universities at their own median.
sch_l <- d |> distinct(URN, .keep_all = TRUE)
l4_mid <- median(sch_l$z_l4[as.character(sch_l$gor_name) != "London" & sch_l$uni_km > UNI_KM], na.rm = TRUE)
PLACES <- c("London", "Near a university", "Elsewhere, more graduates", "Elsewhere, fewer graduates")
d <- d |> mutate(place4 = factor(case_when(as.character(gor_name) == "London" ~ PLACES[1], uni_km <= UNI_KM ~ PLACES[2],
                                           z_l4 >= l4_mid ~ PLACES[3], !is.na(z_l4) ~ PLACES[4]), PLACES))

# ---- 3. the models ---------------------------------------------------------------------
ZM <- paste0("z_", names(MEAS))
SPECS <- list(
  `v2.1 + school's own ethnicity` = "z_eth",
  `+ deprivation` = c("z_eth", "z_dep"),
  `+ deprivation x neighbourhood education` = c("z_eth", "z_dep", "z_dep:z_l4"),
  `+ deprivation by kind of place` = c("z_eth", "place4", "z_dep:place4"),
  `+ deprivation and early-career measures` = c("z_eth", "z_dep", "z_young", "z_private_rent", "z_student"),
  `Deprivation in parts` = c("z_eth", "z_overcrowd", "z_bad_health", "z_lt_sick", "z_unemployed"),
  `In parts, with early-career measures` = c("z_eth", "z_overcrowd", "z_bad_health", "z_lt_sick", "z_unemployed",
                                             "z_young", "z_private_rent", "z_student"))
vc_of <- function(m) { v <- as.data.frame(VarCorr(m)); setNames(v$vcov, v$grp) }
run_group <- function(group) {
  dg <- model_rows(d, group, extra = c("z_eth", ZM, "place4"))
  message(sprintf("%s: %d school-years", group, nrow(dg)))
  base_f <- paste0("log(", GROUPS[[group]], ") ~ ", paste(fixed_for(group), collapse = " + "), " + ", paste(RANDOM_V2, collapse = " + "))
  fit <- function(x) lmer(as.formula(paste(base_f, "+", paste(x, collapse = " + "))), data = dg, REML = FALSE, control = CTRL)
  fits <- lapply(SPECS, fit)
  a0 <- AIC(fits[[1]]); v0 <- vc_of(fits[[1]])
  bind_rows(lapply(names(fits), function(k) {
    m <- fits[[k]]; ct <- as.data.frame(summary(m)$coefficients); ct$term <- rownames(ct); v <- vc_of(m)
    keep <- ct |> filter(grepl("^z_|place4", term), term != "z_l4" | k == "") |> transmute(term, est = Estimate, se = `Std. Error`)
    tibble(group = group, spec = k, rows = nrow(dg), aic_change = AIC(m) - a0,
           la_var_change = 100 * (v[["LANAME:gor_name"]] / v0[["LANAME:gor_name"]] - 1),
           bh = 100 * (exp(ranef(m)[["LANAME:gor_name"]][paste0(BH, ":South East"), 1]) - 1),
           b_l4 = ct$Estimate[ct$term == "z_l4"], coefs = list(keep))
  }))
}
res <- bind_rows(lapply(c("all", "disadvantaged", "non_disadvantaged"), run_group))

# ---- 4. the two kinds of deprived neighbourhood -------------------------------------
V <- readRDS(here::here("data", "va_v2.rds"))
kind_of <- d |> filter(year_label == "2024-25", !excluded) |> distinct(URN, .keep_all = TRUE) |>
  mutate(tdep = ntile(dep, 3), tl4 = ntile(z_l4, 3),
         kind = case_when(tdep == 3 & tl4 == 3 ~ "Deprived, many graduates", tdep == 3 & tl4 == 1 ~ "Deprived, few graduates",
                          tdep == 1 & tl4 == 3 ~ "Not deprived, many graduates", tdep == 1 & tl4 == 1 ~ "Not deprived, few graduates")) |>
  select(URN, kind)
# Average value added against England by kind of neighbourhood (the school's,
# region's and authority's parts together, as published), under v2.1 and with
# the school's own ethnicity and neighbourhood deprivation allowed for
kind_va <- bind_rows(lapply(c("all", "disadvantaged", "non_disadvantaged"), function(group) {
  dg <- model_rows(d, group, extra = c("z_eth", "z_dep"))
  base_f <- paste0("log(", GROUPS[[group]], ") ~ ", paste(fixed_for(group), collapse = " + "), " + ", paste(RANDOM_V2, collapse = " + "))
  bind_rows(lapply(list(`v2.1` = character(0), `+ school's ethnicity` = "z_eth", `+ ethnicity and deprivation` = c("z_eth", "z_dep")), function(x) {
    m <- lmer(as.formula(paste(c(base_f, x), collapse = " + ")), data = dg, REML = TRUE, control = CTRL)
    lp <- as.vector(getME(m, "X") %*% fixef(m)) + ranef(m)$year_label[as.character(dg$year_label), 1]
    tibble(URN = dg$URN, r = log(dg[[GROUPS[[group]]]]) - lp) |> group_by(URN) |> summarise(r = mean(r), .groups = "drop") |>
      inner_join(kind_of, by = "URN") |> filter(!is.na(kind)) |> group_by(kind) |>
      summarise(schools = n(), va = 100 * (exp(mean(r)) - 1), se = 100 * sd(r) / sqrt(n()), .groups = "drop")
  }), .id = "model") |> mutate(group = group)
}))
sl <- d |> filter(year_label == "2024-25", !excluded) |> distinct(URN, .keep_all = TRUE) |>
  inner_join(kind_of, by = "URN") |>
  filter(!is.na(kind)) |>
  left_join(V$va |> filter(group == "all") |> select(URN, va_all = rbar_nat), by = "URN") |>
  left_join(V$va |> filter(group == "disadvantaged") |> select(URN, va_dis = rbar_nat), by = "URN")
kinds <- sl |> group_by(kind) |>
  summarise(schools = n(), london = 100 * mean(as.character(gor_name) == "London"), near_uni = 100 * mean(uni_km <= UNI_KM),
            uni_km = median(uni_km), young = mean(young), private_rent = mean(private_rent), overcrowd = mean(overcrowd),
            bad_health = mean(bad_health), lt_sick = mean(lt_sick), unemployed = mean(unemployed), l4 = mean(l4, na.rm = TRUE),
            dep = mean(dep), fsm = mean(PTFSM6CLA1A, na.rm = TRUE), wbri = mean(wbri_sch, na.rm = TRUE),
            va_all = 100 * (exp(mean(va_all, na.rm = TRUE)) - 1), va_dis = 100 * (exp(mean(va_dis, na.rm = TRUE)) - 1), .groups = "drop")
top_las <- sl |> filter(kind %in% c("Deprived, many graduates", "Deprived, few graduates")) |>
  count(kind, LANAME, sort = TRUE) |> group_by(kind) |> slice_head(n = 12) |> ungroup()
place_n <- d |> distinct(URN, .keep_all = TRUE) |> filter(!is.na(place4)) |> count(place4) |> mutate(share = 100 * n / sum(n))
place_cor <- cor(as.matrix(sch_l |> select(z_dep, z_l4, z_young, z_private_rent, z_student, z_overcrowd, z_bad_health, z_lt_sick, z_unemployed)),
                 use = "pairwise.complete.obs")

saveRDS(list(built = Sys.Date(), k = K, uni_km = UNI_KM, uni_n = nrow(uni), meas = MEAS, places = PLACES, res = res, kinds = kinds, kind_va = kind_va,
             top_las = top_las, place_n = place_n, cors = place_cor), here::here("data", "graduate_deprivation.rds"))
message("Saved data/graduate_deprivation.rds")
for (i in seq_len(nrow(res))) {
  cat("\n==", res$group[i], "|", res$spec[i], "| AIC", round(res$aic_change[i], 1), "| LA var", round(res$la_var_change[i], 1), "| BH", round(res$bh[i], 2), "\n")
  print(as.data.frame(res$coefs[[i]] |> mutate(pct = round(100 * (exp(est) - 1), 2), t = round(est / se, 1)) |> select(term, pct, t)))
}
print(as.data.frame(kinds |> mutate(across(where(is.double), ~ round(.x, 1)))))
print(as.data.frame(top_las)); print(place_n); print(round(place_cor, 2))
print(as.data.frame(kind_va |> mutate(across(where(is.double), ~ round(.x, 2)))))
