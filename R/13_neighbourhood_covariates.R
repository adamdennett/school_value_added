# 13_neighbourhood_covariates.R - Do other neighbourhood measures explain the
# place effects in value added? An experiment.
# ---------------------------------------------------------------------------
# The Output Area Classification (R/11) suggested the local authority
# effects, for disadvantaged pupils especially, go with diversity and the
# character of places, not with adult qualifications. This tests specific
# Census 2021 measures, each built exactly as neighbourhood education is:
# the mean over the 150 Output Areas nearest each school.
#
# For all pupils and disadvantaged pupils, the full v2.1 model (R/02) is
# refitted by maximum likelihood on one fixed set of school-years:
#   * as published (baseline);
#   * with each measure added (standardised), one at a time;
#   * with sets of measures.
# For each: the measure's effect (% Attainment 8 per SD), the change in fit
# (AIC), and how much the local authority and region effects shrink. Also
# Brighton and Hove's own effect, as the worked example.
#
# Output: data/neighbourhood_covariates.rds (committed).
# Rscript R/13_neighbourhood_covariates.R   (several minutes)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
source(here::here("pipeline", "08_census.R"))      # fetch_census_table(), fetch_oa_centroids()
suppressPackageStartupMessages(library(lme4))
K <- 150
num <- function(x) suppressWarnings(as.numeric(x))
pc <- function(a, b) 100 * a / b

# ---- 1. Output Area measures ----------------------------------------------------
t21 <- fetch_census_table("ts021"); t04 <- fetch_census_table("ts004"); t29 <- fetch_census_table("ts029")
t19 <- fetch_census_table("ts019"); t11 <- fetch_census_table("ts011"); t54 <- fetch_census_table("ts054")
t62 <- fetch_census_table("ts062"); t03 <- fetch_census_table("ts003")
eth_cols <- grep("^Ethnic group: [^:]+: ", names(t21), value = TRUE)        # the 19 detailed groups
eth <- as.matrix(t21[, eth_cols]); eth_tot <- t21[["Ethnic group: Total: All usual residents"]]
oa <- tibble(
  oa21cd = t21[["geography code"]],
  wbri = pc(t21[["Ethnic group: White: English, Welsh, Scottish, Northern Irish or British"]], eth_tot),
  diversity = 100 * (1 - rowSums((eth / eth_tot)^2)))
oa <- oa |>
  left_join(tibble(oa21cd = t04[["geography code"]],
                   born_abroad = 100 - pc(t04[["Country of birth: Europe: United Kingdom; measures: Value"]],
                                          t04[["Country of birth: Total; measures: Value"]])), by = "oa21cd") |>
  left_join(tibble(oa21cd = t29[["geography code"]],
                   poor_english = pc(t29[["Proficiency in English language: Main language is not English (English or Welsh in Wales): Cannot speak English well"]] +
                                     t29[["Proficiency in English language: Main language is not English (English or Welsh in Wales): Cannot speak English"]],
                                     t29[["Proficiency in English language: Total: All usual residents aged 3 years and over"]])), by = "oa21cd") |>
  left_join(tibble(oa21cd = t19[["geography code"]],
                   new_migrant = pc(t19[["Migrant indicator: Migrant from outside the UK: Address one year ago was outside the UK; measures: Value"]],
                                    t19[["Migrant indicator: Total: All usual residents; measures: Value"]])), by = "oa21cd") |>
  left_join(tibble(oa21cd = t11[["geography code"]],
                   deprived2 = pc(t11[["Household deprivation: Household is deprived in two dimensions; measures: Value"]] +
                                  t11[["Household deprivation: Household is deprived in three dimensions; measures: Value"]] +
                                  t11[["Household deprivation: Household is deprived in four dimensions; measures: Value"]],
                                  t11[["Household deprivation: Total: All households; measures: Value"]])), by = "oa21cd") |>
  left_join(tibble(oa21cd = t54[["geography code"]],
                   social_rent = pc(t54[["Tenure of household: Social rented"]], t54[["Tenure of household: Total: All households"]])), by = "oa21cd") |>
  left_join(tibble(oa21cd = t62[["geography code"]],
                   routine = pc(t62[[grep("L12 Semi-routine", names(t62), value = TRUE)]] + t62[[grep("L13 Routine", names(t62), value = TRUE)]] +
                                t62[[grep("L14.1 and L14.2", names(t62), value = TRUE)]], t62[[grep("Total: All usual residents", names(t62), value = TRUE)]]),
                   higher_prof = pc(t62[[grep("L1, L2 and L3", names(t62), value = TRUE)]], t62[[grep("Total: All usual residents", names(t62), value = TRUE)]])),
            by = "oa21cd") |>
  left_join(tibble(oa21cd = t03[["geography code"]],
                   lone_parent = pc(t03[["Household composition: Single family household: Lone parent family: With dependent children; measures: Value"]],
                                    t03[["Household composition: Total; measures: Value"]])), by = "oa21cd")
COV <- c(wbri = "White British (%)", diversity = "Ethnic diversity (index)", born_abroad = "Born outside the UK (%)",
         poor_english = "Cannot speak English well (%)", new_migrant = "Arrived from abroad in the last year (%)",
         deprived2 = "Households deprived in 2+ dimensions (%)", social_rent = "Social renting (%)",
         routine = "Routine, semi-routine or never worked (%)", higher_prof = "Higher professional (%)",
         lone_parent = "Lone parents with dependent children (%)")

# ---- 2. the 150 nearest OAs to each school ---------------------------------------
cent <- readRDS(here::here("data", "cache", "oa_pwc_2021.rds")) |> inner_join(oa, by = "oa21cd")
d <- readRDS(here::here("data", "inputs.rds"))
gias <- readr::read_csv(here::here("data-raw", "gias", "edubasealldata20260610.csv"),
                        col_types = readr::cols(.default = "c"), progress = FALSE) |>
  transmute(URN = as.integer(URN), easting = num(Easting), northing = num(Northing))
sch <- d |> distinct(URN) |> inner_join(gias, by = "URN") |> filter(!is.na(easting), easting > 0)
nn <- nabor::knn(as.matrix(cent[, c("easting", "northing")]), as.matrix(sch[, c("easting", "northing")]), k = K)
M <- as.matrix(cent[, names(COV)])
sc <- sch |> select(URN)
for (v in names(COV)) sc[[v]] <- rowMeans(matrix(M[as.vector(nn$nn.idx), v], nrow = nrow(sch)), na.rm = TRUE)
# standardise across schools, as neighbourhood education is
zc <- sc |> mutate(across(all_of(names(COV)), ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE), .names = "z_{.col}"))
ZC <- paste0("z_", names(COV))
d <- d |> left_join(zc |> select(URN, all_of(ZC)), by = "URN")
# The school's own pupils: White British share from the January 2026 census.
# v2.1 has no pupil ethnicity (only EAL), so a neighbourhood ethnic measure
# could be standing in for who the school's pupils are. This tests that.
sp <- readr::read_csv(here::here("data-raw", "ees", "spc_school_level_underlying_data_2026.csv"),
                      col_types = readr::cols(.default = "c"), name_repair = "unique_quiet", progress = FALSE) |>
  filter(phase_type_grouping == "State-funded secondary") |>
  transmute(URN = as.integer(urn), sch_wbri = num(`% of pupils classified as white British ethnic origin`))
d <- d |> left_join(sp, by = "URN") |>
  mutate(z_sch_wbri = (sch_wbri - mean(sch_wbri, na.rm = TRUE)) / sd(sch_wbri, na.rm = TRUE))
cors <- cor(cbind(z_l4 = d |> distinct(URN, .keep_all = TRUE) |> pull(z_l4),
                  as.matrix(d |> distinct(URN, .keep_all = TRUE) |> select(all_of(ZC)))), use = "pairwise.complete.obs")

# ---- 3. the experiment -------------------------------------------------------------
SETS <- list(`Diversity and migration` = c("z_diversity", "z_born_abroad", "z_poor_english", "z_new_migrant"),
             `Deprivation and work` = c("z_deprived2", "z_social_rent", "z_routine", "z_higher_prof", "z_lone_parent"),
             `All ten` = ZC,
             `School's own White British share` = "z_sch_wbri",
             `School's own + neighbourhood White British` = c("z_sch_wbri", "z_wbri"),
             `School's own + diversity and migration` = c("z_sch_wbri", "z_diversity", "z_born_abroad", "z_poor_english", "z_new_migrant"),
             `School's own + neighbourhood deprivation` = c("z_sch_wbri", "z_deprived2"),
             `School's own + deprivation and work` = c("z_sch_wbri", "z_deprived2", "z_social_rent", "z_routine", "z_higher_prof", "z_lone_parent"),
             `School's own + all ten` = c("z_sch_wbri", ZC))
vc_of <- function(m) { v <- as.data.frame(VarCorr(m)); setNames(v$vcov, v$grp) }
run_group <- function(group) {
  dg <- model_rows(d, group, extra = c(ZC, "z_sch_wbri"))
  y <- GROUPS[[group]]
  base_f <- paste0("log(", y, ") ~ ", paste(fixed_for(group), collapse = " + "), " + ", paste(RANDOM_V2, collapse = " + "))
  fit <- function(extra = character(0)) {
    f <- as.formula(if (length(extra)) paste(base_f, "+", paste(extra, collapse = " + ")) else base_f)
    lmer(f, data = dg, REML = FALSE, control = CTRL)
  }
  message(sprintf("%s: %d school-years", group, nrow(dg)))
  m0 <- fit(); v0 <- vc_of(m0)
  bh0 <- ranef(m0)[["LANAME:gor_name"]]["Brighton and Hove:South East", 1]
  one <- function(label, extra) {
    m <- fit(extra); v <- vc_of(m); ct <- summary(m)$coefficients
    eff <- if (length(extra) == 1) ct[extra, , drop = FALSE] else NULL
    tibble(group = group, model = label, terms = paste(extra, collapse = "+"), n_terms = length(extra),
           est = if (!is.null(eff)) eff[1, 1] else NA_real_, se = if (!is.null(eff)) eff[1, 2] else NA_real_,
           aic_change = AIC(m) - AIC(m0),
           la_var_change = 100 * (v[["LANAME:gor_name"]] / v0[["LANAME:gor_name"]] - 1),
           reg_var_change = 100 * (v[["gor_name"]] / v0[["gor_name"]] - 1),
           resid_var_change = 100 * (v[["Residual"]] / v0[["Residual"]] - 1),
           bh = 100 * (exp(ranef(m)[["LANAME:gor_name"]]["Brighton and Hove:South East", 1]) - 1),
           coefs = list(tibble(term = extra, est = ct[extra, 1], se = ct[extra, 2])))
  }
  out <- bind_rows(
    tibble(group = group, model = "Baseline (v2.1)", terms = "", n_terms = 0L, est = NA, se = NA, aic_change = 0,
           la_var_change = 0, reg_var_change = 0, resid_var_change = 0, bh = 100 * (exp(bh0) - 1), coefs = list(NULL)),
    bind_rows(lapply(ZC, function(z) { message("  + ", z); one(COV[[sub("^z_", "", z)]], z) })),
    bind_rows(lapply(names(SETS), function(s) { message("  + set ", s); one(s, SETS[[s]]) })))
  out |> mutate(rows = nrow(dg), la_sd0 = sqrt(v0[["LANAME:gor_name"]]), reg_sd0 = sqrt(v0[["gor_name"]]))
}
res <- bind_rows(lapply(c("all", "disadvantaged"), run_group)) |>
  mutate(pct = 100 * (exp(est) - 1), lo = 100 * (exp(est - 1.96 * se) - 1), hi = 100 * (exp(est + 1.96 * se) - 1))

saveRDS(list(built = Sys.Date(), k = K, cov = COV, res = res, cors = cors,
             desc = sc |> summarise(across(all_of(names(COV)), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))))),
        here::here("data", "neighbourhood_covariates.rds"))
message("Saved data/neighbourhood_covariates.rds")
print(res |> select(group, model, pct, aic_change, la_var_change, reg_var_change, bh) |>
        mutate(across(where(is.double), ~ round(.x, 2))), n = 40, width = 200)
print(round(cors, 2))
