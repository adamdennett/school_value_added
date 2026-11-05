# 14_v22_candidates.R - Value Added v2.2 candidates, across every group and year
# ---------------------------------------------------------------------------
# The neighbourhood covariates experiment (R/13) found two things v2.1
# leaves out:
#
#   eth   the school's share of White British pupils (school census, each
#         January, so it varies by year). v2.1 has no pupil ethnicity; DfE's
#         contextual value added had it until 2011.
#   dep   neighbourhood household deprivation: the share of households
#         deprived in two or more of the Census 2021 dimensions, over the
#         150 Output Areas nearest the school, beside neighbourhood
#         education.
#
# Both are standardised across school-years. For every pupil group, on
# identical rows, v2.1 against v2.1 + eth, + dep and + both:
#   - fit: AIC (maximum likelihood) and the new terms' effects;
#   - out of sample: each year held out in turn, predicted from the other
#     three (root mean squared error of log Attainment 8, averaged);
#   - what moves: schools' value added and authorities' effects against
#     v2.1, the local authority variance, and Brighton and Hove;
#   - year by year: the new terms fitted on each year alone;
#   - who gains: the change in each school's value added under "both".
#
# Output: data/v22_candidates.rds.   Rscript R/14_v22_candidates.R (~25 min)
# Needs the school census releases in data-raw/ees/spc_all/ (see README).
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
source(here::here("pipeline", "08_census.R"))      # fetch_census_table()
suppressPackageStartupMessages(library(lme4))
num <- function(x) suppressWarnings(as.numeric(x))
K <- 150; BH <- "Brighton and Hove"

# ---- 1. the candidate terms ---------------------------------------------------
SPC <- c(`2021-22` = "b71871a9-3207-4e83-8c5f-15eefdd2f458", `2022-23` = "5597d8c4-ae2c-43cd-ba21-c6cbe80c5e4a",
         `2023-24` = "2f8715b4-4707-45a3-82e3-c3a1a4366f06", `2024-25` = "63491b17-2037-4533-b719-d3656aaf6ed5")
spc_dir <- here::here("data-raw", "ees", "spc_all"); dir.create(spc_dir, showWarnings = FALSE, recursive = TRUE)
read_wbri <- function(yr) {
  z <- file.path(spc_dir, paste0("spc_", yr, ".zip"))
  if (!file.exists(z)) download.file(sprintf("https://content.explore-education-statistics.service.gov.uk/api/all-files/%s/v1", SPC[[yr]]),
                                     z, mode = "wb", quiet = TRUE)
  f <- grep("supporting-files/spc_school_level_underlying_data", unzip(z, list = TRUE)$Name, value = TRUE)
  x <- readr::read_csv(unz(z, f[1]), col_types = readr::cols(.default = "c"), progress = FALSE, name_repair = "unique_quiet")
  col <- grep("^% of pupils classified as white British", names(x), ignore.case = TRUE, value = TRUE)[1]
  urn <- x[[grep("^urn$", names(x), ignore.case = TRUE, value = TRUE)[1]]]
  tibble(URN = as.integer(urn), year_label = yr, wbri_sch = num(x[[col]]))
}
eth <- bind_rows(lapply(names(SPC), read_wbri)) |> filter(!is.na(URN), !is.na(wbri_sch)) |> distinct(URN, year_label, .keep_all = TRUE)

t11 <- fetch_census_table("ts011")
dep_oa <- tibble(oa21cd = t11[["geography code"]],
                 dep = 100 * (t11[["Household deprivation: Household is deprived in two dimensions; measures: Value"]] +
                              t11[["Household deprivation: Household is deprived in three dimensions; measures: Value"]] +
                              t11[["Household deprivation: Household is deprived in four dimensions; measures: Value"]]) /
                   t11[["Household deprivation: Total: All households; measures: Value"]])
cent <- readRDS(here::here("data", "cache", "oa_pwc_2021.rds")) |> inner_join(dep_oa, by = "oa21cd")
d0 <- readRDS(here::here("data", "inputs.rds"))
gias <- readr::read_csv(here::here("data-raw", "gias", "edubasealldata20260610.csv"),
                        col_types = readr::cols(.default = "c"), progress = FALSE) |>
  transmute(URN = as.integer(URN), easting = num(Easting), northing = num(Northing))
sch <- d0 |> distinct(URN) |> inner_join(gias, by = "URN") |> filter(!is.na(easting), easting > 0)
nn <- nabor::knn(as.matrix(cent[, c("easting", "northing")]), as.matrix(sch[, c("easting", "northing")]), k = K)
sch$dep <- rowMeans(matrix(cent$dep[as.vector(nn$nn.idx)], nrow = nrow(sch)))

d0 <- d0 |> left_join(eth, by = c("URN", "year_label")) |> left_join(sch |> select(URN, dep), by = "URN")
# a school that changed URN: carry its ethnicity from the published URN if needed
if ("urn_published" %in% names(d0)) d0 <- d0 |>
  left_join(eth |> rename(urn_published = URN, wbri_pub = wbri_sch), by = c("urn_published", "year_label")) |>
  mutate(wbri_sch = coalesce(wbri_sch, wbri_pub)) |> select(-wbri_pub)
zsc <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
d0 <- d0 |> mutate(z_eth = zsc(wbri_sch), z_dep = zsc(dep))
term_sd <- c(z_eth = sd(d0$wbri_sch, na.rm = TRUE), z_dep = sd(d0$dep, na.rm = TRUE))
message(sprintf("Ethnicity found for %.1f%% of school-years", 100 * mean(!is.na(d0$wbri_sch))))

CANDIDATES <- list(v2.1 = character(0), eth = "z_eth", dep = "z_dep", both = c("z_eth", "z_dep"))
LABELS <- c(v2.1 = "Value Added v2.1", eth = "+ pupils' ethnicity", dep = "+ neighbourhood deprivation", both = "+ both")
rows_for <- function(g) model_rows(d0, g, extra = c("z_eth", "z_dep"))
f_for <- function(g, extra, re = RANDOM_V2)
  as.formula(paste0("log(", GROUPS[[g]], ") ~ ", paste(c(fixed_for(g), extra, re), collapse = " + ")))

# ---- 2. helpers, as R/07 ------------------------------------------------------------
school_va <- function(m, d, g) {
  xb <- as.vector(getME(m, "X") %*% fixef(m))
  uy <- ranef(m)$year_label[as.character(d$year_label), 1]
  tibble(URN = d$URN, va = d[[GROUPS[[g]]]] - exp(xb + uy) * exp(sigma(m)^2 / 2)) |>
    group_by(URN) |> summarise(va = mean(va), .groups = "drop")
}
la_effects <- function(m) { r <- ranef(m)$`LANAME:gor_name`; setNames(r[, 1], sub(":.*", "", rownames(r))) }
oos_year <- function(g, extra, d, hold) {
  tr <- d |> filter(year_label != hold) |> droplevels(); te <- d |> filter(year_label == hold, LANAME %in% tr$LANAME)
  m <- lmer(f_for(g, extra), data = tr, REML = TRUE, control = CTRL)
  X <- model.matrix(delete.response(terms(lme4::nobars(f_for(g, extra)))), te)[, colnames(getME(m, "X")), drop = FALSE]
  la <- la_effects(m); rg <- ranef(m)$gor_name
  p <- as.vector(X %*% fixef(m)) + mean(ranef(m)$year_label[, 1]) +
    coalesce(la[as.character(te$LANAME)], 0) + coalesce(rg[as.character(te$gor_name), 1], 0)
  sqrt(mean((log(te[[GROUPS[[g]]]]) - p)^2))
}

# ---- 3. every group ---------------------------------------------------------------------
YEARS <- sort(unique(d0$year_label))
compare_group <- function(g) {
  d <- rows_for(g); message(sprintf("%s: %d school-years, %d schools", g, nrow(d), n_distinct(d$URN)))
  fits <- lapply(CANDIDATES, function(x) lmer(f_for(g, x), data = d, REML = TRUE, control = CTRL))
  base_va <- school_va(fits$v2.1, d, g); base_la <- la_effects(fits$v2.1)
  vla0 <- as.data.frame(VarCorr(fits$v2.1)) |> filter(grp == "LANAME:gor_name") |> pull(vcov)
  cmp <- bind_rows(lapply(names(CANDIDATES), function(k) {
    m <- fits[[k]]; x <- CANDIDATES[[k]]; cf <- summary(m)$coefficients
    va <- school_va(m, d, g) |> inner_join(base_va, by = "URN", suffix = c("", "_v21"))
    la <- la_effects(m)
    vla <- as.data.frame(VarCorr(m)) |> filter(grp == "LANAME:gor_name") |> pull(vcov)
    message("  ", k)
    tibble(group = g, spec = k, label = LABELS[[k]], rows = nrow(d), schools = n_distinct(d$URN),
           aic = AIC(refitML(m)),
           b_eth = if ("z_eth" %in% x) cf["z_eth", 1] else NA_real_, se_eth = if ("z_eth" %in% x) cf["z_eth", 2] else NA_real_,
           b_dep = if ("z_dep" %in% x) cf["z_dep", 1] else NA_real_, se_dep = if ("z_dep" %in% x) cf["z_dep", 2] else NA_real_,
           rmse_oos = mean(sapply(YEARS, function(h) oos_year(g, x, d, h))),
           r_school = cor(va$va, va$va_v21), moved_quintile = mean(ntile(va$va, 5) != ntile(va$va_v21, 5)),
           mean_abs_change = mean(abs(va$va - va$va_v21)),
           r_la = cor(la, base_la[names(la)]), la_var_change = 100 * (vla / vla0 - 1),
           bh_effect = 100 * (exp(unname(la[BH])) - 1), bh_rank = unname(rank(-la)[BH]), n_la = length(la))
  }))
  # who gains under "both": change in each school's value added, with its
  # ethnicity, disadvantage and neighbourhood deprivation
  vb <- school_va(fits$both, d, g) |> inner_join(base_va, by = "URN", suffix = c("", "_v21")) |>
    left_join(d |> group_by(URN) |> summarise(wbri = mean(wbri_sch), fsm = mean(PTFSM6CLA1A), dep = first(dep),
                                              LANAME = first(LANAME), gor_name = first(gor_name), .groups = "drop"), by = "URN") |>
    mutate(group = g, change = va - va_v21)
  list(cmp = cmp, gain = vb)
}
RES <- lapply(names(GROUPS), compare_group)
comparison <- bind_rows(lapply(RES, `[[`, "cmp"))
gains <- bind_rows(lapply(RES, `[[`, "gain"))

# ---- 4. year by year ---------------------------------------------------------------------
yearly <- bind_rows(lapply(names(GROUPS), function(g) {
  d <- rows_for(g)
  bind_rows(lapply(YEARS, function(yr) {
    dy <- d |> filter(year_label == yr)
    f <- as.formula(paste0("log(", GROUPS[[g]], ") ~ ", paste(c(fixed_for(g), "z_eth", "z_dep", "(1 | gor_name/LANAME)"), collapse = " + ")))
    m <- lmer(f, data = dy, REML = TRUE, control = CTRL); cf <- summary(m)$coefficients
    tibble(group = g, year = yr, rows = nrow(dy), term = c("z_eth", "z_dep"), est = cf[c("z_eth", "z_dep"), 1], se = cf[c("z_eth", "z_dep"), 2])
  }))
})) |> mutate(pct = 100 * (exp(est) - 1), lo = 100 * (exp(est - 1.96 * se) - 1), hi = 100 * (exp(est + 1.96 * se) - 1))

gain_summary <- gains |> group_by(group) |>
  mutate(eth_q = ntile(wbri, 5), fsm_q = ntile(fsm, 5)) |>
  group_by(group, eth_q) |> summarise(schools = n(), wbri = mean(wbri), change = mean(change), .groups = "drop")
gain_region <- gains |> group_by(group, gor_name) |> summarise(schools = n(), change = mean(change), .groups = "drop")

saveRDS(list(built = Sys.Date(), comparison = comparison, yearly = yearly, gain_summary = gain_summary, gain_region = gain_region,
             gains = gains |> select(URN, group, va_v21, va, change, wbri, fsm, dep, LANAME, gor_name),
             term_sd = term_sd, labels = LABELS, eth_coverage = mean(!is.na(d0$wbri_sch))),
        here::here("data", "v22_candidates.rds"))
message("Saved data/v22_candidates.rds")
print(comparison |> select(group, spec, aic, b_eth, b_dep, rmse_oos, r_school, moved_quintile, la_var_change, bh_effect, bh_rank) |>
        mutate(across(where(is.double), ~ round(.x, 4))), n = 30, width = 220)
print(yearly |> select(group, year, term, pct, lo, hi) |> mutate(across(where(is.double), ~ round(.x, 2))), n = 40)
print(gain_summary |> mutate(across(where(is.double), ~ round(.x, 2))), n = 30)
