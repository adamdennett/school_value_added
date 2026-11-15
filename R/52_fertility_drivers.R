# 52_fertility_drivers.R - What goes with low fertility across districts: education, housing, or who lives there?
# ---------------------------------------------------------------------------
# Follows R/49 (fertility against affordability) and the quick R/51 (all adults' Level 4+).
# The question, from writeup/bh_falling_rolls_backlog.md: is low fertility in Brighton and
# Hove (0.98 in 2025) about housing costs, as councillors say, or about women's education,
# as the long literature on fertility suggests, or about who lives in the city at
# family-forming ages?
#
# Outcome: total fertility rate, 2023-25 average (R/49, ONS via Nomis NM_207_1).
# Candidates, Census 2021 unless stated, districts:
#   fem_l4       % of women aged 25 to 44 with Level 4+ qualifications (ONS Census 2021 API,
#                custom table highest_qualification x sex x resident_age_18b, ltla)
#   young        women aged 20 to 24 as % of women 15 to 44 (same table): a large student or
#                young-adult population, which lowers a period rate without any change in what
#                women eventually do. TS068 "students" is not used: it counts schoolchildren too.
#   married      % of residents 16+ married or in a civil partnership (TS002, Nomis NM_2022_1)
#   private_rent % of households renting privately (TS054, Nomis NM_2072_1)
#   born_abroad  % of residents born outside the UK (TS004, Nomis NM_2024_1)
#   log_ratio    log house price to residence-based earnings ratio, 2023-25 (R/49, ONS table 5c)
# Change: % of residents 16+ with Level 4+, 2011 (KS501EW, Nomis NM_623_1) to 2021 (TS067,
#   R/51), against the change in TFR from 2013-15 to 2023-25; districts whose code is
#   unchanged since 2011.
#
# Models (OLS across districts), predictors standardised (per standard deviation) so their
# coefficients compare:
#   m_aff   TFR ~ log_ratio
#   m_edu   TFR ~ fem_l4
#   m_both  TFR ~ fem_l4 + log_ratio
#   m_full  TFR ~ fem_l4 + young + married + private_rent + born_abroad + log_ratio
#   unique  R2 lost when each candidate is dropped from m_full
#   m_chg   change in TFR ~ change in Level 4+ (2011 to 2021)
# and Brighton and Hove's residual (actual minus predicted TFR) under each.
#
# Output: data/fertility_drivers.rds.   Rscript R/52_fertility_drivers.R   (about a minute)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(data.table))
cdir <- here::here("data-raw", "census2021")

# ---- women by qualification and age (ONS Census 2021 API) --------------------------------
j <- jsonlite::fromJSON(file.path(cdir, "hiqual_sex_age18b_ltla_ons_api.json"), simplifyVector = FALSE)$observations
Q <- rbindlist(lapply(j, function(z) list(code = z$dimensions[[1]]$option_id, name = z$dimensions[[1]]$option,
                                         qual = z$dimensions[[2]]$option_id, sex = z$dimensions[[3]]$option,
                                         age = z$dimensions[[4]]$option, n = z$observation)))
W <- Q[sex == "Female"]
a25_44 <- c("Aged 25 to 29 years", "Aged 30 to 34 years", "Aged 35 to 39 years", "Aged 40 to 44 years")
a15_44 <- c("Aged 15 to 19 years", "Aged 20 to 24 years", a25_44)
edu <- W[age %in% a25_44 & qual != "-8", .(fem_l4 = 100 * sum(n[qual == "5"]) / sum(n)), by = code]   # option 5 = Level 4+
stopifnot(all(W[qual == "5", unique(age)] %in% unique(W$age)))
young <- W[age %in% a15_44, .(young = 100 * sum(n[age == "Aged 20 to 24 years"]) / sum(n)), by = code]

# ---- Nomis Census 2021 tables (percentages) ---------------------------------------------------
nomis_pct <- function(f, cat) {
  x <- fread(file.path(cdir, f)); cn <- grep("^C2021_.*_NAME$", names(x), value = TRUE)
  x[MEASURES_NAME == "Percent" & get(cn) == cat, .(code = GEOGRAPHY_CODE, v = OBS_VALUE)]
}
married <- nomis_pct("ts002_partnership_lad_nomis_NM_2022_1.csv", "Married or in a registered civil partnership")[, .(code, married = v)]
rent <- nomis_pct("ts054_tenure_lad_nomis_NM_2072_1.csv", "Private rented")[, .(code, private_rent = v)]
cob <- nomis_pct("ts004_cob_lad_nomis_NM_2024_1.csv", "Europe: United Kingdom")[, .(code, born_abroad = 100 - v)]

# ---- fertility and affordability (R/49) -------------------------------------------------------
FA <- readRDS(here::here("data", "bh_fertility_affordability.rds"))
D <- FA$D_chg[, .(code, name, tfr = tfr_1, tfr_0, d_tfr, pct_tfr, ratio = ratio_1, log_ratio = log(ratio_1))]
X <- Reduce(function(a, b) merge(a, b, by = "code"), list(D, edu, young, married, rent, cob))
message(nrow(X), " districts with every measure; dropped from R/49's ", nrow(D), ": ",
        paste(setdiff(D$name, X$name), collapse = ", "))

# ---- models ----------------------------------------------------------------------------------
vars <- c("fem_l4", "young", "married", "private_rent", "born_abroad", "log_ratio")
lab <- c(fem_l4 = "Women 25-44 with Level 4+ (%)", young = "Women 20-24, % of women 15-44", married = "Married or civil partnership, 16+ (%)",
         private_rent = "Households renting privately (%)", born_abroad = "Born outside the UK (%)", log_ratio = "House price to earnings ratio (log)")
Z <- copy(X); for (v in vars) Z[[v]] <- as.numeric(scale(Z[[v]]))
fit <- function(f) lm(f, data = Z)
m_aff <- fit(tfr ~ log_ratio); m_edu <- fit(tfr ~ fem_l4); m_both <- fit(tfr ~ fem_l4 + log_ratio)
m_full <- fit(reformulate(vars, "tfr"))
r2 <- function(m) summary(m)$r.squared
unique_r2 <- sapply(vars, function(v) r2(m_full) - r2(fit(reformulate(setdiff(vars, v), "tfr"))))
coefs <- as.data.table(summary(m_full)$coefficients, keep.rownames = "term")[term != "(Intercept)"]
setnames(coefs, c("term", "est", "se", "t", "p"))
coefs[, `:=`(lo = est - 1.96 * se, hi = est + 1.96 * se, label = lab[term], unique_r2 = unique_r2[term])]
bh_res <- sapply(list(affordability = m_aff, education = m_edu, both = m_both, full = m_full),
                 function(m) unname(resid(m)[Z$name == "Brighton and Hove"]))
ranks <- sapply(c("fem_l4", "young", "married", "private_rent", "born_abroad"), function(v) X[, frank(-get(v))][X$name == "Brighton and Hove"])
Z[, res_full := resid(m_full)]
worst <- Z[order(res_full)][1:12, .(name, tfr, res_full)]

# ---- change: Level 4+ 2011 to 2021 against change in fertility ---------------------------------
k11 <- fread(file.path(cdir, "ks501ew_2011_lad_nomis_NM_623_1.csv"))
setnames(k11, c("code", "name", "cell", "pct"))
l4_11 <- k11[cell == "Highest level of qualification: Level 4 qualifications and above", .(code, l4_2011 = pct)]
q21 <- fread(file.path(cdir, "ts067_lad_pct_nomis_NM_2084_1.csv")); setnames(q21, c("code", "name", "cat", "pct"))
l4_21 <- q21[cat == "Level 4 qualifications or above", .(code, l4_2021 = pct)]
C <- Reduce(function(a, b) merge(a, b, by = "code"), list(X[, .(code, name, d_tfr, pct_tfr)], l4_11, l4_21))
C[, d_l4 := l4_2021 - l4_2011]
m_chg <- lm(d_tfr ~ d_l4, data = C)

res <- list(X = X, coefs = coefs, r2 = c(affordability = r2(m_aff), education = r2(m_edu), both = r2(m_both), full = r2(m_full)),
            edu_coef = summary(m_edu)$coefficients, both_coef = summary(m_both)$coefficients, bh_res = bh_res, bh_ranks = ranks,
            n = nrow(X), worst = worst, change = list(n = nrow(C), coef = summary(m_chg)$coefficients, r2 = r2(m_chg),
            bh = C[name == "Brighton and Hove"]), sds = sapply(X[, ..vars], sd), labels = lab, built = Sys.Date())
saveRDS(res, here::here("data", "fertility_drivers.rds"))

options(width = 150)
cat("\nR2:", round(res$r2, 3), "\n")
cat("\nFull model, per SD:\n"); print(coefs[, .(label, est = round(est, 3), lo = round(lo, 3), hi = round(hi, 3), p = signif(p, 2), unique_r2 = round(unique_r2, 3))])
cat("\nBrighton and Hove residual by model:\n"); print(round(bh_res, 3))
cat("\nBrighton and Hove values and ranks (1 = highest):\n"); print(X[name == "Brighton and Hove", c("tfr", vars), with = FALSE]); print(ranks)
cat("\nLargest negative residuals, full model:\n"); print(worst, digits = 3)
cat("\nChange: d TFR on d Level 4+ (n =", nrow(C), "), R2", round(r2(m_chg), 3), "\n"); print(round(summary(m_chg)$coefficients, 4))
cat("\nSDs:\n"); print(round(res$sds, 2))

# ---- Brighton and Hove against the average district: each candidate's share of the gap ------
# predicted TFR minus the average district's, split into coefficient x Brighton and Hove's
# standardised value (the full model's terms), plus the residual
zb <- unlist(Z[name == "Brighton and Hove", ..vars]); b <- coef(m_full)[vars]
bh_parts <- data.table(term = vars, label = lab[vars], z = zb, contribution = b * zb)
bh_gap <- X[name == "Brighton and Hove", tfr] - mean(X$tfr)
res$bh_parts <- bh_parts; res$bh_gap <- bh_gap
saveRDS(res, here::here("data", "fertility_drivers.rds"))
cat("\nBrighton and Hove: TFR minus the average district", round(bh_gap, 3), "= parts + residual\n")
print(bh_parts[, .(label, z = round(z, 2), contribution = round(contribution, 3))]); cat("sum of parts", round(sum(bh_parts$contribution), 3), " residual", round(res$bh_res[["full"]], 3), "\n")
