# 61_abie_fairness.R - Is a similar-schools benchmark fair to schools whose intake it does not see?
# ---------------------------------------------------------------------------
# DfE matches each school to 20 similar schools on deprivation, SEN, ethnicity, EAL, place, funding
# and size, but not on prior attainment, pupil mobility or admissions policy, which our absence model
# finds matter most. If those omitted characteristics matter, a benchmark built from similar schools
# will be too demanding for some schools and too easy for others. Three tests, on the rebuilt
# similar-schools comparison (R/59) and our model (R/08 specification, refitted each year in R/59):
#
#   1. The level. For each school in 2025/26 (autumn and spring), the gap between the similar-schools
#      benchmark and our model's expectation, in percentage points of absence:
#         gap_median = similar-schools median - our expectation
#         gap_target = similar-schools top quarter - our expectation  (DfE's "realistic target")
#      Negative means the benchmark asks for lower absence than the school's full intake predicts
#      (harder); positive, easier. The gap is related to what the matching leaves out:
#         ks2_rel   prior attainment relative to what the school's deprivation and EAL predict
#                   (residual of KS2 on FSM, IDACI and EAL across schools), in fifths
#         admissions: grammar; comprehensive in a selective area; comprehensive elsewhere
#      and, jointly, by OLS of the gap on the characteristics the matching omits (standardised),
#      standard errors robust (HC1).
#   2. The progress. ABIEs rest on "the progress of similar schools". For 2023/24 to 2024/25 (full
#      years), each school's change in absence against its similar schools' median change, and
#      whether the omitted characteristics predict who improves more, given that.
#   3. Brighton and Hove: each school's gap.
# Output: data/abie_fairness.rds, writeup/figures/abie_fair_*.png    Rscript R/61_abie_fairness.R
# ---------------------------------------------------------------------------
source(here::here("R", "site_helpers.R"))
suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(sandwich); library(lmtest) })
AB <- readRDS(here::here("data", "abie_benchmark.rds"))
D <- as.data.table(readRDS(here::here("data", "absence_school_frame.rds")))
B <- AB$B; Fm <- AB$features
X <- merge(D[, .(URN, ks2 = ks2_c, mobile, z_l4, wbri, semh, asd, ehcp, adm = as.character(ADMPOL_PT), sixth)], Fm, by = "URN")

# ---- 1. the level, 2025/26 ----------------------------------------------------------------
L <- merge(B[year == "2025/26" & !is.na(sim_median)], X, by = "URN")
L[, `:=`(gap_median = sim_median - model, gap_target = sim_q1 - model)]
L[, ks2_rel := resid(lm(ks2 ~ fsm + idaci + eal, data = L))]
L[, ks2_q := cut(ks2_rel, quantile(ks2_rel, 0:5 / 5), include.lowest = TRUE,
                 labels = c("Lowest fifth", "Second", "Middle", "Fourth", "Highest fifth"))]
L[, adm_lab := fcase(adm == "SEL", "Grammar school", adm == "NON SEL IN HIGHLY SEL AREA", "Comprehensive, selective area", default = "Comprehensive elsewhere")]
mean_ci <- function(x) { m <- mean(x); s <- sd(x) / sqrt(length(x)); list(mean = m, lo = m - 1.96 * s, hi = m + 1.96 * s, n = length(x)) }
by_q <- rbindlist(lapply(c("gap_median", "gap_target"), function(g) L[, c(list(gap = g), mean_ci(get(g))), by = ks2_q]))
by_adm <- rbindlist(lapply(c("gap_median", "gap_target"), function(g) L[, c(list(gap = g), mean_ci(get(g))), by = adm_lab]))
Z <- copy(L)[, c("ks2", "mobile", "z_l4", "wbri", "asd", "ehcp") := lapply(.SD, function(v) (v - mean(v)) / sd(v)), .SDcols = c("ks2", "mobile", "z_l4", "wbri", "asd", "ehcp")]
Z[, `:=`(grammar = as.integer(adm == "SEL"), comp_sel = as.integer(adm == "NON SEL IN HIGHLY SEL AREA"))]
m_gap <- lm(gap_median ~ ks2 + mobile + z_l4 + wbri + asd + ehcp + grammar + comp_sel + sixth, data = Z)
ct <- coeftest(m_gap, vcov = vcovHC(m_gap, type = "HC1"))
coef_gap <- data.table(term = rownames(ct), b = ct[, 1], se = ct[, 2])[term != "(Intercept)"][, `:=`(lo = b - 1.96 * se, hi = b + 1.96 * se)]
r2_gap <- summary(m_gap)$r.squared
share_hard <- L[, .(harder_1pt = mean(gap_median <= -1), easier_1pt = mean(gap_median >= 1))]

# ---- 2. the progress, 2023/24 to 2024/25 ------------------------------------------------------
A <- dcast(B[year %in% c("2023/24", "2024/25"), .(URN, year, actual)], URN ~ year, value.var = "actual")
setnames(A, c("URN", "a23", "a24")); A[, chg := a24 - a23]
sim_chg <- rbindlist(lapply(names(AB$similar), function(u) {
  s <- A[URN %in% AB$similar[[u]], chg]
  data.table(URN = as.integer(u), sim_chg = if (sum(!is.na(s)) >= 10) median(s, na.rm = TRUE) else NA_real_)
}))
P <- merge(merge(A, sim_chg, by = "URN"), Z[, .(URN, ks2, mobile, z_l4, wbri, grammar, comp_sel, a_level = NULL)], by = "URN")
P <- P[!is.na(chg) & !is.na(sim_chg)]
m_prog0 <- lm(chg ~ sim_chg + a23, data = P)
m_prog <- lm(chg ~ sim_chg + a23 + ks2 + mobile + z_l4 + wbri + grammar + comp_sel, data = P)
ctp <- coeftest(m_prog, vcov = vcovHC(m_prog, type = "HC1"))
coef_prog <- data.table(term = rownames(ctp), b = ctp[, 1], se = ctp[, 2])[term %in% c("ks2", "mobile", "z_l4", "wbri", "grammar", "comp_sel", "sim_chg")][, `:=`(lo = b - 1.96 * se, hi = b + 1.96 * se)]
prog_f <- anova(m_prog0, m_prog)

# ---- 3. Brighton and Hove -----------------------------------------------------------------------
bh <- L[la == "Brighton and Hove"][order(gap_median), .(SCHNAME, actual, model, sim_median, sim_q1, gap_median, gap_target, ks2_rel)]

res <- list(L = L[, .(URN, SCHNAME, la, actual, model, sim_median, sim_q1, gap_median, gap_target, ks2_rel, ks2_q, adm_lab)],
            by_q = by_q, by_adm = by_adm, coef_gap = coef_gap, r2_gap = r2_gap, share_hard = share_hard,
            coef_prog = coef_prog, prog_f = prog_f, n_prog = nrow(P), r2_prog = c(base = summary(m_prog0)$r.squared, full = summary(m_prog)$r.squared),
            bh = bh, n = nrow(L), built = Sys.Date())
saveRDS(res, here::here("data", "abie_fairness.rds"))
options(width = 160)
print(by_q); print(by_adm); print(coef_gap[, .(term, b = round(b, 3), lo = round(lo, 3), hi = round(hi, 3))]); cat("R2 gap", round(r2_gap, 3), "\n"); print(share_hard)
print(coef_prog[, .(term, b = round(b, 3), lo = round(lo, 3), hi = round(hi, 3))]); print(res$r2_prog); print(prog_f)
print(bh[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
