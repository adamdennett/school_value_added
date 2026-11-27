# 60_twins_vs_dfe.R - Our twins against DfE's statistically similar schools
# ---------------------------------------------------------------------------
# For every state-funded secondary with both sets:
#   ours  the 20 closest twins in England on the Find a twin distance, with the
#         overall-absence weights (R/twins_data.R, as twins.js computes it)
#   DfE   the 20 similar schools rebuilt on DfE's published factors (R/59)
# Compares: how many schools the two sets share; how alike each set is on our
# similarity score; and how well each set's mean predicts the school's own
# overall absence (autumn and spring 2025/26) and Attainment 8.
# Output: data/twins_vs_dfe.rds      Rscript R/60_twins_vs_dfe.R
# ---------------------------------------------------------------------------
source(here::here("R", "site_helpers.R")); source(here::here("R", "twins_data.R"))
suppressPackageStartupMessages(library(data.table))
AB <- readRDS(here::here("data", "abie_benchmark.rds"))
SD <- sapply(MNUM, function(v) sd(d[[v]], na.rm = TRUE))
tw_dist <- function(i, wt) {
  tot <- 0; den <- 0
  for (v in MNUM) { g <- pmin(abs(d[[v]] - d[[v]][i]) / SD[[v]] / 3, 1); ok <- !is.na(g)
    tot <- tot + ifelse(ok, wt[[v]] * g, 0); den <- den + ifelse(ok, wt[[v]], 0) }
  for (v in MCAT) { g <- as.numeric(d[[v]] != d[[v]][i]); ok <- !is.na(g)
    tot <- tot + ifelse(ok, wt[[v]] * g, 0); den <- den + ifelse(ok, wt[[v]], 0) }
  out <- tot / den; out[i] <- NA; out
}
K <- 20
dfe <- AB$similar
keep <- which(as.character(d$URN) %in% names(dfe))
y_abs <- d$overall; y_att <- d$att8
R <- rbindlist(lapply(keep, function(i) {
  di <- tw_dist(i, W$overall)
  ours <- order(di)[1:K]
  theirs <- match(dfe[[as.character(d$URN[i])]], d$URN); theirs <- theirs[!is.na(theirs)]
  data.table(URN = d$URN[i], name = d$n[i], la = d$la[i], shared = length(intersect(ours, theirs)), n_dfe = length(theirs),
             sim_ours = 100 * (1 - mean(di[ours])), sim_dfe = 100 * (1 - mean(di[theirs])),
             abs = y_abs[i], abs_ours = mean(y_abs[ours], na.rm = TRUE), abs_dfe = mean(y_abs[theirs], na.rm = TRUE),
             att = y_att[i], att_ours = mean(y_att[ours], na.rm = TRUE), att_dfe = mean(y_att[theirs], na.rm = TRUE),
             ours = list(d$n[ours]), dfe = list(d$n[theirs]))
}))
summ <- R[, .(schools = .N, shared_mean = mean(shared), shared_none = mean(shared == 0), shared_half = mean(shared >= 10),
              sim_ours = mean(sim_ours), sim_dfe = mean(sim_dfe),
              r_abs_ours = cor(abs, abs_ours, use = "complete.obs"), r_abs_dfe = cor(abs, abs_dfe, use = "complete.obs"),
              r_att_ours = cor(att, att_ours, use = "complete.obs"), r_att_dfe = cor(att, att_dfe, use = "complete.obs"))]
bh <- R[la == "Brighton and Hove"][order(name)]
saveRDS(list(R = R[, !c("ours", "dfe")], summ = summ, bh = bh, k = K, built = Sys.Date()), here::here("data", "twins_vs_dfe.rds"))
print(summ); print(bh[, .(name, shared, sim_ours = round(sim_ours), sim_dfe = round(sim_dfe))])
