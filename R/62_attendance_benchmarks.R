# 62_attendance_benchmarks.R - An open dataset of expected absence for every secondary, two ways
# ---------------------------------------------------------------------------
# For every state-funded secondary in the absence model frame (R/08), for each of the seven
# absence measures (autumn and spring 2025/26), what the school recorded and what two methods
# would expect of it:
#
#   A. Our model (R/08): the expectation from the school's whole intake and its region, the
#      region-only prediction of the mixed model back-transformed with a smearing factor
#      (AC$pred$fit_region). Its "top quarter" is the absence a school in the best quarter of
#      schools with the same intake would record: the expectation moved by the 25th percentile of
#      the model's residuals on the log scale,
#         model_q1 = exp(log(model + 0.1) + q25(log(actual + 0.1) - log(model + 0.1))) - 0.1.
#   B. DfE-style similar schools (R/59): the median and the top quarter (25th percentile) of the
#      same measure across the school's 20 similar schools, rebuilt from published data on the
#      factors DfE names. Needs at least 10 of the 20 with a value, as in R/59. DfE uses the top
#      quarter of similar schools as "a realistic target". This is our rebuild, not DfE's ABIE:
#      DfE does not publish the similar-school sets, the ABIE formula or the values, so every
#      figure here is an estimate to be checked against a school's own DfE report.
#
#   Differences, in percentage points: actual - model, actual - similar median, and
#   similar median - model (negative: the similar-schools benchmark asks for lower absence than
#   the whole intake predicts).
#   Overall absence for 2024/25 (full year) is added from R/59.
# Outputs:
#   data/attendance_benchmarks.rds              (wide table, dictionary, similar-school sets)
#   data/attendance_benchmarks_2025-26.csv      one row per school
#   data/attendance_benchmarks_dictionary.csv   every column explained
#   data/attendance_similar_schools.csv         each school's 20 rebuilt similar schools
#                                                Rscript R/62_attendance_benchmarks.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table) })
AC <- readRDS(here::here("data", "absence_causes.rds"))
AB <- readRDS(here::here("data", "abie_benchmark.rds"))
S <- as.data.table(AC$local$schools)
P <- as.data.table(AC$pred)
OUT <- names(AC$outcomes)
LAB <- AC$outcomes

# ---- A. our model -------------------------------------------------------------------------
P[, `:=`(fit_region = unname(fit_region), r = log(obs + 0.1) - log(unname(fit_region) + 0.1))]
P[, model_q1 := exp(log(fit_region + 0.1) + quantile(r, 0.25)) - 0.1, by = outcome]

# ---- B. similar schools -----------------------------------------------------------------------
sim <- AB$similar
SV <- S[, c("URN", OUT), with = FALSE]
simB <- rbindlist(lapply(names(sim), function(u) {
  v <- SV[match(sim[[u]], URN)]
  c(list(URN = as.integer(u), n_similar = sum(!is.na(v$overall))),
    unlist(lapply(OUT, function(o) {
      s <- v[[o]][!is.na(v[[o]])]
      ok <- length(s) >= 10
      setNames(list(if (ok) median(s) else NA_real_, if (ok) unname(quantile(s, 0.25)) else NA_real_),
               paste0(o, c("_similar_median", "_similar_q1")))
    }), recursive = FALSE))
}))

# ---- the wide table ---------------------------------------------------------------------------
W <- dcast(P[, .(URN, outcome, actual = obs, model = fit_region, model_q1)], URN ~ outcome,
           value.var = c("actual", "model", "model_q1"), sep = "|")
setnames(W, names(W), sub("^(.*)[|](.*)$", "\\2_\\1", names(W)))
W <- merge(S[, .(URN, school = SCHNAME, la = as.character(la), region = as.character(region))], W, by = "URN")
W <- merge(W, simB, by = "URN", all.x = TRUE)
for (o in OUT) {
  a <- paste0(o, "_actual"); m <- paste0(o, "_model"); s <- paste0(o, "_similar_median")
  W[, paste0(o, "_actual_vs_model") := get(a) - get(m)]
  W[, paste0(o, "_actual_vs_similar") := get(a) - get(s)]
  W[, paste0(o, "_similar_vs_model") := get(s) - get(m)]
}
W[, overall_similar_q1_vs_model_q1 := overall_similar_q1 - overall_model_q1]
b24 <- AB$B[year == "2024/25", .(URN, overall_2425_actual = actual, overall_2425_model = model,
                                  overall_2425_similar_median = sim_median, overall_2425_similar_q1 = sim_q1)]
W <- merge(W, b24, by = "URN", all.x = TRUE)
ord <- c("URN", "school", "la", "region", "n_similar",
         unlist(lapply(OUT, function(o) paste0(o, c("_actual", "_model", "_model_q1", "_similar_median", "_similar_q1",
                                                    "_actual_vs_model", "_actual_vs_similar", "_similar_vs_model")))),
         "overall_similar_q1_vs_model_q1", names(b24)[-1])
setcolorder(W, ord)
num <- names(W)[sapply(W, is.numeric) & !names(W) %in% c("URN", "n_similar")]
W[, (num) := lapply(.SD, round, 2), .SDcols = num]
setorder(W, region, la, school)

# ---- the dictionary -----------------------------------------------------------------------------
unit <- function(o) if (o %in% c("persistent", "severe")) "% of pupils" else "% of sessions"
desc <- c(
  actual = "Recorded by the school, autumn and spring terms 2025/26 (DfE, Pupil absence in schools in England: autumn and spring terms 2025/26).",
  model = "Our model's expectation from the school's whole intake and its region (R/08: published January 2026 census, SEN, KS4 and GIAS characteristics; region effect, no local-authority effect).",
  model_q1 = "Our model's top quarter: what a school in the best quarter of schools with this intake would record (expectation moved by the 25th percentile of the model's residuals, log scale).",
  similar_median = "Median of the school's 20 rebuilt DfE-style similar schools (R/59; at least 10 with a value). Estimate, to be checked against the school's DfE report.",
  similar_q1 = "Top quarter (25th percentile) of the 20 rebuilt similar schools: DfE's 'realistic target' benchmark, rebuilt. Estimate, to be checked against the school's DfE report.",
  actual_vs_model = "Actual minus our model's expectation, percentage points. Positive: more absence than the intake predicts.",
  actual_vs_similar = "Actual minus the similar-schools median, percentage points. Positive: more absence than similar schools.",
  similar_vs_model = "Similar-schools median minus our model's expectation, percentage points. Negative: the similar-schools benchmark asks for lower absence than the whole intake predicts.")
dict <- rbind(
  data.table(column = c("URN", "school", "la", "region", "n_similar"),
             measure = "", unit = c("", "", "", "", "schools"),
             description = c("Unique reference number (GIAS).", "School name (GIAS).", "Local authority.", "Region.",
                             "How many of the 20 rebuilt similar schools have a 2025/26 overall absence figure.")),
  rbindlist(lapply(OUT, function(o) data.table(column = paste0(o, "_", names(desc)), measure = LAB[[o]],
                                              unit = ifelse(grepl("_vs_", names(desc)), "percentage points", unit(o)),
                                              description = unname(desc)))),
  data.table(column = c("overall_similar_q1_vs_model_q1", names(b24)[-1]),
             measure = "Overall absence",
             unit = c("percentage points", rep("% of sessions", 4)),
             description = c("Similar-schools top quarter minus our model's top quarter: how much harder (negative) or easier the DfE-style target is than the intake-based one.",
                             "Recorded overall absence, full year 2024/25.",
                             "Our model's expectation for 2024/25 (R/08 specification refitted to 2024/25, R/59).",
                             "Median of the 20 rebuilt similar schools, 2024/25.",
                             "Top quarter of the 20 rebuilt similar schools, 2024/25.")))
stopifnot(identical(dict$column, names(W)))

# ---- the similar-school sets ------------------------------------------------------------------------
nm <- setNames(S$SCHNAME, S$URN)
SS <- rbindlist(lapply(names(sim), function(u) data.table(URN = as.integer(u), school = nm[[u]], rank = seq_along(sim[[u]]),
                                                         similar_URN = sim[[u]], similar_school = unname(nm[as.character(sim[[u]])]))))
SS <- merge(SS, S[, .(similar_URN = URN, similar_la = as.character(la))], by = "similar_URN")
setcolorder(SS, c("URN", "school", "rank", "similar_URN", "similar_school", "similar_la")); setorder(SS, URN, rank)

res <- list(W = W, dict = dict, similar = SS, model_q1_shift = P[, .(q25 = quantile(r, 0.25)), by = outcome], built = Sys.Date())
saveRDS(res, here::here("data", "attendance_benchmarks.rds"))
fwrite(W, here::here("data", "attendance_benchmarks_2025-26.csv"))
fwrite(dict, here::here("data", "attendance_benchmarks_dictionary.csv"))
fwrite(SS, here::here("data", "attendance_similar_schools.csv"))
cat(nrow(W), "schools;", sum(!is.na(W$overall_similar_median)), "with a similar-schools benchmark\n")
print(W[la == "Brighton and Hove", .(school, overall_actual, overall_model, overall_model_q1, overall_similar_median, overall_similar_q1,
                                     illness_model, illness_similar_median, persistent_model, persistent_similar_median)])
print(W[, lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 2)), .SDcols = patterns("_similar_vs_model$")])
