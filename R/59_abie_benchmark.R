# 59_abie_benchmark.R - Our modelled absence against DfE's similar-schools benchmark (the basis of ABIE)
# ---------------------------------------------------------------------------
# DfE's attendance baseline improvement expectations (ABIEs) are school-level
# expectations "based on the progress of similar schools": each school is set
# against 20 statistically similar schools in its phase, matched on free school
# meals, SEN support (including SEMH primary need), Gypsy/Roma/Traveller pupils,
# IDACI deprivation of pupils' home postcodes, region, urban/rural setting and
# average travel distance, EAL, funding per pupil and number on roll (DfE,
# "Monitor your school attendance: user guide", updated 23 November 2026). The
# ABIE values themselves are not published: schools see their own, and local
# authorities receive school-level workbooks. So this script rebuilds the
# comparison DfE describes from published data and sets our own model beside it.
#
#   1. Similar schools. For every state-funded secondary in the absence model
#      frame (R/08: published January 2026 census, SEN and GIAS data), the 20
#      nearest on the DfE factors we can measure, each standardised:
#        FSM %, SEN support %, SEMH primary need %, GRT %, IDACI score of the
#        school's own LSOA (IoD 2019; DfE uses pupils' home postcodes), EAL %,
#        income per pupil 2024-25 (CFR/AAR, R/43), log pupils on roll,
#        urban/rural (three bands); plus a penalty of one standard unit for a
#        school in another region (DfE "may include schools from other regions").
#      Average travel distance is not published, so it is left out.
#   2. Benchmarks, each year 2021/22 to 2024/25 (full year) and 2025/26 (autumn
#      and spring): the median overall absence of the 20 similar schools, and
#      the top quarter (their 25th percentile), which DfE uses as "a realistic
#      target" in its area-of-focus analysis. The same 20 schools every year.
#   3. Our model, each year: R/08's overall-absence model (log absence on the
#      school characteristics, region and local authority effects) refitted to
#      each year's absence with the same January 2026 characteristics; the
#      expectation is the prediction from intake and region (no local authority
#      effect), back-transformed with a smearing factor.
# Output: data/abie_benchmark.rds, writeup/figures/abie_*.png     Rscript R/59_abie_benchmark.R
# ---------------------------------------------------------------------------
source(here::here("R", "00_config.R")); source(here::here("R", "site_helpers.R"))
suppressPackageStartupMessages({ library(data.table); library(lme4); library(ggplot2) })
num <- function(x) suppressWarnings(as.numeric(x))
AC <- readRDS(here::here("data", "absence_causes.rds"))
D <- as.data.table(readRDS(here::here("data", "absence_school_frame.rds")))

# ---- the matching factors ------------------------------------------------------------
G <- fread(here::here("data-raw", "gias", "edubasealldata20260610.csv"), select = c("URN", "LSOA (code)"))
setnames(G, c("URN", "lsoa11"))
I <- fread(here::here("data-raw", "iod2019", "File_7_IoD2019.csv"), select = c(1, 32)); setnames(I, c("lsoa11", "idaci"))
Fp <- as.data.table(readRDS(here::here("data", "cache", "finance_panel.rds")))[year == "2024-25" & !is.na(income_pp), .(URN = cid, income_pp)]
Fp <- Fp[, .(income_pp = mean(income_pp)), by = URN]
M <- merge(D[, .(URN, SCHNAME, la, region, fsm, sen_sup, semh, grt, eal, size, place)], G, by = "URN", all.x = TRUE)
M <- merge(M, I, by = "lsoa11", all.x = TRUE); M <- merge(M, Fp, by = "URN", all.x = TRUE)
M[, place_n := as.integer(place)]
feat <- c("fsm", "sen_sup", "semh", "grt", "idaci", "eal", "income_pp", "size", "place_n")
M <- M[complete.cases(M[, ..feat])]
message(nrow(M), " schools with every matching factor (of ", nrow(D), ")")
X <- scale(as.matrix(M[, ..feat]))
reg <- as.character(M$region)
K <- 20
nbr <- lapply(seq_len(nrow(M)), function(i) {
  d2 <- colSums((t(X) - X[i, ])^2) + ifelse(reg == reg[i], 0, 1)
  d2[i] <- Inf
  order(d2)[1:K]
})
same_region <- mean(sapply(seq_len(nrow(M)), function(i) mean(reg[nbr[[i]]] == reg[i])))

# ---- absence, each year ----------------------------------------------------------------
raw <- fread(here::here("data", "cache", "absence_api_raw.csv"), colClasses = "character",
             select = c("time_period", "geographic_level", "education_phase", "school_urn", "sess_overall_percent"))
raw <- raw[geographic_level == "School" & education_phase == "State-funded secondary" & time_period %in% c("202122", "202223", "202324", "202425")]
raw <- raw[, .(year = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, 6)), URN = as.integer(school_urn), overall = num(sess_overall_percent))]
t2 <- fread(text = readLines(unz(here::here("data-raw", "ees", "absence_2term_school_2025-26.zip"), "data/Absence_2term_school.csv")), colClasses = "character",
            select = c("geographic_level", "education_phase", "school_urn", "sess_overall_percent"))
t2 <- t2[geographic_level == "School" & education_phase == "State-funded secondary", .(year = "2025/26", URN = as.integer(school_urn), overall = num(sess_overall_percent))]
A <- unique(rbind(raw, t2)[!is.na(overall) & overall > 0], by = c("year", "URN"))
YEARS <- c("2021/22", "2022/23", "2023/24", "2024/25", "2025/26")

# ---- benchmarks: median and top quarter of the 20 similar schools ------------------------
bench <- rbindlist(lapply(YEARS, function(yr) {
  a <- A[year == yr]; v <- a$overall[match(M$URN, a$URN)]
  rbindlist(lapply(seq_len(nrow(M)), function(i) {
    s <- v[nbr[[i]]]; s <- s[!is.na(s)]
    data.table(URN = M$URN[i], year = yr, n_similar = length(s),
               sim_median = if (length(s) >= 10) median(s) else NA_real_,
               sim_q1 = if (length(s) >= 10) unname(quantile(s, 0.25)) else NA_real_)
  }))
}))

# ---- our model, refitted to each year ------------------------------------------------------
CONT <- AC$cont; FACT <- AC$fact
Z <- copy(D)[, (names(CONT)) := lapply(.SD, function(x) (x - mean(x)) / sd(x)), .SDcols = names(CONT)]
RHS <- paste(c(names(CONT), names(FACT)), collapse = " + ")
model <- rbindlist(lapply(YEARS, function(yr) {
  dd <- merge(Z[, !c("overall")], A[year == yr, .(URN, overall)], by = "URN")
  dd[, .y := log(overall + 0.1)]
  m <- lmer(as.formula(paste(".y ~", RHS, "+ (1 | region / la)")), data = dd, REML = TRUE, control = CTRL)
  lp <- predict(m, re.form = ~ (1 | region))
  data.table(URN = dd$URN, year = yr, actual = dd$overall, model = exp(lp) * mean(exp(dd$.y - lp)) - 0.1)
}))

B <- merge(model, bench, by = c("URN", "year"))
B <- merge(B, M[, .(URN, SCHNAME, la, region)], by = "URN")
B[, bh := la == "Brighton and Hove"]
agree <- B[!is.na(sim_median), .(schools = .N, r = cor(model, sim_median), mae = mean(abs(model - sim_median)),
                                 mean_model = mean(model), mean_sim = mean(sim_median), mean_actual = mean(actual),
                                 r_actual_model = cor(actual, model), r_actual_sim = cor(actual, sim_median)), by = year]
bh <- B[bh == TRUE][order(year, SCHNAME)]
bh_sum <- B[bh == TRUE & !is.na(sim_median), .(actual = mean(actual), model = mean(model), sim_median = mean(sim_median), sim_q1 = mean(sim_q1)), by = year]
res <- list(B = B, agree = agree, bh = bh, bh_sum = bh_sum, n_match = nrow(M), n_frame = nrow(D), same_region = same_region,
            similar = setNames(lapply(nbr, function(ix) M$URN[ix]), M$URN),
            features = M[, c("URN", feat, "region"), with = FALSE],
            features = feat, k = K, built = Sys.Date())
saveRDS(res, here::here("data", "abie_benchmark.rds"))
options(width = 160); print(agree); print(bh_sum); cat("share of similar schools in the same region:", round(same_region, 3), "\n")
print(bh[year == "2025/26", .(SCHNAME, actual = round(actual, 1), model = round(model, 1), sim_median = round(sim_median, 1), sim_q1 = round(sim_q1, 1))])

# ---- figures ---------------------------------------------------------------------------------
fig <- here::here("writeup", "figures")
P <- B[year == "2025/26" & !is.na(sim_median)]
g1 <- ggplot(P, aes(model, sim_median)) + geom_abline(slope = 1, intercept = 0, colour = GREY, linetype = 2) +
  geom_point(data = P[bh == FALSE], colour = "#c9c2d6", size = 1.1, alpha = 0.7) +
  geom_point(data = P[bh == TRUE], colour = BPURPLE, size = 3) +
  ggrepel::geom_text_repel(data = P[bh == TRUE], aes(label = sub(" School.*| Community.*| High.*", "", SCHNAME)), size = 3, colour = DPURPLE, seed = 1) +
  labs(x = "Our model: expected overall absence from intake and region (%)", y = "Median of 20 similar schools (%)",
       caption = "Autumn and spring 2025/26, state-funded secondaries. Similar schools rebuilt on DfE's published factors (R/59).") + theme_note(11)
ggsave(file.path(fig, "abie_model_vs_similar.png"), g1, width = 8.5, height = 6, dpi = 150)
