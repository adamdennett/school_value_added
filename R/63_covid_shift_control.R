# 63_covid_shift_control.R - Do similar-schools targets point back to before the pandemic, and how much
#                            of the post-pandemic rise in absence is within a school's reach?
# ---------------------------------------------------------------------------
# Two questions about ABIEs, from published school-level data. All absence is the autumn and spring
# terms (R/09's panel, the same basis every year, schools followed through changes of URN), so
# 2018/19 and 2025/26 compare like with like.
#
#   1. Back to before the pandemic? For each school's 20 rebuilt similar schools (R/59), their
#      median and top quarter in 2018/19 and in 2025/26. How far the 2025/26 top quarter (DfE's
#      "realistic target") goes back towards the same schools' 2018/19 median; and how many schools
#      now record no more absence than their similar schools typically did before the pandemic.
#   2. Within a school's reach? Four comparisons:
#      a. The common shift. Our absence model (R/08 specification, January 2026 characteristics,
#         region and local-authority effects) fitted to each year from 2016/17. The national level
#         rises; does the spread of schools around what their intake predicts (the residual, the
#         part a school might own) grow with it, or does the intake explain as much as before?
#      b. Proportional or not. Did schools that did better than their intake predicted before the
#         pandemic rise less? log change 2018/19 to 2025/26 on the 2018/19 residual and intake.
#         A coefficient near zero means the rise was proportional: good schools stayed good, by the
#         same ratio, and rose with everyone else.
#      c. Magnitudes, in % of a school's absence: the national rise; the gap between a school at the
#         median and one in the best quarter for its intake (2025/26 residuals); the change of trust
#         effect (R/09 event study); and the range of intake effects (interquartile range of the
#         model's expectation).
#      d. Composition. England's rise split into the change within pupil groups and the change in
#         the groups' shares (free school meals; SEN provision), full year 2018/19 to 2024/25.
# Output: data/covid_shift.rds                    Rscript R/63_covid_shift_control.R
# ---------------------------------------------------------------------------
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(lme4) })
num <- function(x) suppressWarnings(as.numeric(x))
AC <- readRDS(here::here("data", "absence_causes.rds"))
AB <- readRDS(here::here("data", "abie_benchmark.rds"))
A9 <- readRDS(here::here("data", "absence_change.rds"))
D <- as.data.table(readRDS(here::here("data", "absence_school_frame.rds")))
OUT <- c("overall", "illness", "auth_other", "unauth", "persistent", "severe")
PRE <- 201819L; NOW <- 202526L

# each current school's chain of absence, by year
Sr <- as.data.table(A9$series); Nw <- as.data.table(A9$now)[, .(cid, URN = URN_now)]
Y <- merge(Sr[, c("cid", "tp", OUT), with = FALSE], Nw, by = "cid")
yv <- function(o, t) { y <- Y[tp == t]; setNames(y[[o]], y$URN) }

# ---- 1. back to before the pandemic? ------------------------------------------------------
sim <- AB$similar
back <- rbindlist(lapply(c("overall", "persistent", "unauth", "severe", "illness"), function(o) {
  p <- yv(o, PRE); n <- yv(o, NOW)
  rbindlist(lapply(names(sim), function(u) {
    s <- as.character(sim[[u]]); sp <- p[s]; sn <- n[s]; sp <- sp[!is.na(sp)]; sn <- sn[!is.na(sn)]
    if (length(sp) < 10 || length(sn) < 10) return(NULL)
    pm <- median(sp)
    data.table(URN = as.integer(u), outcome = o, own_pre = unname(p[u]), own_now = unname(n[u]),
               pre_med = pm, pre_q1 = unname(quantile(sp, 0.25)), now_med = median(sn), now_q1 = unname(quantile(sn, 0.25)),
               n_at_pre = mean(sn <= pm))
  }))
}))
back[, `:=`(gap = now_med - pre_med, target_step = now_med - now_q1)]
back[, share_back := fifelse(gap > 0, target_step / gap, NA_real_)]
back_sum <- back[, .(schools = .N, pre_med = mean(pre_med), now_med = mean(now_med), now_q1 = mean(now_q1),
                     share_back_median = median(share_back, na.rm = TRUE),
                     q1_below_pre = mean(now_q1 <= pre_med),          # the target is at or below the old typical level
                     own_now_at_pre = mean(own_now <= pre_med, na.rm = TRUE),   # schools already back to their group's old median
                     sim_at_pre = mean(n_at_pre)), by = outcome]
# what getting back to the old median by 2028/29 would mean: three years from 2025/26
back_sum[, annual_cut_pct := 100 * (1 - (pre_med / now_med)^(1 / 3))]

# ---- 2a. the common shift and what the intake explains, each year --------------------------------
CONT <- AC$cont; FACT <- AC$fact
Z <- copy(D)[, (names(CONT)) := lapply(.SD, function(x) (x - mean(x)) / sd(x)), .SDcols = names(CONT)]
RHS <- paste(c(names(CONT), names(FACT)), collapse = " + ")
YEARS <- c(201617L, 201718L, 201819L, 202122L, 202223L, 202324L, 202425L, 202526L)
fits <- list(); resid_all <- list()
for (o in c("overall", "persistent", "unauth", "illness")) for (t in YEARS) {
  yy <- yv(o, t); dd <- copy(Z[, !OUT[OUT %in% names(Z)], with = FALSE])[, y := unname(yy[as.character(URN)])][!is.na(y)]
  dd[, .y := log(y + 0.1)]
  m <- lmer(as.formula(paste(".y ~", RHS, "+ (1 | region / la)")), data = dd, REML = TRUE, control = CTRL)
  vc <- as.data.frame(VarCorr(m)); vfix <- var(as.vector(getME(m, "X") %*% fixef(m)))
  v <- setNames(vc$vcov, vc$grp)
  r <- dd$.y - predict(m, re.form = ~ (1 | region))     # what a school owns: residual after intake and region (LA left in)
  fits[[paste(o, t)]] <- data.table(outcome = o, tp = t, schools = nrow(dd), mean = mean(dd$y), sd_log = sd(dd$.y),
                                    var_fix = vfix, var_region = v[["region"]], var_la = v[["la:region"]], var_school = v[["Residual"]],
                                    r2_intake = vfix / (vfix + sum(v)), sd_resid_log = sd(r),
                                    q25 = unname(quantile(r, 0.25)), q50 = unname(quantile(r, 0.5)), q75 = unname(quantile(r, 0.75)),
                                    iqr_expect = unname(diff(quantile(predict(m, re.form = NA), c(0.25, 0.75)))))
  resid_all[[paste(o, t)]] <- data.table(URN = dd$URN, outcome = o, tp = t, y = dd$y, r = r, fit = predict(m, re.form = ~ (1 | region)))
}
fits <- rbindlist(fits); R <- rbindlist(resid_all)

# ---- 2b. proportional or not ---------------------------------------------------------------------
# Single years are noisy, so a school's standing before is the mean of its residuals over
# 2016/17-2018/19 and after over 2023/24-2025/26 (log scale). Noise alone pulls the coefficient
# below zero (regression to the mean): with perfectly lasting school effects and reliability rel
# of the three-year mean, b = -(1 - rel). rel comes from the correlation of residuals in adjacent
# years (r1), stepped up to three years: rel = 3 r1 / (1 + 2 r1).
PRE3 <- c(201617L, 201718L, 201819L); POST3 <- c(202324L, 202425L, 202526L)
prop <- rbindlist(lapply(c("overall", "persistent", "unauth", "illness"), function(o) {
  ro <- R[outcome == o]
  w <- dcast(ro, URN ~ tp, value.var = "r")
  r1_pre <- mean(c(cor(w[["201617"]], w[["201718"]], use = "complete"), cor(w[["201718"]], w[["201819"]], use = "complete")))
  r1_post <- mean(c(cor(w[["202324"]], w[["202425"]], use = "complete"), cor(w[["202425"]], w[["202526"]], use = "complete")))
  a <- ro[tp %in% PRE3, .(r_pre = mean(r), ly_pre = mean(log(y + 0.1)), n_pre = .N), by = URN][n_pre == 3]
  b <- ro[tp %in% POST3, .(r_now = mean(r), ly_now = mean(log(y + 0.1)), n_now = .N), by = URN][n_now == 3]
  ab <- merge(merge(a, b, by = "URN"), Z, by = "URN")
  ab[, dlog := ly_now - ly_pre]
  m <- lm(as.formula(paste("dlog ~ r_pre +", RHS)), data = ab)
  ct <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC1"))
  mi <- lm(as.formula(paste("dlog ~", RHS)), data = ab)
  mla <- lmer(dlog ~ 1 + (1 | region / la), data = ab, REML = TRUE, control = CTRL)
  vl <- as.data.frame(VarCorr(mla)); vl <- setNames(vl$vcov, vl$grp)
  rel_pre <- 3 * r1_pre / (1 + 2 * r1_pre); rel_post <- 3 * r1_post / (1 + 2 * r1_post)
  rp <- cor(ab$r_pre, ab$r_now)
  ab[, q := cut(r_pre, quantile(r_pre, 0:4 / 4), include.lowest = TRUE, labels = c("Best quarter", "Second", "Third", "Worst quarter"))]
  data.table(outcome = o, schools = nrow(ab), r1_pre = r1_pre, r1_post = r1_post, rel_pre = rel_pre, rel_post = rel_post,
             r_persist = rp, r_persist_true = min(1, rp / sqrt(rel_pre * rel_post)),
             b_rpre = ct["r_pre", 1], se_rpre = ct["r_pre", 2], b_noise = -(1 - rel_pre),
             mean_dlog = mean(ab$dlog), sd_dlog = sd(ab$dlog),
             r2_intake_change = summary(mi)$r.squared,
             share_region = vl[["region"]] / sum(vl), share_la = vl[["la:region"]] / sum(vl),
             rise_q = list(ab[, .(rise = 100 * (exp(mean(dlog)) - 1), schools = .N), by = q][order(q)]))
}))

# ---- 2c. magnitudes, % of a school's absence ---------------------------------------------------------
tr <- as.data.table(A9$trend)
nat <- function(o, t) { v <- switch(o, overall = "overall", persistent = "persistent"); tr[tp == t][[v]] }
att <- as.data.table(A9$es$trust$att)
mag <- rbindlist(lapply(c("overall", "persistent"), function(o) {
  f <- fits[outcome == o & tp == NOW]
  data.table(outcome = o,
             what = c("Rise since before the pandemic (England)",
                      "Best quarter of schools for their intake, against the middle",
                      "A change of trust (R/09)",
                      "Intake: a school at the upper quartile of expected absence against the lower"),
             pct = c(100 * (nat(o, NOW) / nat(o, PRE) - 1),
                     100 * (exp(f$q25 - f$q50) - 1),
                     100 * (exp(att[outcome == o, est]) - 1),
                     100 * (exp(f$iqr_expect) - 1)),
             se = c(NA, NA, 100 * exp(att[outcome == o, est]) * att[outcome == o, se], NA))
}))

# ---- 2d. composition: within pupil groups or more pupils in high-absence groups ---------------------
E <- fread(here::here("data-raw", "ees", "ees_absence_characteristics.csv"), colClasses = "character",
           select = c("time_period", "geographic_level", "education_phase", "breakdown_topic", "breakdown", "sess_possible", "sess_overall", "enrolments_pa_10_exact", "enrolments"))
E <- E[geographic_level == "National" & education_phase == "State-funded secondary" & time_period %in% c("201819", "202425")]
shiftshare <- function(topic, groups) {
  e <- E[breakdown_topic == topic & breakdown %in% groups]
  e[, `:=`(poss = num(sess_possible), sess = num(sess_overall), enr = num(enrolments), pa = num(enrolments_pa_10_exact))]
  rbindlist(lapply(c("overall", "persistent"), function(o) {
    e[, `:=`(rate = if (o == "overall") 100 * sess / poss else 100 * pa / enr, w = if (o == "overall") poss else enr)]
    e[, w := w / sum(w), by = time_period]
    a <- e[time_period == "201819", .(breakdown, r0 = rate, w0 = w)]; b <- e[time_period == "202425", .(breakdown, r1 = rate, w1 = w)]
    m <- merge(a, b, by = "breakdown")
    data.table(topic = topic, outcome = o, total0 = sum(m$w0 * m$r0), total1 = sum(m$w1 * m$r1),
               within = sum(m$w0 * (m$r1 - m$r0)), composition = sum((m$w1 - m$w0) * m$r0), interaction = sum((m$w1 - m$w0) * (m$r1 - m$r0)),
               groups = list(m))
  }))
}
comp <- rbind(shiftshare("FSM", c("FSM eligible", "FSM not eligible")),
              shiftshare("SEN provision", c("No identified SEN", "SEN support", "EHC plan", "Statement or EHC plan")))

res <- list(back = back, back_sum = back_sum, fits = fits, prop = prop, mag = mag, comp = comp,
            trend = tr, pre = PRE, now = NOW, built = Sys.Date())
saveRDS(res, here::here("data", "covid_shift.rds"))
options(width = 200)
print(back_sum, digits = 3)
print(fits[outcome %in% c("overall", "persistent"), .(outcome, tp, mean = round(mean, 2), r2 = round(r2_intake, 3), sd_resid = round(sd_resid_log, 3),
                                                    q25 = round(q25, 3), var_la = round(var_la, 4), var_school = round(var_school, 4))])
print(prop[, !"rise_q"], digits = 3); print(prop$rise_q); print(mag, digits = 3); print(comp[, !"groups"], digits = 3)
print(comp$groups)
