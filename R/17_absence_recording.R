# 17_absence_recording.R - What absence is made of, and how it is recorded
# ---------------------------------------------------------------------------
# Supports the later sections of absence_causes.qmd:
#
#   1. Register codes, suspension and Brighton & Hove's code-by-code gap,
#      carried from the Exclusions and Attendance report in the sibling
#      project (school_attainment_tool, R/15_exclusions.R).
#   2. Illness or recording: how illness and unauthorised absence move
#      against each other, between authorities and within them over time.
#   3. The same split for all, disadvantaged and non-disadvantaged pupils,
#      from DfE's absence by characteristic file (full years, to 2024/25).
#   4. Is it coastal? Every school's distance from the sea comes from
#      R/15b_coastal_absence.R in the sibling project. The coastal term is
#      added to this page's own multilevel model, and the same pupils are
#      followed from Year 7 to Year 8.
#
# All published data; associations between schools and places.
#
# Output: data/absence_recording.rds (committed).
# Rscript R/17_absence_recording.R, after R/08_absence_causes.R.
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({
  library(lme4); library(sandwich); library(lmtest)
})
num <- function(x) suppressWarnings(as.numeric(x))

X  <- readRDS(file.path(SAT, "data", "exclusions_absence.rds"))
CO <- readRDS(file.path(SAT, "data", "coastal_absence.rds"))
dat <- readRDS(here::here("data", "absence_school_frame.rds"))
A <- readRDS(here::here("data", "absence_causes.rds"))

coast_la <- CO$la |> transmute(la = la_name, share, coast, fsm)
BH <- "Brighton and Hove"

# ---- absence by pupil group and year group, authorities and England --------
GROUPS <- c("Total" = "All pupils", "FSM eligible in last 6 years" = "Disadvantaged",
            "FSM not eligible in last 6 years" = "Non-disadvantaged")
ch <- readr::read_csv(file.path(SAT, "data", "cache", "ees_absence_characteristics.csv"),
                      col_select = c(time_period, geographic_level, la_name, education_phase,
                                     breakdown_topic, breakdown, sess_possible, sess_overall,
                                     sess_auth_illness, sess_unauth_other),
                      col_types = readr::cols(.default = "c"), progress = FALSE) |>
  filter(geographic_level %in% c("Local authority", "National")) |>
  mutate(tp = as.integer(time_period), poss = num(sess_possible),
         overall = 100 * num(sess_overall) / poss,
         illness = 100 * num(sess_auth_illness) / poss,
         unauth = 100 * num(sess_unauth_other) / poss)
latest <- max(ch$tp)

g3 <- ch |>
  filter(education_phase == "State-funded secondary", breakdown %in% names(GROUPS),
         breakdown_topic %in% c("Total", "FSM ever 6")) |>
  mutate(group = factor(unname(GROUPS[breakdown]), GROUPS),
         la = ifelse(geographic_level == "National", "England", la_name),
         illness_share = 100 * illness / overall) |>
  select(tp, la, group, overall, illness, unauth, illness_share)
g3_now <- g3 |> filter(tp == latest)

group_compare <- g3_now |> filter(la %in% c("England", BH)) |>
  tidyr::pivot_wider(id_cols = group, names_from = la,
                     values_from = c(overall, illness, unauth, illness_share))
group_rank <- g3_now |> filter(la != "England", la %in% coast_la$la) |>
  group_by(group) |> mutate(rank = rank(-illness), n = n()) |> ungroup() |>
  filter(la == BH) |> select(group, rank, n)

fit_row <- function(formula, d, label, term = "coastTRUE", cluster = NULL) {
  d <- d |> filter(if_all(all_of(all.vars(formula)), ~ !is.na(.x)))
  m <- lm(formula, d)
  v <- if (is.null(cluster)) vcovHC(m, "HC1") else vcovCL(m, cluster = d[[cluster]])
  r <- coeftest(m, v)[term, ]
  tibble(label = label, est = r[[1]], se = r[[2]], p = r[[4]], n = nobs(m))
}

# Coastal against inland, for each pupil group, allowing for the authority's
# disadvantage. Authorities only: there is no school-level split by reason.
gd <- g3_now |> filter(la != "England") |> inner_join(coast_la, by = "la")
group_coast <- bind_rows(lapply(GROUPS, function(g) {
  d <- gd |> filter(group == g)
  bind_rows(lapply(c(illness = "illness", unauth = "unauth", overall = "overall"), function(y)
    fit_row(as.formula(paste(y, "~ coast + fsm")), d, g)), .id = "outcome")
}))

# ---- illness or recording? ------------------------------------------------
ic <- X$illness_ctx
between_r <- cor(ic$illness, ic$unauth_other)
fsm_r <- list(illness = cor(ic$fsm, ic$illness), unauth = cor(ic$fsm, ic$unauth_other),
              total = cor(ic$fsm, ic$total))
quartiles <- ic |>
  mutate(q = cut(fsm, quantile(fsm, 0:4 / 4), include.lowest = TRUE,
                 labels = c("Least disadvantaged quarter", "2nd quarter", "3rd quarter",
                            "Most disadvantaged quarter"))) |>
  group_by(q) |>
  summarise(illness = mean(illness), unauth = mean(unauth_other), total = mean(total), .groups = "drop")

# If places re-label one as the other, an authority whose illness rises
# faster than England's should see unauthorised absence rise more slowly.
pan <- g3 |> filter(group == "All pupils", la != "England", tp != 202021)
within_fit <- fit_row(unauth ~ illness + factor(la) + factor(tp), pan, "Within authorities",
                      term = "illness", cluster = "la")
pooled_fit <- fit_row(unauth ~ illness + factor(tp), pan, "Between and within",
                      term = "illness", cluster = "la")
excess <- ic |> transmute(la = la_name, resid = as.numeric(resid), unauth_other, illness, total)
excess_r <- cor(excess$resid, excess$unauth_other)

# ---- the coastal term, in this page's own model -----------------------------
km <- CO$school_km |> transmute(URN = as.integer(urn), km)
Z <- dat |> inner_join(km, by = "URN") |>
  mutate(coast = km <= 10, across(all_of(names(A$cont)), ~ (.x - mean(.x)) / sd(.x)))
CTRL <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
RHS <- c(names(A$cont), names(A$fact))
fit_coast <- function(y, vars, spec) {
  dd <- Z |> mutate(.y = log(Z[[y]] + 0.1))
  m <- lmer(as.formula(paste(".y ~ coast +", paste(vars, collapse = " + "), "+ (1 | region / la)")),
            data = dd, REML = TRUE, control = CTRL)
  s <- summary(m)$coefficients["coastTRUE", ]
  tibble(outcome = y, spec = spec, b = s[[1]], se = s[[2]],
         pct = 100 * (exp(s[[1]]) - 1), lo = 100 * (exp(s[[1]] - 1.96 * s[[2]]) - 1),
         hi = 100 * (exp(s[[1]] + 1.96 * s[[2]]) - 1))
}
NO_ETH <- setdiff(RHS, c("eal", "wbri", "grt"))
coast_models <- bind_rows(lapply(names(A$outcomes), function(y) bind_rows(
  fit_coast(y, RHS, "Everything on this page"),
  fit_coast(y, NO_ETH, "Without ethnicity and language"),
  fit_coast(y, "fsm", "Free school meals only"))))

# ---- the same pupils, Year 7 to Year 8 ------------------------------------
yg <- ch |>
  filter(geographic_level == "Local authority", breakdown_topic == "Year group",
         (education_phase == "State-funded secondary" & breakdown %in% c("Year 7", "Year 8")) |
           (education_phase == "State-funded primary" & breakdown == "Year 6")) |>
  transmute(tp, la = la_name, yg = sub("Year ", "y", breakdown), overall, illness, unauth)
intake <- Z |> group_by(la) |>
  summarise(region = first(as.character(region)),
            eal = weighted.mean(dat$eal[match(URN, dat$URN)], exp(dat$size[match(URN, dat$URN)])),
            sen = weighted.mean(dat$sen_sup[match(URN, dat$URN)] + dat$ehcp[match(URN, dat$URN)],
                                exp(dat$size[match(URN, dat$URN)])), .groups = "drop")
cohort_step <- function(y) {
  w <- yg |> select(tp, la, yg, value = all_of(y)) |>
    tidyr::pivot_wider(names_from = yg, values_from = value)
  w |> filter(tp %in% c(202223, 202324)) |>
    transmute(t1 = tp + 101L, la, y7_a = y7) |>
    inner_join(w |> transmute(t1 = tp, la, y8_b = y8), by = c("t1", "la")) |>
    transmute(t1, la, measure = y, step = y8_b - y7_a)
}
steps <- bind_rows(lapply(c("overall", "illness", "unauth"), cohort_step)) |>
  inner_join(coast_la, by = "la") |> left_join(intake, by = "la")
cohort_means <- steps |> group_by(measure, coast) |>
  summarise(step = mean(step, na.rm = TRUE), .groups = "drop")
cohort_models <- bind_rows(lapply(c("overall", "illness", "unauth"), function(y) {
  d <- steps |> filter(measure == y)
  bind_rows(
    fit_row(step ~ coast + fsm + factor(t1), d, "Disadvantage", cluster = "la"),
    fit_row(step ~ coast + fsm + eal + sen + region + factor(t1), d,
            "Disadvantage, EAL, SEN and region", cluster = "la"),
    fit_row(step ~ coast + fsm + eal + sen + region + factor(t1), d |> filter(la != BH),
            "As above, without Brighton and Hove", cluster = "la")) |>
    mutate(measure = y)
}))
cohort_bh <- steps |> filter(la == BH) |> select(t1, measure, step)

saveRDS(list(
  # carried from the Exclusions and Attendance report
  reasons = X$reasons, reason_gap = X$reason_gap, reason_ranks = X$reason_ranks,
  reason_series = X$reason_series, national = X$national, abs_models = X$abs_models,
  att_models = X$att_models, la_suspension = X$la, illness_ctx = X$illness_ctx,
  bh_schools = X$bh_by_school, latest_absence = X$latest_absence,
  latest_exclusions = X$latest_exclusions,
  # new here
  group_compare = group_compare, group_rank = group_rank, group_coast = group_coast,
  quartiles = quartiles, between_r = between_r, fsm_r = fsm_r, within_fit = within_fit,
  pooled_fit = pooled_fit, excess_r = excess_r,
  coast_la = CO$la, coast_school = CO$school_models, coast_models = coast_models,
  cohort_means = cohort_means, cohort_models = cohort_models, cohort_bh = cohort_bh,
  y6_bh = CO$y6_bh, group_year = latest, built_at = Sys.time()),
  here::here("data", "absence_recording.rds"))
message("Saved data/absence_recording.rds")
print(as.data.frame(group_compare), digits = 3)
print(as.data.frame(group_rank))
print(as.data.frame(group_coast |> mutate(across(c(est, se, p), ~ round(.x, 3)))))
cat("between r", round(between_r, 2), " excess r", round(excess_r, 2), "\n")
print(as.data.frame(rbind(within_fit, pooled_fit)), digits = 3)
print(as.data.frame(coast_models |> mutate(across(c(pct, lo, hi), ~ round(.x, 1))) |> select(outcome, spec, pct, lo, hi)))
print(as.data.frame(cohort_means), digits = 3)
print(as.data.frame(cohort_models |> mutate(across(c(est, se, p), ~ round(.x, 3)))))
print(as.data.frame(cohort_bh), digits = 3)
