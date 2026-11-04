# 08_absence_causes.R - What goes with school absence, school by school
# ---------------------------------------------------------------------------
# A first look at the causes of school-level absence, framed by the research
# map in absence_causes.qmd. Every input is published school-level data;
# associations between schools are not effects on pupils.
#
#   1. The trend by reason: England's state-funded secondary schools,
#      2016/17 to 2024/25 (full years, sessions-weighted), from the EES
#      school-level absence file the panel already downloads.
#   2. Six absence outcomes for 2025/26 (autumn and spring terms): overall,
#      illness, medical appointments, other authorised, unauthorised,
#      persistent (10%+ missed) and severe (50%+ missed).
#   3. School characteristics, as close in time as published: the January
#      2026 school census (FSM, EAL, ethnicity, young carers, girls, size,
#      rural-urban, faith), the January 2026 SEN file (SEN support, EHC
#      plans, and the main types of need), and from the model inputs the
#      latest KS2 prior attainment, neighbourhood education, mobility,
#      admissions policy and sixth form; governance from GIAS.
#   4. For each outcome: a multilevel model, log(outcome) on the
#      standardised characteristics with region and local authority random
#      effects; per-SD effects; how much of the authority-level variation
#      is left once the intake is allowed for; and Shapley shares of R-squared
#      by group of characteristics (OLS, region in the Place group).
#
# Output: data/absence_causes.rds (committed).   Rscript R/08_absence_causes.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(lme4))
num <- function(x) suppressWarnings(as.numeric(x))
ees <- function(...) here::here("data-raw", "ees", ...)

# ---- 1. the trend by reason --------------------------------------------------
raw <- readr::read_csv(here::here("data", "cache", "absence_api_raw.csv"), show_col_types = FALSE,
                       col_types = readr::cols(.default = "c"), progress = FALSE) |>
  filter(geographic_level == "School", education_phase == "State-funded secondary")
trend <- raw |>
  transmute(year = paste0(substr(time_period, 1, 4), "/", substr(time_period, 5, 6)),
            possible = num(sess_possible), enrol = num(enrolments),
            illness = num(sess_auth_illness), appointments = num(sess_auth_appointments),
            auth_all = num(sess_authorised), unauth_holiday = num(sess_unauth_holiday),
            unauth_all = num(sess_unauthorised), pa10 = num(enrolments_pa_10_exact),
            pa50 = num(enrolments_pa_50_exact)) |>
  filter(possible > 0) |>
  group_by(year) |>
  summarise(across(c(illness, appointments, auth_all, unauth_holiday, unauth_all), ~ 100 * sum(.x, na.rm = TRUE) / sum(possible)),
            persistent = 100 * sum(pa10, na.rm = TRUE) / sum(enrol, na.rm = TRUE),
            severe = 100 * sum(pa50, na.rm = TRUE) / sum(enrol, na.rm = TRUE), .groups = "drop") |>
  transmute(year, Illness = illness, `Medical appointments` = appointments,
            `Other authorised` = auth_all - illness - appointments,
            `Unauthorised holiday` = unauth_holiday, `Other unauthorised` = unauth_all - unauth_holiday,
            persistent, severe)

# ---- 2. outcomes, 2025/26 autumn and spring --------------------------------
ab <- readr::read_csv(unz(ees("absence_2term_school_2025-26.zip"), "data/Absence_2term_school.csv"),
                      col_types = readr::cols(.default = "c"), progress = FALSE) |>
  filter(geographic_level == "School", education_phase == "State-funded secondary") |>
  transmute(URN = as.integer(school_urn),
            overall = num(sess_overall_percent), illness = num(sess_auth_illness_rate),
            appointments = num(sess_auth_appointments_rate),
            auth_other = num(sess_authorised_percent) - illness - appointments,
            unauth = num(sess_unauthorised_percent),
            persistent = num(enrolments_pa_10_exact_percent), severe = num(enrolments_pa_50_exact_percent)) |>
  filter(!is.na(overall), overall > 0) |>
  distinct(URN, .keep_all = TRUE)
OUTCOMES <- c(overall = "Overall absence", illness = "Illness", appointments = "Medical appointments",
              auth_other = "Other authorised", unauth = "Unauthorised", persistent = "Persistent absence (10%+)",
              severe = "Severe absence (50%+)")

# ---- 3. school characteristics ------------------------------------------------
sp <- readr::read_csv(ees("spc_school_level_underlying_data_2026.csv"), col_types = readr::cols(.default = "c"), name_repair = "unique_quiet",
                      progress = FALSE) |>
  filter(phase_type_grouping == "State-funded secondary")
pc <- function(x) num(sp[[x]])
census <- tibble(
  URN = as.integer(sp$urn), region = sp$region_name, la = sp$la_name,
  fsm = pc("% of pupils known to be eligible for free school meals"),
  eal = pc("% of pupils whose first language is known or believed to be other than English"),
  wbri = pc("% of pupils classified as white British ethnic origin"),
  grt = pc("% of pupils classified as traveller of Irish heritage ethnic origin") +
        pc("% of pupils classified as Gypsy/Roma ethnic origin"),
  carer = pc("% of pupils who are a young carer"),
  girls = 100 * pc("headcount total female") / pc("headcount of pupils"),
  size = log(pc("headcount of pupils")),
  place = case_when(startsWith(sp$urban_rural, "Urban: Nearer") ~ "Urban, near a major town or city",
                    startsWith(sp$urban_rural, "Urban: Further") ~ "Urban, further from one",
                    grepl("rural", sp$urban_rural) ~ "Rural", TRUE ~ NA_character_),
  faith = !(sp$denomination %in% c("No religious character", "Does not apply", "None", "", NA)))
sen <- readr::read_csv(unz(ees("sen_school_level_2025-26.zip"), "supporting-files/School level underlying data 2026.csv"),
                       col_types = readr::cols(.default = "c"), progress = FALSE) |>
  filter(phase_type_grouping == "State-funded secondary")
sn <- function(x) num(sen[[x]])
need <- function(k) 100 * (sn(paste0("EHC_Primary_need_", k)) + sn(paste0("SUP_Primary_need_", k))) / sn("total_pupils")
send <- tibble(URN = as.integer(sen$URN),
               sen_sup = 100 * sn("SEN_support") / sn("total_pupils"),
               ehcp = 100 * sn("EHC_plan") / sn("total_pupils"),
               semh = need("semh"), asd = need("asd"),
               learning = need("mld") + need("spld"))
d_in <- readRDS(here::here("data", "inputs.rds")) |>
  arrange(URN, desc(year_label)) |> distinct(URN, .keep_all = TRUE) |>
  transmute(URN, urn_published, ks2_c, z_l4, mobile, age16, ADMPOL_PT, SCHNAME)
ctx <- readRDS(here::here("data", "school_context.rds")) |> select(URN, gov_nat)
# a school that has changed URN since its last results is matched on either
key <- function(x) x |> left_join(d_in, by = "URN") |>
  left_join(d_in |> rename(URN2 = URN) |> select(-SCHNAME), by = c("URN" = "urn_published"),
            suffix = c("", ".p"), relationship = "many-to-one") |>
  mutate(across(c(ks2_c, z_l4, mobile, age16, ADMPOL_PT), ~ coalesce(.x, get(paste0(cur_column(), ".p"))))) |>
  select(-ends_with(".p"), -URN2, -urn_published)

dat <- ab |> inner_join(census, by = "URN") |> inner_join(send, by = "URN") |> key() |>
  left_join(ctx, by = "URN") |>
  mutate(mat = case_when(startsWith(gov_nat, "Multi") ~ "Multi-academy trust",
                         gov_nat == "Maintained" ~ "Maintained", TRUE ~ "Single-academy trust or not recorded"),
         mat = factor(mat, c("Maintained", "Multi-academy trust", "Single-academy trust or not recorded")),
         place = factor(place, c("Urban, near a major town or city", "Urban, further from one", "Rural")),
         sixth = !age16, region = factor(region), la = factor(la))
CONT <- c(fsm = "Free school meals (%)", sen_sup = "SEN support (%)", ehcp = "EHC plans (%)",
          semh = "Social, emotional and mental health need (%)", asd = "Autism (%)",
          learning = "Learning difficulty, moderate or specific (%)", carer = "Young carers (%)",
          eal = "English as an additional language (%)", wbri = "White British (%)",
          grt = "Gypsy, Roma and Traveller (%)", girls = "Girls (%)", mobile = "Joined mid-phase (%)",
          ks2_c = "KS2 prior attainment", z_l4 = "Neighbourhood education", size = "Size (log pupils)")
FACT <- c(sixth = "Has a sixth form", ADMPOL_PT = "Admissions", mat = "Governance", faith = "Religious character",
          place = "Rural or urban")
dat <- dat |> filter(if_all(all_of(c(names(CONT), names(FACT), "region", "la")), ~ !is.na(.x)))
message(sprintf("%d schools with every outcome and characteristic (of %d with absence)", nrow(dat), nrow(ab)))
Z <- dat |> mutate(across(all_of(names(CONT)), ~ (.x - mean(.x)) / sd(.x)))

# ---- 4. the models -----------------------------------------------------------
RHS <- paste(c(names(CONT), names(FACT)), collapse = " + ")
ly <- function(y) log(Z[[y]] + 0.1)   # severe absence can be zero
fit_one <- function(y) {
  dd <- Z |> mutate(.y = ly(y))
  null <- lmer(.y ~ 1 + (1 | region / la), data = dd, REML = TRUE, control = CTRL)
  full <- lmer(as.formula(paste(".y ~", RHS, "+ (1 | region / la)")), data = dd, REML = TRUE, control = CTRL)
  vc <- function(m) { v <- as.data.frame(VarCorr(m)); setNames(v$vcov, v$grp) }
  v0 <- vc(null); v1 <- vc(full)
  cf <- summary(full)$coefficients[-1, , drop = FALSE]
  eff <- tibble(outcome = y, term = rownames(cf), b = cf[, 1], se = cf[, 2]) |>
    mutate(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))
  re <- ranef(full)[["la:region"]]
  la <- tibble(outcome = y, la = sub(":.*$", "", rownames(re)), region = sub("^.*:", "", rownames(re)),
               la_pct = 100 * (exp(re[, 1] + ranef(full)$region[sub("^.*:", "", rownames(re)), 1]) - 1))
  part <- tibble(outcome = y,
                 la_null = unname(v0["la:region"] / sum(v0)), region_null = unname(v0["region"] / sum(v0)),
                 la_full = unname(v1["la:region"] / sum(v0)), region_full = unname(v1["region"] / sum(v0)),
                 school_full = unname(v1["Residual"] / sum(v0)),
                 r2_fixed = unname(1 - sum(v1) / sum(v0)))
  # The full outputs, for the page's model tables: every coefficient with
  # its t, the variance at each level before and after, and the fit
  all_cf <- summary(full)$coefficients
  coefs <- tibble(outcome = y, term = rownames(all_cf), b = all_cf[, 1], se = all_cf[, 2], t = all_cf[, 3])
  vfix <- var(as.vector(getME(full, "X") %*% fixef(full)))
  random <- tibble(outcome = y, level = c("Region", "Local authority", "School"),
                   var_null = unname(v0[c("region", "la:region", "Residual")]),
                   var_full = unname(v1[c("region", "la:region", "Residual")]))
  ml <- update(full, REML = FALSE)
  fit <- tibble(outcome = y, n_school = nobs(full), n_la = ngrps(full)[["la:region"]],
                n_region = ngrps(full)[["region"]], mean_y = mean(Z[[y]]),
                r2_marginal = vfix / (vfix + sum(v1)), r2_conditional = (vfix + v1[["region"]] + v1[["la:region"]]) / (vfix + sum(v1)),
                loglik = as.numeric(logLik(ml)), aic = AIC(ml), bic = BIC(ml),
                loglik_null = as.numeric(logLik(update(null, REML = FALSE))))
  list(eff = eff, la = la, part = part, coefs = coefs, random = random, fit = fit)
}
message("Fitting the multilevel models ...")
fits <- lapply(names(OUTCOMES), fit_one)
effects <- bind_rows(lapply(fits, `[[`, "eff"))
la_effects <- bind_rows(lapply(fits, `[[`, "la"))
partition <- bind_rows(lapply(fits, `[[`, "part"))
coefs <- bind_rows(lapply(fits, `[[`, "coefs"))
random <- bind_rows(lapply(fits, `[[`, "random"))
fit_stats <- bind_rows(lapply(fits, `[[`, "fit"))
# What one SD is in each characteristic's own units, and how common each category is
x_desc <- tibble(var = names(CONT), mean = sapply(dat[, names(CONT)], mean), sd = sapply(dat[, names(CONT)], sd))
cat_desc <- bind_rows(lapply(names(FACT), function(f) {
  tb <- table(dat[[f]]); tibble(var = f, level = names(tb), n = as.integer(tb))
}))

# Shapley shares of R-squared, by group of characteristics (OLS)
GROUPS_X <- list(
  "Poverty" = "fsm",
  "SEND" = c("sen_sup", "ehcp", "semh", "asd", "learning"),
  "Young carers" = "carer",
  "Ethnicity and language" = c("eal", "wbri", "grt"),
  "Prior attainment and neighbourhood" = c("ks2_c", "z_l4"),
  "Mobility and sex" = c("mobile", "girls"),
  "The school" = c("size", "sixth", "ADMPOL_PT", "mat", "faith"),
  "Place" = c("place", "region"))
shapley_groups <- function(y) {
  dd <- Z |> mutate(.y = ly(y))
  p <- length(GROUPS_X)
  sub <- expand.grid(rep(list(c(FALSE, TRUE)), p))
  r2 <- apply(sub, 1, function(z) {
    v <- unlist(GROUPS_X[as.logical(z)])
    if (!length(v)) return(0)
    summary(lm(as.formula(paste(".y ~", paste(v, collapse = " + "))), data = dd))$r.squared
  })
  names(r2) <- apply(sub, 1, function(z) paste(as.integer(z), collapse = ""))
  phi <- sapply(seq_len(p), function(j) {
    s <- 0
    for (i in which(!sub[[j]])) {
      z <- as.integer(sub[i, ]); k <- sum(z); z2 <- z; z2[j] <- 1L
      s <- s + factorial(k) * factorial(p - k - 1) / factorial(p) *
        (r2[paste(z2, collapse = "")] - r2[paste(z, collapse = "")])
    }
    s
  })
  tibble(outcome = y, group = names(GROUPS_X), shapley = unname(phi), r2_full = unname(r2[paste(rep(1L, p), collapse = "")]))
}
message("Shapley decomposition ...")
shap <- bind_rows(lapply(names(OUTCOMES), shapley_groups))

# Where the national outcomes sit, for the text
desc <- dat |> summarise(n = n(), across(all_of(names(OUTCOMES)), list(mean = mean, sd = sd)))
cors <- cor(dat[, names(OUTCOMES)])

saveRDS(list(trend = trend, effects = effects, la_effects = la_effects, partition = partition, shapley = shap,
             outcomes = OUTCOMES, cont = CONT, fact = FACT, groups = GROUPS_X, desc = desc, cors = cors,
             n_schools = nrow(dat), n_absence = nrow(ab), sds = sapply(dat[, names(CONT)], sd),
             coefs = coefs, random = random, fit = fit_stats, x_desc = x_desc, cat_desc = cat_desc),
        here::here("data", "absence_causes.rds"))
message("Saved data/absence_causes.rds")
print(partition)
print(shap |> group_by(outcome) |> summarise(r2 = first(r2_full)))
