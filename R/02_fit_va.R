# 02_fit_va.R - Fit Value Added v2.1 for every pupil group
# ---------------------------------------------------------------------------
# For each group (all, disadvantaged, non-disadvantaged, girls, boys):
#
#   1. Fit log(Attainment 8) on the v2 terms (00_config.R) with random
#      intercepts for year and for region / local authority (REML).
#   2. School-year value added in Attainment 8 points, the published way
#      (observed minus the lognormal-corrected prediction), two ways:
#        va_nat  against England: the prediction carries only the year effect
#        va_la   within the local authority: every random effect, as in v1
#   3. Per school: the mean over its years, and a shrunken estimate with a
#      95% interval. A second fit with a school random intercept gives the
#      persistent school variance; the mean log residual is shrunk towards
#      zero by its reliability, tau2 / (tau2 + sigma2 / n), and converted
#      back to points against the school's expected Attainment 8.
#   4. The contribution of every term to each school-year's prediction, so
#      an area's gap to England splits exactly into one piece per term.
#   5. The same again with the kind of absence (KIND_TERM) added: value
#      added allowing for it, published beside the headline (va_kind).
#
# Output: data/va_v2.rds (models summarised, not stored) and
#         data/va_v2_schools.csv (one row a school, every group).
#   Rscript R/02_fit_va.R        (a few minutes)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(lme4))

COMPONENT <- function(col) dplyr::case_when(
  col == "log(PTFSM6CLA1A)" ~ "Disadvantage", col == "log(PERCTOT)" ~ "Absence",
  col == "log(PNUMEAL)" ~ "English as an additional language", col == "ks2_c" ~ "Prior attainment (KS2)",
  startsWith(col, "ADMPOL_PT") ~ "Admissions (selection)", col == "z_l4" ~ "Neighbourhood education",
  startsWith(col, "sen_") ~ "Special educational needs", col == "mobile" ~ "Pupil mobility",
  grepl("_school", col) ~ "Single-sex school", startsWith(col, "age16") ~ "School stops at 16",
  col == "ill_share" ~ "Kind of absence", TRUE ~ NA_character_)

fit_group <- function(d, group, extra = character(0)) {
  y <- GROUPS[[group]]
  dg <- model_rows(d, group, extra)
  f <- as.formula(paste0("log(", y, ") ~ ", paste(c(fixed_for(group), extra), collapse = " + "), " + ",
                         paste(RANDOM_V2, collapse = " + ")))
  message(sprintf("  %s: %d school-years, %d schools", group, nrow(dg), n_distinct(dg$URN)))
  m  <- lmer(f, data = dg, REML = TRUE, control = CTRL)
  ms <- lmer(update(f, . ~ . + (1 | URN)), data = dg, REML = TRUE, control = CTRL)

  # The prediction built from its parts, so each can be left in or out:
  # fixed part, year, region and local authority effects
  bias <- exp(0.5 * sigma(m)^2)
  X <- getME(m, "X"); b <- fixef(m)
  xb <- as.vector(X %*% b)
  re <- ranef(m)
  u_year <- re$year_label[as.character(dg$year_label), 1]
  u_reg  <- re$gor_name[as.character(dg$gor_name), 1]
  u_la   <- re[["LANAME:gor_name"]][paste(dg$LANAME, dg$gor_name, sep = ":"), 1]
  lp_nat <- xb + u_year                   # against England
  lp_la  <- xb + u_year + u_reg + u_la    # within the local authority (= fitted)
  stopifnot(isTRUE(all.equal(lp_la, unname(fitted(m)))))
  sy <- tibble(URN = dg$URN, year_label = as.character(dg$year_label), group = group,
               obs = dg[[y]], exp_nat = exp(lp_nat) * bias, exp_la = exp(lp_la) * bias,
               r_nat = log(dg[[y]]) - lp_nat, r_la = log(dg[[y]]) - lp_la) |>
    mutate(va_nat = obs - exp_nat, va_la = obs - exp_la)

  vc <- as.data.frame(VarCorr(ms))
  v <- function(g) vc$vcov[vc$grp == g]
  tau2 <- v("URN"); sig2 <- sigma(ms)^2
  tau2_nat <- tau2 + v("LANAME:gor_name") + v("gor_name")
  shrink <- function(rbar, n, t2) { lam <- t2 / (t2 + sig2 / n)
    list(u = lam * rbar, psd = sqrt(lam * sig2 / n), rel = lam) }
  sch <- sy |>
    group_by(URN, group) |>
    summarise(years = n(), va_nat = mean(va_nat), va_la = mean(va_la),
              rbar_nat = mean(r_nat), rbar_la = mean(r_la), base = mean(exp_nat),
              base_la = mean(exp_la), att8 = mean(obs), .groups = "drop")
  sn <- shrink(sch$rbar_nat, sch$years, tau2_nat); sl <- shrink(sch$rbar_la, sch$years, tau2)
  sch <- sch |> mutate(
    va_shrunk = base * (exp(sn$u) - 1),
    va_lo = base * (exp(sn$u - 1.96 * sn$psd) - 1), va_hi = base * (exp(sn$u + 1.96 * sn$psd) - 1),
    reliability = sn$rel, va_la_shrunk = base_la * (exp(sl$u) - 1),
    sig = va_lo > 0 | va_hi < 0)

  # Every term's contribution to each school-year's prediction (log scale)
  fx <- sweep(X, 2, b, `*`)
  comp <- COMPONENT(colnames(X))
  contrib <- bind_cols(
    tibble(URN = dg$URN, year_label = as.character(dg$year_label), group = group,
           LANAME = as.character(dg$LANAME), gor_name = as.character(dg$gor_name),
           log_obs = log(dg[[y]])),
    as_tibble(sapply(unique(na.omit(comp)), function(k) rowSums(fx[, comp %in% k, drop = FALSE]))),
    tibble(Year = u_year, Region = u_reg, `Local authority` = u_la,
           `Unexplained (school)` = log(dg[[y]]) - lp_la))

  re <- ranef(m, condVar = TRUE)[["LANAME:gor_name"]]
  la <- tibble(key = rownames(re), effect = re[, 1],
               se = sqrt(as.numeric(attr(re, "postVar")))) |>
    mutate(LANAME = sub(":.*$", "", key), group = group,
           lo = effect - 1.96 * se, hi = effect + 1.96 * se, sig = lo > 0 | hi < 0,
           rank = rank(-effect), n_la = n()) |> select(-key)

  s <- summary(m)$coefficients
  r2 <- performance::r2_nakagawa(m, tolerance = 1e-10)
  list(
    summary = tibble(group = group, rows = nrow(dg), schools = n_distinct(dg$URN),
                     n_la = n_distinct(dg$LANAME), n_region = n_distinct(dg$gor_name),
                     n_year = n_distinct(dg$year_label),
                     loglik = as.numeric(logLik(refitML(m))), aic = AIC(refitML(m)), bic = BIC(refitML(m)),
                     r2_marginal = r2$R2_marginal, r2_conditional = r2$R2_conditional,
                     sigma = sigma(m), tau_school = sqrt(tau2), tau_school_nat = sqrt(tau2_nat),
                     reliability_4yr = mean(tau2_nat / (tau2_nat + sig2 / 4)),
                     mean_att8 = mean(dg[[y]])),
    coefs = tibble(group = group, term = rownames(s), estimate = s[, 1], se = s[, 2], t = s[, 3]),
    vc = as.data.frame(VarCorr(m)) |> mutate(group = group),
    school_years = sy, schools = sch, contrib = contrib, la = la)
}

#' v1 value added, as published (within the LA), mean over the four years,
#' under the same one-identity-per-school URNs as v2 (ids: urn_published -> URN).
#' v1 is fixed history: its yearly residuals are a snapshot of the original
#' project's published model, in data/lookups/v1_published.csv.
v1_va <- function(ids) {
  read.csv(here::here("data", "lookups", "v1_published.csv")) |>
    rename(URN = urn_published) |> mutate(urn_published = URN) |>
    left_join(ids, by = "urn_published") |>
    transmute(URN = coalesce(URN.y, urn_published), all, disadvantaged, non_disadvantaged) |>
    pivot_longer(-URN, names_to = "group", values_to = "va") |>
    filter(!is.na(va)) |>
    group_by(URN, group) |> summarise(va_v1 = mean(va), years_v1 = n(), .groups = "drop")
}

build_va <- function() {
  d <- readRDS(here::here("data", "inputs.rds"))
  fits <- lapply(names(GROUPS), function(g) fit_group(d, g)) |> setNames(names(GROUPS))
  pick <- function(k) bind_rows(lapply(fits, `[[`, k))
  message("Allowing for the kind of absence ...")
  kind <- lapply(names(GROUPS), function(g) fit_group(d, g, KIND_TERM)) |> setNames(names(GROUPS))
  pick_kind <- function(k) bind_rows(lapply(kind, `[[`, k))

  latest <- max(d$year_label)
  schools <- d |>
    arrange(URN, desc(year_label)) |>
    distinct(URN, .keep_all = TRUE) |>
    transmute(URN, SCHNAME, TOWN, LANAME = as.character(LANAME), gor_name = as.character(gor_name),
              lad, lon, lat, latest_year = year_label, type = MINORGROUP, GENDER,
              religion = RELCHAR, admissions = as.character(ADMPOL_PT), pupils = TOTPUPS,
              fsm = PTFSM6CLA1A, absence = PERCTOT, eal = PNUMEAL, ks2_c, mobile,
              sen_support, sen_ehcp, l4, att8 = ATT8SCR, att8_dis = ATT8SCR_FSM6CLA1A,
              att8_non = ATT8SCR_NFSM6CLA1A, att8_girls = ATT8SCR_GIRLS, att8_boys = ATT8SCR_BOYS,
              ofsted_model = as.character(OFSTEDRATING_1), ofsted_model_date = OFSTEDLASTINSP,
              ofsted_latest, ofsted_latest_date, ofsted_kind, ofsted_rc, ofsted_nog, run_by, gov, route)

  out <- list(
    spec = list(version = VERSION, fixed = FIXED_V2, random = RANDOM_V2, kind = KIND_TERM,
                groups = GROUPS, labels = GROUP_LABELS, years = sort(unique(d$year_label))),
    summary = pick("summary"), coefs = pick("coefs"), vc = pick("vc"),
    school_years = pick("school_years"), va = pick("schools"), contrib = pick("contrib"),
    la = pick("la"), v1 = v1_va(d |> distinct(urn_published, URN)), schools = schools,
    va_kind = pick_kind("schools") |> select(URN, group, years, va_nat, va_shrunk, va_lo, va_hi, sig),
    coefs_kind = pick_kind("coefs"), summary_kind = pick_kind("summary"),
    excluded = d |> filter(excluded) |>
      transmute(URN, SCHNAME, LANAME = as.character(LANAME), year_label, att8 = ATT8SCR,
                gcse_entries = TAVENT_G_PTQ_EE, any_qual = PTANYQ_PTQ_EE, reason = excl_reason),
    ofsted_as_at = attr(d, "ofsted_as_at"), built_at = Sys.time())
  saveRDS(out, here::here("data", "va_v2.rds"))

  wide <- out$va |>
    select(URN, group, years, va_nat, va_shrunk, va_lo, va_hi, va_la) |>
    pivot_wider(names_from = group, values_from = c(years, va_nat, va_shrunk, va_lo, va_hi, va_la),
                names_glue = "{group}_{.value}") |>
    left_join(schools |> select(URN, SCHNAME, LANAME, lad, TOWN), by = "URN") |>
    left_join(out$va_kind |> select(URN, group, va_shrunk) |>
                pivot_wider(names_from = group, values_from = va_shrunk, names_glue = "{group}_va_kind_shrunk"),
              by = "URN") |>
    relocate(URN, SCHNAME, TOWN, LANAME, lad) |>
    mutate(across(where(is.double), ~round(.x, 2)))
  readr::write_csv(wide, here::here("data", "va_v2_schools.csv"))
  print(out$summary)
  invisible(out)
}

if (sys.nframe() == 0) build_va()
