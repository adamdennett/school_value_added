# 04_place_and_absence.R - The v1 area analyses, refitted on Value Added v2.1
# ---------------------------------------------------------------------------
# The original Sussex documents (school_attainment_tool, AREA_REPORTS.md)
# rest on five model-based analyses beyond value added itself. This refits
# each on the v2.1 specification:
#
#   1. Absence against intake. log(absence) on the v2 intake terms with
#      year and region effects but no local authority effect, so an
#      authority's excess absence over what its intake predicts stays
#      visible (as v1's intake-only stage 1). Per school-year, observed
#      minus predicted, in percentage points; per school and per authority.
#   2. Value added with no allowance for absence: the v2 model without the
#      absence term (v1's M2), for the map and the area comparison.
#   3. The district test: a district level inside the authority, with and
#      without each school's own record (v1's R/18).
#   4. Trusts: a multi-academy trust level, with the academy route and a
#      school term so trusts that take on struggling schools are not
#      penalised for it (v1's R/19).
#   5. Absence scenarios: what reaching England's average school absence
#      would be worth, on the v2 absence coefficients.
#
# Output: data/place_absence.rds.  Rscript R/04_place_and_absence.R (~15 min)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(lme4))

INTAKE <- c("log(PTFSM6CLA1A)", "log(PNUMEAL)", "ks2_c", "ADMPOL_PT", "z_l4", "sen_support",
            "sen_ehcp", "mobile", "girls_school", "boys_school", "age16")
SUSSEX_LADS <- c("Brighton and Hove", "Eastbourne", "Hastings", "Lewes", "Rother", "Wealden", "Adur",
                 "Arun", "Chichester", "Crawley", "Horsham", "Mid Sussex", "Worthing")

base_rows <- function(d) {
  keep <- !d$excluded & d$PTFSM6CLA1A > 0 & d$PERCTOT > 0 & d$PNUMEAL > 0 &
    complete.cases(d[, c("ks2_c", "ADMPOL_PT", "z_l4", "sen_support", "sen_ehcp", "mobile", "age16",
                         "gor_name", "LANAME", "PTFSM6CLA1A", "PERCTOT", "PNUMEAL")])
  d[which(keep), ] |>
    mutate(year_label = factor(year_label), gor_name = factor(gor_name), LANAME = factor(LANAME))
}
fit <- function(f, d, reml = TRUE) lmer(as.formula(f), data = d, REML = reml, control = CTRL)
rhs <- function(terms, re) paste(c(terms, re), collapse = " + ")

# ---- 1. Absence against intake ---------------------------------------------
absence_intake <- function(d) {
  m <- fit(paste("log(PERCTOT) ~", rhs(INTAKE, c("(1 | year_label)", "(1 | gor_name)"))), d)
  sy <- tibble(URN = d$URN, LANAME = as.character(d$LANAME), year_label = as.character(d$year_label),
               obs = d$PERCTOT, exp = exp(fitted(m))) |> mutate(excess = obs - exp)
  list(model = m, r2 = cor(log(sy$obs), log(sy$exp))^2,
       school_years = sy,
       schools = sy |> group_by(URN) |>
         summarise(abs_obs = mean(obs), abs_exp = mean(exp), abs_excess = mean(excess), .groups = "drop"),
       la = sy |> group_by(LANAME) |>
         summarise(abs_obs = mean(obs), abs_exp = mean(exp), excess = mean(excess), schools = n_distinct(URN),
                   .groups = "drop") |>
         mutate(rank_excess = rank(-excess), n_la = n()))
}

# ---- 2. Value added with no allowance for absence --------------------------
va_without_absence <- function(d) {
  bind_rows(lapply(c("all", "disadvantaged"), function(g) {
    y <- GROUPS[[g]]
    dg <- d[!is.na(d[[y]]) & d[[y]] > 0, ]
    terms <- setdiff(fixed_for(g), "log(PERCTOT)")
    m <- fit(paste0("log(", y, ") ~ ", rhs(terms, RANDOM_V2)), dg)
    xb <- as.vector(getME(m, "X") %*% fixef(m))
    u_year <- ranef(m)$year_label[as.character(dg$year_label), 1]
    pred <- exp(xb + u_year) * exp(0.5 * sigma(m)^2)
    tibble(URN = dg$URN, group = g, va = dg[[y]] - pred) |>
      group_by(URN, group) |> summarise(va_m2 = mean(va), .groups = "drop")
  }))
}

# ---- 3. The district test ---------------------------------------------------
district_test <- function(d, abs_model_terms) {
  dd <- d |> filter(!is.na(lad)) |> mutate(lad = factor(lad)) |> droplevels()
  outcomes <- list(
    all = list(lhs = "log(ATT8SCR)", terms = fixed_for("all"), label = "All pupils"),
    disadvantaged = list(lhs = "log(ATT8SCR_FSM6CLA1A)", terms = fixed_for("disadvantaged"), label = "Disadvantaged"),
    non_disadvantaged = list(lhs = "log(ATT8SCR_NFSM6CLA1A)", terms = fixed_for("non_disadvantaged"), label = "Non-disadvantaged"),
    absence = list(lhs = "log(PERCTOT)", terms = abs_model_terms, label = "Absence (intake-adjusted)"))
  res <- list(); eff <- list()
  for (k in names(outcomes)) for (school in c(FALSE, TRUE)) {
    o <- outcomes[[k]]
    y <- sub("^log\\((.*)\\)$", "\\1", o$lhs)
    dk <- dd[!is.na(dd[[y]]) & dd[[y]] > 0, ] |> droplevels()
    extra <- if (school) "(1 | URN)" else NULL
    spec <- if (school) "With a school term" else "Published specification"
    message(sprintf("  district test: %s, %s", o$label, spec))
    m0 <- fit(paste(o$lhs, "~", rhs(o$terms, c("(1 | year_label)", "(1 | gor_name/LANAME)", extra))), dk, FALSE)
    m1 <- fit(paste(o$lhs, "~", rhs(o$terms, c("(1 | year_label)", "(1 | gor_name/LANAME/lad)", extra))), dk, FALSE)
    lrt <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m0)))
    vc <- as.data.frame(VarCorr(m1))
    res[[paste(k, spec)]] <- tibble(outcome = o$label, spec = spec, lrt = lrt,
      p = 0.5 * pchisq(lrt, 1, lower.tail = FALSE),
      sd_lad = vc$sdcor[startsWith(vc$grp, "lad")], sd_la = vc$sdcor[vc$grp == "LANAME:gor_name"])
    rr <- ranef(m1, condVar = TRUE); re <- rr[[grep("^lad", names(rr), value = TRUE)[1]]]
    eff[[paste(k, spec)]] <- tibble(key = rownames(re), effect = re[, 1], se = sqrt(as.numeric(attr(re, "postVar")))) |>
      mutate(lad = sub(":.*$", "", key), outcome = o$label, spec = spec,
             ci_lo = effect - 1.96 * se, ci_hi = effect + 1.96 * se, sig = ci_lo > 0 | ci_hi < 0) |>
      filter(lad %in% SUSSEX_LADS) |> select(-key)
  }
  list(tests = bind_rows(res), effects = bind_rows(eff))
}

# ---- 4. Trusts ---------------------------------------------------------------
route_of <- function(nftype) case_when(
  nftype == "AC" ~ "Sponsor-led academy", nftype == "ACC" ~ "Converter academy",
  nftype %in% c("F", "UTC", "SS", "CTC") ~ "Free school, UTC or studio",
  nftype %in% c("CY", "FD", "VA", "VC") ~ "Maintained", TRUE ~ "Other")

trust_by_year <- function() {
  zip <- tail(sort(list.files(here::here("data-raw", "ees"), "^exclusions_school_.*\\.zip$", full.names = TRUE)), 1)
  t <- readr::read_csv(unz(zip, "data/exc_school.csv"), col_types = readr::cols(.default = "c"),
                       progress = FALSE) |>
    transmute(year_label = paste0(substr(time_period, 1, 4), "-", substr(time_period, 5, 6)),
              urn_published = as.integer(school_urn), trust = trimws(trust_name)) |>
    mutate(trust = ifelse(trust %in% c("", "z", "x", "NA") | is.na(trust), NA, trust))
  sizes <- t |> filter(!is.na(trust)) |> group_by(year_label, trust) |>
    summarise(trust_size = n_distinct(urn_published), .groups = "drop")
  t |> left_join(sizes, by = c("year_label", "trust")) |> distinct(year_label, urn_published, .keep_all = TRUE)
}

trusts <- function(d, abs_model_terms) {
  dt <- d |>
    mutate(yr = as.character(year_label)) |>
    left_join(trust_by_year(), by = c("yr" = "year_label", "urn_published")) |>
    mutate(route = factor(route_of(NFTYPE)),
           trust_kind = case_when(MINORGROUP != "Academy" ~ "Maintained",
                                  !is.na(trust) & trust_size > 1 ~ "Multi-academy trust",
                                  !is.na(trust) ~ "Single-academy trust", TRUE ~ "Academy, trust not recorded"),
           in_mat = as.numeric(trust_kind == "Multi-academy trust"),
           trust_group = factor(ifelse(in_mat == 1, trust, paste0("none_", URN))))
  outcomes <- list(all = c("log(ATT8SCR)", "All pupils"), disadvantaged = c("log(ATT8SCR_FSM6CLA1A)", "Disadvantaged"),
                   absence = c("log(PERCTOT)", "Absence (intake-adjusted)"))
  out <- lapply(names(outcomes), function(k) {
    lhs <- outcomes[[k]][1]; y <- sub("^log\\((.*)\\)$", "\\1", lhs)
    terms <- if (k == "absence") abs_model_terms else fixed_for(k)
    dk <- dt[!is.na(dt[[y]]) & dt[[y]] > 0, ] |> droplevels()
    message("  trusts: ", outcomes[[k]][2])
    r <- rhs(c(terms, "route"), c("(1 | year_label)", "(1 | gor_name/LANAME)", "(1 | URN)"))
    m0 <- fit(paste(lhs, "~", r), dk, FALSE)
    m1 <- fit(paste(lhs, "~", r, "+ (0 + in_mat | trust_group)"), dk, FALSE)
    lrt <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m0)))
    re <- ranef(m1, condVar = TRUE)$trust_group
    te <- tibble(trust_group = rownames(re), effect = re[["in_mat"]], se = sqrt(attr(re, "postVar")[1, 1, ])) |>
      inner_join(dk |> filter(trust_kind == "Multi-academy trust") |> group_by(trust_group) |>
                   summarise(schools_in_data = n_distinct(URN),
                             las = paste(sort(unique(as.character(LANAME))), collapse = "; "), .groups = "drop"),
                 by = "trust_group") |>
      mutate(outcome = outcomes[[k]][2], ci_lo = effect - 1.96 * se, ci_hi = effect + 1.96 * se,
             sig = ci_lo > 0 | ci_hi < 0)
    vc <- as.data.frame(VarCorr(m1))
    list(test = tibble(outcome = outcomes[[k]][2], lrt = lrt, p = 0.5 * pchisq(lrt, 1, lower.tail = FALSE),
                       sd_trust = vc$sdcor[vc$grp == "trust_group"], sd_school = vc$sdcor[vc$grp == "URN"],
                       sd_la = vc$sdcor[vc$grp == "LANAME:gor_name"], n_trusts = sum(grepl("^none_", rownames(re)) == FALSE)),
         effects = te,
         route = as.data.frame(summary(m1)$coefficients) |> tibble::rownames_to_column("term") |>
           filter(grepl("^route", term)) |>
           transmute(outcome = outcomes[[k]][2], term = sub("^route", "", term), estimate = Estimate, t = `t value`))
  })
  list(tests = bind_rows(lapply(out, `[[`, "test")), effects = bind_rows(lapply(out, `[[`, "effects")),
       route = bind_rows(lapply(out, `[[`, "route")))
}

# ---- 5. Absence scenarios -----------------------------------------------------
scenarios <- function(d, V) {
  b <- function(g) V$coefs$estimate[V$coefs$group == g & V$coefs$term == "log(PERCTOT)"]
  latest <- max(as.character(d$year_label))
  nat_abs <- mean(d$PERCTOT[d$year_label == latest])
  d |> filter(year_label == latest) |>
    transmute(URN, absence = PERCTOT, att8 = ATT8SCR, att8_dis = ATT8SCR_FSM6CLA1A,
              shift = log(pmin(absence, nat_abs)) - log(absence),
              gain_all = att8 * (exp(b("all") * shift) - 1),
              gain_dis = att8_dis * (exp(b("disadvantaged") * shift) - 1)) |>
    one_row_a_school() |>
    mutate(nat_abs = nat_abs, year = latest)
}

#' Two schools that later merged can share a year under one identity:
#' average their rows, as va_years() does
one_row_a_school <- function(x) x |> group_by(URN) |> summarise(across(everything(), mean), .groups = "drop")

build_place_absence <- function() {
  d <- base_rows(readRDS(here::here("data", "inputs.rds")))
  V <- readRDS(here::here("data", "va_v2.rds"))
  message("Absence against intake ...")
  ai <- absence_intake(d)
  abs_terms <- INTAKE
  message("Value added with no allowance for absence ...")
  m2 <- va_without_absence(d)
  message("District test ...")
  dt <- district_test(d, abs_terms)
  message("Trusts ...")
  tr <- trusts(d, abs_terms)
  out <- list(abs_school_years = ai$school_years, abs_schools = ai$schools, abs_la = ai$la, abs_r2 = ai$r2,
              abs_coefs = as.data.frame(summary(ai$model)$coefficients) |> tibble::rownames_to_column("term"),
              va_m2 = m2, district = dt, trusts = tr, scenarios = scenarios(d, V), built_at = Sys.time())
  saveRDS(out, here::here("data", "place_absence.rds"))
  print(dt$tests); print(tr$tests)
  invisible(out)
}

if (sys.nframe() == 0) build_place_absence()
