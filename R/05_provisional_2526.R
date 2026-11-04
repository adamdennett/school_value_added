# 05_provisional_2526.R - Provisional Value Added v2.1 for 2025/26
# ---------------------------------------------------------------------------
# DfE's provisional 2025/26 KS4 results are out; the revised results follow
# in February 2027, and the census, SEN and mobility data for the year later
# still. So 2025/26 is PREDICTED from the fitted v2.1 model, not refitted,
# exactly as the original project extended v1 (school_attainment_tool,
# R/05b_value_added_2526.R). The four model years are untouched.
#
#   - Absence: 2025/26 publishes autumn and spring only. Full-year absence
#     is bridged from the two-term figure, a log-log fit on every earlier
#     school-year with both.
#   - Prior attainment, SEN support and EHCP rates, mobility, the
#     single-sex flag and whether the school stops at 16 are carried forward from each school's latest earlier
#     year. Disadvantage and EAL are 2025/26's own.
#   - Neighbourhood education is the 2021 Census figure, as for every year.
#   - The year effect for 2025/26 is not estimated by the model, so it is set
#     so England's schools average zero on the log scale, as the model's year
#     effects do for the four fitted years.
#
# Value added is against England only (the headline v2 measure), unshrunk:
# one provisional year. Output: data/va_2526_provisional.rds.
#   Rscript R/05_provisional_2526.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))

NEW_YEAR <- "2025-26"
RELEASE  <- "provisional KS4 results (October 2026)"
NEXT     <- "the revised KS4 release in February 2027"

#' log(full-year absence) from log(two-term absence), every earlier year
bridge_absence <- function(p) {
  d <- p |> filter(year_label != NEW_YEAR, PERCTOT > 0, PERCTOT_2term > 0)
  m <- lm(log(PERCTOT) ~ log(PERCTOT_2term), data = d)
  list(model = m, r2 = summary(m)$r.squared, n = nrow(d))
}

build_provisional <- function() {
  V <- readRDS(here::here("data", "va_v2.rds"))
  d_in <- readRDS(here::here("data", "inputs.rds"))
  p5 <- readRDS(here::here("data", "panel_data_5yr.rds")) |>
    mutate(year_label = as.character(year_label))
  br <- bridge_absence(p5)

  ids <- d_in |> distinct(urn_published, URN)
  # Each school's latest earlier values, to carry forward
  carry <- d_in |> arrange(URN, desc(year_label)) |> distinct(URN, .keep_all = TRUE) |>
    transmute(URN, ks2_c, sen_support, sen_ehcp, mobile, girls_school, boys_school, z_l4, age16,
              gor_name = as.character(gor_name), LANAME_m = as.character(LANAME))

  new <- p5 |>
    filter(year_label == NEW_YEAR, MINORGROUP %in% c("Academy", "Maintained school"),
           !is.na(ATT8SCR), ATT8SCR > 0) |>
    mutate(urn_published = as.integer(URN)) |>
    select(-URN) |>
    left_join(ids, by = "urn_published") |>
    mutate(URN = coalesce(URN, urn_published)) |>
    # one row a school: where an old and a new URN both reported, the current one
    arrange(URN, desc(urn_published == URN), desc(TOTPUPS)) |>
    distinct(URN, .keep_all = TRUE) |>
    select(URN, SCHNAME, ATT8SCR, ATT8SCR_FSM6CLA1A, ATT8SCR_NFSM6CLA1A, ATT8SCR_GIRLS, ATT8SCR_BOYS,
           PTFSM6CLA1A, PNUMEAL, ADMPOL_PT, PERCTOT_2term) |>
    inner_join(carry, by = "URN") |>
    # only schools with v2 value added: a school excluded in every model year
    # (it does not mainly take GCSEs) stays out of the provisional year too
    filter(URN %in% V$va$URN) |>
    mutate(PERCTOT = ifelse(PERCTOT_2term > 0, exp(predict(br$model, newdata = pick(everything()))), NA_real_),
           ADMPOL_PT = as.character(ADMPOL_PT))
  message(sprintf("2025/26: %d schools with provisional results and carried-forward intake", nrow(new)))

  out <- bind_rows(lapply(names(GROUPS), function(g) {
    y <- GROUPS[[g]]
    b <- V$coefs |> filter(group == g) |> (\(x) setNames(x$estimate, x$term))()
    sig <- V$summary$sigma[V$summary$group == g]
    ss <- single_sex_term(g)
    dg <- new |> filter(!is.na(.data[[y]]), .data[[y]] > 0, PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0,
                        !is.na(ks2_c), !is.na(z_l4), !is.na(sen_support), !is.na(sen_ehcp), !is.na(mobile))
    xb <- b[["(Intercept)"]] + b[["log(PTFSM6CLA1A)"]] * log(dg$PTFSM6CLA1A) +
      b[["log(PERCTOT)"]] * log(dg$PERCTOT) + b[["log(PNUMEAL)"]] * log(dg$PNUMEAL) +
      b[["ks2_c"]] * dg$ks2_c + b[["z_l4"]] * dg$z_l4 + b[["sen_support"]] * dg$sen_support +
      b[["sen_ehcp"]] * dg$sen_ehcp + b[["mobile"]] * dg$mobile +
      coalesce(b[paste0("ADMPOL_PT", dg$ADMPOL_PT)], 0) +
      b[[paste0(ss, "TRUE")]] * as.numeric(dg[[ss]]) + b[["age16TRUE"]] * as.numeric(dg$age16)
    # England's schools average zero: the year effect the model would give
    u_year <- mean(log(dg[[y]]) - xb)
    pred <- exp(xb + u_year) * exp(0.5 * sig^2)
    tibble(URN = dg$URN, group = g, att8_2526 = dg[[y]], expected_2526 = pred, va_2526 = dg[[y]] - pred,
           year_effect = u_year)
  }))

  # How the provisional year sits against the four-year measure
  agree <- out |> inner_join(V$va |> select(URN, group, va_shrunk), by = c("URN", "group")) |>
    group_by(group) |> summarise(r = cor(va_2526, va_shrunk), n = n(), .groups = "drop")
  res <- list(va = out, agree = agree, bridge_r2 = br$r2, bridge_n = br$n,
              schools = nrow(new), year = NEW_YEAR, release = RELEASE, next_release = NEXT,
              carried = c("prior attainment (KS2)", "SEN support and EHCP rates", "pupil mobility",
                          "the single-sex flag", "whether the school stops at 16"),
              built_at = Sys.time())
  saveRDS(res, here::here("data", "va_2526_provisional.rds"))
  readr::write_csv(out |> select(URN, group, att8_2526, expected_2526, va_2526) |>
                     pivot_wider(names_from = group, values_from = c(att8_2526, expected_2526, va_2526),
                                 names_glue = "{group}_{.value}") |>
                     left_join(V$schools |> select(URN, SCHNAME, LANAME), by = "URN") |>
                     relocate(URN, SCHNAME, LANAME) |>
                     mutate(across(where(is.double), ~round(.x, 2)), status = "PROVISIONAL: to be updated with the revised KS4 release, February 2027"),
                   here::here("data", "va_2526_provisional.csv"))
  print(agree)
  invisible(res)
}

if (sys.nframe() == 0) build_provisional()
