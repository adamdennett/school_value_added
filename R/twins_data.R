# twins_data.R - The matching data and weights for Find a twin
# ---------------------------------------------------------------------------
# One row a state-funded secondary school, with the fifteen characteristics the twins are
# matched on and the measures they are compared on, and the matching weights for each
# measure. Shared by twins.qmd (the page) and method.qmd (#sec-twins-method), so the
# methodology describes exactly what the page does. The Shiny app (app/R/mod_twin.R) uses
# the same characteristics and weights through R/40_app_data.R.
#
# Characteristics (from R/08_absence_causes.R, AC$local$schools):
#   January 2026 school census (EES, school level): fsm (% known to be eligible for free
#     school meals), eal, wbri (White British %), girls (%), size (log headcount),
#     place (rural-urban classification in three), faith (religious character, yes or no)
#   January 2026 SEN file (EES): sen (SEN support %), ehcp (EHC plans %), semh (social,
#     emotional and mental health as primary need, %)
#   The Value Added v2.1 inputs, latest year (R/01_inputs.R): ks2 (KS2 prior attainment,
#     centred), l4z (neighbourhood education, standardised), mobile (% joining mid-phase),
#     adm (admissions policy), sixth (sixth form or 11-16)
# Needs site_helpers.R sourced first. Defines AC, V, w, d, MNUM, MCAT, contrib(), FITS, W, W_R2.
# ---------------------------------------------------------------------------

AC <- readRDS(here::here("data", "absence_causes.rds"))
V <- load_va()
L <- AC$local$schools
w <- va_wide(V)
# Each school against similar schools in its own authority, 2025/26
# (the national absence models' school-level residuals, as %)
rs <- AC$resid |> filter(outcome %in% c("overall", "unauth")) |>
  transmute(URN, outcome = paste0("r_", outcome), r = 100 * (exp(r_2526) - 1)) |>
  tidyr::pivot_wider(names_from = outcome, values_from = r)
ADM <- c("SEL" = "Selective", "OTHER NON SEL" = "Non-selective",
         "NON SEL IN HIGHLY SEL AREA" = "Non-selective, selective area")
d <- L |>
  left_join(w |> select(URN, lad, gor_name, att8, att8_dis, att8_non, va_all = va_shrunk_all, va_dis = va_shrunk_disadvantaged,
                        va_non = va_shrunk_non_disadvantaged,
                        l4, run_by, ofsted_latest, lat, lon), by = "URN") |>
  left_join(rs, by = "URN") |>
  left_join(readRDS(here::here("data", "school_context.rds")) |> select(URN, run_by_nat), by = "URN") |>
  mutate(run_by = coalesce(run_by, run_by_nat)) |>
  transmute(URN, n = SCHNAME, la = as.character(la), lad = coalesce(lad, as.character(la)),
            rg = as.character(region),
            fsm, ks2 = ks2_c, eal, wbri, mobile, girls, sen = sen_sup, ehcp, semh, l4z = z_l4, l4,
            pupils = round(exp(size)), size,
            adm = unname(coalesce(ADM[as.character(ADMPOL_PT)], as.character(ADMPOL_PT))),
            sixth = ifelse(sixth, "Has a sixth form", "11-16"),
            place = as.character(place), faith = ifelse(faith, "Religious character", "None"),
            gov = as.character(mat), run_by, ofsted = ofsted_latest,
            overall, illness, unauth, persistent, severe, r_overall, r_unauth,
            att8, att8_dis, att8_non, va_all, va_dis, va_non, lat = round(lat, 4), lon = round(lon, 4)) |>
  mutate(across(where(is.double) & !c(lat, lon), ~ round(.x, 3)))
# ---- matching weights: what matters for each measure ------------------------
# Each characteristic is weighted by how much it moves the measure across
# schools: the standard deviation of its fitted contribution in a regression
# of the measure on all the matching characteristics (log for absence).
# Value added and Attainment 8 take their weights from the Attainment 8
# model, since value added is itself what is left once intake is allowed for.
MNUM <- c("fsm", "ks2", "eal", "wbri", "mobile", "girls", "sen", "ehcp", "semh", "l4z", "size")
MCAT <- c("place", "adm", "sixth", "faith")
contrib <- function(y) {
  dd <- d |> mutate(.y = y) |> filter(!is.na(.y), if_all(all_of(c(MNUM, MCAT)), ~ !is.na(.x)))
  m <- lm(reformulate(c(MNUM, MCAT), ".y"), data = dd)
  list(sd = apply(predict(m, type = "terms"), 2, sd)[c(MNUM, MCAT)], r2 = summary(m)$r.squared)
}
norm_w <- function(s) round(pmax(s / mean(s), 0.05), 3)       # mean 1; nothing quite ignored
FITS <- c(lapply(c(overall = "overall", illness = "illness", unauth = "unauth", persistent = "persistent",
                   severe = "severe"), function(o) contrib(log(d[[o]] + 0.1))),
          list(att8 = contrib(d$att8)))
W <- lapply(FITS, function(f) as.list(norm_w(f$sd)))
W$r_unauth <- W$unauth
W$va_all <- W$va_dis <- W$va_non <- W$att8_dis <- W$att8_non <- W$att8
W$combo <- as.list(norm_w((unlist(W$overall) + unlist(W$att8)) / 2))
W_R2 <- sapply(FITS, `[[`, "r2")
