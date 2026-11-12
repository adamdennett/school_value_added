# 40_app_data.R - The data bundle for the School Information System v2.0 (Shiny, app/)
# ---------------------------------------------------------------------------
# The Shiny app in app/ is deployed to Posit Connect Cloud from this repository, so it
# cannot read data/ (inputs.rds alone is 50 MB). This writes the slim tables it needs to
# app/data/app_data.rds, all from files the site already builds:
#
#   schools     one row a school: names, places (region, combined or mayoral authority,
#               local authority, district, trust), intake, attendance, Ofsted, who runs it
#               (va_v2.rds via va_wide(), explore_data.R, school_context.rds)
#   years       value added and Attainment 8 year by year, every group: observed,
#               expected against England, and the difference (va_v2.rds school_years,
#               unshrunk), then provisional 2025/26 flagged (va_2526_provisional.rds)
#   four        the four-year figure: shrunk value added with its 95% interval, mean
#               Attainment 8 and mean expected (va_v2.rds va)
#   absence     absence against intake, year by year and over four years (place_absence.rds,
#               R/04: log absence on the v2 intake terms with year and region effects)
#   la_effects  each authority's effect on results, by group (geography.rds, R/12)
#   sim         the simulator's schools and coefficients (as simulator.qmd)
#   twins       the matching characteristics and weights (as twins.qmd)
#   travel      straight-line distance by authority and district (R/18), journeys on
#               real networks in 109 mostly urban authorities (R/27-R/29), climb (R/25)
#   bounds      upper-tier authority boundaries (geography.rds)
#
# Nothing here is pupil-level: every figure is from DfE, ONS, GIAS or Ofsted publications.
# Rscript R/40_app_data.R   (about a minute)
# ---------------------------------------------------------------------------

source(here::here("R", "site_helpers.R"))
source(here::here("R", "explore_data.R"))   # V, P, CTX and w (one row a school, with ca)
suppressPackageStartupMessages(library(sf))

out_dir <- here::here("app", "data"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
I <- readRDS(here::here("data", "inputs.rds"))
lat_in <- I |> filter(!excluded) |> arrange(URN, desc(year_label)) |> distinct(URN, .keep_all = TRUE)

# ---- schools ----------------------------------------------------------------
NOT_TRUST <- c("Maintained by the council", "Single-academy trust or not recorded")
schools <- w |>
  left_join(CTX |> select(URN, run_by_nat), by = "URN") |>
  left_join(lat_in |> select(URN, age16, ADMPOL_PT), by = "URN") |>
  transmute(URN, name = SCHNAME, town = TOWN, la = LANAME, lad = coalesce(lad, LANAME),
            region = gor_name, ca, lat, lon, type, gender = GENDER, religion,
            admissions = as.character(ADMPOL_PT), age16 = as.logical(age16), pupils,
            fsm, eal, ks2_c, sen_support, sen_ehcp, mobile, l4, absence, persistent, illness, unauth,
            susp_rate, gov = gov_nat, run_by = run_by_nat,
            trust = ifelse(is.na(run_by_nat) | run_by_nat %in% NOT_TRUST, NA_character_, run_by_nat),
            ofsted = ofsted_combined, ofsted_latest, latest_year) |>
  mutate(across(c(fsm, eal, ks2_c, sen_support, sen_ehcp, mobile, l4, absence, persistent, illness, unauth, susp_rate),
                ~ round(.x, 2)))

# ---- value added and Attainment 8, year by year and over four years ----------
years <- V$school_years |>
  group_by(URN, group, year = year_label) |>          # two schools later merged can share a year
  summarise(att8 = mean(obs), exp = mean(exp_nat), va = mean(va_nat), .groups = "drop") |>
  mutate(prov = FALSE)
pv <- load_prov()
if (!is.null(pv)) years <- bind_rows(years, pv$va |>
  transmute(URN, group, year = pv$year, att8 = att8_2526, exp = expected_2526, va = va_2526, prov = TRUE))
years <- years |> filter(!is.na(att8)) |> mutate(across(c(att8, exp, va), ~ round(.x, 2)))
four <- V$va |> transmute(URN, group, years, att8 = round(att8, 2), exp = round(base, 2),
                          va = round(va_shrunk, 2), lo = round(va_lo, 2), hi = round(va_hi, 2), sig)

# ---- absence against intake ---------------------------------------------------
absence <- list(
  years = P$abs_school_years |> transmute(URN, year = year_label, obs = round(obs, 2), exp = round(exp, 2),
                                          excess = round(excess, 2)),
  four = P$abs_schools |> transmute(URN, obs = round(abs_obs, 2), exp = round(abs_exp, 2), excess = round(abs_excess, 2)),
  r2 = P$abs_r2, coefs = P$abs_coefs)

# ---- each authority's effect on results ----------------------------------------
G <- readRDS(here::here("data", "geography.rds"))
la_effects <- G$la |> transmute(la = LANAME, group, pct = round(pct, 2), lo = round(lo_pct, 2), hi = round(hi_pct, 2),
                                sig, rank, n_la, region = gor_name)
bounds <- G$bounds |> st_transform(27700) |> st_simplify(dTolerance = 150, preserveTopology = TRUE) |>
  st_transform(4326) |> rename(la = LANAME)

# ---- simulator: as simulator.qmd ------------------------------------------------
res <- V$school_years |> inner_join(lat_in |> select(URN, year_label), by = c("URN", "year_label")) |>
  select(URN, group, obs) |>
  pivot_wider(names_from = group, values_from = obs, names_glue = "a_{group}", values_fn = mean)
sim_schools <- lat_in |>
  transmute(URN, sim_year = year_label, fsm = PTFSM6CLA1A, abs = PERCTOT, eal = PNUMEAL, ks2 = ks2_c,
            l4 = z_l4, sen = sen_support, ehcp = sen_ehcp, mob = mobile, adm = as.character(ADMPOL_PT),
            age16 = as.integer(age16), gs = as.integer(girls_school), bs = as.integer(boys_school)) |>
  inner_join(res, by = "URN") |>
  filter(!is.na(a_all), !is.na(abs), abs > 0, !is.na(fsm), fsm > 0) |>
  mutate(across(where(is.double), ~ round(.x, 3)))
sim <- list(schools = sim_schools,
            coefs = lapply(split(V$coefs, V$coefs$group), function(x) setNames(x$estimate, x$term)),
            term_labels = TERM_LABELS)

# ---- twins: as twins.qmd ----------------------------------------------------------
AC <- readRDS(here::here("data", "absence_causes.rds"))
ADM <- c("SEL" = "Selective", "OTHER NON SEL" = "Non-selective", "NON SEL IN HIGHLY SEL AREA" = "Non-selective, selective area")
td <- AC$local$schools |>
  transmute(URN, fsm, ks2 = ks2_c, eal, wbri, mobile, girls, sen = sen_sup, ehcp, semh, l4z = z_l4, size,
            adm = unname(coalesce(ADM[as.character(ADMPOL_PT)], as.character(ADMPOL_PT))),
            sixth = ifelse(sixth, "Has a sixth form", "11-16"), place = as.character(place),
            faith = ifelse(faith, "Religious character", "None"),
            overall, illness, unauth, persistent, severe) |>
  inner_join(w |> select(URN, att8), by = "URN") |>
  mutate(across(where(is.double), ~ round(.x, 3)))
MNUM <- c("fsm", "ks2", "eal", "wbri", "mobile", "girls", "sen", "ehcp", "semh", "l4z", "size")
MCAT <- c("place", "adm", "sixth", "faith")
# Each characteristic is weighted by the SD of its fitted contribution in a regression of
# the measure on all of them (log absence for absence), mean 1, floor 0.05 (twins.qmd)
contrib <- function(y) {
  dd <- td |> mutate(.y = y) |> filter(!is.na(.y), if_all(all_of(c(MNUM, MCAT)), ~ !is.na(.x)))
  m <- lm(reformulate(c(MNUM, MCAT), ".y"), data = dd)
  list(sd = apply(predict(m, type = "terms"), 2, sd)[c(MNUM, MCAT)], r2 = summary(m)$r.squared)
}
norm_w <- function(s) round(pmax(s / mean(s), 0.05), 3)
fits <- list(att8 = contrib(td$att8), absence = contrib(log(td$overall + 0.1)))
twins <- list(schools = td, num = MNUM, cat = MCAT,
              weights = lapply(fits, function(f) norm_w(f$sd)), r2 = sapply(fits, `[[`, "r2"),
              labels = c(fsm = "Disadvantaged (FSM6) %", ks2 = "KS2 prior attainment", eal = "EAL %",
                         wbri = "White British %", mobile = "Mobility %", girls = "Girls %", sen = "SEN support %",
                         ehcp = "EHC plans %", semh = "SEMH needs %", l4z = "Neighbourhood education",
                         size = "Size (log pupils)", place = "Kind of place", adm = "Admissions",
                         sixth = "Sixth form", faith = "Religious character"))

# ---- travel and distance -----------------------------------------------------------
D <- readRDS(here::here("data", "distance.rds"))
J <- readRDS(here::here("data", "journey_la.rds"))
C <- readRDS(here::here("data", "climb.rds"))
jcols <- intersect(c("la", "gor", "london", "kids_all", "kids_dep", "kids_rest",
                     paste0(rep(c("t_bus", "t_walk", "km1", "p_two", "late", "over30", "up_back", "n30"), each = 2), c("_dep", "_rest"))),
                   names(J$places))
travel <- list(
  dist_la = D$authorities |> transmute(la = place, kids, dep2, urban_share, near_all, near_dep, near_rest, gap, ratio,
                                       abs_all, abs_dis, abs_non, region = gor_name, london),
  dist_lad = D$districts |> transmute(lad = place, kids, dep2, urban_share, near_all, near_dep, near_rest, gap, ratio, kind),
  dist_eng = D$england,
  jny_la = as.data.frame(J$places)[, jcols],
  jny_school = as.data.frame(J$school_frame)[, c("URN", "children", "km_s", "t_bus", "t_walk", "two_share", "bus_share",
                                                  "late", "up_out", "up_back")] |> mutate(URN = as.integer(URN)),
  climb_la = as.data.frame(C$places),
  built = list(distance = D$built, journeys = J$built, climb = C$built, n_jny_la = nrow(J$places)))
travel$jny_school <- travel$jny_school |> mutate(across(-URN, ~ round(.x, 2)))

# ---- the model, for the About page -------------------------------------------------
model <- list(version = V$spec$version, years = V$spec$years, summary = V$summary |> select(group, rows, schools, n_la, r2_marginal, r2_conditional, reliability_4yr, mean_att8),
              labels = V$spec$labels, excluded = nrow(V$excluded), ofsted_as_at = V$ofsted_as_at,
              prov_year = if (is.null(pv)) NA_character_ else pv$year, prov_note = PROV_NOTE,
              ca_notes = notes)

bundle <- list(schools = schools, years = years, four = four, absence = absence, la_effects = la_effects,
               sim = sim, twins = twins, travel = travel, bounds = bounds, model = model,
               built = Sys.time())
saveRDS(bundle, file.path(out_dir, "app_data.rds"), compress = "xz")
message(sprintf("app_data.rds: %d schools, %d school-years, %.1f MB", nrow(schools), nrow(years),
                file.size(file.path(out_dir, "app_data.rds")) / 1024^2))
