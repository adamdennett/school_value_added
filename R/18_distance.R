# 18_distance.R - Distance to school: the straight-line check
# ---------------------------------------------------------------------------
# Do children in deprived neighbourhoods live further from a secondary school
# than their neighbours, where is that unusual, and does absence go with it?
# A first pass in straight lines, before journey times on the real networks.
#
#   1. Schools: every open state-funded secondary, all-through and middle
#      deemed secondary school in GIAS. GIAS, not the
#      KS4 panel: the panel has no coordinates for some all-through schools,
#      and misses schools too new to have a GCSE cohort.
#   2. Neighbourhoods: 2021 LSOAs, each at the mean of its Output Areas'
#      population-weighted centroids; Census 2021 household deprivation
#      (deprived in two or more dimensions, TS011) and 10-14 year olds (TS007a,
#      the weight). Distance to the nearest school, the mean of the three
#      nearest, and the nearest non-selective, non-faith school.
#   3. In each district and authority: children in the most deprived third of
#      its neighbourhoods against the rest. Rural-urban from ONS RUC 2021.
#   4. Does absence go with it? By authority for all three groups of pupils
#      (DfE publishes absence by disadvantage no lower), and by school for
#      all pupils, with each school's "nearest children": the Output Areas
#      for which it is the nearest school, and how far they live from it.
#   5. Results by group, given v2.1's terms, with and without absence.
#
# Output: data/distance.rds (committed).
# Rscript R/18_distance.R   (first run fetches ONS lookups; ~3 min)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(lme4); library(fixest) })
num <- function(x) suppressWarnings(as.numeric(x))
rd <- function(f) readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, name_repair = "unique_quiet")
BH <- "Brighton and Hove"
SUSSEX_LADS <- c("Brighton and Hove", "Eastbourne", "Hastings", "Lewes", "Rother", "Wealden", "Adur",
                 "Arun", "Chichester", "Crawley", "Horsham", "Mid Sussex", "Worthing")
wm <- function(x, w) sum(x * w) / sum(w)
wcor <- function(x, y, w) if (length(x) > 2 && sd(y) > 0 && sd(x) > 0) cov.wt(cbind(x, y), wt = w, cor = TRUE)$cor[1, 2] else NA_real_

# ---- 1. Schools --------------------------------------------------------------
g <- rd(here::here("data-raw", "gias", "edubasealldata20260610.csv"))
schools <- g |> filter(`EstablishmentStatus (name)` == "Open",
                       `EstablishmentTypeGroup (name)` %in% c("Academies", "Free Schools", "Local authority maintained schools"),
                       `PhaseOfEducation (name)` %in% c("Secondary", "All-through", "Middle deemed secondary")) |>
  transmute(URN = as.integer(URN), name = EstablishmentName, la = `LA (name)`, phase = `PhaseOfEducation (name)`,
            low = num(StatutoryLowAge), high = num(StatutoryHighAge), admissions = `AdmissionsPolicy (name)`,
            religion = `ReligiousCharacter (name)`, urban_rural = `UrbanRural (name)`,
            e = num(Easting), n = num(Northing)) |>
  filter(high >= 16, low <= 13, !is.na(e), e > 0)
schools <- schools |> mutate(
  selective = grepl("^Selective", admissions),
  faith = !is.na(religion) & !religion %in% c("None", "Does not apply", "Not applicable"),
  open_to_all = !selective & !faith,
  place = case_when(startsWith(urban_rural, "Urban: Nearer") ~ "Urban, near a major town or city",
                    startsWith(urban_rural, "Urban: Further") ~ "Urban, further from one",
                    grepl("rural", urban_rural, ignore.case = TRUE) ~ "Rural", TRUE ~ NA_character_))
message(nrow(schools), " schools (", sum(schools$open_to_all), " neither selective nor faith)")

# Successors, so a school with results under an old URN finds itself here
links <- rd(here::here("data-raw", "gias", "links_edubasealldata20260610.csv")) |>
  transmute(URN = as.integer(URN), link = as.integer(LinkURN), type = LinkType) |> filter(grepl("Successor", type))
to_current <- function(u) { for (i in 1:3) { m <- !u %in% schools$URN; j <- match(u[m], links$URN); u[m][!is.na(j)] <- links$link[j[!is.na(j)]] }; u }

# ---- 2. Neighbourhoods -------------------------------------------------------
cache <- here::here("data-raw", "census2021", "ons_custom"); dir.create(cache, showWarnings = FALSE, recursive = TRUE)
ons_table <- function(service, fields, code, file) {
  f <- file.path(cache, file); if (file.exists(f)) return(readr::read_csv(f, show_col_types = FALSE))
  base <- paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/", service, "/FeatureServer/0/query")
  out <- list(); last <- ""
  repeat {
    w <- utils::URLencode(sprintf("%s>'%s'", code, last), reserved = TRUE)
    r <- jsonlite::fromJSON(paste0(base, "?where=", w, "&outFields=", paste(fields, collapse = ","), "&orderByFields=", code,
                                   "&returnGeometry=false&f=json&resultRecordCount=2000"))
    a <- r$features$attributes; if (is.null(a) || !nrow(a)) break
    out[[length(out) + 1]] <- a; last <- a[[code]][nrow(a)]
  }
  x <- bind_rows(out); readr::write_csv(x, f); x
}
ruc <- ons_table("LSOA21_RUC21_EW_LU", c("LSOA21CD", "RUC21NM"), "LSOA21CD", "lsoa_ruc21.csv")
lad <- ons_table("LSOA21_LAD23_EW_LU", c("LSOA21CD", "LAD23NM"), "LSOA21CD", "lsoa_lad23.csv")
utla <- ons_table("LSOA21_UTLA23_EW_LU", c("LSOA21CD", "UTLA23NM"), "LSOA21CD", "lsoa_utla23.csv")

oa <- readRDS(here::here("data", "cache", "oa_pwc_2021.rds"))
lk <- readr::read_csv(file.path(cache, "oa_lsoa_msoa_2021.csv"), show_col_types = FALSE) |>
  transmute(oa21cd = OA21CD, lsoa = LSOA21CD)
oa <- oa |> mutate(easting = num(easting), northing = num(northing)) |> inner_join(lk, by = "oa21cd")
census_col <- function(tb, level, col) {
  x <- readr::read_csv(here::here("data-raw", "census2021", tb, sprintf("census2021-%s-%s.csv", tb, level)), show_col_types = FALSE)
  x |> select(code = `geography code`, all_of(col))
}
dep_cols <- paste0("Household deprivation: Household is deprived in ", c("one", "two", "three", "four"), " dimension", c("", "s", "s", "s"), "; measures: Value")
dep_tot <- "Household deprivation: Total: All households; measures: Value"
dep <- census_col("ts011", "lsoa", c(dep_tot, dep_cols)) |>
  transmute(lsoa = code, dep2 = 100 * rowSums(across(all_of(dep_cols[2:4]))) / .data[[dep_tot]],
            dep1 = 100 * rowSums(across(all_of(dep_cols))) / .data[[dep_tot]])
kids_lsoa <- census_col("ts007a", "lsoa", "Age: Aged 10 to 14 years") |> rename(lsoa = code, kids = 2)
kids_oa <- census_col("ts007a", "oa", "Age: Aged 10 to 14 years") |> rename(oa21cd = code, kids_oa = 2)

ls <- oa |> group_by(lsoa) |> summarise(x = mean(easting), y = mean(northing), .groups = "drop") |>
  inner_join(dep, by = "lsoa") |> inner_join(kids_lsoa, by = "lsoa") |>
  inner_join(ruc |> transmute(lsoa = LSOA21CD, ruc = RUC21NM), by = "lsoa") |>
  inner_join(lad |> transmute(lsoa = LSOA21CD, lad = LAD23NM), by = "lsoa") |>
  inner_join(utla |> transmute(lsoa = LSOA21CD, la = recode(UTLA23NM, "Bristol" = "Bristol, City of", "Herefordshire" = "Herefordshire, County of",
                                                            "Kingston upon Hull" = "Kingston upon Hull, City of")), by = "lsoa") |>
  filter(startsWith(lsoa, "E")) |>
  mutate(urban = startsWith(ruc, "Urban"))
message(nrow(ls), " LSOAs in England")

km_near <- function(px, py, sch, k = 3) nabor::knn(as.matrix(sch[, c("e", "n")]), cbind(px, py), k = k)
nn <- km_near(ls$x, ls$y, schools)
ls$near1 <- nn$nn.dists[, 1] / 1000
ls$near3 <- rowMeans(nn$nn.dists) / 1000
ls$near_open <- km_near(ls$x, ls$y, schools |> filter(open_to_all), 1)$nn.dists[, 1] / 1000
# The same at Output Area level, weighted to the LSOA by its children
oa2 <- oa |> inner_join(kids_oa, by = "oa21cd") |> filter(lsoa %in% ls$lsoa)
nno <- km_near(oa2$easting, oa2$northing, schools, 1)
oa2$near1_oa <- nno$nn.dists[, 1] / 1000; oa2$school <- schools$URN[nno$nn.idx[, 1]]
ls <- ls |> left_join(oa2 |> group_by(lsoa) |> summarise(near1_oa = weighted.mean(near1_oa, pmax(kids_oa, 1)), .groups = "drop"), by = "lsoa")

# ---- 3. Most deprived third against the rest, place by place -----------------
gap_row <- function(z, m = "near1", depv = "dep2") {
  q <- z[[depv]] >= quantile(z[[depv]], 2 / 3); d <- z[[m]]; w <- z$kids
  tibble(n_lsoa = nrow(z), kids = sum(w), dep2 = wm(z$dep2, w), urban_share = 100 * wm(z$urban, w), near_all = wm(d, w),
         near_dep = wm(d[q], w[q]), near_rest = wm(d[!q], w[!q]), gap = near_dep - near_rest, ratio = near_dep / near_rest,
         cor = wcor(z[[depv]], d, w))
}
by_place <- function(col) ls |> group_by(place = .data[[col]]) |> group_modify(~ gap_row(.x)) |> ungroup()
districts <- by_place("lad") |> filter(n_lsoa >= 30) |>
  mutate(kind = case_when(urban_share >= 80 ~ "Mostly urban", urban_share >= 40 ~ "Mixed", TRUE ~ "Mostly rural"),
         rank_gap = rank(-gap, ties.method = "min"), rank_ratio = rank(-ratio, ties.method = "min"), rank_cor = rank(-cor, ties.method = "min"),
         rank_near_dep = rank(-near_dep, ties.method = "min")) |>
  group_by(kind) |> mutate(rank_in_kind = rank(-ratio, ties.method = "min"), n_kind = n()) |> ungroup()
authorities <- by_place("la")

# Does the answer depend on how it is measured?
check_specs <- list(
  "Nearest school (headline)" = list(m = "near1", d = "dep2"),
  "Nearest, measured from Output Areas" = list(m = "near1_oa", d = "dep2"),
  "Mean of the three nearest" = list(m = "near3", d = "dep2"),
  "Nearest neither selective nor faith" = list(m = "near_open", d = "dep2"),
  "Nearest; deprived in one or more dimensions" = list(m = "near1", d = "dep1"))
checks <- bind_rows(lapply(names(check_specs), function(s) {
  sp <- check_specs[[s]]
  all <- ls |> group_by(lad) |> group_modify(~ gap_row(.x, sp$m, sp$d)) |> ungroup() |> filter(n_lsoa >= 30) |> mutate(rank = rank(-ratio, ties.method = "min"))
  all |> filter(lad %in% c(BH, "Hastings", "Blackpool")) |> transmute(spec = s, lad, near_dep, near_rest, ratio, rank, n = nrow(all))
}))

# England: how far, by deprivation and kind of place
ls <- ls |> mutate(dep_dec = ntile(dep2, 10), kind3 = case_when(
  startsWith(ruc, "Urban: Nearer") ~ "Urban, near a major town or city",
  startsWith(ruc, "Urban: Further") ~ "Urban, further from one", TRUE ~ "Rural"))
national <- ls |> group_by(kind3, dep_dec) |> summarise(near = wm(near1, kids), kids = sum(kids), n = n(), .groups = "drop")
england <- gap_row(ls)

# ---- 4a. Authorities: absence for all three groups ---------------------------
ch <- data.table::fread(sat("data", "cache", "ees_absence_characteristics.csv"),
                        select = c("time_period", "geographic_level", "la_name", "education_phase", "breakdown", "enrolments", "sess_possible", "sess_overall"))
names(ch)[1] <- "time_period"
ch <- as_tibble(ch) |> filter(time_period == 202425, education_phase == "State-funded secondary",
                              geographic_level %in% c("Local authority", "National"),
                              breakdown %in% c("FSM eligible in last 6 years", "FSM not eligible in last 6 years", "Total")) |>
  distinct(geographic_level, la_name, breakdown, .keep_all = TRUE) |>
  mutate(across(c(enrolments, sess_possible, sess_overall), num), abs = 100 * sess_overall / sess_possible,
         g = recode(breakdown, "FSM eligible in last 6 years" = "dis", "FSM not eligible in last 6 years" = "non", "Total" = "all"))
la_abs <- ch |> select(geographic_level, la_name, g, abs, enrolments) |>
  pivot_wider(names_from = g, values_from = c(abs, enrolments)) |>
  mutate(share = 100 * enrolments_dis / (enrolments_dis + enrolments_non))
eng_abs <- la_abs |> filter(geographic_level == "National")
la_abs <- la_abs |> filter(geographic_level == "Local authority")
la <- authorities |> inner_join(la_abs, by = c("place" = "la_name"))
message(nrow(la), " of ", nrow(la_abs), " authorities matched; unmatched: ", paste(setdiff(la_abs$la_name, la$place), collapse = "; "))
la <- la |> filter(n_lsoa >= 30) |> identity()
reg_of <- readRDS(here::here("data", "inputs.rds")) |> distinct(LANAME, gor_name) |> mutate(LANAME = as.character(LANAME))
la <- la |> left_join(reg_of, by = c("place" = "LANAME")) |> mutate(london = gor_name %in% "London")
LA_GROUPS <- c(abs_dis = "Disadvantaged", abs_non = "Non-disadvantaged", abs_all = "All pupils")
la_models <- bind_rows(lapply(names(LA_GROUPS), function(y) {
  specs <- list("Distance alone" = "near_dep",
                "Given the share disadvantaged and how urban" = "near_dep + share + urban_share",
                "And without London" = "near_dep + share + urban_share")
  bind_rows(lapply(names(specs), function(s) {
    dat <- if (s == "And without London") la |> filter(!london) else la
    m <- lm(as.formula(paste(y, "~", specs[[s]])), data = dat)
    v <- summary(m)$coefficients["near_dep", ]
    tibble(group = LA_GROUPS[[y]], spec = s, b = v[["Estimate"]], se = v[["Std. Error"]], t = v[["t value"]], n = nrow(dat))
  }))
}))
la_ratio_models <- bind_rows(lapply(names(LA_GROUPS), function(y) {
  m <- lm(as.formula(paste(y, "~ ratio + share + urban_share")), data = la)
  v <- summary(m)$coefficients["ratio", ]
  tibble(group = LA_GROUPS[[y]], b = v[["Estimate"]], se = v[["Std. Error"]], t = v[["t value"]], r = cor(la[[y]], la$ratio), n = nrow(la))
}))

# ---- 4b. Schools: how far their nearest children live, against absence -------
catch <- oa2 |> group_by(URN = school) |>
  summarise(catch_km = weighted.mean(near1_oa, pmax(kids_oa, 1)), catch_kids = sum(kids_oa), .groups = "drop")
nn2 <- nabor::knn(as.matrix(schools[, c("e", "n")]), as.matrix(schools[, c("e", "n")]), k = 2)
schools$next_km <- nn2$nn.dists[, 2] / 1000
schools <- schools |> left_join(catch, by = "URN")

PA <- readRDS(here::here("data", "place_absence.rds"))
frame <- readRDS(here::here("data", "absence_school_frame.rds"))
sch_abs <- PA$abs_schools |> mutate(now = to_current(as.integer(URN))) |>
  inner_join(schools |> select(now = URN, catch_km, catch_kids, next_km, place, selective, la), by = "now") |>
  filter(!is.na(catch_km), catch_kids >= 100, !is.na(place), !selective) |>
  mutate(place = factor(place, c("Urban, near a major town or city", "Urban, further from one", "Rural")), l2 = log2(catch_km))
sch_excess <- bind_rows(
  { m <- feols(abs_excess ~ l2 | la, sch_abs, cluster = ~la); tibble(place = "All schools", b = coef(m)[["l2"]], se = se(m)[["l2"]], n = m$nobs) },
  bind_rows(lapply(levels(sch_abs$place), function(p) {
    dat <- sch_abs |> filter(place == p); m <- feols(abs_excess ~ l2 | la, dat, cluster = ~la)
    tibble(place = p, b = coef(m)[["l2"]], se = se(m)[["l2"]], n = m$nobs)
  })))

# 2025/26, by kind of absence, with the school's own intake
OUT <- c(overall = "Overall absence", illness = "Illness", unauth = "Unauthorised", persistent = "Persistent absence (% of pupils)")
fr <- frame |> mutate(now = to_current(as.integer(URN))) |>
  inner_join(schools |> select(now = URN, catch_km, catch_kids, selective), by = "now") |>
  filter(!is.na(catch_km), catch_kids >= 100, !selective, !is.na(place)) |> mutate(l2 = log2(catch_km), fsm_c = fsm - mean(fsm, na.rm = TRUE))
controls <- "fsm + eal + ks2_c + z_l4 + sen_sup + ehcp + mobile + girls + place + age16"
kinds <- bind_rows(lapply(names(OUT), function(o) {
  m <- feols(as.formula(paste(o, "~ l2 +", controls, "| la")), fr, cluster = ~la)
  m2 <- feols(as.formula(paste(o, "~ l2 * fsm_c + eal + ks2_c + z_l4 + sen_sup + ehcp + mobile + girls + place + age16 | la")), fr, cluster = ~la)
  tibble(outcome = OUT[[o]], b = coef(m)[["l2"]], se = se(m)[["l2"]], n = m$nobs,
         mean = mean(fr[[o]], na.rm = TRUE), b_int = coef(m2)[["l2:fsm_c"]], se_int = se(m2)[["l2:fsm_c"]])
}))
kinds_place <- bind_rows(lapply(names(OUT), function(o) bind_rows(lapply(levels(factor(fr$place)), function(p) {
  m <- feols(as.formula(paste(o, "~ l2 + fsm + eal + ks2_c + z_l4 + sen_sup + ehcp + mobile + girls + age16 | la")), fr |> filter(place == p), cluster = ~la)
  tibble(outcome = OUT[[o]], place = p, b = coef(m)[["l2"]], se = se(m)[["l2"]], n = m$nobs)
}))))

# ---- 5. Results for all three groups, with and without absence ---------------
d <- readRDS(here::here("data", "inputs.rds")) |> mutate(now = to_current(as.integer(urn_published))) |>
  left_join(schools |> select(now = URN, catch_km, catch_kids), by = "now") |> filter(!is.na(catch_km), catch_kids >= 100) |> mutate(l2 = log2(catch_km))
results <- bind_rows(lapply(c("all", "disadvantaged", "non_disadvantaged"), function(g) {
  y <- GROUPS[[g]]; dg <- model_rows(d, g, c("l2"))
  bind_rows(lapply(c(TRUE, FALSE), function(ab) {
    tm <- fixed_for(g); if (!ab) tm <- setdiff(tm, "log(PERCTOT)")
    m <- lmer(as.formula(paste0("log(", y, ") ~ l2 + ", paste(c(tm, RANDOM_V2, "(1 | URN)"), collapse = " + "))), data = dg, REML = FALSE, control = CTRL)
    v <- summary(m)$coefficients["l2", ]
    tibble(group = GROUP_LABELS[[g]], absence_in_model = ab, b = 100 * (exp(v[["Estimate"]]) - 1), se = 100 * v[["Std. Error"]], n = nobs(m))
  }))
}))

# ---- 6. Tables and points for the page ---------------------------------------
COMPARE <- c(BH, "Portsmouth", "Southampton", "Bristol", "Plymouth", "Blackpool", "Newham", "Tower Hamlets", "Hastings")
compare <- districts |> filter(place %in% COMPARE) |> mutate(la_name = recode(place, "Bristol" = "Bristol, City of")) |>
  left_join(la_abs |> select(la_name, abs_dis, abs_non, abs_all, share), by = "la_name")

# What if? Brighton and Hove with a school back on CoMArt's site, and with
# Longhill on the Elm Grove site (coordinates as in the bh-school-system model)
comart <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(-0.099449, 50.823301)), crs = 4326), 27700))
whatif_row <- function(label, sch) {
  z <- ls |> filter(lad == BH); z$near1 <- km_near(z$x, z$y, sch, 1)$nn.dists[, 1] / 1000
  r <- gap_row(z); tibble(scenario = label, near_dep = r$near_dep, near_rest = r$near_rest, gap = r$gap, ratio = r$ratio, near_all = r$near_all,
                          rank = 1L + sum(districts$ratio[districts$place != BH] > r$ratio))
}
LH <- which(schools$name == "Longhill High School")
moved <- schools; moved$e[LH] <- 533118; moved$n[LH] <- 105231
whatif <- bind_rows(
  whatif_row("Today", schools),
  whatif_row("A school back on CoMArt's site", bind_rows(schools, tibble(e = comart[1, "X"], n = comart[1, "Y"]))),
  whatif_row("Longhill moved to the top of Elm Grove", moved))
sussex <- districts |> filter(place %in% SUSSEX_LADS)
dots <- ls |> filter(lad %in% c(BH, "Hastings")) |> group_by(lad) |>
  mutate(dep_third = dep2 >= quantile(dep2, 2 / 3)) |> ungroup() |> select(lad, lsoa, x, y, kids, dep2, near1, dep_third)
dot_schools <- schools |> filter(e > min(dots$x) - 8000, n > min(dots$y) - 8000, e < max(dots$x) + 8000, n < max(dots$y) + 8000) |>
  select(URN, name, la, e, n)

out <- list(built = Sys.Date(), n_schools = nrow(schools), n_lsoa = nrow(ls), n_open = sum(schools$open_to_all),
            gias_date = "June 2026", districts = districts, authorities = la, compare = compare, sussex = sussex,
            england = england, eng_abs = eng_abs, national = national, checks = checks,
            la_models = la_models, la_ratio_models = la_ratio_models, sch_excess = sch_excess, kinds = kinds,
            kinds_place = kinds_place, results = results, dots = dots, dot_schools = dot_schools, whatif = whatif,
            school_km = schools |> filter(!is.na(catch_km)) |> summarise(p10 = quantile(catch_km, .1), p50 = median(catch_km), p90 = quantile(catch_km, .9), n = n()),
            no_children = sum(is.na(schools$catch_km)))
saveRDS(out, here::here("data", "distance.rds"))
message("Saved data/distance.rds")
print(as.data.frame(compare |> transmute(place, near_dep = round(near_dep, 2), near_rest = round(near_rest, 2), ratio = round(ratio, 2), rank_ratio, rank_gap, kind, rank_in_kind, n_kind)))
