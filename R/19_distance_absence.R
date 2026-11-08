# 19_distance_absence.R - How firm is the link between distance and absence?
# ---------------------------------------------------------------------------
# R/18_distance.R found that disadvantaged pupils' absence goes with how far an
# authority's deprived neighbourhoods are from a secondary school, in one year,
# on one measure. This tries to break that finding, three independent ways:
#
#   A. Authorities, by pupil group (DfE absence by characteristics, full years
#      2018/19 to 2024/25, no 2019/20). Every year, ten measures of distance,
#      six sets of controls, five samples: a specification curve. Year-specific
#      school networks from GIAS opening and closing dates. Falsification tests
#      on primary absence, which should not depend on the distance to a
#      secondary school if the link is about the journey. Authority fixed
#      effects: does absence move when the school network does?
#      Is it composition? A Gelbach decomposition of what the controls take away, and
#      the same test within each ethnic group (all pupils in that group).
#   B. Within authorities (DfE absence by pupil residence and IDACI decile,
#      2023/24 and 2024/25). Pupils in the same authority, by how deprived their
#      home neighbourhood is, against the distance from those neighbourhoods.
#      Authority effects hold constant anything about the place as a whole.
#      DfE publishes this for all pupils in each decile, not by disadvantage.
#   C. Schools (DfE school-level absence, autumn and spring, six years). Each
#      school's nearest children each year; the interaction with free school
#      meals; school fixed effects, using schools whose nearest children changed
#      as schools opened and closed. All pupils only: DfE publishes no split.
#
# Sources (all public; method.qmd#sec-distance-method documents each step):
#   DfE EES data catalogue, absence by pupil characteristic: e2eb493a-dc95-41f4-96ef-b886c5cd2019
#   DfE EES data catalogue, absence by pupil residence and IDACI: 6dd7f70e-440f-4c0f-b8d1-056678c25301
#   DfE EES, absence in schools in England, school level two-term file (via R/09)
#   DfE EES, Schools, pupils and their characteristics 2024/25: pupils by FSM and ethnicity (spc_pupils_fsm_ethnicity_yrgp.csv)
#   DfE compare school performance census files; school census and SEN, January 2026 (via R/09)
#   DfE Get Information About Schools, all establishments and links, June 2026
#   MHCLG English Indices of Deprivation 2019, File 7 (IDACI scores, 2011 LSOAs)
#   ONS Open Geography: OA 2021 population-weighted centroids, OA to LSOA 2021, LSOA 2011 to 2021,
#     LSOA 2021 to LAD 2023 and UTLA 2023, Rural Urban Classification 2021
#   Census 2021 via nomis: TS006, TS007a, TS011, TS021, TS037, TS045, TS054, TS067
#
# Output: data/distance_absence.rds (committed).
# Rscript R/19_distance_absence.R   (first run fetches what it lacks; ~5 min)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(fixest); library(sandwich) })
setFixest_notes(FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
rd <- function(f) readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, name_repair = "unique_quiet")
BH <- "Brighton and Hove"
wm <- function(x, w) sum(x * w) / sum(w)
GROUP <- c(dis = "Disadvantaged", non = "Non-disadvantaged", all = "All pupils")
YEARS <- c(201819L, 202021L, 202122L, 202223L, 202324L, 202425L)
yl <- function(tp) sprintf("%d/%02d", tp %/% 100L, tp %% 100L)
raw_dir <- here::here("data-raw", "census2021"); ons <- file.path(raw_dir, "ons_custom")
dir.create(ons, showWarnings = FALSE, recursive = TRUE)
iod_dir <- here::here("data-raw", "iod2019"); dir.create(iod_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 0a. Sources -------------------------------------------------------------
census <- function(tb) {
  f <- file.path(raw_dir, tb, sprintf("census2021-%s-lsoa.csv", tb))
  if (!file.exists(f)) {
    z <- file.path(raw_dir, sprintf("census2021-%s.zip", tb))
    if (!file.exists(z)) download.file(sprintf("https://www.nomisweb.co.uk/output/census/2021/census2021-%s.zip", tb), z, mode = "wb", quiet = TRUE)
    unzip(z, exdir = file.path(raw_dir, tb))
  }
  x <- readr::read_csv(f, show_col_types = FALSE); names(x) <- sub("; measures: Value$", "", names(x)); x
}
col1 <- function(x, pat) { n <- grep(pat, names(x), value = TRUE); stopifnot(length(n) == 1); x[[n]] }
ons_paged <- function(service, fields, file) {
  f <- file.path(ons, file); if (file.exists(f)) return(readr::read_csv(f, show_col_types = FALSE))
  base <- paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/", service, "/FeatureServer/0/query")
  out <- list(); off <- 0
  repeat {
    r <- jsonlite::fromJSON(paste0(base, "?where=1%3D1&outFields=", paste(fields, collapse = ","), "&orderByFields=ObjectId&returnGeometry=false&f=json&resultRecordCount=2000&resultOffset=", off))
    a <- r$features$attributes; if (is.null(a) || !nrow(a)) break
    out[[length(out) + 1]] <- a; off <- off + nrow(a)
  }
  x <- bind_rows(out); readr::write_csv(x, f); x
}

# DfE catalogue files: kept in data-raw/ees, taken from the sibling project if it
# has them, otherwise downloaded from the catalogue
ees_cat <- function(file, id) {
  f <- here::here("data-raw", "ees", file)
  if (!file.exists(f)) {
    dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
    s <- sat("data", "cache", file)
    if (file.exists(s)) file.copy(s, f) else download.file(sprintf("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/%s/csv", id), f, mode = "wb", quiet = TRUE)
  }
  f
}
EES_CHAR <- ees_cat("ees_absence_characteristics.csv", "e2eb493a-dc95-41f4-96ef-b886c5cd2019")
EES_RES <- ees_cat("ees_absence_residence_idaci.csv", "6dd7f70e-440f-4c0f-b8d1-056678c25301")

# IDACI 2019: the vintage DfE's absence statistics use for 2021/22 to 2024/25
iod_f <- file.path(iod_dir, "File_7_IoD2019.csv")
if (!file.exists(iod_f)) download.file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv", iod_f, mode = "wb", quiet = TRUE)
iod <- readr::read_csv(iod_f, show_col_types = FALSE)
iod <- tibble(l11 = col1(iod, "^LSOA code"), score = col1(iod, "IDACI\\) Score"), dec = col1(iod, "IDACI\\) Decile"))
cuts <- iod |> group_by(dec) |> summarise(lo = min(score), .groups = "drop") |> arrange(desc(lo))
lk11 <- ons_paged("LSOA11_LSOA21_LAD22_EW_LU_v5", c("ObjectId", "LSOA11CD", "LSOA21CD", "CHGIND"), "lsoa11_lsoa21.csv")
idaci <- lk11 |> filter(startsWith(LSOA21CD, "E")) |> inner_join(iod, by = c("LSOA11CD" = "l11")) |>
  group_by(lsoa = LSOA21CD) |> summarise(idaci = mean(score), .groups = "drop") |>
  mutate(idaci_dec = vapply(idaci, function(s) min(cuts$dec[s >= cuts$lo], 10L), 1))

# ---- 0b. Neighbourhoods ------------------------------------------------------
ruc <- readr::read_csv(file.path(ons, "lsoa_ruc21.csv"), show_col_types = FALSE)
lad <- readr::read_csv(file.path(ons, "lsoa_lad23.csv"), show_col_types = FALSE)
utla <- readr::read_csv(file.path(ons, "lsoa_utla23.csv"), show_col_types = FALSE)
oa <- readRDS(here::here("data", "cache", "oa_pwc_2021.rds")) |> mutate(easting = num(easting), northing = num(northing)) |>
  inner_join(readr::read_csv(file.path(ons, "oa_lsoa_msoa_2021.csv"), show_col_types = FALSE) |> transmute(oa21cd = OA21CD, lsoa = LSOA21CD), by = "oa21cd")

t11 <- census("ts011"); t07 <- census("ts007a"); t67 <- census("ts067"); t37 <- census("ts037")
t21 <- census("ts021"); t54 <- census("ts054"); t45 <- census("ts045"); t06 <- census("ts006")
dep_cols <- paste0("Household deprivation: Household is deprived in ", c("one", "two", "three", "four"), " dimension", c("", "s", "s", "s"))
dep_tot <- "Household deprivation: Total: All households"
cen_ls <- tibble(lsoa = t11$`geography code`, hh = t11[[dep_tot]],
                 dep2 = 100 * rowSums(t11[dep_cols[2:4]]) / hh, dep1 = 100 * rowSums(t11[dep_cols]) / hh) |>
  inner_join(tibble(lsoa = t07$`geography code`, kids = t07$`Age: Aged 10 to 14 years`, kids_pri = t07$`Age: Aged 5 to 9 years`), by = "lsoa") |>
  inner_join(tibble(lsoa = t67$`geography code`, adults = col1(t67, "Total: All usual residents aged 16"), grad_n = col1(t67, "Level 4 qualifications and above")), by = "lsoa") |>
  inner_join(tibble(lsoa = t37$`geography code`, pop = col1(t37, "Total: All usual residents"), bad_n = col1(t37, "General health: Bad health") + col1(t37, "General health: Very bad health")), by = "lsoa") |>
  inner_join(tibble(lsoa = t21$`geography code`, wbri_n = col1(t21, "White: English, Welsh, Scottish, Northern Irish or British"), asian_n = col1(t21, "^Ethnic group: Asian, Asian British or Asian Welsh$"),
                          black_n = col1(t21, "^Ethnic group: Black, Black British, Black Welsh, Caribbean or African$"), mixed_n = col1(t21, "^Ethnic group: Mixed or Multiple ethnic groups$")), by = "lsoa") |>
  inner_join(tibble(lsoa = t54$`geography code`, social_n = col1(t54, "^Tenure of household: Social rented$")), by = "lsoa") |>
  inner_join(tibble(lsoa = t45$`geography code`, nocar_n = col1(t45, "No cars or vans")), by = "lsoa") |>
  inner_join(tibble(lsoa = t06$`geography code`, density = col1(t06, "Persons per square kilometre")), by = "lsoa")

ls <- oa |> group_by(lsoa) |> summarise(x = mean(easting), y = mean(northing), .groups = "drop") |>
  inner_join(cen_ls, by = "lsoa") |>
  inner_join(ruc |> transmute(lsoa = LSOA21CD, ruc = RUC21NM), by = "lsoa") |>
  inner_join(lad |> transmute(lsoa = LSOA21CD, lad = LAD23NM), by = "lsoa") |>
  inner_join(utla |> transmute(lsoa = LSOA21CD, la = recode(UTLA23NM, "Bristol" = "Bristol, City of", "Herefordshire" = "Herefordshire, County of",
                                                            "Kingston upon Hull" = "Kingston upon Hull, City of")), by = "lsoa") |>
  filter(startsWith(lsoa, "E")) |> mutate(urban = startsWith(ruc, "Urban")) |>
  left_join(idaci, by = "lsoa")
message(nrow(ls), " LSOAs; ", sum(is.na(ls$idaci)), " with no IDACI")
ls$idaci[is.na(ls$idaci)] <- median(ls$idaci, na.rm = TRUE)
kids_oa <- readr::read_csv(file.path(raw_dir, "ts007a", "census2021-ts007a-oa.csv"), show_col_types = FALSE)
oa2 <- oa |> filter(lsoa %in% ls$lsoa) |> inner_join(tibble(oa21cd = kids_oa$`geography code`, kids_oa = kids_oa$`Age: Aged 10 to 14 years`), by = "oa21cd")
NAT_THIRD <- quantile(ls$dep2, 2 / 3)

# ---- 0c. The school network as it stood each September -----------------------
g <- rd(here::here("data-raw", "gias", "edubasealldata20260610.csv"))
links <- rd(here::here("data-raw", "gias", "links_edubasealldata20260610.csv")) |> transmute(URN = as.integer(URN), link = as.integer(LinkURN))
gs <- g |> filter(`EstablishmentTypeGroup (name)` %in% c("Academies", "Free Schools", "Local authority maintained schools"),
                  `EstablishmentStatus (name)` != "Proposed to open") |>
  transmute(URN = as.integer(URN), name = EstablishmentName, la = `LA (name)`, phase = `PhaseOfEducation (name)`, status = `EstablishmentStatus (name)`,
            low = num(StatutoryLowAge), high = num(StatutoryHighAge), admissions = `AdmissionsPolicy (name)`, religion = `ReligiousCharacter (name)`,
            urban_rural = `UrbanRural (name)`, open = as.Date(OpenDate, "%d-%m-%Y"), close = as.Date(CloseDate, "%d-%m-%Y"),
            e = num(Easting), n = num(Northing)) |>
  mutate(e = ifelse(e > 0, e, NA), n = ifelse(n > 0, n, NA),
         selective = grepl("^Selective", admissions), faith = !is.na(religion) & !religion %in% c("None", "Does not apply", "Not applicable"),
         open_to_all = !selective & !faith,
         place = case_when(startsWith(urban_rural, "Urban: Nearer") ~ "Urban, near a major town or city",
                           startsWith(urban_rural, "Urban: Further") ~ "Urban, further from one",
                           grepl("rural", urban_rural, ignore.case = TRUE) ~ "Rural", TRUE ~ NA_character_))
# A closed school with no grid reference takes its linked school's: usually the same site
miss <- which(is.na(gs$e)); lk <- links |> inner_join(gs |> filter(!is.na(e)) |> transmute(link = URN, le = e, ln = n), by = "link") |> distinct(URN, .keep_all = TRUE)
j <- match(gs$URN[miss], lk$URN); gs$e[miss] <- lk$le[j]; gs$n[miss] <- lk$ln[j]
sec_phase <- gs$phase %in% c("Secondary", "All-through", "Middle deemed secondary")
prov_grid <- list(no_grid = sum(sec_phase & seq_len(nrow(gs)) %in% miss), filled = sum(sec_phase & seq_len(nrow(gs)) %in% miss & !is.na(gs$e)))

sec_all <- gs |> filter(phase %in% c("Secondary", "All-through", "Middle deemed secondary"), is.na(high) | high >= 16, is.na(low) | low <= 13, !is.na(e))
pri_all <- gs |> filter(phase %in% c("Primary", "All-through", "Middle deemed primary"), !is.na(e))
network <- function(sch, start) {
  d0 <- as.Date(sprintf("%d-09-30", start)); d1 <- as.Date(sprintf("%d-09-01", start))
  sch |> filter(is.na(open) | open <= d0, (status != "Closed" & is.na(close)) | (!is.na(close) & close > d1))
}
km_near <- function(px, py, sch, k = 1) nabor::knn(as.matrix(sch[, c("e", "n")]), cbind(px, py), k = k)

# ---- 0d. Distance each year --------------------------------------------------
per_la <- function(z) {
  q <- z$dep2 >= quantile(z$dep2, 2 / 3); qd <- z$dep1 >= quantile(z$dep1, 2 / 3); q5 <- z$dep2 >= quantile(z$dep2, 0.8); nat <- z$dep2 >= NAT_THIRD; w <- z$kids
  tibble(near_dep = wm(z$near1[q], w[q]), near3_dep = wm(z$near3[q], w[q]), open_dep = wm(z$near_open[q], w[q]), oa_dep = wm(z$near1_oa[q], w[q]),
         dep1_dep = wm(z$near1[qd], w[qd]), q5_dep = wm(z$near1[q5], w[q5]),
         nat_dep = if (sum(nat) >= 5) wm(z$near1[nat], w[nat]) else NA_real_,
         wt_dep = sum(z$near1 * w * z$dep2) / sum(w * z$dep2), idaci_dep = sum(z$near1 * w * z$idaci) / sum(w * z$idaci),
         p2km = 100 * wm(z$near1[q] > 2, w[q]), near_all = wm(z$near1, w), near_rest = wm(z$near1[!q], w[!q]),
         pri_dep = sum(z$near_pri[q] * z$kids_pri[q]) / sum(z$kids_pri[q]))
}
year_dist <- function(tp) {
  start <- tp %/% 100L; sch <- network(sec_all, start); pri <- network(pri_all, start)
  nn <- km_near(ls$x, ls$y, sch, 3)
  z <- ls; z$near1 <- nn$nn.dists[, 1] / 1000; z$near3 <- rowMeans(nn$nn.dists) / 1000
  z$near_open <- km_near(ls$x, ls$y, sch |> filter(open_to_all))$nn.dists[, 1] / 1000
  z$near_pri <- km_near(ls$x, ls$y, pri)$nn.dists[, 1] / 1000
  no <- km_near(oa2$easting, oa2$northing, sch); o <- oa2; o$d <- no$nn.dists[, 1] / 1000; o$school <- sch$URN[no$nn.idx[, 1]]
  z <- z |> left_join(o |> group_by(lsoa) |> summarise(near1_oa = weighted.mean(d, pmax(kids_oa, 1)), .groups = "drop"), by = "lsoa")
  catch <- o |> group_by(URN = school) |> summarise(catch_km = weighted.mean(d, pmax(kids_oa, 1)), catch_kids = sum(kids_oa), .groups = "drop") |> mutate(tp = tp)
  message(yl(tp), ": ", nrow(sch), " secondaries, ", nrow(pri), " primaries")
  list(z = z, catch = catch, n_sec = nrow(sch), la = z |> group_by(la) |> group_modify(~ per_la(.x)) |> ungroup() |> mutate(tp = tp))
}
ALL_TP <- c(201819L, 202021L, 202122L, 202223L, 202324L, 202425L, 202526L)
yd <- setNames(lapply(ALL_TP, year_dist), ALL_TP)
network_n <- tibble(tp = ALL_TP, year = yl(ALL_TP), schools = vapply(yd, function(v) v$n_sec, 1L))
dist_la <- bind_rows(lapply(yd[as.character(YEARS)], function(v) v$la)) |> mutate(log_dep = log2(near_dep))

# ---- 0e. Authority characteristics --------------------------------------------
pl <- ls |> group_by(la) |> summarise(n_lsoa = n(), urban_share = 100 * wm(urban, kids), graduates = 100 * sum(grad_n) / sum(adults), bad_health = 100 * sum(bad_n) / sum(pop),
                                      white_british = 100 * sum(wbri_n) / sum(pop), social_rent = 100 * sum(social_n) / sum(hh), no_car = 100 * sum(nocar_n) / sum(hh),
                                      deprived = 100 * sum(dep2 * hh / 100) / sum(hh), log_density = wm(log(pmax(density, 1)), kids), x = wm(x, kids), y = wm(y, kids), .groups = "drop") |>
  filter(n_lsoa >= 30)
reg_of <- readRDS(here::here("data", "inputs.rds")) |> distinct(LANAME, gor_name) |> filter(!is.na(gor_name)) |>
  mutate(LANAME = as.character(LANAME)) |> distinct(LANAME, .keep_all = TRUE)
pl <- pl |> left_join(reg_of, by = c("la" = "LANAME")) |> mutate(london = gor_name %in% "London")
frame <- readRDS(here::here("data", "absence_school_frame.rds"))
pup <- frame |> mutate(w = if (median(size, na.rm = TRUE) < 20) exp(size) else size) |> group_by(la) |>
  summarise(across(c(sen_sup, ehcp, ks2_c, eal, wbri, mobile), ~ weighted.mean(.x, w, na.rm = TRUE)), .groups = "drop")
pl <- pl |> left_join(pup, by = "la")
ll <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(pl, coords = c("x", "y"), crs = 27700), 4326)); pl$lon <- ll[, 1]; pl$lat <- ll[, 2]

# ---- A1. Absence by pupil group ----------------------------------------------
ch <- data.table::fread(EES_CHAR,
                        select = c("time_period", "geographic_level", "la_name", "education_phase", "breakdown", "enrolments", "sess_possible", "sess_overall", "sess_unauthorised", "sess_auth_illness", "enrolments_pa_10_exact"))
ch <- as_tibble(ch) |> filter(time_period %in% YEARS, education_phase %in% c("State-funded secondary", "State-funded primary"), geographic_level == "Local authority",
                              breakdown %in% c("FSM eligible in last 6 years", "FSM not eligible in last 6 years", "Total")) |>
  distinct(time_period, la_name, education_phase, breakdown, .keep_all = TRUE) |>
  mutate(across(c(enrolments, sess_possible, sess_overall, sess_unauthorised, sess_auth_illness, enrolments_pa_10_exact), num),
         group = recode(breakdown, "FSM eligible in last 6 years" = "dis", "FSM not eligible in last 6 years" = "non", "Total" = "all"),
         phase = ifelse(education_phase == "State-funded secondary", "sec", "pri"), tp = as.integer(time_period), la = la_name,
         overall = 100 * sess_overall / sess_possible, illness = 100 * sess_auth_illness / sess_possible,
         unauth = 100 * sess_unauthorised / sess_possible, persistent = 100 * enrolments_pa_10_exact / enrolments)
shr <- ch |> select(tp, la, phase, group, enrolments) |> pivot_wider(names_from = group, values_from = enrolments, names_prefix = "n_") |>
  mutate(share = 100 * n_dis / (n_dis + n_non)) |> select(tp, la, phase, share, n_all)
A <- ch |> select(tp, la, phase, group, overall, illness, unauth, persistent) |>
  left_join(shr, by = c("tp", "la", "phase")) |> inner_join(pl, by = "la") |> inner_join(dist_la, by = c("la", "tp")) |>
  filter(!is.na(overall))
pri_abs <- A |> filter(phase == "pri") |> transmute(tp, la, group, pri_overall = overall)
A <- A |> left_join(pri_abs, by = c("tp", "la", "group"))
n_la_year <- A |> filter(phase == "sec", group == "dis") |> count(tp, name = "authorities")
message("Authorities by year: ", paste(n_la_year$authorities, collapse = ", "))

# ---- A2. The specification curve ----------------------------------------------
MEASURES <- c(near_dep = "Nearest school (headline)", log_dep = "Nearest school, logged", near3_dep = "Mean of the three nearest",
              open_dep = "Nearest neither selective nor faith", oa_dep = "Nearest, from Output Areas", dep1_dep = "Deprived in one or more dimensions",
              q5_dep = "Most deprived fifth", nat_dep = "England's most deprived third", wt_dep = "Weighted by household deprivation",
              idaci_dep = "Weighted by child poverty (IDACI)", p2km = "Share of children over 2 km")
CENSUS <- c("graduates", "bad_health", "white_british", "social_rent", "no_car", "deprived", "log_density")
PUPILS <- c("sen_sup", "ehcp", "ks2_c", "eal", "wbri", "mobile")
CONTROLS <- list("None" = character(0),
                 "Share disadvantaged, how urban" = c("share", "urban_share"),
                 "And region" = c("share", "urban_share", "gor_name"),
                 "And Census place" = c("share", "urban_share", "gor_name", CENSUS),
                 "And pupils" = c("share", "urban_share", "gor_name", CENSUS, PUPILS),
                 "And primary absence" = c("share", "urban_share", "gor_name", CENSUS, PUPILS, "pri_overall"))
SAMPLES <- list("All authorities" = function(d) d, "Without London" = function(d) filter(d, !london),
                "Mostly urban" = function(d) filter(d, urban_share >= 80), "Mostly urban, without London" = function(d) filter(d, urban_share >= 80, !london),
                "Less urban" = function(d) filter(d, urban_share < 80))
sds <- A |> filter(phase == "sec", group == "all") |> group_by(tp) |> summarise(across(all_of(names(MEASURES)), ~ sd(.x, na.rm = TRUE)), .groups = "drop")
fit1 <- function(dat, y, x, ctrl) {
  v <- c(x, ctrl); dat <- dat[stats::complete.cases(dat[, c(y, v)]), , drop = FALSE]
  if (nrow(dat) < length(v) + 8) return(NULL)
  m <- lm(reformulate(v, y), data = dat)
  if (is.na(coef(m)[[x]])) return(NULL)
  V <- tryCatch(vcovHC(m, "HC3"), error = function(e) NULL)
  s <- if (!is.null(V)) sqrt(V[x, x]) else NA_real_
  if (!is.finite(s)) s <- sqrt(vcovHC(m, "HC1")[x, x])
  tibble(b = coef(m)[[x]], se = s, n = nrow(dat), r2 = summary(m)$r.squared)
}
curve <- list()
for (ph in c("sec", "pri")) for (gr in names(GROUP)) for (tp0 in YEARS) {
  d0 <- A |> filter(phase == ph, group == gr, tp == tp0)
  for (ms in names(MEASURES)) for (ct in names(CONTROLS)) for (sa in names(SAMPLES)) {
    if (ph == "pri" && ct == "And primary absence") next
    r <- fit1(SAMPLES[[sa]](d0), "overall", ms, CONTROLS[[ct]])
    if (!is.null(r)) curve[[length(curve) + 1]] <- r |> mutate(phase = ph, group = gr, tp = tp0, measure = ms, control = ct, sample = sa)
  }
}
curve <- bind_rows(curve) |> left_join(sds |> pivot_longer(-tp, names_to = "measure", values_to = "sd_x"), by = c("tp", "measure")) |>
  mutate(b_sd = b * sd_x, se_sd = se * sd_x, t = b / se)
message(nrow(curve), " specifications")

# ---- A3. Slope year by year, per km, for all three groups ----------------------
by_year <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(YEARS, function(tp0) bind_rows(lapply(c("Share disadvantaged, how urban", "And region", "And pupils"), function(ct) {
  bind_rows(lapply(c("All authorities", "Without London"), function(sa) {
    r <- fit1(SAMPLES[[sa]](A |> filter(phase == "sec", group == gr, tp == tp0)), "overall", "near_dep", CONTROLS[[ct]])
    r |> mutate(group = gr, tp = tp0, control = ct, sample = sa)
  }))
}))))))

# ---- A4. Pooled, group difference, secondary against primary -------------------
pool <- function(dat, y, ctrl = c("share", "urban_share"), fe = "tp", cl = "la") {
  m <- feols(as.formula(paste0(y, " ~ near_dep", if (length(ctrl)) paste0(" + ", paste(ctrl, collapse = " + ")), " | ", fe)), dat, cluster = as.formula(paste0("~", cl)))
  tibble(b = coef(m)[["near_dep"]], se = se(m)[["near_dep"]], n = m$nobs, las = n_distinct(dat$la))
}
wide <- A |> select(tp, la, phase, group, overall) |> pivot_wider(names_from = c(group, phase), values_from = overall) |>
  inner_join(A |> filter(phase == "sec", group == "all") |> select(tp, la, share, urban_share, gor_name, london, near_dep, near_rest, pri_dep, all_of(CENSUS), all_of(PUPILS)), by = c("tp", "la")) |>
  mutate(gap_sec = dis_sec - non_sec, gap_pri = dis_pri - non_pri, step_dis = dis_sec - dis_pri, step_non = non_sec - non_pri, step_all = all_sec - all_pri, step_gap = step_dis - step_non)
OUT <- c(dis_sec = "Secondary: disadvantaged", non_sec = "Secondary: non-disadvantaged", all_sec = "Secondary: all pupils", gap_sec = "Secondary: disadvantaged minus non-disadvantaged",
         dis_pri = "Primary: disadvantaged", non_pri = "Primary: non-disadvantaged", all_pri = "Primary: all pupils",
         step_dis = "Secondary minus primary: disadvantaged", step_non = "Secondary minus primary: non-disadvantaged", step_all = "Secondary minus primary: all pupils",
         step_gap = "Secondary minus primary: disadvantaged minus non-disadvantaged")
CT2 <- list("Share, urban" = c("share", "urban_share"), "And region" = c("share", "urban_share", "gor_name"), "And Census place, pupils" = c("share", "urban_share", "gor_name", CENSUS, PUPILS))
pooled <- bind_rows(lapply(names(OUT), function(y) bind_rows(lapply(names(CT2), function(ct) bind_rows(
  pool(wide, y, CT2[[ct]]) |> mutate(sample = "All authorities"), pool(wide |> filter(!london), y, CT2[[ct]]) |> mutate(sample = "Without London")) |>
  mutate(out = y, label = OUT[[y]], control = ct)))))
# Does the distance to a primary school do the work in primary?
pri_own <- bind_rows(lapply(c("dis_pri", "non_pri", "all_pri"), function(y) {
  m <- feols(as.formula(paste0(y, " ~ near_dep + pri_dep + share + urban_share | tp")), wide, cluster = ~la)
  tibble(out = y, label = OUT[[y]], b_sec = coef(m)[["near_dep"]], se_sec = se(m)[["near_dep"]], b_pri = coef(m)[["pri_dep"]], se_pri = se(m)[["pri_dep"]], n = m$nobs)
}))
# By kind of absence
kinds_la <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(c("overall", "illness", "unauth", "persistent"), function(y) bind_rows(lapply(names(SAMPLES)[1:2], function(sa) {
  pool(SAMPLES[[sa]](A |> filter(phase == "sec", group == gr)), y) |> mutate(group = gr, outcome = y, sample = sa,
       mean = mean(A[[y]][A$phase == "sec" & A$group == gr], na.rm = TRUE))
}))))))

# ---- A5. Does it survive? Spatial errors, outliers, form, a horse race ---------
LAST <- 202425L
last <- function(gr) A |> filter(phase == "sec", group == gr, tp == LAST)
checks_la <- bind_rows(lapply(names(GROUP), function(gr) {
  d <- last(gr); f <- overall ~ near_dep + share + urban_share
  base <- lm(f, d); bb <- coef(base)[["near_dep"]]
  loo <- vapply(seq_len(nrow(d)), function(i) coef(lm(f, d[-i, ]))[["near_dep"]], 0)
  lev <- order(-cooks.distance(base))
  rl <- MASS::rlm(f, data = d, method = "MM", maxit = 200); rs <- summary(rl)$coefficients["near_dep", ]
  wt <- lm(f, d, weights = n_all); wv <- sqrt(vcovHC(wt, "HC3")["near_dep", "near_dep"])
  cz <- feols(overall ~ near_dep + share + urban_share, d, vcov = vcov_conley(lat = ~lat, lon = ~lon, cutoff = 100))
  dd <- d[-lev[1:5], ]; b5 <- lm(f, dd); v5 <- sqrt(vcovHC(b5, "HC3")["near_dep", "near_dep"])
  nb <- lm(f, d |> filter(la != BH)); vb <- sqrt(vcovHC(nb, "HC3")["near_dep", "near_dep"])
  bind_rows(tibble(check = "Baseline, 2024/25 (robust errors)", b = bb, se = sqrt(vcovHC(base, "HC3")["near_dep", "near_dep"])),
            tibble(check = "Errors allowing for neighbouring authorities (within 100 km)", b = coef(cz)[["near_dep"]], se = se(cz)[["near_dep"]]),
            tibble(check = "Weighted by pupils", b = coef(wt)[["near_dep"]], se = wv),
            tibble(check = "Robust regression (down-weights outliers)", b = rs[["Value"]], se = rs[["Std. Error"]]),
            tibble(check = "Without the five most influential authorities", b = coef(b5)[["near_dep"]], se = v5),
            tibble(check = "Without Brighton and Hove", b = coef(nb)[["near_dep"]], se = vb)) |>
    mutate(group = gr, loo_min = min(loo), loo_max = max(loo), top5 = paste(d$la[lev[1:5]], collapse = "; "),
           spearman = cor(d$overall, d$near_dep, method = "spearman"), pearson = cor(d$overall, d$near_dep))
}))
race <- bind_rows(lapply(names(GROUP), function(gr) {
  bind_rows(lapply(names(SAMPLES)[1:2], function(sa) {
    m <- feols(overall ~ near_dep + near_rest + share + urban_share | tp, SAMPLES[[sa]](A |> filter(phase == "sec", group == gr)), cluster = ~la)
    tibble(group = gr, sample = sa, b_dep = coef(m)[["near_dep"]], se_dep = se(m)[["near_dep"]], b_rest = coef(m)[["near_rest"]], se_rest = se(m)[["near_rest"]], cor = cor(A$near_dep, A$near_rest, use = "complete.obs"))
  }))
}))
# Shape, urban and rural: pooled slope in bands of distance, and by how urban
bands <- A |> filter(phase == "sec") |> mutate(band = cut(near_dep, c(0, 1, 1.5, 2, 3, Inf), labels = c("Under 1 km", "1 to 1.5 km", "1.5 to 2 km", "2 to 3 km", "Over 3 km")))
shape <- bind_rows(lapply(names(GROUP), function(gr) {
  d <- bands |> filter(group == gr); m <- feols(overall ~ i(band, ref = "Under 1 km") + share + urban_share | tp, d, cluster = ~la)
  cf <- coef(m); s <- se(m); k <- grep("^band::", names(cf))
  bind_rows(tibble(band = "Under 1 km", b = 0, se = 0), tibble(band = sub("^band::", "", names(cf)[k]), b = cf[k], se = s[k])) |>
    mutate(group = gr, las = vapply(band, function(bn) n_distinct(d$la[d$band == bn & d$tp == LAST]), 1L))
}))
urb_sample <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(c("Mostly urban", "Mostly urban, without London", "Less urban"), function(sa) {
  bind_rows(lapply(names(CT2)[1:2], function(ct) pool(SAMPLES[[sa]](A |> filter(phase == "sec", group == gr)), "overall", CT2[[ct]]) |> mutate(group = gr, sample = sa, control = ct)))
}))))
inter <- bind_rows(lapply(names(GROUP), function(gr) {
  d <- A |> filter(phase == "sec", group == gr) |> mutate(urb_c = (urban_share - 80) / 10)
  m <- feols(overall ~ near_dep * urb_c + share | tp, d, cluster = ~la)
  tibble(group = gr, b_near = coef(m)[["near_dep"]], se_near = se(m)[["near_dep"]], b_int = coef(m)[["near_dep:urb_c"]], se_int = se(m)[["near_dep:urb_c"]])
}))
car <- bind_rows(lapply(names(GROUP), function(gr) {
  d <- A |> filter(phase == "sec", group == gr) |> mutate(car_c = no_car - mean(no_car))
  m <- feols(overall ~ near_dep * car_c + share + urban_share | tp, d, cluster = ~la)
  tibble(group = gr, b_near = coef(m)[["near_dep"]], se_near = se(m)[["near_dep"]], b_int = coef(m)[["near_dep:car_c"]], se_int = se(m)[["near_dep:car_c"]])
}))

# ---- A5b0. The ethnic mix of each group of pupils --------------------------------
# DfE, Schools, pupils and their characteristics: pupils by ethnicity and free school
# meal eligibility, by authority and phase. Eligible now, not in the last six years,
# so a close stand-in for disadvantaged pupils. No figures before the file starts.
mix_file <- here::here("data-raw", "ees", "fsm_ethnicity_la.csv")
if (!file.exists(mix_file)) {
  if (!file.exists(here::here("data-raw", "ees", "spc_all", "spc_2024-25.zip"))) stop("Download the Schools, pupils and their characteristics 2024/25 data files to data-raw/ees/spc_all/spc_2024-25.zip")
  z <- readr::read_csv(unz(here::here("data-raw", "ees", "spc_all", "spc_2024-25.zip"), "data/spc_pupils_fsm_ethnicity_yrgp.csv"),
                       col_types = readr::cols(.default = "c"), progress = FALSE, name_repair = "unique_quiet")
  names(z)[1] <- "time_period"
  z <- z |> filter(geographic_level == "Local authority", phase_type_grouping %in% c("State-funded secondary", "State-funded primary"), characteristic_group %in% c("Ethnicity Major", "Ethnicity Minor"),
                   !grepl("^Total", fsm_eligibility)) |> select(time_period, la_name, phase_type_grouping, characteristic_group, characteristic, fsm_eligibility, number_of_pupils)
  readr::write_csv(z, mix_file)
}
mx <- readr::read_csv(mix_file, col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
  mutate(n = num(number_of_pupils), tp = as.integer(time_period), la = la_name, phase = ifelse(phase_type_grouping == "State-funded secondary", "sec", "pri"),
         group = ifelse(grepl("Known to be eligible", fsm_eligibility), "dis", "non"))
mix_counts <- mx |> filter(characteristic != "Unclassified") |>
  mutate(k = case_when(characteristic == "White - White British" ~ "eth_wb",
                       grepl("Asian", characteristic) & !grepl("^Mixed", characteristic) ~ "eth_asian",
                       grepl("Black", characteristic) & !grepl("^Mixed", characteristic) ~ "eth_black",
                       grepl("^Mixed", characteristic) ~ "eth_mixed",
                       TRUE ~ "other")) |>
  group_by(tp, la, phase, group, k) |> summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
denom <- mix_counts |> group_by(tp, la, phase, group) |> summarise(tot = sum(n), .groups = "drop")
mix_wide <- mix_counts |> filter(k != "other") |> inner_join(denom, by = c("tp", "la", "phase", "group")) |> mutate(s = 100 * n / tot) |>
  select(tp, la, phase, group, k, s) |> pivot_wider(names_from = k, values_from = s, values_fill = 0)
# All pupils in the same place
mix_all <- mix_counts |> group_by(tp, la, phase, k) |> summarise(n = sum(n), .groups = "drop") |> group_by(tp, la, phase) |> mutate(tot = sum(n)) |> ungroup() |>
  filter(k != "other") |> mutate(s = 100 * n / tot, group = "all") |> select(tp, la, phase, group, k, s) |> pivot_wider(names_from = k, values_from = s, values_fill = 0)
A <- A |> left_join(bind_rows(mix_wide, mix_all), by = c("tp", "la", "phase", "group"))
ETHV <- c("eth_wb", "eth_asian", "eth_black", "eth_mixed")
mix_years <- A |> filter(phase == "sec", group == "dis") |> group_by(tp) |> summarise(authorities = n(), with_mix = sum(!is.na(eth_wb)), wb = median(eth_wb, na.rm = TRUE), .groups = "drop")
mix_cor <- A |> filter(phase == "sec", group == "dis", tp == 202425L) |> summarise(cor_wb_km = cor(eth_wb, near_dep, use = "complete.obs"), cor_wb_pupils = cor(eth_wb, wbri, use = "complete.obs"),
                                                                                  r2 = summary(lm(overall ~ eth_wb, pick(everything())))$r.squared)
BH_mix <- A |> filter(phase == "sec", la == BH, tp == 202425L) |> select(group, all_of(ETHV))
EN_mix <- A |> filter(phase == "sec", tp == 202425L, group == "dis") |> summarise(wb_med = median(eth_wb, na.rm = TRUE))
# The slope given the mix of the same group of pupils, same years throughout
group_mix <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(c("All authorities", "Without London", "Mostly urban", "Mostly urban, without London", "Less urban"), function(sa) {
  d <- SAMPLES[[sa]](A |> filter(phase == "sec", group == gr, !is.na(eth_wb)))
  sets <- list("Share and urbanity" = c("share", "urban_share"), "And White British share of this group" = c("share", "urban_share", "eth_wb"),
               "And full ethnic mix of this group" = c("share", "urban_share", ETHV), "And region" = c("share", "urban_share", "gor_name"),
               "And ethnic mix and region" = c("share", "urban_share", ETHV, "gor_name"), "And ethnic mix, region and density" = c("share", "urban_share", ETHV, "gor_name", "log_density"),
               "And every control" = c("share", "urban_share", ETHV, "gor_name", CENSUS, PUPILS))
  bind_rows(lapply(names(sets), function(s) pool(d, "overall", sets[[s]]) |> mutate(group = gr, sample = sa, set = s)))
}))))
group_mix_kinds <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(c("overall", "illness", "unauth", "persistent"), function(y) bind_rows(lapply(c("All authorities", "Mostly urban, without London"), function(sa) {
  d <- SAMPLES[[sa]](A |> filter(phase == "sec", group == gr, !is.na(eth_wb)))
  pool(d, y, c("share", "urban_share", ETHV, "gor_name")) |> mutate(group = gr, outcome = y, sample = sa)
}))))))
# Primary, given its own mix; and the step from primary to secondary, given the secondary mix
group_mix_pri <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(c("All authorities", "Mostly urban, without London"), function(sa) {
  dp <- SAMPLES[[sa]](A |> filter(phase == "pri", group == gr, !is.na(eth_wb)))
  ds <- SAMPLES[[sa]](A |> filter(phase == "sec", group == gr, !is.na(eth_wb))) |> mutate(step = overall - pri_overall)
  bind_rows(pool(dp, "overall", c("share", "urban_share", ETHV)) |> mutate(out = "Primary"), pool(ds, "step", c("share", "urban_share", ETHV)) |> mutate(out = "Secondary minus primary"),
            pool(dp, "overall", c("share", "urban_share", ETHV, "gor_name")) |> mutate(out = "Primary, and region"), pool(ds, "step", c("share", "urban_share", ETHV, "gor_name")) |> mutate(out = "Secondary minus primary, and region")) |> mutate(group = gr, sample = sa)
}))))

# Brighton and Hove against what its mix and setting would predict
bh_resid <- { d <- A |> filter(phase == "sec", group == "dis", tp == 202425L, !is.na(eth_wb))
  m0 <- lm(reformulate(c("share", "urban_share", ETHV, "gor_name"), "overall"), d); m1 <- lm(reformulate(c("near_dep", "share", "urban_share", ETHV, "gor_name"), "overall"), d)
  e0 <- resid(m0); e1 <- resid(m1); i <- which(d$la == BH)
  list(actual = d$overall[i], resid_no_km = e0[[i]], resid_km = e1[[i]], rank_no_km = rank(-e0)[i], rank_km = rank(-e1)[i], n = nrow(d), wb = d$eth_wb[i], wb_med = median(d$eth_wb), near_part = coef(m1)[["near_dep"]] * (d$near_dep[i] - mean(d$near_dep))) }
# ---- A5b. Which controls take the slope away? ----------------------------------
ONE <- c(gor_name = "Region", graduates = "Graduates (Census)", bad_health = "Bad health (Census)", white_british = "White British, residents (Census)",
         social_rent = "Social renting (Census)", no_car = "No car (Census)", deprived = "Household deprivation (Census)", log_density = "Population density (Census)",
         sen_sup = "SEN support (pupils)", ehcp = "EHC plans (pupils)", ks2_c = "Prior attainment (pupils)", eal = "English as an additional language (pupils)",
         wbri = "White British (pupils)", mobile = "Mobility (pupils)")
SAMP4 <- SAMPLES[c("All authorities", "Without London", "Mostly urban", "Mostly urban, without London")]
one_at_a_time <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(names(SAMP4), function(sa) {
  d <- SAMP4[[sa]](A |> filter(phase == "sec", group == gr))
  bind_rows(
    pool(d, "overall") |> mutate(added = "None beyond share and urbanity"),
    bind_rows(lapply(names(ONE), function(v) pool(d, "overall", c("share", "urban_share", v)) |> mutate(added = ONE[[v]]))),
    pool(d, "overall", c("share", "urban_share", "gor_name", CENSUS)) |> mutate(added = "All Census and region"),
    pool(d, "overall", c("share", "urban_share", "gor_name", CENSUS, PUPILS)) |> mutate(added = "All Census, pupils and region"),
    pool(d, "overall", c("share", "urban_share", "gor_name", setdiff(CENSUS, "log_density"), PUPILS)) |> mutate(added = "All except density"),
    pool(d, "overall", c("share", "urban_share", "gor_name", setdiff(CENSUS, "white_british"), setdiff(PUPILS, "wbri"))) |> mutate(added = "All except ethnicity"),
    pool(d, "overall", c("share", "urban_share", "gor_name", setdiff(CENSUS, c("white_british", "log_density")), setdiff(PUPILS, "wbri"))) |> mutate(added = "All except density and ethnicity")) |>
    mutate(group = gr, sample = sa)
}))))
# How much of distance is anything else? R-squared of distance on the controls
d25s <- A |> filter(phase == "sec", group == "all", tp == 202425L)
dist_on <- function(d, v) summary(lm(reformulate(v, "near_dep"), d))$r.squared
collinear <- bind_rows(lapply(names(SAMP4), function(sa) { d <- SAMP4[[sa]](d25s); tibble(sample = sa, n = nrow(d),
  r2_density = dist_on(d, "log_density"), r2_share_urban = dist_on(d, c("share", "urban_share")), r2_white = dist_on(d, "white_british"),
  r2_all = dist_on(d, c("share", "urban_share", "gor_name", CENSUS, PUPILS)),
  cor_density = cor(d$near_dep, d$log_density), cor_white = cor(d$near_dep, d$white_british), cor_nocar = cor(d$near_dep, d$no_car)) }))
# Urban authorities without the most influential, and without Brighton and Hove
urban_checks <- bind_rows(lapply(names(GROUP), function(gr) {
  d <- A |> filter(phase == "sec", group == gr, urban_share >= 80, !london)
  f <- overall ~ near_dep + share + urban_share
  dl <- d |> filter(tp == 202425L); base <- lm(f, dl); top <- dl$la[order(-cooks.distance(base))[1:5]]
  bind_rows(pool(d, "overall") |> mutate(check = "Mostly urban, without London"),
            pool(d |> filter(la != BH), "overall") |> mutate(check = "And without Brighton and Hove"),
            pool(d |> filter(!la %in% top), "overall") |> mutate(check = "And without the five most influential"),
            pool(d, "overall", c("share", "urban_share", "gor_name", "log_density")) |> mutate(check = "Given region and density"),
            pool(d, "overall", c("share", "urban_share", "gor_name", "white_british", "wbri")) |> mutate(check = "Given region and ethnicity"),
            pool(d, "overall", c("share", "urban_share", "gor_name", "log_density", "white_british", "wbri")) |> mutate(check = "Given region, density and ethnicity")) |>
    mutate(group = gr, top5 = paste(top, collapse = "; "))
}))
urban_year <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(YEARS, function(t0) {
  bind_rows(lapply(c("Mostly urban", "Mostly urban, without London", "Less urban"), function(sa) fit1(SAMPLES[[sa]](A |> filter(phase == "sec", group == gr, tp == t0)), "overall", "near_dep", c("share", "urban_share")) |> mutate(group = gr, tp = t0, sample = sa)))
}))))
urban_step <- bind_rows(lapply(c("step_dis", "step_non", "step_all", "step_gap", "gap_sec", "dis_pri", "non_pri", "all_pri"), function(y) bind_rows(lapply(c("Mostly urban", "Mostly urban, without London", "Less urban"), function(sa) {
  dd <- switch(sa, "Mostly urban" = wide |> filter(urban_share >= 80), "Mostly urban, without London" = wide |> filter(urban_share >= 80, !london), wide |> filter(urban_share < 80))
  pool(dd, y) |> mutate(out = y, label = OUT[[y]], sample = sa)
}))))
measure_simple <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(names(MEASURES), function(ms) {
  d <- A |> filter(phase == "sec", group == gr) |> mutate(z = .data[[ms]] / sds[[ms]][match(tp, sds$tp)])
  bind_rows(lapply(names(SAMP4), function(sa) { dd <- SAMP4[[sa]](d) |> filter(!is.na(z))
    m <- feols(overall ~ z + share + urban_share | tp, dd, cluster = ~la); tibble(group = gr, measure = ms, label = MEASURES[[ms]], sample = sa, b_sd = coef(m)[["z"]], se_sd = se(m)[["z"]]) }))
}))))

# ---- A5c. What takes the slope away, and is it confounding or mediation? --------
# Gelbach (2016): the fall in the slope from the base model to the full model is
# exactly the sum, over the added controls, of (the control's coefficient in the
# full model) x (the slope of that control on distance, given the base). Unlike
# adding controls one at a time, it does not depend on their order. Intervals
# from a cluster bootstrap over authorities.
BLOCKS <- list("Ethnic mix of the group, the school population and residents" = c(ETHV, "white_british", "wbri", "eal"), "Region" = "gor_name", "Population density" = "log_density",
               "Health" = "bad_health", "Graduates, deprivation, renting, cars" = c("graduates", "deprived", "social_rent", "no_car"),
               "Pupils' needs, prior attainment, mobility" = c("sen_sup", "ehcp", "ks2_c", "mobile"))
gelbach <- function(d, y = "overall") {
  zs <- unlist(BLOCKS); d <- d[stats::complete.cases(d[, c(y, "near_dep", "share", "urban_share", zs)]), ]
  Z <- do.call(cbind, lapply(names(BLOCKS), function(bk) { m <- model.matrix(reformulate(BLOCKS[[bk]]), d)[, -1, drop = FALSE]; colnames(m) <- paste0(bk, "::", colnames(m)); m }))
  Z <- Z[, apply(Z, 2, function(x) sd(x) > 0), drop = FALSE]
  B <- model.matrix(~ near_dep + share + urban_share + factor(tp), d)
  fit <- function(X, yy) stats::lm.fit(X, yy)$coefficients
  b0 <- fit(B, d[[y]]); Fm <- cbind(B, Z); bf <- fit(Fm, d[[y]])
  dl <- vapply(colnames(Z), function(k) fit(B, Z[, k])[["near_dep"]], 0)
  ct <- bf[colnames(Z)] * dl; ct[is.na(ct)] <- 0
  bk <- sub("::.*", "", colnames(Z))
  c(base = b0[["near_dep"]], full = bf[["near_dep"]], vapply(names(BLOCKS), function(n) sum(ct[bk == n]), 0))
}
set.seed(2026)
gel_tab <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(c("All authorities", "Without London", "Mostly urban, without London"), function(sa) {
  d <- SAMPLES[[sa]](A |> filter(phase == "sec", group == gr)); las <- unique(d$la); est <- gelbach(d)
  bs <- replicate(300, { pick <- sample(las, replace = TRUE); gelbach(bind_rows(lapply(seq_along(pick), function(i) d[d$la == pick[i], ]))) })
  tibble(group = gr, sample = sa, term = names(est), est = est, se = apply(bs, 1, sd, na.rm = TRUE))
}))))
# Stratify instead of adjusting: among authorities alike in ethnic make-up or density
terc <- function(x) cut(x, quantile(x, c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE), labels = c("Lowest third", "Middle third", "Highest third"), include.lowest = TRUE)
la_vars <- A |> filter(phase == "sec", group == "all", tp == 202425L) |> transmute(la, wb3 = terc(wbri), dn3 = terc(log_density))
strata <- bind_rows(lapply(names(GROUP), function(gr) {
  d <- A |> filter(phase == "sec", group == gr) |> inner_join(la_vars, by = "la")
  bind_rows(lapply(c(wb3 = "White British share of pupils", dn3 = "Population density"), function(lbl) {
    v <- if (lbl == "Population density") "dn3" else "wb3"
    bind_rows(lapply(levels(d[[v]]), function(k) { dd <- d[d[[v]] == k, ]; pool(dd, "overall") |> mutate(group = gr, by = lbl, stratum = k, km_mean = mean(dd$near_dep), km_sd = sd(dd$near_dep), abs_mean = mean(dd$overall)) }))
  }))
}))
# Does the step from primary to secondary survive each one? It nets out what is common to both phases
step_ctrl <- bind_rows(lapply(c("step_dis", "step_non", "step_all", "dis_pri", "non_pri", "all_pri", "dis_sec", "non_sec", "all_sec"), function(y) {
  sets <- list("Share and urbanity" = c("share", "urban_share"), "And ethnic make-up" = c("share", "urban_share", "wbri", "white_british"), "And density" = c("share", "urban_share", "log_density"),
               "And region" = c("share", "urban_share", "gor_name"), "And ethnic make-up, density, region" = c("share", "urban_share", "wbri", "white_british", "log_density", "gor_name"))
  bind_rows(lapply(names(sets), function(s) pool(wide, y, sets[[s]]) |> mutate(out = y, set = s)))
}))
# What the slope looks like with ethnic make-up as a moderator
wb_inter <- bind_rows(lapply(names(GROUP), function(gr) {
  d <- A |> filter(phase == "sec", group == gr) |> mutate(wb_c = (wbri - mean(wbri, na.rm = TRUE)) / 10)
  m <- feols(overall ~ near_dep * wb_c + share + urban_share | tp, d, cluster = ~la)
  tibble(group = gr, b_near = coef(m)[["near_dep"]], se_near = se(m)[["near_dep"]], b_wb = coef(m)[["wb_c"]], se_wb = se(m)[["wb_c"]], b_int = coef(m)[["near_dep:wb_c"]], se_int = se(m)[["near_dep:wb_c"]])
}))
# The full model: what is left, how much distance varies once the controls are held, and what the controls do
full_models <- bind_rows(lapply(names(GROUP), function(gr) bind_rows(lapply(c("All authorities", "Without London", "Mostly urban", "Mostly urban, without London", "Less urban"), function(sa) {
  d <- SAMPLES[[sa]](A |> filter(phase == "sec", group == gr, !is.na(eth_wb))); v <- c("share", "urban_share", ETHV, "gor_name", CENSUS, PUPILS)
  m <- feols(as.formula(paste("overall ~ near_dep +", paste(v, collapse = " + "), "| tp")), d, cluster = ~la)
  m0 <- feols(as.formula(paste("overall ~", paste(v, collapse = " + "), "| tp")), d, cluster = ~la)
  x <- feols(as.formula(paste("near_dep ~", paste(v, collapse = " + "), "| tp")), d)
  tibble(group = gr, sample = sa, b = coef(m)[["near_dep"]], se = se(m)[["near_dep"]], n = m$nobs, las = n_distinct(d$la), r2_with = r2(m, "r2"), r2_without = r2(m0, "r2"),
         sd_km = sd(d$near_dep), sd_km_resid = sd(resid(x)), r2_dist = r2(x, "r2"))
}))))
full_coefs <- bind_rows(lapply(names(GROUP), function(gr) {
  d <- A |> filter(phase == "sec", group == gr); v <- c("near_dep", "share", "urban_share", CENSUS, PUPILS)
  z <- d |> mutate(across(all_of(v), ~ as.numeric(scale(.x))))
  m <- feols(as.formula(paste("overall ~", paste(v, collapse = " + "), "+ gor_name | tp")), z, cluster = ~la)
  tibble(group = gr, term = v, b_sd = coef(m)[v], se_sd = se(m)[v])
}))

# ---- A5d. Is it composition? The same test within each ethnic group -------------
# DfE publishes absence by ethnic group for each authority, for all pupils in that
# group (not split by disadvantage). Distance is measured two ways: from the
# authority's deprived neighbourhoods, and from where that group lives (the mean
# distance over all neighbourhoods, weighted by the group's Census residents).
ETH <- c("English / Welsh / Scottish / Northern Irish / British" = "White British", "Asian / Asian British" = "Asian",
         "Black / African / Caribbean / Black British" = "Black", "Mixed / Multiple ethnic groups" = "Mixed heritage")
ETH_N <- c("White British" = "wbri_n", "Asian" = "asian_n", "Black" = "black_n", "Mixed heritage" = "mixed_n")
eth_raw <- as_tibble(data.table::fread(EES_CHAR, select = c("time_period", "geographic_level", "la_name", "education_phase", "breakdown_topic", "breakdown", "enrolments", "sess_possible", "sess_overall", "sess_unauthorised", "enrolments_pa_10_exact"))) |>
  filter(time_period %in% YEARS, education_phase %in% c("State-funded secondary", "State-funded primary"), breakdown_topic %in% c("Ethnicity Major", "Ethnicity Minor")) |>
  distinct(time_period, geographic_level, la_name, education_phase, breakdown_topic, breakdown, .keep_all = TRUE) |>
  mutate(across(c(enrolments, sess_possible, sess_overall, sess_unauthorised, enrolments_pa_10_exact), num), tp = as.integer(time_period),
         phase = ifelse(education_phase == "State-funded secondary", "sec", "pri"), overall = 100 * sess_overall / sess_possible, unauth = 100 * sess_unauthorised / sess_possible, persistent = 100 * enrolments_pa_10_exact / enrolments)
eth_d <- bind_rows(lapply(yd[as.character(YEARS)], function(v) v$z |> mutate(tp = v$la$tp[1]) |> group_by(la, tp) |>
  summarise(`White British` = sum(near1 * wbri_n) / sum(wbri_n), Asian = sum(near1 * asian_n) / sum(asian_n), Black = sum(near1 * black_n) / sum(black_n), `Mixed heritage` = sum(near1 * mixed_n) / sum(mixed_n), .groups = "drop"))) |>
  pivot_longer(-c(la, tp), names_to = "eth", values_to = "near_e")
base_la <- A |> filter(group == "all") |> select(tp, la, phase, share, urban_share, london, gor_name, near_dep, log_density)
E <- eth_raw |> filter(geographic_level == "Local authority", breakdown %in% names(ETH)) |> mutate(eth = unname(ETH[breakdown]), la = la_name) |>
  filter(enrolments >= 100, !is.na(overall)) |> select(tp, la, phase, eth, enrolments, overall, unauth, persistent) |>
  inner_join(base_la, by = c("tp", "la", "phase")) |> left_join(eth_d, by = c("tp", "la", "eth"))
E_SAMP <- list("All authorities" = function(d) d, "Without London" = function(d) filter(d, !london), "Mostly urban" = function(d) filter(d, urban_share >= 80),
               "Mostly urban, without London" = function(d) filter(d, urban_share >= 80, !london), "Less urban" = function(d) filter(d, urban_share < 80))
eth_fit <- function(d, x, y = "overall", ctl = c("share", "urban_share")) {
  if (n_distinct(d$la) < 15) return(NULL)
  m <- feols(as.formula(paste0(y, " ~ ", x, " + ", paste(ctl, collapse = " + "), " | tp")), d, cluster = ~la)
  tibble(b = coef(m)[[x]], se = se(m)[[x]], n = m$nobs, las = n_distinct(d$la))
}
within_eth <- bind_rows(lapply(unname(ETH), function(e) bind_rows(lapply(c("sec", "pri"), function(ph) bind_rows(lapply(names(E_SAMP), function(sa) {
  d <- E_SAMP[[sa]](E |> filter(eth == e, phase == ph))
  bind_rows(eth_fit(d, "near_dep") |> mutate(distance = "From deprived neighbourhoods", control = "Share, urban"),
            eth_fit(d, "near_e") |> mutate(distance = "From where the group lives", control = "Share, urban"),
            eth_fit(d, "near_dep", ctl = c("share", "urban_share", "gor_name")) |> mutate(distance = "From deprived neighbourhoods", control = "And region"),
            eth_fit(d, "near_e", ctl = c("share", "urban_share", "gor_name")) |> mutate(distance = "From where the group lives", control = "And region")) |>
    mutate(eth = e, phase = ph, sample = sa)
}))))))
eth_kinds <- bind_rows(lapply(unname(ETH), function(e) bind_rows(lapply(c("overall", "unauth", "persistent"), function(y) {
  bind_rows(eth_fit(E |> filter(eth == e, phase == "sec"), "near_dep", y) |> mutate(sample = "All authorities"),
            eth_fit(E |> filter(eth == e, phase == "sec", urban_share >= 80, !london), "near_dep", y) |> mutate(sample = "Mostly urban, without London")) |> mutate(eth = e, outcome = y)
}))))
# Direct standardisation, all pupils: what absence would be with the authority's own
# ethnic mix and national rates for each minor ethnic group, against what it is
nat_e <- eth_raw |> filter(geographic_level == "National", breakdown_topic == "Ethnicity Minor") |> transmute(tp, phase, breakdown, nat = overall)
mix <- eth_raw |> filter(geographic_level == "Local authority", breakdown_topic == "Ethnicity Minor", !is.na(enrolments)) |> inner_join(nat_e, by = c("tp", "phase", "breakdown")) |>
  group_by(tp, la = la_name, phase) |> summarise(expected = sum(enrolments * nat, na.rm = TRUE) / sum(enrolments[!is.na(nat)]), .groups = "drop")
stdz <- A |> filter(group == "all") |> inner_join(mix, by = c("tp", "la", "phase")) |> mutate(beyond = overall - expected)
decomp_mix <- bind_rows(lapply(c("sec", "pri"), function(ph) bind_rows(lapply(names(E_SAMP), function(sa) {
  d <- E_SAMP[[sa]](stdz |> filter(phase == ph))
  bind_rows(pool(d, "overall") |> mutate(part = "Actual absence"), pool(d, "expected") |> mutate(part = "Explained by ethnic mix"), pool(d, "beyond") |> mutate(part = "Beyond ethnic mix")) |> mutate(phase = ph, sample = sa)
}))))

# ---- A6. Within authorities over time: does absence move with the network? -----
fe_la <- bind_rows(lapply(names(OUT), function(y) bind_rows(lapply(c("near_dep"), function(x) {
  m <- feols(as.formula(paste0(y, " ~ ", x, " + share | la + tp")), wide, cluster = ~la)
  tibble(out = y, label = OUT[[y]], measure = x, b = coef(m)[[x]], se = se(m)[[x]], n = m$nobs, las = n_distinct(wide$la))
}))))
chg <- dist_la |> filter(tp %in% c(201819L, 202425L), la %in% pl$la) |> select(la, tp, near_dep) |> pivot_wider(names_from = tp, values_from = near_dep, names_prefix = "y") |>
  mutate(change = y202425 - y201819)
fe_var <- list(n_changed = sum(abs(chg$change) >= 0.05, na.rm = TRUE), n = sum(!is.na(chg$change)), sd = sd(chg$change, na.rm = TRUE),
               biggest = chg |> arrange(desc(abs(change))) |> slice_head(n = 6), bh = chg |> filter(la == BH))
# Long differences, 2018/19 to 2024/25: change in absence on the level of distance
ld <- wide |> filter(tp %in% c(201819L, 202425L)) |> select(la, tp, dis_sec, non_sec, all_sec, near_dep, share, urban_share) |>
  pivot_wider(names_from = tp, values_from = c(dis_sec, non_sec, all_sec, near_dep, share)) |>
  mutate(d_dis = dis_sec_202425 - dis_sec_201819, d_non = non_sec_202425 - non_sec_201819, d_all = all_sec_202425 - all_sec_201819, d_share = share_202425 - share_201819)
rise <- bind_rows(lapply(c(d_dis = "dis", d_non = "non", d_all = "all") |> names(), function(y) {
  m <- lm(as.formula(paste0(y, " ~ near_dep_201819 + d_share + urban_share")), ld); s <- sqrt(vcovHC(m, "HC3")["near_dep_201819", "near_dep_201819"])
  tibble(group = sub("d_", "", y), b = coef(m)[["near_dep_201819"]], se = s, n = nobs(m))
}))

# ---- B. Within authorities, by how deprived the pupils' home neighbourhood is --
rs_all <- as_tibble(data.table::fread(EES_RES))
rs <- rs_all |>
  filter(geographic_level == "Local authority", measure == "IDACI", education_phase %in% c("State-funded secondary", "State-funded primary"),
         !pup_idaci %in% c("Total", "IDACI unclassified")) |>
  mutate(dec = as.integer(sub("^(\\d+) -.*", "\\1", pup_idaci)) / 10 + 1, tp = as.integer(time_period), la = la_name,
         phase = ifelse(education_phase == "State-funded secondary", "sec", "pri"),
         enrol = num(enrolments), overall = num(sess_overall_percent), auth = num(sess_authorised_percent), unauth = num(sess_unauthorised_percent), persistent = num(enrolments_pa_10_exact_percent)) |>
  select(tp, la, dec, phase, enrol, overall, auth, unauth, persistent) |> filter(!is.na(overall), overall > 0, enrol >= 30)
cell_dist <- function(tp0) yd[[as.character(tp0)]]$z |> group_by(la, dec = idaci_dec) |>
  summarise(lsoas = n(), kids_cell = sum(kids), near = wm(near1, kids), near3 = wm(near3, kids), near_pri_c = wm(near_pri, kids_pri), urban_c = 100 * wm(urban, kids),
            dep2_c = wm(dep2, kids), dens_c = wm(log(pmax(density, 1)), kids), grad_c = 100 * sum(grad_n) / sum(adults), wbri_c = 100 * sum(wbri_n) / sum(pop), nocar_c = 100 * sum(nocar_n) / sum(hh), social_c = 100 * sum(social_n) / sum(hh), bad_c = 100 * sum(bad_n) / sum(pop), .groups = "drop") |>
  mutate(tp = tp0)
cd <- bind_rows(lapply(c(202324L, 202425L), cell_dist))
B <- rs |> inner_join(cd, by = c("tp", "la", "dec")) |> filter(la %in% pl$la) |> left_join(pl |> select(la, london, urban_share, gor_name), by = "la") |>
  mutate(l2 = log2(near), band = factor(case_when(dec <= 3 ~ "Most deprived three tenths", dec <= 7 ~ "Middle four tenths", TRUE ~ "Least deprived three tenths"),
                                        c("Most deprived three tenths", "Middle four tenths", "Least deprived three tenths")))
# Do the IDACI cells line up with DfE's? Pupils by decile against children by decile
valid <- B |> filter(tp == 202425L, phase == "sec") |> summarise(r = cor(enrol, kids_cell), cells = n(), las = n_distinct(la))
bfit <- function(dat, f, x = "near") { m <- feols(f, dat, weights = ~enrol, cluster = ~la); tibble(b = coef(m)[[x]], se = se(m)[[x]], n = m$nobs, las = n_distinct(dat$la)) }
sec <- B |> filter(phase == "sec")
B_CTRL <- "urban_c + dens_c + grad_c + wbri_c + nocar_c + social_c + bad_c"
within_la <- bind_rows(
  bfit(sec, overall ~ near | la^tp + dec^tp) |> mutate(spec = "Same authority, same decile across England"),
  bfit(sec, as.formula(paste("overall ~ near + urban_c + dens_c | la^tp + dec^tp"))) |> mutate(spec = "And how urban and dense the cell is"),
  bfit(sec, as.formula(paste("overall ~ near +", B_CTRL, "| la^tp + dec^tp"))) |> mutate(spec = "And its Census character"),
  bfit(sec |> filter(!london), as.formula(paste("overall ~ near +", B_CTRL, "| la^tp + dec^tp"))) |> mutate(spec = "And without London"),
  bfit(sec |> filter(urban_share >= 80), as.formula(paste("overall ~ near +", B_CTRL, "| la^tp + dec^tp"))) |> mutate(spec = "Mostly urban authorities"),
  bfit(sec |> filter(urban_share < 80), as.formula(paste("overall ~ near +", B_CTRL, "| la^tp + dec^tp"))) |> mutate(spec = "Less urban authorities"),
  bfit(sec |> filter(urban_c >= 80), as.formula(paste("overall ~ near +", B_CTRL, "| la^tp + dec^tp"))) |> mutate(spec = "Mostly urban cells only"),
  bfit(sec |> filter(tp == 202324L), as.formula(paste("overall ~ near +", B_CTRL, "| la + dec"))) |> mutate(spec = "2023/24 alone"),
  bfit(sec |> filter(tp == 202425L), as.formula(paste("overall ~ near +", B_CTRL, "| la + dec"))) |> mutate(spec = "2024/25 alone"),
  bfit(sec, as.formula(paste("overall ~ near3 +", B_CTRL, "| la^tp + dec^tp")), "near3") |> mutate(spec = "Mean of the three nearest"),
  bfit(sec, as.formula(paste("overall ~ l2 +", B_CTRL, "| la^tp + dec^tp")), "l2") |> mutate(spec = "Logged (per doubling)"))
by_band <- bind_rows(lapply(levels(B$band), function(bn) {
  bind_rows(lapply(c("sec", "pri"), function(ph) {
    bfit(B |> filter(band == bn, phase == ph), as.formula(paste("overall ~ near +", B_CTRL, "| la^tp + dec^tp"))) |> mutate(band = bn, phase = ph)
  }))
}))
m_int <- feols(as.formula(paste("overall ~ near:band +", B_CTRL, "| la^tp + dec^tp")), sec, weights = ~enrol, cluster = ~la)
band_diff <- { co <- coef(m_int); V <- vcov(m_int); a <- "near:bandMost deprived three tenths"; b <- "near:bandLeast deprived three tenths"
  tibble(diff = co[[a]] - co[[b]], se = sqrt(V[a, a] + V[b, b] - 2 * V[a, b])) }
b_kinds <- bind_rows(lapply(c("overall", "auth", "unauth", "persistent"), function(y) bind_rows(lapply(c("sec", "pri"), function(ph) {
  bfit(B |> filter(phase == ph), as.formula(paste(y, "~ near +", B_CTRL, "| la^tp + dec^tp"))) |> mutate(outcome = y, phase = ph)
}))))
b_step <- { s <- B |> select(tp, la, dec, phase, overall, enrol) |> pivot_wider(names_from = phase, values_from = c(overall, enrol)) |>
    inner_join(sec |> select(tp, la, dec, near, near_pri_c, all_of(strsplit(gsub(" ", "", B_CTRL), "\\+")[[1]])), by = c("tp", "la", "dec")) |> mutate(step = overall_sec - overall_pri, enrol = enrol_sec) |> filter(!is.na(step))
  bfit(s, as.formula(paste("step ~ near +", B_CTRL, "| la^tp + dec^tp"))) }
b_pri_own <- { m <- feols(as.formula(paste("overall ~ near + near_pri_c +", B_CTRL, "| la^tp + dec^tp")), B |> filter(phase == "pri"), weights = ~enrol, cluster = ~la)
  tibble(b_sec = coef(m)[["near"]], se_sec = se(m)[["near"]], b_pri = coef(m)[["near_pri_c"]], se_pri = se(m)[["near_pri_c"]], n = m$nobs) }
within_sort <- { d <- sec |> filter(band == levels(B$band)[1]); dm <- function(v) d[[v]] - ave(d[[v]], interaction(d$la, d$tp)) - ave(d[[v]], interaction(d$dec, d$tp)) + mean(d[[v]])
  vapply(c(wbri = "wbri_c", urban = "urban_c", density = "dens_c", graduates = "grad_c", no_car = "nocar_c", social = "social_c", bad_health = "bad_c"), function(v) cor(dm("near"), dm(v), use = "complete.obs"), 0) }
bh_cells <- B |> filter(la == BH, tp == 202425L, phase == "sec") |> select(dec, enrol, overall, near, lsoas)
b_shape <- bind_rows(lapply(c("sec", "pri"), function(ph) {
  d <- B |> filter(phase == ph) |> mutate(nb = cut(near, c(0, 0.75, 1, 1.5, 2, 3, Inf), labels = c("Under 0.75 km", "0.75 to 1 km", "1 to 1.5 km", "1.5 to 2 km", "2 to 3 km", "Over 3 km")))
  m <- feols(as.formula(paste("overall ~ i(nb, ref = 'Under 0.75 km') +", B_CTRL, "| la^tp + dec^tp")), d, weights = ~enrol, cluster = ~la)
  cf <- coef(m); s <- se(m); k <- grep("^nb::", names(cf))
  bind_rows(tibble(band = "Under 0.75 km", b = 0, se = 0), tibble(band = sub("^nb::", "", names(cf)[k]), b = cf[k], se = s[k])) |> mutate(phase = ph, cells = vapply(band, function(bn) sum(d$nb == bn & d$tp == 202425L, na.rm = TRUE), 1L))
}))

# The same cells between authorities and within them
between <- bind_rows(lapply(levels(B$band), function(bn) {
  d <- sec |> filter(band == bn)
  bind_rows(bfit(d, overall ~ near | dec^tp) |> mutate(comparison = "Between authorities"),
            bfit(d, overall ~ near + urban_c + dens_c | dec^tp) |> mutate(comparison = "Between, given how urban and dense"),
            bfit(d, overall ~ near | la^tp + dec^tp) |> mutate(comparison = "Within authorities"),
            bfit(d, as.formula(paste("overall ~ near +", B_CTRL, "| la^tp + dec^tp"))) |> mutate(comparison = "Within, given Census character")) |> mutate(band = bn)
}))
band_place <- bind_rows(lapply(levels(B$band), function(bn) bind_rows(lapply(c("sec", "pri"), function(ph) {
  d <- B |> filter(band == bn, phase == ph); f <- as.formula(paste("overall ~ near +", B_CTRL, "| la^tp + dec^tp"))
  bind_rows(bfit(d |> filter(urban_share >= 80), f) |> mutate(sample = "Mostly urban authorities"),
            bfit(d |> filter(urban_share >= 80, !london), f) |> mutate(sample = "Mostly urban, without London"),
            bfit(d |> filter(urban_share < 80), f) |> mutate(sample = "Less urban authorities"),
            bfit(d |> filter(!london), f) |> mutate(sample = "Without London"),
            bfit(d |> filter(la != BH), f) |> mutate(sample = "Without Brighton and Hove"),
            bfit(d |> filter(urban_c >= 80), f) |> mutate(sample = "Mostly urban cells")) |> mutate(band = bn, phase = ph)
}))))
band_kinds <- bind_rows(lapply(c("overall", "auth", "unauth", "persistent"), function(y) bind_rows(lapply(c("sec", "pri"), function(ph) {
  bfit(B |> filter(phase == ph, band == levels(B$band)[1]), as.formula(paste(y, "~ near +", B_CTRL, "| la^tp + dec^tp"))) |> mutate(outcome = y, phase = ph)
}))))
band_year <- bind_rows(lapply(c(202324L, 202425L), function(t0) bfit(B |> filter(phase == "sec", band == levels(B$band)[1], tp == t0), as.formula(paste("overall ~ near +", B_CTRL, "| la + dec"))) |> mutate(tp = t0)))
dec1 <- bind_rows(lapply(c("sec", "pri"), function(ph) bind_rows(lapply(1:10, function(k) {
  bfit(B |> filter(dec == k, phase == ph), as.formula(paste("overall ~ near +", B_CTRL, "| tp"))) |> mutate(dec = k, phase = ph)
}))))
nat_dec <- rs_all |> filter(geographic_level == "National", measure == "IDACI", time_period == 202425L, education_phase %in% c("State-funded secondary", "State-funded primary"),
                            !pup_idaci %in% c("Total", "IDACI unclassified")) |>
  transmute(dec = as.integer(sub("^(\\d+) -.*", "\\1", pup_idaci)) / 10 + 1, phase = ifelse(education_phase == "State-funded secondary", "sec", "pri"), overall = num(sess_overall_percent), unauth = num(sess_unauthorised_percent), persistent = num(enrolments_pa_10_exact_percent), enrol = num(enrolments)) |>
  mutate(place = "England")
nat_km <- yd[["202425"]]$z |> group_by(dec = idaci_dec) |> summarise(near = wm(near1, kids), .groups = "drop")
deciles <- bind_rows(nat_dec |> left_join(nat_km, by = "dec"),
                     B |> filter(la == BH, tp == 202425L) |> transmute(dec, phase, overall, unauth, persistent, enrol, place = BH, near))

# ---- C. Schools, year by year and within school ---------------------------------
Ac <- readRDS(here::here("data", "absence_change.rds"))
ees <- function(...) here::here("data-raw", "ees", ...)
pn <- function(x) num(gsub("[%,]", "", x))
read_cscp <- function(ay) {
  d <- readr::read_csv(here::here("data", "extracted", ay, "england_census.csv"), col_types = readr::cols(.default = "c"), progress = FALSE)
  nor <- pn(d$NOR)
  tibble(URN = as.integer(d$URN), tp = as.integer(paste0(substr(ay, 1, 4), substr(ay, 8, 9))), nor = nor, fsm = 100 * pn(d$NUMFSM) / nor,
         sen_sup = 100 * pn(d$TSENELK) / nor, ehcp = 100 * pn(d$TSENELSE) / nor,
         eal = 100 * pn(d$NUMEAL) / (pn(d$NUMEAL) + pn(d$NUMENGFL) + pn(d$NUMUNCFL)), girls = 100 * pn(d$NORG) / nor)
}
cen <- bind_rows(lapply(c("2018-2019", "2021-2022", "2022-2023", "2023-2024", "2024-2025"), read_cscp))
sp <- readr::read_csv(ees("spc_school_level_underlying_data_2026.csv"), col_types = readr::cols(.default = "c"), name_repair = "unique_quiet", progress = FALSE) |> filter(phase_type_grouping == "State-funded secondary")
sen <- readr::read_csv(unz(ees("sen_school_level_2025-26.zip"), "supporting-files/School level underlying data 2026.csv"), col_types = readr::cols(.default = "c"), progress = FALSE) |> filter(phase_type_grouping == "State-funded secondary")
c26 <- tibble(URN = as.integer(sp$urn), tp = 202526L, nor = num(sp$`headcount of pupils`),
              fsm = 100 * num(sp$`number of pupils known to be eligible for free school meals`) / num(sp$`headcount of pupils`),
              eal = num(sp$`% of pupils whose first language is known or believed to be other than English`),
              girls = 100 * num(sp$`headcount total female`) / num(sp$`headcount of pupils`)) |>
  left_join(tibble(URN = as.integer(sen$URN), sen_sup = 100 * num(sen$SEN_support) / num(sen$total_pupils), ehcp = 100 * num(sen$EHC_plan) / num(sen$total_pupils)), by = "URN")
cen <- bind_rows(cen, c26) |> filter(nor > 0) |> distinct(URN, tp, .keep_all = TRUE)
catch <- bind_rows(lapply(yd, function(v) v$catch))
# A school's number can change: fall back to the catchment of its successor that year
S <- as_tibble(Ac$series) |> filter(tp %in% ALL_TP) |> inner_join(cen, by = c("URN", "tp")) |>
  left_join(catch, by = c("URN", "tp")) |>
  left_join(gs |> select(URN, la_g = la, selective, place) |> distinct(URN, .keep_all = TRUE), by = "URN") |>
  group_by(cid) |> mutate(place = { p <- place[!is.na(place)]; if (length(p)) p[length(p)] else NA_character_ }, la_g = { p <- la_g[!is.na(la_g)]; if (length(p)) p[length(p)] else NA_character_ },
                          selective = any(selective %in% TRUE)) |> ungroup()
match_rate <- S |> group_by(tp) |> summarise(schools = n(), matched = sum(!is.na(catch_km)), .groups = "drop")
S <- S |> filter(!is.na(catch_km), catch_kids >= 100, !selective, is.finite(fsm), is.finite(eal), is.finite(sen_sup), is.finite(ehcp), is.finite(girls), !is.na(place)) |>
  group_by(tp) |> mutate(l2 = log2(catch_km), fsm_c = fsm - mean(fsm), lsize = log(nor)) |> ungroup() |>
  mutate(place = factor(place, c("Urban, near a major town or city", "Urban, further from one", "Rural")))
OUT_S <- c(overall = "Overall absence", illness = "Illness", unauth = "Unauthorised", persistent = "Persistent absence (% of pupils)")
S_CT <- "fsm_c + sen_sup + ehcp + eal + girls + lsize"
sch_year <- bind_rows(lapply(names(OUT_S), function(o) bind_rows(lapply(sort(unique(S$tp)), function(t0) {
  d <- S |> filter(tp == t0, !is.na(.data[[o]]))
  m <- feols(as.formula(paste0(o, " ~ l2 + l2:fsm_c + ", S_CT, " + place | la_g")), d, cluster = ~la_g)
  m0 <- feols(as.formula(paste0(o, " ~ l2 + ", S_CT, " + place | la_g")), d, cluster = ~la_g)
  tibble(outcome = o, tp = t0, b = coef(m0)[["l2"]], se = se(m0)[["l2"]], b_int = coef(m)[["l2:fsm_c"]], se_int = se(m)[["l2:fsm_c"]], n = m$nobs, mean = mean(d[[o]]))
}))))
sch_place <- bind_rows(lapply(levels(S$place), function(p) {
  d <- S |> filter(place == p)
  m <- feols(as.formula(paste0("overall ~ l2 + l2:fsm_c + ", S_CT, " | la_g^tp")), d, cluster = ~la_g)
  tibble(place = p, b = coef(m)[["l2"]], se = se(m)[["l2"]], b_int = coef(m)[["l2:fsm_c"]], se_int = se(m)[["l2:fsm_c"]], n = m$nobs, schools = n_distinct(d$cid))
}))
# With school effects, l2 is identified by schools whose nearest children changed.
# l2:fsm_c is not: most of it comes from FSM moving at schools whose distance did not.
sch_fe <- bind_rows(lapply(names(OUT_S), function(o) {
  d <- S |> filter(!is.na(.data[[o]]))
  ma <- feols(as.formula(paste0(o, " ~ l2 + ", S_CT, " | cid + tp")), d, cluster = ~cid)
  mb <- feols(as.formula(paste0(o, " ~ l2 + l2:fsm_c + ", S_CT, " | cid + tp")), d, cluster = ~cid)
  me <- feols(as.formula(paste0(o, " ~ l2 + l2:fsm_c + ", S_CT, " | cid + la_g^tp")), d, cluster = ~cid)
  tibble(outcome = o, b = coef(ma)[["l2"]], se = se(ma)[["l2"]], b_int = coef(mb)[["l2:fsm_c"]], se_int = se(mb)[["l2:fsm_c"]], b_la = coef(me)[["l2"]], se_la = se(me)[["l2"]], n = ma$nobs, schools = n_distinct(d$cid))
}))
sch_move <- S |> group_by(cid) |> summarise(ch = max(l2) - min(l2), .groups = "drop") |> summarise(schools = n(), changed10 = sum(ch >= log2(1.1)), changed25 = sum(ch >= log2(1.25)))

# Ethnic make-up at school level: the White British share in the January 2026 census,
# held for each school across years (earlier years' shares are not in these files)
wb_sch <- as_tibble(Ac$series) |> filter(tp == 202526L) |> select(cid, URN) |> inner_join(frame |> transmute(URN = as.integer(URN), wbri), by = "URN") |> select(cid, wbri) |> distinct(cid, .keep_all = TRUE)
S2 <- S |> left_join(wb_sch, by = "cid") |> filter(!is.na(wbri)) |> mutate(wb_c = wbri - mean(wbri))
sch_wb <- bind_rows(lapply(names(OUT_S), function(o) bind_rows(lapply(sort(unique(S2$tp)), function(t0) {
  d <- S2 |> filter(tp == t0, !is.na(.data[[o]]))
  m1 <- feols(as.formula(paste0(o, " ~ l2 + l2:fsm_c + ", S_CT, " + place | la_g")), d, cluster = ~la_g)
  m2 <- feols(as.formula(paste0(o, " ~ l2 + l2:fsm_c + wbri + ", S_CT, " + place | la_g")), d, cluster = ~la_g)
  tibble(outcome = o, tp = t0, b = coef(m1)[["l2"]], se = se(m1)[["l2"]], b_wb = coef(m2)[["l2"]], se_wb = se(m2)[["l2"]], int = coef(m1)[["l2:fsm_c"]], se_int = se(m1)[["l2:fsm_c"]], int_wb = coef(m2)[["l2:fsm_c"]], se_int_wb = se(m2)[["l2:fsm_c"]], n = m2$nobs)
}))))
sch_wb_pool <- bind_rows(lapply(names(OUT_S), function(o) {
  d <- S2 |> filter(!is.na(.data[[o]]))
  m0 <- feols(as.formula(paste0(o, " ~ l2 + l2:fsm_c + ", S_CT, " | la_g^tp + place")), d, cluster = ~la_g)
  m1 <- feols(as.formula(paste0(o, " ~ l2 + l2:fsm_c + wbri + ", S_CT, " | la_g^tp + place")), d, cluster = ~la_g)
  m2 <- feols(as.formula(paste0(o, " ~ l2 + l2:fsm_c + l2:wb_c + wbri + ", S_CT, " | la_g^tp + place")), d, cluster = ~la_g)
  tibble(outcome = o, b0 = coef(m0)[["l2"]], se0 = se(m0)[["l2"]], int0 = coef(m0)[["l2:fsm_c"]], se_int0 = se(m0)[["l2:fsm_c"]], b1 = coef(m1)[["l2"]], se1 = se(m1)[["l2"]], int1 = coef(m1)[["l2:fsm_c"]], se_int1 = se(m1)[["l2:fsm_c"]],
         b_wb_int = coef(m2)[["l2:wb_c"]], se_wb_int = se(m2)[["l2:wb_c"]], n = m1$nobs, schools = n_distinct(d$cid))
}))

sch_fe_place <- bind_rows(lapply(levels(S$place), function(p) {
  d <- S |> filter(place == p); mb <- feols(as.formula(paste0("overall ~ l2 + l2:fsm_c + ", S_CT, " | cid + tp")), d, cluster = ~cid)
  tibble(place = p, b = coef(mb)[["l2"]], se = se(mb)[["l2"]], b_int = coef(mb)[["l2:fsm_c"]], se_int = se(mb)[["l2:fsm_c"]], n = mb$nobs, schools = n_distinct(d$cid))
}))
sch_fe_moved <- { ch <- S |> group_by(cid) |> summarise(ch = max(l2) - min(l2), .groups = "drop") |> filter(ch >= log2(1.1)); d <- S |> filter(cid %in% ch$cid)
  mb <- feols(as.formula(paste0("overall ~ l2 + l2:fsm_c + ", S_CT, " | cid + tp")), d, cluster = ~cid); tibble(b = coef(mb)[["l2"]], se = se(mb)[["l2"]], b_int = coef(mb)[["l2:fsm_c"]], se_int = se(mb)[["l2:fsm_c"]], schools = nrow(ch), mean_fsm = mean(d$fsm)) }
fsm_ref <- list(mean = mean(S$fsm), p10 = quantile(S$fsm, 0.1), p90 = quantile(S$fsm, 0.9))

# ---- D. What it might mean for Brighton and Hove --------------------------------
d25 <- A |> filter(phase == "sec", group == "dis", tp == LAST)
med <- median(d25$near_dep); bh_near <- d25$near_dep[d25$la == BH]; bh_abs <- d25$overall[d25$la == BH]

out <- list(built = Sys.Date(), years = YEARS, network_n = network_n, n_la_year = n_la_year, measures = MEASURES, controls = names(CONTROLS), samples = names(SAMPLES),
            curve = curve, by_year = by_year, pooled = pooled, pri_own = pri_own, kinds_la = kinds_la, checks_la = checks_la, race = race, shape = shape,
            urb_sample = urb_sample, inter = inter, car = car, fe_la = fe_la, fe_var = fe_var, rise = rise,
            within_la = within_la, by_band = by_band, band_diff = band_diff, b_kinds = b_kinds, b_step = b_step, b_pri_own = b_pri_own, bh_cells = bh_cells, b_shape = b_shape, valid = valid,
            n_cells = nrow(sec), sch_year = sch_year, sch_place = sch_place, sch_fe = sch_fe, sch_move = sch_move, match_rate = match_rate, sch_fe_place = sch_fe_place, sch_fe_moved = sch_fe_moved, fsm_ref = fsm_ref,
            one_at_a_time = one_at_a_time, collinear = collinear, urban_checks = urban_checks, urban_year = urban_year, urban_step = urban_step, measure_simple = measure_simple,
            between = between, band_place = band_place, band_kinds = band_kinds, band_year = band_year, dec1 = dec1,
            deciles = deciles,
            bh_resid = bh_resid,
            within_sort = within_sort, far_las = A |> filter(phase == "sec", group == "all", tp == 202425L, near_dep >= 2) |> select(la, near_dep, urban_share, log_density),
            group_mix = group_mix, group_mix_kinds = group_mix_kinds, group_mix_pri = group_mix_pri, mix_years = mix_years, mix_cor = mix_cor, BH_mix = BH_mix, EN_mix = EN_mix, sch_wb = sch_wb, sch_wb_pool = sch_wb_pool,
            within_eth = within_eth, eth_kinds = eth_kinds, decomp_mix = decomp_mix,
            gel_tab = gel_tab, strata = strata, step_ctrl = step_ctrl, wb_inter = wb_inter, full_models = full_models, full_coefs = full_coefs,
            bh = list(near = bh_near, med = med, abs = bh_abs, med_urban = median(d25$near_dep[d25$urban_share >= 80]), med_urban_nl = median(d25$near_dep[d25$urban_share >= 80 & !d25$london]),
                      eng_abs = NA_real_, rank_urban = rank(-d25$near_dep[d25$urban_share >= 80], ties.method = "min")[d25$la[d25$urban_share >= 80] == BH]),
            prov = c(prov_grid, list(lsoa_change = as.list(table(lk11$CHGIND[startsWith(lk11$LSOA21CD, "E")])), n_lsoa = nrow(ls), n_oa = nrow(oa2), no_idaci = sum(!ls$lsoa %in% idaci$lsoa),
                     n_sec_all = nrow(sec_all), n_pri_all = nrow(pri_all), corr_cells = valid$r)))
saveRDS(out, here::here("data", "distance_absence.rds"))
message("Saved data/distance_absence.rds")
