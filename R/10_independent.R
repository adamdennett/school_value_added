# 10_independent.R - Independent schools: the data, and how far a comparison
# with the state sector can go
# ---------------------------------------------------------------------------
# Everything here is published data:
#   * GIAS (June 2026): every open independent school, its age range, sex,
#     boarding, admissions, religious character, inspectorate, location.
#   * The January 2026 school census (SPC) and SEN files: headcounts and
#     SEN for independent schools (no FSM, EAL, ethnicity or absence).
#   * KS4 performance tables 2024/25 (CSCP): Attainment 8 and English and
#     maths for the independent schools that have a KS4 cohort.
#   * KS5 performance tables 2024/25 (CSCP): A level average point score
#     and DfE's 16-18 A level value added, which covers independent schools.
#   * The v2.1 model inputs and value added for state schools, to test what
#     a reduced specification (only what independent schools report) gets
#     wrong where we can check it.
#
# Output: data/independent.rds (committed).   Rscript R/10_independent.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
num <- function(x) suppressWarnings(as.numeric(x))
rd  <- function(f) readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, name_repair = "unique_quiet")
ex  <- function(y, f) {
  p <- here::here("data", "extracted", y, f)
  if (!file.exists(p)) {
    source(here::here("pipeline", "01_download_cscp.R"))
    cscp_download(y, c("ks5", "vaqual"))
  }
  rd(p)
}

# ---- 1. the sector, from GIAS and the census ---------------------------------
g <- rd(here::here("data-raw", "gias", "edubasealldata20260610.csv"))
gias <- g |> filter(`EstablishmentStatus (name)` == "Open") |>
  transmute(URN = as.integer(URN), name = EstablishmentName, la = `LA (name)`, region = `GOR (name)`,
            group = `EstablishmentTypeGroup (name)`, type = `TypeOfEstablishment (name)`,
            low = num(StatutoryLowAge), high = num(StatutoryHighAge), pupils_gias = num(NumberOfPupils),
            sex = `Gender (name)`, boarding = `BoardingEstablishment (name)`, admissions = `AdmissionsPolicy (name)`,
            religion = `ReligiousCharacter (name)`, inspectorate = `InspectorateName (name)`,
            urban_rural = `UrbanRural (name)`, proprietor = PropsName,
            easting = num(Easting), northing = num(Northing))
# mainstream independent schools teaching secondary ages
ind <- gias |> filter(type == "Other independent school", high >= 15, low <= 13)
pts <- ind |> filter(!is.na(easting), easting > 0) |>
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) |> sf::st_transform(4326)
ind <- ind |> left_join(tibble(URN = pts$URN, lon = sf::st_coordinates(pts)[, 1], lat = sf::st_coordinates(pts)[, 2]), by = "URN")

sen <- rd(unz(here::here("data-raw", "ees", "sen_school_level_2025-26.zip"),
              "supporting-files/School level underlying data 2026.csv")) |>
  transmute(URN = as.integer(URN), phase = phase_type_grouping, tot = num(total_pupils),
            sen_sup = 100 * num(SEN_support) / tot, ehcp = 100 * num(EHC_plan) / tot)
sp <- rd(here::here("data-raw", "ees", "spc_school_level_underlying_data_2026.csv"))
# which census fields are published for independent schools and which are not
census_fill <- function(phase) {
  s <- sp |> filter(phase_type_grouping == phase)
  f <- function(col) { v <- s[[col]]; if (is.null(v)) return(NA_real_); mean(!is.na(v) & !v %in% c("", "x", "z", "c", "u", ":", "NA")) }
  c(fsm = f("% of pupils known to be eligible for free school meals"),
    eal = f("% of pupils whose first language is known or believed to be other than English"),
    wbri = f("% of pupils classified as white British ethnic origin"),
    carer = f("% of pupils who are a young carer"),
    headcount = f("headcount of pupils"), boarders = f("total boarders"))
}
census_cov <- rbind(Independent = census_fill("Independent school"), `State-funded secondary` = census_fill("State-funded secondary"))
sen_cov <- sen |> filter(phase == "State-funded secondary" | URN %in% ind$URN) |>
  mutate(indep = phase == "Independent school") |> group_by(indep) |>
  summarise(schools = n(), sen_pub = mean(!is.na(sen_sup)), ehcp_pub = mean(!is.na(ehcp)),
            sen_sup = median(sen_sup, na.rm = TRUE), ehcp = median(ehcp, na.rm = TRUE), .groups = "drop")

# ---- 2. KS4: what is published -------------------------------------------------
k4 <- ex("2024-2025", "england_ks4provisional.csv") |>
  transmute(URN = as.integer(URN), nftype = NFTYPE, rectype = RECTYPE, school = SCHNAME,
            tpup = num(TPUP), att8_raw = ATT8SCR, att8 = num(ATT8SCR), basics = num(PTL2BASICS_94),
            ebacc_entry = num(PTEBACC_E_PTQ_EE), p8 = num(P8MEA), fsm6 = num(PTFSM6CLA1A), prior_lo = num(PTPRIORLO),
            gender = EGENDER) |>
  mutate(sector = case_when(nftype == "IND" ~ "Independent",
                            nftype %in% c("INDSS", "SS", "NMSS", "ACS", "ACCS", "CYS", "FDS", "FS") ~ "Special",
                            rectype == "1" ~ "State-funded mainstream", TRUE ~ "Other"))
ks4_cov <- k4 |> filter(sector %in% c("Independent", "State-funded mainstream")) |> group_by(sector) |>
  summarise(schools = n(), pupils = sum(tpup, na.rm = TRUE),
            att8_pub = mean(!is.na(att8)), att8_supp = mean(att8_raw %in% c("SUPP", "NE", "NP", "LOWCOV")),
            p8_pub = mean(!is.na(p8)), fsm_pub = mean(!is.na(fsm6)), prior_pub = mean(!is.na(prior_lo)),
            att8_med = median(att8, na.rm = TRUE), att8_q1 = quantile(att8, 0.25, na.rm = TRUE),
            att8_q3 = quantile(att8, 0.75, na.rm = TRUE),
            under20 = mean(att8 < 20, na.rm = TRUE), basics_med = median(basics, na.rm = TRUE), .groups = "drop")

# ---- 3. KS5: A level results and value added ------------------------------------
k5 <- ex("2024-2025", "england_ks5final.csv") |>
  transmute(URN = as.integer(URN), nftype = `NFTYPE/FESITYPE`, rectype = RECTYPE,
            alev_n = num(TALLPUP_ALEV_1618), alev_aps = num(TALLPPE_ALEV_1618), alev_grade = TALLPPEGRD_ALEV_1618,
            va_alev = num(VA_INS_ALEV), va_alev_3 = rowMeans(cbind(num(VA_INS_ALEV), num(VA_INS_ALEV_24), num(VA_INS_ALEV_23)), na.rm = TRUE)) |>
  mutate(sector5 = case_when(nftype == "IND" ~ "Independent",
                             nftype %in% c("ACC", "AC", "CY", "VA", "F", "FD", "VC", "UTC", "CTC", "ACC1619", "AC1619", "F1619") ~ "State school sixth forms",
                             grepl("College", nftype) ~ "Colleges", TRUE ~ "Other"))
vq <- ex("2024-2025", "england_vaqual.csv") |> filter(SUBLEVNO == "111") |>
  transmute(URN = as.integer(URN), va_q = num(`Qualification Value Added Score`), lo_q = num(`Lower Confidence Limit`),
            hi_q = num(`Upper Confidence Limit`), entries = num(`Number of Entries`))
alev <- k5 |> left_join(vq, by = "URN") |> filter(sector5 != "Other")
alev_sum <- alev |> filter(!is.na(va_alev)) |> group_by(sector5) |>
  summarise(schools = n(), students = sum(alev_n, na.rm = TRUE), va_med = median(va_alev), va_q1 = quantile(va_alev, 0.25),
            va_q3 = quantile(va_alev, 0.75), aps_med = median(alev_aps, na.rm = TRUE),
            sig_up = mean(lo_q > 0, na.rm = TRUE), sig_dn = mean(hi_q < 0, na.rm = TRUE), .groups = "drop")

# KS4 against KS5 for the same schools: is a low Attainment 8 real?
k45 <- k4 |> filter(sector %in% c("Independent", "State-funded mainstream"), !is.na(att8)) |>
  inner_join(k5 |> select(URN, alev_aps, alev_n, va_alev), by = "URN") |>
  filter(!is.na(alev_aps), alev_n >= 10)
k45_cor <- k45 |> group_by(sector) |> summarise(schools = n(), r = cor(att8, alev_aps), .groups = "drop")
# independent schools whose A level results are above the state median but
# whose Attainment 8 is in the bottom quarter of state schools
st_q1 <- quantile(k4$att8[k4$sector == "State-funded mainstream"], 0.25, na.rm = TRUE)
st_aps_med <- median(k45$alev_aps[k45$sector == "State-funded mainstream"])
k45_flag <- k45 |> filter(sector == "Independent") |>
  summarise(n = n(), mismatch = sum(att8 < st_q1 & alev_aps > st_aps_med), share = mean(att8 < st_q1 & alev_aps > st_aps_med))

# ---- 4. a reduced model, tested on state schools ---------------------------------
# What could be put into an expectation for an independent school: the
# neighbourhood it sits in, its sex, whether it selects, whether it has a
# sixth form, its size, its SEN and its region and setting. Fit that on
# state schools and see how far it agrees with the full v2.1 model.
V <- readRDS(here::here("data", "va_v2.rds"))
I <- readRDS(here::here("data", "inputs.rds")) |> filter(!excluded, year_label == "2024-25") |> distinct(URN, .keep_all = TRUE)
st <- I |>
  inner_join(V$school_years |> filter(group == "all", year_label == "2024-25") |> select(URN, r_nat, va_nat, exp_nat), by = "URN") |>
  left_join(V$schools |> select(URN, pupils), by = "URN") |>
  left_join(gias |> select(URN, urban_rural), by = "URN") |>
  mutate(rural = grepl("rural", urban_rural, ignore.case = TRUE), single = girls_school | boys_school,
         lsize = log(pmax(pupils, 50)), sel = ADMPOL_PT == "SEL", ly = log(ATT8SCR)) |>
  filter(complete.cases(ly, z_l4, sen_support, sen_ehcp, lsize, age16, single, sel, gor_name, rural, r_nat))
REDUCED <- ly ~ z_l4 + sen_support + sen_ehcp + lsize + age16 + girls_school + sel + rural + gor_name
FULL_LM <- ly ~ log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + ks2_c + ADMPOL_PT + z_l4 + sen_support + sen_ehcp +
  mobile + girls_school + age16 + gor_name
m_red <- lm(REDUCED, data = st)
m_full <- lm(FULL_LM, data = st |> filter(PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0))
st$r_red <- residuals(m_red)
# drop one missing term at a time from the full model: what each costs
terms_missing <- c(`Prior attainment (KS2)` = "ks2_c", `Disadvantage (FSM6)` = "log(PTFSM6CLA1A)",
                   `Absence` = "log(PERCTOT)", `EAL` = "log(PNUMEAL)", `Mobility` = "mobile")
stf <- st |> filter(PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0)
drop_one <- bind_rows(lapply(names(terms_missing), function(k) {
  f <- update(FULL_LM, as.formula(paste(". ~ . -", terms_missing[[k]])))
  m <- lm(f, data = stf)
  tibble(term = k, r2 = summary(m)$r.squared, r_va = cor(residuals(m), residuals(m_full)))
}))
q5 <- function(x) ntile(x, 5)
reduced_test <- tibble(
  r2_full = summary(m_full)$r.squared, r2_reduced = summary(m_red)$r.squared,
  r_resid = cor(st$r_red, st$r_nat),
  same_fifth = mean(q5(st$r_red) == q5(st$r_nat)),
  top_fifth_kept = mean(q5(st$r_nat)[q5(st$r_red) == 5] == 5),
  n = nrow(st))
# who the reduced model flatters: residuals as % by kind of school
by_kind <- st |>
  filter(!sel) |>   # selection is in the reduced model as a flag, so grammar schools average zero by construction
  mutate(kind = case_when(single ~ "Single-sex",
                          PTFSM6CLA1A < quantile(PTFSM6CLA1A, 0.2) ~ "Least disadvantaged fifth (FSM6)",
                          PTFSM6CLA1A > quantile(PTFSM6CLA1A, 0.8) ~ "Most disadvantaged fifth (FSM6)",
                          TRUE ~ "Other non-selective")) |>
  group_by(kind) |>
  summarise(schools = n(), full = 100 * (exp(mean(r_nat)) - 1), reduced = 100 * (exp(mean(r_red)) - 1), .groups = "drop")

# ---- 5. save ------------------------------------------------------------------
out <- list(
  built = Sys.Date(),
  sector = ind |> select(-easting, -northing),
  sector_counts = list(
    schools = nrow(ind), pupils = sum(ind$pupils_gias, na.rm = TRUE),
    all_independent = sum(gias$group == "Independent schools"),
    inspectorate = table(ind$inspectorate, useNA = "ifany"),
    admissions = table(ind$admissions, useNA = "ifany"),
    sex = table(ind$sex, useNA = "ifany"), boarding = table(ind$boarding, useNA = "ifany"),
    state_secondary_pupils = sum(num(sp$`headcount of pupils`)[sp$phase_type_grouping == "State-funded secondary"], na.rm = TRUE),
    indep_all_pupils = sum(num(sp$`headcount of pupils`)[sp$phase_type_grouping == "Independent school"], na.rm = TRUE)),
  census_cov = census_cov, sen_cov = sen_cov,
  ks4 = k4 |> filter(sector %in% c("Independent", "State-funded mainstream")) |> select(URN, sector, school, tpup, att8, basics, ebacc_entry),
  ks4_cov = ks4_cov,
  alev = alev |> select(URN, sector5, alev_n, alev_aps, alev_grade, va_alev, va_alev_3, va_q, lo_q, hi_q, entries),
  alev_sum = alev_sum,
  k45 = k45 |> select(URN, sector, school, att8, alev_aps, alev_n), k45_cor = k45_cor, k45_flag = k45_flag,
  st_q1 = st_q1, st_aps_med = st_aps_med,
  reduced_test = reduced_test, drop_one = drop_one, by_kind = by_kind,
  reduced_coefs = data.frame(term = names(coef(m_red)), est = unname(coef(m_red)))
)
saveRDS(out, here::here("data", "independent.rds"))
message("Saved data/independent.rds")
print(ks4_cov); print(alev_sum); print(k45_cor); print(k45_flag); print(reduced_test); print(drop_one); print(by_kind)
print(census_cov); print(sen_cov)
