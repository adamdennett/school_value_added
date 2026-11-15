# 57_school_intakes_children.R - From neighbourhoods to school intakes
# ---------------------------------------------------------------------------
# R/54 and R/56 show where young children are few. This links that to schools' intakes, with
# published data only.
# Home area: each Census 2021 Output Area is given to its nearest state-funded mainstream primary
# school open in 2025/26 (straight line from the OA's population-weighted centre to the school;
# schools with Reception pupils in January 2026). A school's home area is the OAs nearest to it. That
# is a geographic stand-in for where pupils come from, not an admissions catchment.
# Home-area ratio: children 0-4 per 1,000 residents aged 20-44 across those OAs (Census 2021 TS007a).
#   The children aged 0-4 in March 2021 entered Reception from September 2021 to September 2025.
# A. England. Each primary's change in Reception pupils, January 2020 to January 2026 (school census;
#    100 x log change; URNs followed through one-to-one GIAS predecessor links, so academy conversions
#    keep their history), against its home-area ratio: within the same local authority, standard errors
#    clustered by authority, schools with 10+ Reception pupils in both years. Also against unfilled
#    places in 2024/25 (DfE School capacity, school level).
# B. Brighton and Hove primary planning areas: the council's own Reception forecasts to 2029/30 and
#    estimated Reception places (DfE School capacity 2024/25, place planning estimates), against each
#    planning area's home-area ratio (OAs whose nearest primary is in that planning area).
# C. Brighton and Hove primaries: home-area ratio against the change in Reception pupils. Published
#    figures show the schools unnamed apart from the two in the council's November 2026 admissions
#    proposals (Brunswick and Coldean). The named school table goes to data-raw/private (not
#    committed, not published); see the agreed rule on naming schools.
# Inputs: data-raw/census2021 (TS007a OA), E:/school_active_travel origins (OA centres), data-raw/ees
#   (school census 2019/20 to 2025/26, School capacity 2024/25, applications and offers 2026),
#   data-raw/gias (locations, predecessor links)
# Outputs: data/school_intakes_children.rds, data-raw/private/bh_primary_intakes.csv,
#   writeup/figures/intakes_*.png        Rscript R/57_school_intakes_children.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(fixest); library(ggplot2); library(sf); library(arrow) })
sf_use_s2(FALSE)
source(here::here("R", "site_helpers.R"))
SAT <- normalizePath(Sys.getenv("SCHOOL_ACTIVE_TRAVEL", file.path(here::here(), "..", "school_active_travel")), "/")
ees <- function(...) here::here("data-raw", "ees", ...)

# ---- school census: Reception pupils by school, January 2020 to January 2026 ----------------------
rec_col <- "Number of reception pupils (year R)"
read_spc <- function(path, yr) {
  h <- names(fread(path, nrows = 0)); u <- h[tolower(h) == "urn"][1]
  x <- fread(path, select = c(u, rec_col), colClasses = "character"); setnames(x, c("urn", "rec"))
  x[, .(urn = as.integer(urn), rec = suppressWarnings(as.numeric(rec)), year = yr)]
}
td <- file.path(tempdir(), "spc"); dir.create(td, showWarnings = FALSE)
P <- rbindlist(lapply(2019:2024, function(y) {
  z <- ees("spc_all", sprintf("spc_%d-%02d.zip", y, (y + 1) %% 100))
  f <- grep("supporting-files/spc_school_level_underlying_data", unzip(z, list = TRUE)$Name, value = TRUE)[1]
  unzip(z, files = f, exdir = td, overwrite = TRUE); read_spc(file.path(td, f), y) }))
P <- rbind(P, read_spc(ees("spc_school_level_underlying_data_2026.csv"), 2025L))
S26 <- fread(ees("spc_school_level_underlying_data_2026.csv"), select = c("urn", "school_name", "la_name", "phase_type_grouping", rec_col))
setnames(S26, rec_col, "rec")
# predecessor links: carry each old URN forward to the URN open now (one-to-one links only)
L <- fread(here::here("data-raw", "gias", "links_edubasealldata20260610.csv"), select = c("URN", "LinkURN", "LinkType"))
succ <- L[LinkType == "Predecessor", .(old = LinkURN, new = URN)][!duplicated(old)]
P[, urn_now := urn]
for (i in 1:5) { P[succ, on = .(urn_now = old), urn_now := i.new] }
P <- P[!is.na(rec), .(rec = sum(rec)), by = .(urn = urn_now, year)]

# ---- state primaries open in January 2026, and their locations --------------------------------------
G <- fread(here::here("data-raw", "gias", "edubasealldata20260610.csv"), select = c("URN", "Easting", "Northing"))
SP <- S26[phase_type_grouping == "State-funded primary" & rec > 0]
SP <- merge(SP, G[, .(urn = URN, e = Easting, n = Northing)], by = "urn")[!is.na(e) & e > 0]

# ---- Output Areas: young children and adults, nearest state primary ----------------------------------
A <- fread(text = readLines(unz(here::here("data-raw", "census2021", "census2021-ts007a.zip"), "census2021-ts007a-oa.csv"), warn = FALSE), check.names = FALSE)
ag <- function(lbl) A[[grep(lbl, names(A), fixed = TRUE)]]
OA <- data.table(oa = A[["geography code"]], age0_4 = ag("4 years and under"),
                 a20_44 = ag("20 to 24") + ag("25 to 29") + ag("30 to 34") + ag("35 to 39") + ag("40 to 44"))
O <- as.data.table(read_parquet(file.path(SAT, "data", "out", "origins.parquet"), col_select = c("oa21cd", "e", "n", "lad25nm")))
OA <- merge(OA, O[, .(oa = oa21cd, e, n, la = lad25nm)], by = "oa")
oa_sf <- st_as_sf(OA, coords = c("e", "n"), crs = 27700); sp_sf <- st_as_sf(SP, coords = c("e", "n"), crs = 27700)
OA[, urn := SP$urn[st_nearest_feature(oa_sf, sp_sf)]]
H <- OA[, .(oas = .N, age0_4 = sum(age0_4), a20_44 = sum(a20_44)), by = urn][, ratio := 1000 * age0_4 / a20_44]

# ---- A. England: does a low home-area ratio go with a falling intake? ----------------------------------
W <- dcast(P[year %in% c(2019, 2022, 2025)], urn ~ year, value.var = "rec"); setnames(W, c("urn", "r19", "r22", "r25"))
Cap <- fread(ees("school_capacity_2425", "data", "capacity_school_200910-202425.csv"),
             select = c("time_period", "school_urn", "primary_planning_area_code", "school_places", "unfilled_places_percent", "la_name"))[time_period == 202425]
E <- merge(SP[, .(urn, school_name, la_name, rec)], H, by = "urn")
E <- merge(E, W, by = "urn", all.x = TRUE)
E <- merge(E, Cap[, .(urn = school_urn, pa = suppressWarnings(as.integer(primary_planning_area_code)), places = school_places, unfilled = suppressWarnings(as.numeric(unfilled_places_percent)))], by = "urn", all.x = TRUE)
E[, `:=`(chg = 100 * log(r25 / r19), chg3 = 100 * log(r25 / r22))]
E[, ratio_z := (ratio - mean(ratio)) / sd(ratio)]
Ek <- E[r19 >= 10 & r25 >= 10 & a20_44 >= 100]
mA <- list(change = feols(chg ~ ratio_z | la_name, data = Ek, cluster = ~la_name),
           change_3yr = feols(chg3 ~ ratio_z | la_name, data = Ek[r22 >= 10], cluster = ~la_name),
           change_national = feols(chg ~ ratio_z, data = Ek, cluster = ~la_name),
           unfilled = feols(unfilled ~ ratio_z | la_name, data = Ek[!is.na(unfilled)], cluster = ~la_name))
cA <- rbindlist(lapply(mA, function(m) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[term == "ratio_z"]; setnames(ct, 2:3, c("b", "se"))
  ct[, .(b, se, lo = b - 1.96 * se, hi = b + 1.96 * se, n = nobs(m))] }), idcol = "model")
Ek[, decile := cut(ratio, quantile(ratio, 0:10 / 10), include.lowest = TRUE, labels = FALSE)]
dec <- Ek[, .(schools = .N, ratio_lo = min(ratio), ratio_hi = max(ratio), ratio = 1000 * sum(age0_4) / sum(a20_44),
              chg = 100 * log(sum(r25) / sum(r19)), unfilled = mean(unfilled, na.rm = TRUE)), by = decile][order(decile)]

# ---- B. Brighton and Hove planning areas: the council's forecasts ---------------------------------------
PE <- fread(ees("school_capacity_2425", "data", "place-planning-estimates_national-regional-la-pa_202425.csv"))
PE <- PE[la_name == "Brighton and Hove" & geographic_level == "Planning area" & education_phase == "Primary" & nc_year_group == "Reception",
         .(year = as.integer(substr(time_period, 1, 4)), pa = as.integer(planning_area_code), pa_name = planning_area_name, forecast = pupil_forecast, places = estimated_capacity)]
FC <- fread(ees("school_capacity_2425", "data", "forecasts-pupils-hdc-hif_national-regional-la-pa_202425.csv"))
FC <- FC[la_name == "Brighton and Hove" & geographic_level == "Planning area" & nc_year_group == "Reception",
         .(year = as.integer(substr(time_period, 1, 4)), pa = as.integer(planning_area_code), pa_name = planning_area_name, forecast = as.numeric(pupil_forecast_and_202425_actuals))]
PA <- merge(FC, PE[, .(year, pa, places)], by = c("year", "pa"), all.x = TRUE)
pa_places <- PE[, .(places = places[year == min(year)]), by = pa]       # estimated Reception places, 2025/26
PA <- merge(PA[, !"places"], pa_places, by = "pa")
B <- E[la_name == "Brighton and Hove"]
pa_ratio <- B[!is.na(pa), .(schools = .N, age0_4 = sum(age0_4), a20_44 = sum(a20_44), r19 = sum(r19, na.rm = TRUE), r25 = sum(r25, na.rm = TRUE)), by = pa][, ratio := 1000 * age0_4 / a20_44]
PAS <- merge(PA[, .(rec_2425 = forecast[year == 2024], rec_2930 = forecast[year == 2029], places = places[1]), by = .(pa, pa_name)], pa_ratio, by = "pa")
PAS[, `:=`(fc_chg = 100 * (rec_2930 / rec_2425 - 1), surplus_2930 = 100 * (1 - rec_2930 / places), name = tools::toTitleCase(tolower(sub(" Primary$", "", pa_name))))]
PAS[, name := sub("The 'deans'", "The Deans", sub("And", "and", name), ignore.case = TRUE)]

# ---- C. Brighton and Hove primaries ------------------------------------------------------------------------
AO <- fread(ees("admissions_2026", "AppsandOffers_2026_SchoolLevel29062026.csv"))
AO <- AO[time_period == max(time_period) & school_phase == "Primary" & la_name == "Brighton and Hove" & entry_year == "R", .(urn = as.integer(school_urn), offers_2026 = total_number_places_offered, first_prefs_2026 = times_put_as_1st_preference)]
B <- merge(B, AO, by = "urn", all.x = TRUE)
B[, named := school_name %in% c("Brunswick Primary School", "Coldean Primary School")]
stopifnot(B[named == TRUE, .N] == 2)
dir.create(here::here("data-raw", "private"), showWarnings = FALSE)
fwrite(B[order(ratio), .(urn, school_name, planning_area = pa, home_area_oas = oas, under5s = age0_4, adults_20_44 = a20_44, ratio = round(ratio),
                         reception_2019_20 = r19, reception_2022_23 = r22, reception_2025_26 = r25, change_pct = round(100 * (r25 / r19 - 1), 1),
                         capacity_2024_25 = places, unfilled_pct_2024_25 = round(unfilled, 1), offers_2026, first_prefs_2026)],
       here::here("data-raw", "private", "bh_primary_intakes.csv"))
mB <- feols(chg ~ ratio_z, data = B[r19 >= 10 & r25 >= 10], vcov = "hetero")
bh_cor <- B[r19 >= 10 & r25 >= 10, cor(ratio, chg)]
bh_tot <- B[, .(schools = .N, low = sum(ratio < 100), low_r19 = sum(r19[ratio < 100], na.rm = TRUE), low_r25 = sum(r25[ratio < 100], na.rm = TRUE),
                high_r19 = sum(r19[ratio >= 100], na.rm = TRUE), high_r25 = sum(r25[ratio >= 100], na.rm = TRUE))]

res <- list(coef_A = cA, deciles = dec, n_A = nrow(Ek), n_schools = nrow(E), r2_A = r2(mA$change, "wr2"),
            pa = PA, pa_summary = PAS, bh_coef = as.data.table(coeftable(mB), keep.rownames = "term"), bh_cor = bh_cor, bh_tot = bh_tot,
            bh_points = B[, .(ratio, chg, r19, r25, named, label = ifelse(named, sub(" School$", "", school_name), NA_character_))],
            bh_named = B[named == TRUE, .(school_name, ratio, r19, r22, r25, chg, offers_2026, places, unfilled)],
            bh_ratio_rank = B[, .(school_name, rank = frank(ratio))][school_name %in% c("Brunswick Primary School", "Coldean Primary School")], built = Sys.Date())
saveRDS(res, here::here("data", "school_intakes_children.rds"))
options(width = 160)
print(cA); print(dec); print(PAS[order(ratio)]); print(res$bh_coef); cat("BH cor", bh_cor, "\n"); print(bh_tot); print(res$bh_named); print(res$bh_ratio_rank)

# ---- figures -------------------------------------------------------------------------------------------------
fig <- here::here("writeup", "figures")
g1 <- ggplot(dec, aes(decile, chg)) + geom_hline(yintercept = 0, colour = GREY) + geom_col(fill = DPURPLE, width = 0.7) +
  scale_x_continuous(breaks = 1:10, labels = sprintf("%d\n(%.0f)", 1:10, dec$ratio)) +
  labs(x = "Home-area ratio, tenths of England's primaries (children 0-4 per 1,000 aged 20-44 in brackets)", y = "Change in Reception pupils,\nJanuary 2020 to January 2026 (%, log)") + theme_note(10)
ggsave(file.path(fig, "intakes_deciles.png"), g1, width = 9, height = 4.2, dpi = 150)
PAS2 <- PAS[order(ratio)][, name := factor(name, name)]
g2 <- ggplot(PAS2, aes(ratio, surplus_2930)) + geom_point(colour = DPURPLE, size = 3) +
  ggrepel::geom_text_repel(aes(label = name), size = 3, seed = 1) +
  labs(x = "Planning area home-area ratio: children 0-4 per 1,000 aged 20-44 (Census 2021)", y = "Unfilled Reception places, 2029/30,\non the council's forecast (%)") + theme_note(10)
ggsave(file.path(fig, "intakes_bh_planning_areas.png"), g2, width = 8, height = 4.6, dpi = 150)
BP <- res$bh_points[r19 >= 10 & r25 >= 10]
g3 <- ggplot(BP, aes(ratio, chg)) + geom_hline(yintercept = 0, colour = GREY) + geom_smooth(method = "lm", formula = y ~ x, colour = MPURPLE, fill = MPURPLE, alpha = 0.15) +
  geom_point(aes(fill = named), shape = 21, size = 2.8, colour = "white", stroke = 0.4) +
  scale_fill_manual(values = c(`FALSE` = DPURPLE, `TRUE` = "#ffd23f"), guide = "none") +
  ggrepel::geom_label_repel(data = BP[named == TRUE], aes(label = label), size = 2.9, fill = "white", label.size = 0.15, seed = 3, box.padding = 0.6) +
  labs(x = "Home-area ratio: children 0-4 per 1,000 aged 20-44 (Census 2021)", y = "Change in Reception pupils,\nJanuary 2020 to January 2026 (%, log)") + theme_note(10)
ggsave(file.path(fig, "intakes_bh_schools.png"), g3, width = 8, height = 4.6, dpi = 150)
