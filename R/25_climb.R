# 25_climb.R - Does the climb to and from school go with absence, across England?
# ---------------------------------------------------------------------------
# R/21 found a link between absence and the metres climbed on the walking route, in
# 62 schools in 8 authorities: too few to trust, and found while looking for something
# else. This tests it properly. Climb needs no routing as a first approximation:
# elevation from OS Terrain 50 is sampled every 50 m along the straight line from every
# Output Area in England to its nearest state secondary school, in each direction.
# In the routed places this is checked against climb along the real route.
#
# Tests, all with controls for distance:
#   A. Schools: all pupils (no split is published), within authority, with intake,
#      ethnic make-up and kind of place. 2025/26, and absence above what intake
#      predicts, 2021/22 to 2024/25.
#   B. Within authorities, by IDACI tenth of home neighbourhood: all pupils,
#      2023/24 and 2024/25. Primary absence as a falsification test.
#   C. Authorities, for all three groups of pupils, given each group's ethnic mix.
#
# Sources: as R/19, and OS Terrain 50. method.qmd#sec-journey-method.
# Output: data/climb.rds (committed). Rscript R/25_climb.R
# ---------------------------------------------------------------------------

proj_db <- system.file("proj", package = "terra"); if (nzchar(proj_db)) Sys.setenv(PROJ_LIB = proj_db, PROJ_DATA = proj_db)
source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(fixest) })
setFixest_notes(FALSE)
num <- function(x) suppressWarnings(as.numeric(x)); BH <- "Brighton and Hove"
wm <- function(x, w) { ok <- !is.na(x) & !is.na(w); sum(x[ok] * w[ok]) / sum(w[ok]) }
J <- here::here("data", "cache", "journey"); ons <- here::here("data-raw", "census2021", "ons_custom"); raw <- here::here("data-raw", "census2021")
FR <- readRDS(file.path(J, "frame.rds")); sch <- as.data.table(FR$sch)

# ---- 1. Elevation -----------------------------------------------------------------
dem_f <- here::here("data", "cache", "journey", "terrain50_gb.vrt")
tile_dir <- here::here("data-raw", "r5", "terrain50")
if (!dir.exists(tile_dir)) {
  dir.create(tile_dir); z <- here::here("data-raw", "r5", "src", "terr50_gagg_gb.zip"); fl <- unzip(z, list = TRUE)$Name; fl <- fl[grepl("OST50GRID.*\\.zip$", fl)]
  td <- tempfile(); dir.create(td); unzip(z, files = fl, exdir = td, junkpaths = TRUE)
  for (f in list.files(td, full.names = TRUE)) { n <- unzip(f, list = TRUE)$Name; unzip(f, files = n[grepl("\\.asc$", n)], exdir = tile_dir) }
}
dem <- terra::vrt(list.files(tile_dir, pattern = "\\.asc$", full.names = TRUE), filename = dem_f, overwrite = TRUE)

# ---- 2. Every Output Area in England to its nearest school ----------------------------
oa <- as.data.table(readRDS(here::here("data", "cache", "oa_pwc_2021.rds")))[startsWith(oa21cd, "E"), .(oa21cd, e = num(easting), n = num(northing))][!is.na(e)]
lk <- fread(file.path(ons, "oa_lsoa_msoa_2021.csv"), select = c("OA21CD", "LSOA21CD")); setnames(lk, c("oa21cd", "lsoa")); oa <- merge(oa, lk, by = "oa21cd")
k <- fread(file.path(raw, "ts007a", "census2021-ts007a-oa.csv"), select = c("geography code", "Age: Aged 10 to 14 years")); setnames(k, c("oa21cd", "kids")); oa <- merge(oa, k, by = "oa21cd")
nn <- nabor::knn(as.matrix(sch[, .(e, n)]), as.matrix(oa[, .(e, n)]), k = 1); oa[, `:=`(URN = sch$URN[nn$nn.idx[, 1]], metres = nn$nn.dists[, 1], se = sch$e[nn$nn.idx[, 1]], sn = sch$n[nn$nn.idx[, 1]])]
oa[, steps := pmax(ceiling(metres / 50), 1L)]
pts <- oa[, .(i = seq(0, steps)), by = .(oa21cd, e, n, se, sn, steps)][, `:=`(x = e + (se - e) * i / steps, y = n + (sn - n) * i / steps)]
message(format(nrow(oa), big.mark = ","), " Output Areas, ", format(nrow(pts), big.mark = ","), " points")
pts[, z := terra::extract(dem, as.matrix(pts[, .(x, y)]))[[1]]]
cl <- pts[!is.na(z)][order(oa21cd, i)][, { dz <- diff(z); .(up_out = sum(pmax(dz, 0)), up_back = sum(pmax(-dz, 0)), net = z[.N] - z[1]) }, by = oa21cd]
oa <- merge(oa, cl, by = "oa21cd", all.x = TRUE); oa[, km := metres / 1000]; rm(pts); gc(FALSE)

# Check against the climb along routed walks, in the nine places
ss <- rbindlist(lapply(list.files(J, pattern = "^struct_.*\\.rds$", full.names = TRUE), function(f) readRDS(f)$walk_route[, .(oa21cd = id, r_out = up_out, r_back = up_back, r_m = metres)]))
chk <- merge(oa, ss, by = "oa21cd"); check <- chk[, .(oas = .N, r_out = cor(up_out, r_out, use = "complete.obs"), r_back = cor(up_back, r_back, use = "complete.obs"), mean_line_out = mean(up_out, na.rm = TRUE), mean_route_out = mean(r_out, na.rm = TRUE), mean_line_back = mean(up_back, na.rm = TRUE), mean_route_back = mean(r_back, na.rm = TRUE))]

# ---- 3. Neighbourhood context, as R/19 -------------------------------------------------
census <- function(tb) { x <- fread(file.path(raw, tb, sprintf("census2021-%s-lsoa.csv", tb))); setnames(x, sub("; measures: Value$", "", names(x))); x }
col1 <- function(x, pat) { n <- grep(pat, names(x), value = TRUE); stopifnot(length(n) == 1); x[[n]] }
t11 <- census("ts011"); t67 <- census("ts067"); t37 <- census("ts037"); t21 <- census("ts021"); t54 <- census("ts054"); t45 <- census("ts045"); t06 <- census("ts006")
dc <- grep("deprived in (two|three|four)", names(t11), value = TRUE)
L <- data.table(lsoa = t11$`geography code`, hh = col1(t11, "Total: All households"), dep2 = 100 * rowSums(t11[, ..dc]) / col1(t11, "Total: All households"), adults = col1(t67, "Total: All usual residents aged 16"), grad_n = col1(t67, "Level 4 qualifications and above"), pop = col1(t37, "Total: All usual residents"),
                bad_n = col1(t37, "General health: Bad health") + col1(t37, "General health: Very bad health"), wbri_n = col1(t21, "White: English, Welsh, Scottish, Northern Irish or British"), social_n = col1(t54, "^Tenure of household: Social rented$"), nocar_n = col1(t45, "No cars or vans"), density = col1(t06, "Persons per square kilometre"))
ruc <- fread(file.path(ons, "lsoa_ruc21.csv")); utla <- fread(file.path(ons, "lsoa_utla23.csv")); L <- merge(L, ruc[, .(lsoa = LSOA21CD, urban = startsWith(RUC21NM, "Urban"))], by = "lsoa"); L <- merge(L, utla[, .(lsoa = LSOA21CD, la = fcase(UTLA23NM == "Bristol", "Bristol, City of", UTLA23NM == "Herefordshire", "Herefordshire, County of", UTLA23NM == "Kingston upon Hull", "Kingston upon Hull, City of", default = UTLA23NM))], by = "lsoa")
io <- fread(here::here("data-raw", "iod2019", "File_7_IoD2019.csv")); io <- data.table(l11 = io[[grep("^LSOA code", names(io), value = TRUE)]], score = io[[grep("IDACI\\) Score", names(io), value = TRUE)]], dec = io[[grep("IDACI\\) Decile", names(io), value = TRUE)]])
cuts <- io[, .(lo = min(score)), by = dec][order(-lo)]; lkk <- fread(file.path(ons, "lsoa11_lsoa21.csv")); id <- merge(lkk[startsWith(LSOA21CD, "E"), .(l11 = LSOA11CD, lsoa = LSOA21CD)], io, by = "l11")[, .(idaci = mean(score)), by = lsoa]; id[, dec := vapply(idaci, function(s) min(cuts$dec[s >= cuts$lo], 10L), 1)]
L <- merge(L, id, by = "lsoa", all.x = TRUE); oa <- merge(oa, L, by = "lsoa")

# ---- A. Schools -------------------------------------------------------------------------
frame <- as.data.table(readRDS(here::here("data", "absence_school_frame.rds")))[, URN := as.integer(URN)]
PA <- as.data.table(readRDS(here::here("data", "place_absence.rds"))$abs_schools)[, URN := as.integer(URN)]
sq <- oa[, .(kids_n = sum(kids), km = wm(km, kids), up_out = wm(up_out, kids), up_back = wm(up_back, kids), up_two = wm(up_out + up_back, kids), net = wm(net, kids)), by = URN]
S <- merge(frame, sq, by = "URN"); S <- merge(S, PA[, .(URN, abs_excess)], by = "URN", all.x = TRUE); S[, `:=`(l2 = log2(km), selective = grepl("Selective", ADMPOL_PT))]; S <- S[kids_n >= 100 & !selective & !is.na(overall)]
CTRL <- "fsm + eal + wbri + ks2_c + z_l4 + sen_sup + ehcp + mobile + girls + age16 + place"
OUT <- c(overall = "Overall absence", illness = "Illness", unauth = "Unauthorised", persistent = "Persistent absence (% of pupils)", abs_excess = "Above intake, 2021/22 to 2024/25")
fit_s <- function(y, x, dd, ctl = CTRL, keep = TRUE) { dd <- dd[!is.na(get(y)) & !is.na(get(x))]; m <- feols(as.formula(paste0(y, " ~ ", x, " + l2", if (nzchar(ctl)) paste0(" + ", ctl), " | la")), dd, cluster = ~la); data.table(b = coef(m)[[x]], se = se(m)[[x]], b_km = coef(m)[["l2"]], se_km = se(m)[["l2"]], n = m$nobs, sd_x = sd(dd[[x]])) }
SAMP <- list(`All schools` = S, `Urban` = S[grepl("^Urban", place)], `Urban, near a major town or city` = S[place == "Urban, near a major town or city" | grepl("Nearer", place)], Rural = S[!grepl("^Urban", place)], `Without London` = S[region != "London"], `High free school meals` = S[fsm >= quantile(fsm, .75, na.rm = TRUE)], `Without Brighton and Hove` = S[la != BH])
sch_models <- rbindlist(lapply(names(OUT), function(y) rbindlist(lapply(c("up_out", "up_back"), function(x) rbindlist(lapply(names(SAMP), function(sa) fit_s(y, x, SAMP[[sa]])[, `:=`(outcome = y, term = x, sample = sa)]))))))
sch_nocontrol <- rbindlist(lapply(names(OUT), function(y) fit_s(y, "up_back", S, ctl = "place")[, outcome := y]))
sch_int <- rbindlist(lapply(names(OUT), function(y) { dd <- S[!is.na(get(y))][, fsm_c := fsm - mean(fsm, na.rm = TRUE)]; m <- feols(as.formula(paste0(y, " ~ up_back * fsm_c + l2 + eal + wbri + ks2_c + z_l4 + sen_sup + ehcp + mobile + girls + age16 + place | la")), dd, cluster = ~la); data.table(outcome = y, b = coef(m)[["up_back"]], se = se(m)[["up_back"]], b_int = coef(m)[["up_back:fsm_c"]], se_int = se(m)[["up_back:fsm_c"]]) }))
# Which is it: going, or coming home, once both are in?
both <- rbindlist(lapply(names(OUT), function(y) { dd <- S[!is.na(get(y))]; m <- feols(as.formula(paste0(y, " ~ up_out + up_back + l2 + ", CTRL, " | la")), dd, cluster = ~la); data.table(outcome = y, b_out = coef(m)[["up_out"]], se_out = se(m)[["up_out"]], b_back = coef(m)[["up_back"]], se_back = se(m)[["up_back"]]) }))
shape <- { qq <- S[, .(q = cut(up_back, c(-1, 10, 20, 40, 60, 100, Inf), labels = c("Under 10 m", "10 to 20 m", "20 to 40 m", "40 to 60 m", "60 to 100 m", "Over 100 m")), overall, unauth, persistent, l2, fsm, eal, wbri, ks2_c, z_l4, sen_sup, ehcp, mobile, girls, age16, place, la)]
  rbindlist(lapply(c("overall", "persistent"), function(y) { m <- feols(as.formula(paste0(y, " ~ i(q, ref = 'Under 10 m') + l2 + ", CTRL, " | la")), qq, cluster = ~la); cf <- coef(m); k <- grep("^q::", names(cf)); rbind(data.table(band = "Under 10 m", b = 0, se = 0), data.table(band = sub("^q::", "", names(cf)[k]), b = cf[k], se = se(m)[k]))[, `:=`(outcome = y, schools = vapply(band, function(bn) sum(qq$q == bn, na.rm = TRUE), 1L))] })) }

# ---- B. Within authorities, by tenth of home neighbourhood ---------------------------------
rs <- fread(here::here("data-raw", "ees", "ees_absence_residence_idaci.csv"))[geographic_level == "Local authority" & measure == "IDACI" & education_phase %in% c("State-funded secondary", "State-funded primary") & !pup_idaci %in% c("Total", "IDACI unclassified")]
rs <- rs[, .(tp = as.integer(time_period), la = la_name, dec = as.integer(sub("^(\\d+) -.*", "\\1", pup_idaci)) / 10 + 1, phase = fifelse(education_phase == "State-funded secondary", "sec", "pri"), enrol = num(enrolments), overall = num(sess_overall_percent), unauth = num(sess_unauthorised_percent), persistent = num(enrolments_pa_10_exact_percent))][!is.na(overall) & overall > 0 & enrol >= 30]
ce <- oa[!is.na(dec), .(km = wm(km, kids), up_out = wm(up_out, kids), up_back = wm(up_back, kids), urban_c = 100 * wm(urban, kids), dens_c = wm(log(pmax(density, 1)), kids), grad_c = 100 * sum(grad_n / 1) / sum(adults), wbri_c = 100 * sum(wbri_n) / sum(pop), nocar_c = 100 * sum(nocar_n) / sum(hh), social_c = 100 * sum(social_n) / sum(hh), bad_c = 100 * sum(bad_n) / sum(pop)), by = .(la, dec)]
B <- merge(rs, ce, by = c("la", "dec")); B[, band := fcase(dec <= 3, "Most deprived three tenths", dec <= 7, "Middle four tenths", default = "Least deprived three tenths")]
BC <- "km + urban_c + dens_c + grad_c + wbri_c + nocar_c + social_c + bad_c"
fit_b <- function(y, x, dd, ctl = BC) { m <- feols(as.formula(paste0(y, " ~ ", x, " + ", ctl, " | la^tp + dec^tp")), dd, weights = ~enrol, cluster = ~la); data.table(b = coef(m)[[x]], se = se(m)[[x]], n = m$nobs, las = uniqueN(dd$la), sd_x = sd(dd[[x]])) }
cell_models <- rbindlist(lapply(c("overall", "unauth", "persistent"), function(y) rbindlist(lapply(c("up_out", "up_back"), function(x) rbindlist(lapply(c("sec", "pri"), function(ph) rbindlist(lapply(c("All tenths", "Most deprived three tenths", "Middle four tenths", "Least deprived three tenths"), function(bn) { dd <- B[phase == ph & (bn == "All tenths" | band == bn)]; fit_b(y, x, dd)[, `:=`(outcome = y, term = x, phase = ph, band = bn)] }))))))))
cell_nocontrol <- rbindlist(lapply(c("sec", "pri"), function(ph) fit_b("overall", "up_back", B[phase == ph], ctl = "km")[, phase := ph]))

# ---- C. Authorities, all three groups ---------------------------------------------------------
ch <- fread(here::here("data-raw", "ees", "ees_absence_characteristics.csv"), select = c("time_period", "geographic_level", "la_name", "education_phase", "breakdown", "enrolments", "sess_possible", "sess_overall"))
ch <- unique(ch[geographic_level == "Local authority" & education_phase %in% c("State-funded secondary", "State-funded primary") & breakdown %in% c("FSM eligible in last 6 years", "FSM not eligible in last 6 years", "Total")], by = c("time_period", "la_name", "education_phase", "breakdown"))
ch[, `:=`(g = fcase(breakdown == "Total", "all", startsWith(breakdown, "FSM eligible"), "dis", default = "non"), phase = fifelse(education_phase == "State-funded secondary", "sec", "pri"), abs = 100 * num(sess_overall) / num(sess_possible), e = num(enrolments), tp = as.integer(time_period), la = la_name)]
shr <- dcast(ch, tp + la + phase ~ g, value.var = "e")[, share := 100 * dis / (dis + non)]
la_x <- oa[!is.na(dep2)][, .(n_lsoa = uniqueN(lsoa), km_dep = wm(km[dep2 >= quantile(dep2, 2 / 3)], kids[dep2 >= quantile(dep2, 2 / 3)]), up_dep = wm(up_back[dep2 >= quantile(dep2, 2 / 3)], kids[dep2 >= quantile(dep2, 2 / 3)]), up_out_dep = wm(up_out[dep2 >= quantile(dep2, 2 / 3)], kids[dep2 >= quantile(dep2, 2 / 3)]), urban_share = 100 * wm(urban, kids), wbri = 100 * sum(wbri_n) / sum(pop)), by = la][n_lsoa >= 30]
reg <- as.data.table(readRDS(here::here("data", "inputs.rds")))[!is.na(gor_name), .(la = as.character(LANAME), gor = as.character(gor_name))]; reg <- unique(reg, by = "la")
mx <- fread(here::here("data-raw", "ees", "fsm_ethnicity_la.csv"), colClasses = "character")[characteristic != "Unclassified"]
mx <- mx[, .(tp = as.integer(time_period), la = la_name, phase = fifelse(phase_type_grouping == "State-funded secondary", "sec", "pri"), g = fifelse(grepl("Known", fsm_eligibility), "dis", "non"), wb = characteristic == "White - White British", n = num(number_of_pupils))]
mxx <- rbind(mx[, .(eth_wb = 100 * sum(n[wb], na.rm = TRUE) / sum(n, na.rm = TRUE)), by = .(tp, la, phase, g)], mx[, .(eth_wb = 100 * sum(n[wb], na.rm = TRUE) / sum(n, na.rm = TRUE), g = "all"), by = .(tp, la, phase)])
A <- merge(ch[, .(tp, la, phase, g, abs)], shr[, .(tp, la, phase, share)], by = c("tp", "la", "phase")); A <- merge(A, la_x, by = "la"); A <- merge(A, reg, by = "la", all.x = TRUE); A <- merge(A, mxx, by = c("tp", "la", "phase", "g"), all.x = TRUE); A <- A[!is.na(abs)]
la_models <- rbindlist(lapply(c("dis", "non", "all"), function(gr) rbindlist(lapply(c("sec", "pri"), function(ph) rbindlist(lapply(list(`Share, urbanity, distance` = "share + urban_share + km_dep", `And ethnic mix of the group` = "share + urban_share + km_dep + eth_wb", `And region` = "share + urban_share + km_dep + eth_wb + gor"), function(ct) { d <- A[g == gr & phase == ph & !is.na(eth_wb)]; m <- feols(as.formula(paste0("abs ~ up_dep + ", ct, " | tp")), d, cluster = ~la); data.table(group = gr, phase = ph, b = coef(m)[["up_dep"]], se = se(m)[["up_dep"]], n = m$nobs, las = uniqueN(d$la), sd_x = sd(d$up_dep), control = ct) })))))); la_models[, control := fcase(control == "share + urban_share + km_dep", "Share, urbanity, distance", control == "share + urban_share + km_dep + eth_wb", "And ethnic mix of the group", default = "And region")]

# Places
places <- oa[, .(km = wm(km, kids), up_out = wm(up_out, kids), up_back = wm(up_back, kids)), by = la][order(-up_back)][, rank := .I]
out <- list(built = Sys.Date(), check = check, n_oa = nrow(oa), n_tiles = length(list.files(tile_dir, pattern = "\\.asc$")), sch_models = sch_models, sch_nocontrol = sch_nocontrol, sch_int = sch_int, both = both, shape = shape, n_sch = nrow(S), cell_models = cell_models, cell_nocontrol = cell_nocontrol, la_models = la_models, places = places, up_sd = S[, sd(up_back, na.rm = TRUE)], up_qs = quantile(S$up_back, c(.1, .5, .9), na.rm = TRUE))
saveRDS(out, here::here("data", "climb.rds")); message("Saved data/climb.rds")
