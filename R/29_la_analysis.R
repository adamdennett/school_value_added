# 29_la_analysis.R - Journeys against absence in the 109 mostly urban authorities
# ---------------------------------------------------------------------------
# Reads R/27 and R/28 and asks:
#   1. Is Brighton and Hove unusual? Journey times, changes, waits, what happens if a bus
#      is missed and how many schools can be reached, for its most deprived third of
#      neighbourhoods, the rest and everyone, ranked among the 109.
#   2. Does time say more about absence than distance? Three independent tests: tenths of
#      home neighbourhood within authority (all pupils), schools within authority (all
#      pupils), and authorities (disadvantaged, non-disadvantaged, all pupils).
#   3. Do changes of bus, waits, missing a bus and the climb show up with this many places?
#   4. Do points of interest around stops and along walking routes show up?
#   Primary absence is the falsification test: a secondary journey should not move it.
#
# Journeys go to each Output Area's quickest school, not the school a child attends, and
# deprivation is of the neighbourhood, so this is a stand-in for what children do.
# Sources and formulas: method.qmd#sec-la-routing-method and #sec-poi-method.
# Output: data/journey_la.rds (committed). Rscript R/29_la_analysis.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(fixest) })
setFixest_notes(FALSE)
num <- function(x) suppressWarnings(as.numeric(x)); BH <- "Brighton and Hove"
wm <- function(x, w) { ok <- !is.na(x) & !is.na(w); if (!any(ok) || sum(w[ok]) == 0) return(NA_real_); sum(x[ok] * w[ok]) / sum(w[ok]) }
J <- here::here("data", "cache", "journey_la"); ons <- here::here("data-raw", "census2021", "ons_custom"); raw <- here::here("data-raw", "census2021")
FR <- readRDS(file.path(J, "frame.rds")); N <- readRDS(file.path(J, "nets.rds")); sch <- as.data.table(FR$sch)
FIX <- c("Bristol" = "Bristol, City of", "Kingston upon Hull" = "Kingston upon Hull, City of")
rdset <- function(p) { fs <- list.files(J, pattern = paste0("^", p, "_.*\\.rds$"), full.names = TRUE); lapply(fs, readRDS) }

# ---- 1. Output Areas ------------------------------------------------------------------
FL <- rdset("flat"); message(length(FL), " networks routed")
tm <- rbindlist(lapply(FL, function(f) f$times)); reach <- rbindlist(lapply(FL, function(f) f$reach))
q <- tm[order(scenario, from_id, t)][, .(t1 = t[1], t3 = mean(t[seq_len(min(.N, 3))]), school = to_id[1]), by = .(scenario, from_id)]
qw <- dcast(q, from_id ~ scenario, value.var = c("t1", "t3", "school"))
setnames(qw, c("t1_walk_bus", "t3_walk_bus", "school_walk_bus", "t1_walk", "school_walk"), c("t_bus", "t3_bus", "school", "t_walk", "school_foot"))
if ("t1_walk_bus_f" %in% names(qw)) setnames(qw, "t1_walk_bus_f", "t_bus_f") else qw[, t_bus_f := NA_real_]
rb <- reach[scenario == "walk_bus", .(from_id, n30, n45, t_open, n_open30)]
# Neighbourhood character, as R/19 and R/25
census <- function(tb) { x <- fread(file.path(raw, tb, sprintf("census2021-%s-lsoa.csv", tb))); setnames(x, sub("; measures: Value$", "", names(x))); x }
col1 <- function(x, pat) { n <- grep(pat, names(x), value = TRUE); stopifnot(length(n) == 1); x[[n]] }
t11 <- census("ts011"); t67 <- census("ts067"); t37 <- census("ts037"); t21 <- census("ts021"); t54 <- census("ts054"); t45 <- census("ts045"); t06 <- census("ts006")
dc <- grep("deprived in (two|three|four)", names(t11), value = TRUE)
L <- data.table(lsoa = t11$`geography code`, hh = col1(t11, "Total: All households"), dep2 = 100 * rowSums(t11[, ..dc]) / col1(t11, "Total: All households"), adults = col1(t67, "Total: All usual residents aged 16"), grad_n = col1(t67, "Level 4 qualifications and above"), pop = col1(t37, "Total: All usual residents"),
                bad_n = col1(t37, "General health: Bad health") + col1(t37, "General health: Very bad health"), wbri_n = col1(t21, "White: English, Welsh, Scottish, Northern Irish or British"), asian_n = col1(t21, "^Ethnic group: Asian, Asian British or Asian Welsh$"), black_n = col1(t21, "^Ethnic group: Black, Black British, Black Welsh, Caribbean or African$"),
                social_n = col1(t54, "^Tenure of household: Social rented$"), nocar_n = col1(t45, "No cars or vans"), density = col1(t06, "Persons per square kilometre"))
io <- fread(here::here("data-raw", "iod2019", "File_7_IoD2019.csv")); io <- data.table(l11 = io[[grep("^LSOA code", names(io), value = TRUE)]], score = io[[grep("IDACI\\) Score", names(io), value = TRUE)]], dec = io[[grep("IDACI\\) Decile", names(io), value = TRUE)]])
cuts <- io[, .(lo = min(score)), by = dec][order(-lo)]; lkk <- fread(file.path(ons, "lsoa11_lsoa21.csv"))
id <- merge(lkk[startsWith(LSOA21CD, "E"), .(l11 = LSOA11CD, lsoa = LSOA21CD)], io, by = "l11")[, .(idaci = mean(score)), by = lsoa]; id[, idaci_dec := vapply(idaci, function(s) min(cuts$dec[s >= cuts$lo], 10L), 1)]
L <- merge(L, id, by = "lsoa", all.x = TRUE)
oa <- merge(as.data.table(FR$oa), L, by = "lsoa"); oa[, la := fifelse(la %in% names(FIX), FIX[la], la)]
th <- unique(oa[, .(la, lsoa, dep2)])[, third := dep2 >= quantile(dep2, 2 / 3), by = la]; oa <- merge(oa, th[, .(lsoa, third)], by = "lsoa")
nn <- nabor::knn(as.matrix(sch[, .(e, n)]), as.matrix(oa[, .(e, n)]), k = 1); oa[, km1 := nn$nn.dists[, 1] / 1000]
oa <- merge(oa, qw[, .(from_id, t_bus, t3_bus, t_walk, t_bus_f, school, school_foot)], by.x = "oa21cd", by.y = "from_id", all.x = TRUE); oa <- merge(oa, rb, by.x = "oa21cd", by.y = "from_id", all.x = TRUE)
# What journeys are made of, averaged over the three departures
ST <- rdset("struct"); stg <- rbindlist(lapply(ST, function(s) s$structure))
jny <- stg[, .(boardings = mean(n_bus), p_walk = mean(n_bus == 0), p_one = mean(n_bus == 1), p_two = mean(n_bus >= 2), wait = mean(wait), walk_m = mean(walk_m), first_walk = mean(first_walk_m, na.rm = TRUE), total = mean(total)), by = from_id]
oa <- merge(oa, jny, by.x = "oa21cd", by.y = "from_id", all.x = TRUE)
WK <- rdset("walk"); wr <- rbindlist(lapply(WK, function(w) w$walk_route))[, .(oa21cd = id, metres, up_out, up_back, t_flat, t_toschool = t_out, t_home = t_back)]; oa <- merge(oa, wr, by = "oa21cd", all.x = TRUE); rm(WK); gc(FALSE)
MS <- rdset("miss"); if (length(MS)) { ms <- rbindlist(lapply(MS, function(m) m$miss)); oa <- merge(oa, ms[bell == "bell_0845", .(oa21cd = from_id, late, options)], by = "oa21cd", all.x = TRUE); oa <- merge(oa, ms[bell == "bell_0830", .(oa21cd = from_id, late30 = late)], by = "oa21cd", all.x = TRUE) } else oa[, `:=`(late = NA_real_, options = NA_real_, late30 = NA_real_)]
PO <- rdset("poi"); POI <- character()
if (length(PO)) { pp <- rbindlist(lapply(PO, function(p) p$oa), fill = TRUE); POI <- setdiff(names(pp), "from_id"); for (k in POI) set(pp, which(is.na(pp[[k]])), k, 0); oa <- merge(oa, pp, by.x = "oa21cd", by.y = "from_id", all.x = TRUE) }
oa[, `:=`(over30 = fifelse(is.na(t_bus), 1, as.numeric(t_bus > 30)), over45 = fifelse(is.na(t_bus), 1, as.numeric(t_bus > 45)), late10 = as.numeric(late >= 10), late_m = 1)]
no_route <- oa[, .(oas = .N, no_bus = sum(is.na(t_bus)), no_walk = sum(is.na(t_walk)), kids_no_bus = sum(kids[is.na(t_bus)])), by = la]
VARS <- c("t_bus", "t3_bus", "t_walk", "t_bus_f", "t_open", "km1", "n30", "n45", "n_open30", "over30", "over45", "p_walk", "p_one", "p_two", "boardings", "wait", "first_walk", "walk_m", "late", "late30", "late10", "options", "metres", "up_out", "up_back", "t_flat", "t_toschool", "t_home", POI)
summ <- function(d, w = d$kids) setNames(lapply(VARS, function(v) wm(d[[v]], w)), VARS)

# ---- 2. Places, for the neighbourhood groups ----------------------------------------
gr <- rbind(oa[, c(.(kids = sum(kids), oas = .N), summ(.SD)), by = la][, grp := "all"],
            oa[third == TRUE, c(.(kids = sum(kids), oas = .N), summ(.SD)), by = la][, grp := "dep"], oa[third == FALSE, c(.(kids = sum(kids), oas = .N), summ(.SD)), by = la][, grp := "rest"],
            # Two checks on "deprived". IDACI weights: a child in an income-deprived family, and any other child, given where they live
            oa[!is.na(idaci), c(.(kids = sum(kids * idaci), oas = .N), summ(.SD, .SD$kids * .SD$idaci)), by = la][, grp := "idaci_dep"], oa[!is.na(idaci), c(.(kids = sum(kids * (1 - idaci)), oas = .N), summ(.SD, .SD$kids * (1 - .SD$idaci))), by = la][, grp := "idaci_non"],
            # England's most deprived three tenths of neighbourhoods by IDACI, where a place has any
            oa[idaci_dec <= 3, c(.(kids = sum(kids), oas = .N), summ(.SD)), by = la][, grp := "dec3"], oa[idaci_dec > 3, c(.(kids = sum(kids), oas = .N), summ(.SD)), by = la][, grp := "dec_rest"])
P <- dcast(gr, la ~ grp, value.var = c("kids", "oas", VARS))
reg <- as.data.table(readRDS(here::here("data", "inputs.rds")))[!is.na(gor_name), .(la = as.character(LANAME), gor = as.character(gor_name))]; reg <- unique(reg, by = "la")
ctx <- oa[, .(urban_share = 100 * wm(urban, kids), log_density = wm(log(pmax(density, 1)), kids), graduates = 100 * sum(unique(data.table(lsoa, grad_n))$grad_n) / sum(unique(data.table(lsoa, adults))$adults), bad_health = 100 * sum(unique(data.table(lsoa, bad_n))$bad_n) / sum(unique(data.table(lsoa, pop))$pop),
              white_british = 100 * sum(unique(data.table(lsoa, wbri_n))$wbri_n) / sum(unique(data.table(lsoa, pop))$pop), social_rent = 100 * sum(unique(data.table(lsoa, social_n))$social_n) / sum(unique(data.table(lsoa, hh))$hh), no_car = 100 * sum(unique(data.table(lsoa, nocar_n))$nocar_n) / sum(unique(data.table(lsoa, hh))$hh), deprived = wm(dep2, kids)), by = la]
P <- merge(merge(P, ctx, by = "la"), reg, by = "la", all.x = TRUE)
P[, in_models := la != "Hastings"]; P[, london := gor %in% "London"]
for (v in c("t_bus", "t_walk", "t_bus_f", "t3_bus", "t_open", "km1", "p_two", "boardings", "wait", "late", "over30", "over45", "n30", "up_back")) { P[[paste0(v, "_gap")]] <- P[[paste0(v, "_dep")]] - P[[paste0(v, "_rest")]]; P[[paste0(v, "_igap")]] <- P[[paste0(v, "_idaci_dep")]] - P[[paste0(v, "_idaci_non")]]; P[[paste0(v, "_dgap")]] <- P[[paste0(v, "_dec3")]] - P[[paste0(v, "_dec_rest")]] }
P[, `:=`(mpk_dep = t_bus_dep / km1_dep, mpk_rest = t_bus_rest / km1_rest, ratio = t_bus_dep / t_bus_rest)]

# ---- 3. Is Brighton and Hove unusual? ---------------------------------------------------
MEAS <- list(
  c("t_bus_dep", "Minutes by walking and bus, deprived third", 1, 1), c("t_bus_rest", "Minutes by walking and bus, other neighbourhoods", 1, 1), c("t_bus_all", "Minutes by walking and bus, all", 1, 1), c("t_bus_gap", "Extra minutes by walking and bus for the deprived third", 1, 1), c("ratio", "Ratio of those minutes, deprived third to the rest", 1, 2),
  c("t_bus_igap", "Extra minutes, child in an income-deprived family against any other (IDACI)", 1, 1), c("t_bus_dgap", "Extra minutes, England's most deprived three tenths against other neighbourhoods", 1, 1), c("t_bus_f_gap", "Extra minutes, walking faster (4.8 km/h)", 1, 1), c("t3_bus_gap", "Extra minutes, mean of the three quickest schools", 1, 1), c("t_open_gap", "Extra minutes, quickest neither selective nor faith", 1, 1),
  c("t_walk_dep", "Minutes walking, deprived third", 1, 1), c("t_walk_rest", "Minutes walking, other neighbourhoods", 1, 1), c("t_walk_gap", "Extra minutes walking for the deprived third", 1, 1), c("km1_gap", "Extra straight-line km for the deprived third", 1, 2),
  c("over30_dep", "Share over 30 minutes, deprived third", 100, 0), c("over30_rest", "Share over 30 minutes, other neighbourhoods", 100, 0), c("over45_dep", "Share over 45 minutes, deprived third", 100, 0), c("over45_rest", "Share over 45 minutes, other neighbourhoods", 100, 0),
  c("p_two_dep", "Share needing two or more buses, deprived third", 100, 0), c("p_two_rest", "Share needing two or more buses, other neighbourhoods", 100, 0), c("p_two_gap", "Extra share needing two or more buses", 100, 0), c("boardings_dep", "Buses per journey, deprived third", 1, 2), c("wait_dep", "Minutes waiting, deprived third", 1, 1),
  c("late_dep", "Minutes late if the last journey is missed, deprived third", 1, 1), c("late_rest", "Minutes late if the last journey is missed, other neighbourhoods", 1, 1), c("late_gap", "Extra minutes late for the deprived third", 1, 1), c("mpk_dep", "Minutes per straight-line km, deprived third", 1, 1),
  c("n30_dep", "Schools within 30 minutes, deprived third (fewest ranked first)", -1, 1), c("n30_rest", "Schools within 30 minutes, other neighbourhoods (fewest ranked first)", -1, 1), c("up_back_dep", "Metres climbed walking home, deprived third", 1, 0), c("up_back_gap", "Extra metres climbed walking home for the deprived third", 1, 0))
outlier <- function(d) rbindlist(lapply(MEAS, function(m) { v <- as.numeric(d[[m[1]]]) * abs(as.numeric(m[3])); s <- sign(as.numeric(m[3])); nm <- d$la; ok <- !is.na(v); v <- v[ok]; nm <- nm[ok]; o <- order(-s * v)
  x <- v[nm == BH]; if (!length(x)) return(NULL); others <- v[nm != BH]; md <- median(v); mad_ <- mad(v)
  data.table(measure = m[1], label = m[2], digits = as.numeric(m[4]), bh = x, rank = match(BH, nm[o]), n = length(v), median = md, p90 = if (s > 0) quantile(v, .9, names = FALSE) else quantile(v, .1, names = FALSE), top = nm[o][1], top_v = v[o][1], next_ = if (nm[o][1] == BH) nm[o][2] else nm[o][1], next_v = if (nm[o][1] == BH) v[o][2] else v[o][1], rz = (x - md) / mad_) }))
Pm <- P[in_models == TRUE & !is.na(t_bus_all)]  # routed places; all 109 once every network is done
out_all <- outlier(Pm); out_nol <- outlier(Pm[london == FALSE])
# Given how far the deprived neighbourhoods are, and how dense the place is, is the journey longer than expected?
adj <- function(y, xs, d) { f <- as.formula(paste0(y, " ~ ", paste(xs, collapse = " + "))); ok <- d[complete.cases(d[, c(y, xs), with = FALSE])]; m <- lm(f, ok); r <- rstudent(m); i <- which(ok$la == BH); o <- order(-r); data.table(y = y, spec = paste(xs, collapse = " + "), n = nrow(ok), r2 = summary(m)$r.squared, actual = ok[[y]][i], expected = fitted(m)[i], resid = resid(m)[i], rstudent = r[i], rank = match(i, o), top = paste(ok$la[o][1:3], collapse = "; ")) }
adj_tab <- rbindlist(list(adj("t_bus_dep", c("km1_dep"), Pm), adj("t_bus_dep", c("km1_dep", "log_density"), Pm), adj("t_bus_gap", c("km1_gap"), Pm), adj("t_bus_gap", c("km1_gap", "log_density"), Pm), adj("t_walk_gap", c("km1_gap"), Pm), adj("p_two_dep", c("km1_dep", "log_density"), Pm),
                          adj("t_bus_dep", c("km1_dep"), Pm[london == FALSE]), adj("t_bus_gap", c("km1_gap"), Pm[london == FALSE]), adj("p_two_dep", c("km1_dep", "log_density"), Pm[london == FALSE])))
# Reproducing the nine-place work where the two overlap
old <- readRDS(here::here("data", "journey_times.rds")); old_p <- as.data.table(old$places)[scenario == "walk_bus", .(la = fifelse(place == "Bristol", FIX[["Bristol"]], place), old_dep = t_dep, old_rest = t_rest, old_gap = gap)]
repro <- merge(old_p, P[, .(la, new_dep = t_bus_dep, new_rest = t_bus_rest, new_gap = t_bus_gap)], by = "la")
# Where in Brighton and Hove: change stops and what is around them
gt <- rbindlist(lapply(c("south_east", "south_west", "london"), function(r) { td <- tempfile(); dir.create(td); unzip(here::here("data-raw", "r5", "src", paste0("gtfs_", r, ".zip")), files = "stops.txt", exdir = td); fread(file.path(td, "stops.txt"), select = c("stop_name", "stop_lat", "stop_lon")) }))[!is.na(stop_lat)]
stp <- rbindlist(lapply(ST, function(s) as.data.table(s$stops))); kid <- oa[, .(from_id = oa21cd, la, kids, third)]
bh_chg <- merge(stp[i < n], kid[la == BH], by = "from_id")[, .(children = sum(kids) / 3, dep_children = sum(kids[third]) / 3), by = .(lon = round(off_lon, 4), lat = round(off_lat, 4))][order(-children)][seq_len(min(.N, 12))]
if (nrow(bh_chg)) { nn_s <- nabor::knn(as.matrix(gt[, .(stop_lon * 0.63, stop_lat)]), as.matrix(bh_chg[, .(lon * 0.63, lat)]), k = 1); bh_chg[, stop := gt$stop_name[nn_s$nn.idx[, 1]]]
  if (length(PO)) { pst <- rbindlist(lapply(PO, function(p) p$stop_table[, net := p$id]), fill = TRUE)[net == "sussex"]; bh_chg <- merge(bh_chg, unique(pst[, .(lon, lat, p_any, p_food_drink, p_shops, p_leisure, p_open_space, p_civic)], by = c("lon", "lat")), by = c("lon", "lat"), all.x = TRUE)[order(-children)] } }
rm(ST); gc(FALSE)

# ---- 4. Tenths of home neighbourhood, within authority ----------------------------------
rs <- fread(here::here("data-raw", "ees", "ees_absence_residence_idaci.csv"))[geographic_level == "Local authority" & measure == "IDACI" & education_phase %in% c("State-funded secondary", "State-funded primary") & !pup_idaci %in% c("Total", "IDACI unclassified")]
rs <- rs[, .(tp = as.integer(time_period), la = la_name, dec = as.integer(sub("^(\\d+) -.*", "\\1", pup_idaci)) / 10 + 1, phase = fifelse(education_phase == "State-funded secondary", "sec", "pri"), enrol = num(enrolments), overall = num(sess_overall_percent), unauth = num(sess_unauthorised_percent), persistent = num(enrolments_pa_10_exact_percent))][!is.na(overall) & overall > 0 & enrol >= 30]
oa_m <- oa[la != "Hastings"]; oa_m[, dec := idaci_dec]
cell <- oa_m[!is.na(dec), c(.(cell_kids = sum(kids), km = wm(km1, kids), urban_c = 100 * wm(urban, kids), dens_c = wm(log(pmax(density, 1)), kids), grad_c = 100 * sum(grad_n) / sum(adults), wbri_c = 100 * sum(wbri_n) / sum(pop), nocar_c = 100 * sum(nocar_n) / sum(hh), social_c = 100 * sum(social_n) / sum(hh), bad_c = 100 * sum(bad_n) / sum(pop)), summ(.SD)), by = .(la, dec)]
B <- merge(rs, cell, by = c("la", "dec")); B[, two_share := 100 * p_two]; B[, bus_share := 100 * (1 - p_walk)]
B <- B[la %in% Pm$la]  # distance and time on the same places while some are not yet routed
message("Cells: ", nrow(B[phase == "sec"]), " in ", uniqueN(B[phase == "sec"]$la), " authorities; not matched: ", paste(setdiff(cell$la, rs$la), collapse = "; "))
CC <- "urban_c + dens_c + grad_c + wbri_c + nocar_c + social_c + bad_c"
TERMS <- c(km = "Straight-line km", t_bus = "Minutes, walking and bus", t_walk = "Minutes, walking", two_share = "Share needing two or more buses", bus_share = "Share needing a bus", boardings = "Buses per journey", wait = "Minutes waiting", late = "Minutes late if the last journey is missed", up_back = "Metres climbed home", up_out = "Metres climbed to school")
POI_TERMS <- c(walk50_any = "Along the walking route, within 50 m", leg50_any = "Along walking legs of the bus journey, within 50 m", p_any_board = "At the first stop, within 200 m", p_any_change = "At change stops, within 200 m", p_any_alight = "At the last stop, within 200 m",
               walk50_food_drink = "Walking route: food and drink", walk50_shops = "Walking route: shops", walk50_leisure = "Walking route: leisure", walk50_open_space = "Walking route: open space", walk50_civic = "Walking route: civic", walk100_any = "Walking route, within 100 m",
               p_food_drink_change = "Change stops: food and drink", p_shops_change = "Change stops: shops", p_leisure_change = "Change stops: leisure", p_open_space_change = "Change stops: open space", q_any_change = "Change stops, within 400 m")
POI_TERMS <- POI_TERMS[names(POI_TERMS) %in% POI]
fit_c <- function(y, x, dd, ctl) { dd <- dd[!is.na(get(x)) & !is.na(get(y))]; m <- feols(as.formula(paste0(y, " ~ ", x, if (nzchar(ctl)) paste0(" + ", ctl), " | la^tp + dec^tp")), dd, weights = ~enrol, cluster = ~la); data.table(b = coef(m)[[x]], se = se(m)[[x]], n = m$nobs, las = uniqueN(dd$la), sd_x = sd(dd[[x]]), p = pvalue(m)[[x]]) }
CS <- list(`Tenth and authority only` = function(x) "", `And Census character` = function(x) CC, `And straight-line km` = function(x) paste0(if (x != "km") "km + ", CC), `And home POIs` = function(x) paste0(if (x != "km") "km + ", CC, if ("home400_any" %in% names(B)) " + home400_any"),
           # A POI at a change needs a change: this asks whether busy changes matter beyond having one
           `And changes of bus` = function(x) paste0("km + ", CC, if ("home400_any" %in% names(B)) " + home400_any", " + two_share + boardings"))
SAMP_C <- list(`All authorities` = function(d) d, `Without London` = function(d) d[!la %in% Pm[london == TRUE, la]], `Without Brighton and Hove` = function(d) d[la != BH], `Most deprived three tenths` = function(d) d[dec <= 3])
cell_models <- rbindlist(lapply(c("overall", "unauth", "persistent"), function(y) rbindlist(lapply(c("sec", "pri"), function(ph) rbindlist(lapply(c(names(TERMS), names(POI_TERMS)), function(x) rbindlist(lapply(names(SAMP_C), function(sa) rbindlist(lapply(names(CS), function(ct) { if (ct %in% c("And home POIs", "And changes of bus") && !x %in% names(POI_TERMS)) return(NULL); dd <- SAMP_C[[sa]](B[phase == ph]); if (all(is.na(dd[[x]]))) return(NULL); fit_c(y, x, dd, CS[[ct]](x))[, `:=`(outcome = y, phase = ph, term = x, sample = sa, ctrl = ct)] }))))))))))
# Horse race: time and distance together; and does time survive POIs?
horse_c <- rbindlist(lapply(c("overall", "unauth", "persistent"), function(y) rbindlist(lapply(c("sec", "pri"), function(ph) rbindlist(lapply(names(SAMP_C), function(sa) { dd <- SAMP_C[[sa]](B[phase == ph]); m <- feols(as.formula(paste0(y, " ~ km + t_bus + ", CC, " | la^tp + dec^tp")), dd, weights = ~enrol, cluster = ~la); data.table(outcome = y, phase = ph, sample = sa, b_km = coef(m)[["km"]], se_km = se(m)[["km"]], b_t = coef(m)[["t_bus"]], se_t = se(m)[["t_bus"]], sd_km = sd(dd$km), sd_t = sd(dd$t_bus, na.rm = TRUE), n = m$nobs) }))))))
# Are the two measures even separable? Within authority and tenth
sep <- { d <- B[phase == "sec" & tp == max(tp) & !is.na(km) & !is.na(t_bus)]; list(r_km_t = cor(d$km, d$t_bus, use = "complete.obs"), r_within = cor(resid(feols(km ~ 1 | la + dec, d)), resid(feols(t_bus ~ 1 | la + dec, d)))) }

# ---- 5. Schools, within authority ----------------------------------------------------------
frame <- as.data.table(readRDS(here::here("data", "absence_school_frame.rds")))[, URN := as.character(URN)]
oa_s <- copy(oa_m)[!is.na(school)]; sq <- oa_s[, c(.(children = sum(kids), km_s = wm(km1, kids)), summ(.SD)), by = .(URN = school)]; sq[, `:=`(two_share = 100 * p_two, bus_share = 100 * (1 - p_walk))]
S <- merge(sq, frame, by = "URN")[children >= 100]; S[, `:=`(l2 = log2(km_s), selective = grepl("Selective", ADMPOL_PT))]; S <- S[selective == FALSE & !is.na(overall)]
SCT <- "fsm + eal + wbri + ks2_c + z_l4 + sen_sup + ehcp + mobile + girls + age16 + place"
SAMP_S <- list(`All schools` = S, `Without London` = S[region != "London"], `High free school meals` = S[fsm >= quantile(fsm, .75, na.rm = TRUE)], `Without Brighton and Hove` = S[la != BH])
fit_s <- function(y, x, dd) { dd <- dd[!is.na(get(y)) & !is.na(get(x))]; k <- if (x == "l2") "" else " + l2"; m <- feols(as.formula(paste0(y, " ~ ", x, k, " + ", SCT, " | la")), dd, cluster = ~la); data.table(b = coef(m)[[x]], se = se(m)[[x]], n = m$nobs, las = uniqueN(dd$la), sd_x = sd(dd[[x]])) }
STERMS <- c("l2", "t_bus", "t_walk", "two_share", "bus_share", "boardings", "wait", "late", "up_back", "up_out", names(POI_TERMS))
school_models <- rbindlist(lapply(c("overall", "unauth", "illness", "persistent"), function(y) rbindlist(lapply(STERMS, function(x) rbindlist(lapply(names(SAMP_S), function(sa) { dd <- SAMP_S[[sa]]; if (all(is.na(dd[[x]]))) return(NULL); fit_s(y, x, dd)[, `:=`(outcome = y, term = x, sample = sa)] }))))))

# ---- 6. Authorities, all three groups ---------------------------------------------------------
YEARS <- c(201819L, 202021L, 202122L, 202223L, 202324L, 202425L)
ch <- fread(here::here("data-raw", "ees", "ees_absence_characteristics.csv"), select = c("time_period", "geographic_level", "la_name", "education_phase", "breakdown", "enrolments", "sess_possible", "sess_overall", "sess_unauthorised", "sess_auth_illness", "enrolments_pa_10_exact"))
ch <- unique(ch[time_period %in% YEARS & geographic_level == "Local authority" & education_phase %in% c("State-funded secondary", "State-funded primary") & breakdown %in% c("FSM eligible in last 6 years", "FSM not eligible in last 6 years", "Total")], by = c("time_period", "la_name", "education_phase", "breakdown"))
ch[, `:=`(g = fcase(breakdown == "Total", "all", startsWith(breakdown, "FSM eligible"), "dis", default = "non"), phase = fifelse(education_phase == "State-funded secondary", "sec", "pri"), overall = 100 * num(sess_overall) / num(sess_possible), unauth = 100 * num(sess_unauthorised) / num(sess_possible), illness = 100 * num(sess_auth_illness) / num(sess_possible), persistent = 100 * num(enrolments_pa_10_exact) / num(enrolments), e = num(enrolments), tp = as.integer(time_period), la = la_name)]
shr <- dcast(ch, tp + la + phase ~ g, value.var = "e")[, share := 100 * dis / (dis + non)]
mx <- fread(here::here("data-raw", "ees", "fsm_ethnicity_la.csv"), colClasses = "character")[characteristic != "Unclassified"]
mx <- mx[, .(tp = as.integer(time_period), la = la_name, phase = fifelse(phase_type_grouping == "State-funded secondary", "sec", "pri"), g = fifelse(grepl("Known", fsm_eligibility), "dis", "non"), n = num(number_of_pupils), k = fcase(characteristic == "White - White British", "eth_wb", grepl("^Mixed", characteristic), "eth_mixed", grepl("Asian", characteristic), "eth_asian", grepl("Black", characteristic), "eth_black", default = "other"))]
mg <- rbind(mx[, .(n = sum(n, na.rm = TRUE)), by = .(tp, la, phase, g, k)], mx[, .(n = sum(n, na.rm = TRUE), g = "all"), by = .(tp, la, phase, k)]); mg[, s := 100 * n / sum(n), by = .(tp, la, phase, g)]
mw <- dcast(mg[k != "other"], tp + la + phase + g ~ k, value.var = "s", fill = 0)
# As R/19: the mix is published for one year, and stands for all
pup <- frame[, w := if (median(size, na.rm = TRUE) < 20) exp(size) else size][, lapply(.SD, function(x) wm(x, w)), by = la, .SDcols = c("sen_sup", "ehcp", "ks2_c", "eal", "wbri", "mobile")]
X <- Pm[, .(la, gor, london, urban_share, log_density, graduates, bad_health, white_british, social_rent, no_car, deprived)]
xvars <- unique(c("km1_dep", "t_bus_dep", "t_walk_dep", "t3_bus_dep", "p_two_dep", "boardings_dep", "wait_dep", "late_dep", "up_back_dep", "n30_dep", paste0(names(POI_TERMS), "_dep"), "home400_any_dep"))
X <- merge(X, Pm[, c("la", intersect(xvars, names(Pm))), with = FALSE], by = "la"); X <- merge(X, pup, by = "la", all.x = TRUE)
A <- merge(ch[, .(tp, la, phase, g, overall, unauth, illness, persistent)], shr[, .(tp, la, phase, share)], by = c("tp", "la", "phase")); A <- merge(A, X, by = "la")
A <- merge(A, mw[, .(la, phase, g, eth_wb, eth_asian, eth_black, eth_mixed)][, .SD[1], by = .(la, phase, g)], by = c("la", "phase", "g"), all.x = TRUE); A <- A[!is.na(overall)]
message("Authorities: ", uniqueN(A$la), "; with ethnic mix: ", uniqueN(A[!is.na(eth_wb)]$la))
ETHV <- c("eth_wb", "eth_asian", "eth_black", "eth_mixed"); CENSUS <- c("graduates", "bad_health", "white_british", "social_rent", "no_car", "deprived", "log_density"); PUPILS <- c("sen_sup", "ehcp", "ks2_c", "eal", "wbri", "mobile")
ACT <- list(`Share disadvantaged, how urban` = c("share", "urban_share"), `And ethnic mix` = c("share", "urban_share", ETHV), `And ethnic mix and region` = c("share", "urban_share", ETHV, "gor"), `And every control` = c("share", "urban_share", ETHV, "gor", CENSUS, PUPILS))
SAMP_A <- list(`All 109` = function(d) d, `Without London` = function(d) d[london == FALSE], `Without Brighton and Hove` = function(d) d[la != BH])
fit_a <- function(y, x, dd, ctl) { dd <- dd[complete.cases(dd[, c(y, x), with = FALSE])]; m <- feols(as.formula(paste0(y, " ~ ", paste(c(x, ctl), collapse = " + "), " | tp")), dd, cluster = ~la); data.table(b = coef(m)[[x[1]]], se = se(m)[[x[1]]], n = m$nobs, las = uniqueN(dd$la), sd_x = sd(dd[[x[1]]])) }
ATERMS <- setdiff(intersect(xvars, names(A)), "home400_any_dep")
la_models <- rbindlist(lapply(c("dis", "non", "all"), function(gr) rbindlist(lapply(c("sec", "pri"), function(ph) rbindlist(lapply(c("overall", "unauth"), function(y) rbindlist(lapply(ATERMS, function(x) rbindlist(lapply(names(SAMP_A), function(sa) rbindlist(lapply(names(ACT), function(ct) { dd <- SAMP_A[[sa]](A[g == gr & phase == ph & !is.na(eth_wb)]); if (all(is.na(dd[[x]]))) return(NULL); fit_a(y, x, dd, ACT[[ct]])[, `:=`(group = gr, phase = ph, outcome = y, term = x, sample = sa, ctrl = ct, given_km = FALSE)] }))))))))))))
la_given <- rbindlist(lapply(c("dis", "non", "all"), function(gr) rbindlist(lapply(c("sec", "pri"), function(ph) rbindlist(lapply(setdiff(ATERMS, "km1_dep"), function(x) rbindlist(lapply(names(SAMP_A), function(sa) rbindlist(lapply(names(ACT), function(ct) { dd <- SAMP_A[[sa]](A[g == gr & phase == ph & !is.na(eth_wb)]); if (all(is.na(dd[[x]]))) return(NULL); fit_a("overall", c(x, "km1_dep"), dd, ACT[[ct]])[, `:=`(group = gr, phase = ph, term = x, sample = sa, ctrl = ct)] }))))))))))
horse_a <- rbindlist(lapply(c("dis", "non", "all"), function(gr) rbindlist(lapply(names(SAMP_A), function(sa) rbindlist(lapply(names(ACT), function(ct) { dd <- SAMP_A[[sa]](A[g == gr & phase == "sec" & !is.na(eth_wb)]); m <- feols(as.formula(paste0("overall ~ km1_dep + t_bus_dep + ", paste(ACT[[ct]], collapse = " + "), " | tp")), dd, cluster = ~la); data.table(group = gr, sample = sa, ctrl = ct, b_km = coef(m)[["km1_dep"]], se_km = se(m)[["km1_dep"]], b_t = coef(m)[["t_bus_dep"]], se_t = se(m)[["t_bus_dep"]], sd_km = sd(dd$km1_dep), sd_t = sd(dd$t_bus_dep, na.rm = TRUE), n = m$nobs, las = uniqueN(dd$la)) }))))))
cor_la <- Pm[, .(r_t_km = cor(t_bus_dep, km1_dep, use = "complete.obs"), r_t_density = cor(t_bus_dep, log_density, use = "complete.obs"), r_gap = cor(t_bus_gap, km1_gap, use = "complete.obs"))]

# ---- 7. POIs: how many, and how complete is the map? ------------------------------------------
poi_rates <- NULL
if (length(PO)) { PP <- readRDS(file.path(J, "pois.rds")); poi_rates <- list(by_cat = PP$by_cat, top_tags = PP$top_tags, osm_date = PP$osm_date, n = length(PP$geom)); rm(PP)
  poi_rates$home <- oa[, .(home = wm(home400_any, kids), route = wm(walk50_any, kids)), by = la] }
place_cols <- c("la", "gor", "london", "in_models", "urban_share", "log_density", grep("^(kids|oas)_", names(P), value = TRUE), grep("_(dep|rest|all|gap|igap|dgap|idaci_dep|idaci_non|dec3|dec_rest)$", names(P), value = TRUE), "ratio")
out <- list(built = Sys.Date(), n_oas = nrow(oa), nets = N$boxes, feed_audit = N$audit, members = N$members, flat_meta = rbindlist(lapply(FL, function(f) data.table(net = f$id, origins = f$n_origins, schools = f$n_schools, feeds = paste(f$feeds, collapse = " + "), date = f$date))), no_route = no_route,
            places = P[, intersect(place_cols, names(P)), with = FALSE], outlier_all = out_all, outlier_nol = out_nol, adj = adj_tab, repro = repro, bh_chg = bh_chg, terms = TERMS, poi_terms = POI_TERMS,
            cell_models = cell_models, horse_c = horse_c, sep = sep, school_models = school_models, n_school = nrow(S), school_las = uniqueN(S$la), la_models = la_models, la_given = la_given, horse_a = horse_a, cor_la = cor_la, poi = poi_rates, n_cells = nrow(B[phase == "sec"]), cell_las = uniqueN(B[phase == "sec"]$la),
            # The model frames, for R/30 (Attainment 8): authorities by year, phase and group, and schools
            la_frame = A, la_controls = ACT, school_frame = S, school_controls = SCT)
saveRDS(out, here::here("data", "journey_la.rds")); message("Saved data/journey_la.rds")
