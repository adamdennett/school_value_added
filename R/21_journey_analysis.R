# 21_journey_analysis.R - What the routed journey times show
# ---------------------------------------------------------------------------
# Reads the matrices from R/20_journey_times.R and asks, for Brighton and Hove
# and its comparator places:
#   1. How long is the journey to the nearest school, walking and by walking and
#      bus, from the most deprived third of neighbourhoods and from the rest?
#   2. Does that change the picture from straight lines? Minutes per straight-line
#      kilometre, by place and by deprivation.
#   3. By IDACI tenth of home neighbourhood, and against absence within authority:
#      does time say more than distance? Eight authorities only, so descriptive.
#      DfE's file by home neighbourhood is for all pupils, not by disadvantage.
#   4. Does the route match the Brighton and Hove model where the two overlap?
#   5. What journeys are made of (R/22): buses, changes, waits, the penalty for missing
#      the last workable journey, the climb each way; and all that against absence.
#   6. What it costs to miss a school bus (R/24).
#
# Deprivation, children and IDACI are as R/18 and R/19. Sources and formulas:
# method.qmd#sec-journey-method. Output: data/journey_times.rds (committed).
# Rscript R/21_journey_analysis.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(fixest) })
setFixest_notes(FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
wm <- function(x, w) { ok <- !is.na(x) & !is.na(w); sum(x[ok] * w[ok]) / sum(w[ok]) }
BH <- "Brighton and Hove"
J <- here::here("data", "cache", "journey")
FR <- readRDS(file.path(J, "frame.rds"))
SCENARIOS <- c(walk_bus = "Walking and bus", walk = "Walking alone", walk_bus_f = "Walking and bus, faster walking", bh_model = "City model's settings")

# ---- 1. Times from each Output Area to its quickest school -----------------------
read_set <- function(prefix) {
  fs <- list.files(J, pattern = paste0("^", prefix, "_.*\\.rds$"), full.names = TRUE)
  rbindlist(lapply(fs, function(f) { r <- readRDS(f); x <- copy(r$times); x[, net := r$id] }))
}
meta_set <- function(prefix) rbindlist(lapply(list.files(J, pattern = paste0("^", prefix, "_.*\\.rds$"), full.names = TRUE), function(f) { r <- readRDS(f)
  data.table(net = r$id, places = paste(r$places, collapse = ", "), pbfs = paste(r$pbfs, collapse = " + "), gtfs = paste(r$gtfs, collapse = " + "), date = r$date, trips = r$trips, origins = r$n_origins, schools = r$n_schools) }))
quick <- function(x) x[order(scenario, from_id, t)][, .(t1 = as.numeric(t[1]), t3 = mean(as.numeric(t[seq_len(min(.N, 3))])), school = to_id[1]), by = .(scenario, from_id)]

# ---- 2. Neighbourhoods: deprivation and IDACI, as R/18 and R/19 ------------------
t11 <- readr::read_csv(here::here("data-raw", "census2021", "ts011", "census2021-ts011-lsoa.csv"), show_col_types = FALSE)
nm <- function(p) grep(p, names(t11), value = TRUE)
dep <- tibble(lsoa = t11$`geography code`, dep2 = 100 * rowSums(t11[c(nm("deprived in two"), nm("deprived in three"), nm("deprived in four"))]) / t11[[nm("Total: All households")]])
lk <- readr::read_csv(here::here("data-raw", "census2021", "ons_custom", "lsoa11_lsoa21.csv"), show_col_types = FALSE)
io <- readr::read_csv(here::here("data-raw", "iod2019", "File_7_IoD2019.csv"), show_col_types = FALSE)
io <- tibble(l11 = io[[grep("^LSOA code", names(io), value = TRUE)]], score = io[[grep("IDACI\\) Score", names(io), value = TRUE)]], dec = io[[grep("IDACI\\) Decile", names(io), value = TRUE)]])
cuts <- io |> group_by(dec) |> summarise(lo = min(score), .groups = "drop") |> arrange(desc(lo))
idaci <- lk |> filter(startsWith(LSOA21CD, "E")) |> inner_join(io, by = c("LSOA11CD" = "l11")) |> group_by(lsoa = LSOA21CD) |> summarise(idaci = mean(score), .groups = "drop") |>
  mutate(idaci_dec = vapply(idaci, function(s) min(cuts$dec[s >= cuts$lo], 10L), 1))
oa <- as_tibble(FR$oa) |> left_join(dep, by = "lsoa") |> left_join(idaci, by = "lsoa") |> group_by(lad) |> mutate(third = dep2 >= quantile(unique(pick(lsoa, dep2))$dep2, 2 / 3)) |> ungroup()
# Straight line from each Output Area to the nearest school, in the same set of schools
nn <- nabor::knn(as.matrix(as_tibble(FR$sch)[, c("e", "n")]), as.matrix(oa[, c("e", "n")]), k = 3)
oa$km1 <- nn$nn.dists[, 1] / 1000; oa$km3 <- rowMeans(nn$nn.dists) / 1000
cat_ <- function(prefix) { q <- quick(read_set(prefix)); as_tibble(q) |> inner_join(oa, by = c("from_id" = "oa21cd")) }

summarise_place <- function(d) d |> group_by(scenario, place = lad) |>
  summarise(oas = n(), children = sum(kids), t_all = wm(t1, kids), t_dep = wm(t1[third], kids[third]), t_rest = wm(t1[!third], kids[!third]), t3_dep = wm(t3[third], kids[third]), t3_rest = wm(t3[!third], kids[!third]),
            km_dep = wm(km1[third], kids[third]), km_rest = wm(km1[!third], kids[!third]),
            over30_dep = 100 * wm(t1[third] > 30, kids[third]), over30_rest = 100 * wm(t1[!third] > 30, kids[!third]), over45_dep = 100 * wm(t1[third] > 45, kids[third]), over45_rest = 100 * wm(t1[!third] > 45, kids[!third]), .groups = "drop") |>
  mutate(gap = t_dep - t_rest, ratio = t_dep / t_rest, km_ratio = km_dep / km_rest, mins_per_km_dep = t_dep / km_dep, mins_per_km_rest = t_rest / km_rest)

fl <- cat_("flat")
places <- summarise_place(fl)
missing <- oa |> summarise(total = n()) |> mutate(routed = n_distinct(fl$from_id[fl$scenario == "walk_bus"]), routed_walk = n_distinct(fl$from_id[fl$scenario == "walk"]))
unreach <- oa |> left_join(fl |> filter(scenario == "walk_bus") |> select(from_id, t1), by = c("oa21cd" = "from_id")) |> group_by(lad) |> summarise(oas = n(), no_route = sum(is.na(t1)), kids_no_route = sum(kids[is.na(t1)]), .groups = "drop")

# ---- 3. By IDACI tenth of home neighbourhood --------------------------------------
tenths <- fl |> filter(scenario %in% c("walk_bus", "walk")) |> group_by(scenario, place = lad, dec = idaci_dec) |>
  summarise(children = sum(kids), t = wm(t1, kids), km = wm(km1, kids), .groups = "drop")

# Absence by home neighbourhood: all pupils, 2023/24 and 2024/25, authority by tenth
rs <- as_tibble(data.table::fread(here::here("data-raw", "ees", "ees_absence_residence_idaci.csv"))) |>
  filter(geographic_level == "Local authority", measure == "IDACI", education_phase == "State-funded secondary", !pup_idaci %in% c("Total", "IDACI unclassified")) |>
  transmute(tp = as.integer(time_period), la = recode(la_name, "Bristol, City of" = "Bristol"), dec = as.integer(sub("^(\\d+) -.*", "\\1", pup_idaci)) / 10 + 1, enrol = num(enrolments), overall = num(sess_overall_percent), unauth = num(sess_unauthorised_percent), persistent = num(enrolments_pa_10_exact_percent)) |>
  filter(!is.na(overall), enrol >= 30)
cells <- tenths |> pivot_wider(names_from = scenario, values_from = c(t, km, children)) |> transmute(la = place, dec, t_bus = t_walk_bus, t_walk, km = km_walk_bus, children = children_walk_bus) |> inner_join(rs, by = c("la", "dec")) |> mutate(band = ifelse(dec <= 3, "Most deprived three tenths", "Other seven tenths"))
fitc <- function(y, x, d = cells) { m <- feols(as.formula(paste0(y, " ~ ", x, " | la^tp + dec^tp")), d, weights = ~enrol, cluster = ~la); tibble(y = y, x = x, b = coef(m)[[x]], se = se(m)[[x]], n = m$nobs, las = n_distinct(d$la), sd_x = sd(d[[x]], na.rm = TRUE)) }
cell_models <- bind_rows(lapply(c("overall", "unauth", "persistent"), function(y) bind_rows(lapply(c("km", "t_walk", "t_bus"), function(x) fitc(y, x) |> mutate(sample = "All tenths")),
                                                                                         lapply(c("km", "t_walk", "t_bus"), function(x) fitc(y, x, cells |> filter(dec <= 3)) |> mutate(sample = "Most deprived three tenths"))))) |> mutate(b_sd = b * sd_x, se_sd = se * sd_x)
horse <- { d <- cells; m <- feols(overall ~ km + t_bus | la^tp + dec^tp, d, weights = ~enrol, cluster = ~la); tibble(b_km = coef(m)[["km"]], se_km = se(m)[["km"]], b_t = coef(m)[["t_bus"]], se_t = se(m)[["t_bus"]], n = m$nobs) }

# Places against authority absence, for all three groups: descriptive, too few to model
D <- readRDS(here::here("data", "distance.rds"))
abs_la <- places |> filter(scenario == "walk_bus") |> left_join(D$compare |> transmute(place, abs_dis, abs_non, abs_all, share), by = "place")
cors <- bind_rows(lapply(c("abs_dis", "abs_non", "abs_all"), function(y) { d <- abs_la |> filter(!is.na(.data[[y]]))
  tibble(group = y, n = nrow(d), r_km = cor(d[[y]], d$km_dep), r_t = cor(d[[y]], d$t_dep), r_km_noL = cor(d[[y]][!d$place %in% c("Newham", "Tower Hamlets")], d$km_dep[!d$place %in% c("Newham", "Tower Hamlets")]), r_t_noL = cor(d[[y]][!d$place %in% c("Newham", "Tower Hamlets")], d$t_dep[!d$place %in% c("Newham", "Tower Hamlets")])) }))

# ---- 4. Brighton and Hove: neighbourhoods, and against the city's own model -------
lsoa_bh <- fl |> filter(lad == BH, scenario %in% c("walk_bus", "walk", "bh_model")) |> group_by(scenario, lsoa) |> summarise(t = wm(t1, kids), km = wm(km1, kids), third = first(third), dep2 = first(dep2), kids = sum(kids), x = mean(e), y = mean(n), .groups = "drop")
schools_used <- as_tibble(FR$sch) |> filter(URN %in% unique(as.integer(fl$school[fl$scenario == "walk_bus"]))) |> select(URN, name, e, n)
val_f <- file.path(dirname(here::here()), "bh_school_system", "data", "travel", "bn_pcds_sch_travel_extended.csv")
validate <- NULL
if (file.exists(val_f)) {
  v <- readr::read_csv(val_f, show_col_types = FALSE, col_select = c(lsoa21, first_closest_time)) |> filter(!is.na(first_closest_time), lsoa21 %in% lsoa_bh$lsoa) |> group_by(lsoa = lsoa21) |> summarise(city_model = mean(first_closest_time), postcodes = n(), .groups = "drop")
  j <- lsoa_bh |> filter(scenario == "bh_model") |> inner_join(v, by = "lsoa")
  validate <- list(lsoas = nrow(j), r = cor(j$t, j$city_model), mean_new = wm(j$t, j$kids), mean_old = wm(j$city_model, j$kids), mae = wm(abs(j$t - j$city_model), j$kids), gap_new = wm(j$t[j$third], j$kids[j$third]) - wm(j$t[!j$third], j$kids[!j$third]),
                   gap_old = wm(j$city_model[j$third], j$kids[j$third]) - wm(j$city_model[!j$third], j$kids[!j$third]))
}

# ---- 5. What each journey is made of (R/22) ------------------------------------
sfs <- list.files(J, pattern = "^struct_.*\\.rds$", full.names = TRUE); SS <- lapply(sfs, readRDS)
stg <- rbindlist(lapply(SS, function(s) s$structure)); oa_dt <- as.data.table(oa)
# Averaged over the three departures, so one timetable quirk does not decide a neighbourhood
jny <- stg[, .(school = school[which.max(tabulate(match(school, unique(school))))], total = mean(total), boardings = mean(n_bus), p_walk = mean(n_bus == 0), p_one = mean(n_bus == 1), p_two = mean(n_bus >= 2), wait = mean(wait), walk_m = mean(walk_m), first_walk = mean(first_walk_m, na.rm = TRUE)), by = from_id]
ms <- rbindlist(lapply(SS, function(s) s$miss))[bell == "bell_0845", .(from_id, late, options)]
ms30 <- rbindlist(lapply(SS, function(s) s$miss))[bell == "bell_0830", .(from_id, late30 = late)]
wr <- rbindlist(lapply(SS, function(s) s$walk_route))[, .(from_id = id, metres, up_out, up_back, t_flat, t_out, t_back)]
jny <- Reduce(function(a, b) merge(a, b, by = "from_id", all.x = TRUE), list(jny, ms, ms30, wr))
jo <- merge(oa_dt[, .(from_id = oa21cd, lad, lsoa, kids, third, idaci_dec, km1, dep2)], jny, by = "from_id")
sum_str <- function(d) d[, .(oas = .N, boardings = wm(boardings, kids), walk_only = 100 * wm(p_walk, kids), one_bus = 100 * wm(p_one, kids), two_plus = 100 * wm(p_two, kids), wait = wm(wait, kids), first_walk = wm(first_walk, kids), late = wm(late, kids), late10 = 100 * wm(late >= 10, kids), options = wm(options, kids),
                         up_out = wm(up_out, kids), up_back = wm(up_back, kids), up_sum = wm(up_out + up_back, kids), t_flat = wm(t_flat, kids), t_out = wm(t_out, kids), t_back = wm(t_back, kids))]
str_third <- rbind(jo[, sum_str(.SD), by = .(place = lad, third)], jo[, sum_str(.SD), by = .(place = lad)][, third := NA], fill = TRUE)
str_tenth <- jo[, sum_str(.SD), by = .(place = lad, dec = idaci_dec)]

# Where children would change buses, weighted by children and the share of the three departures
stp <- rbindlist(lapply(SS, function(s) s$stops))
kids_oa <- oa_dt[, .(from_id = oa21cd, lad, kids, third)]
chg <- merge(stp[i < n], kids_oa, by = "from_id")[, .(children = sum(kids) / 3, dep_children = sum(kids[third]) / 3, journeys = .N), by = .(place = lad, lon = round(off_lon, 4), lat = round(off_lat, 4))][order(-children)]
all_stops <- rbindlist(lapply(c("south_east", "south_west", "north_west", "london"), function(r) { td <- tempfile(); dir.create(td); unzip(here::here("data-raw", "r5", "src", paste0("gtfs_", r, ".zip")), files = "stops.txt", exdir = td); fread(file.path(td, "stops.txt"), select = c("stop_name", "stop_lat", "stop_lon")) }))[!is.na(stop_lat)]
nn_s <- nabor::knn(as.matrix(all_stops[, .(stop_lon * 0.63, stop_lat)]), as.matrix(chg[, .(lon * 0.63, lat)]), k = 1); chg[, stop := all_stops$stop_name[nn_s$nn.idx[, 1]]]
interchanges <- chg[, .SD[seq_len(min(.N, 12))], by = place]

# Does it show against absence? Within authority, by school, on journeys of its quickest-school children. All pupils.
frame <- as.data.table(readRDS(here::here("data", "absence_school_frame.rds")))[, URN := as.character(URN)]
sq <- jo[!is.na(school), .(children = sum(kids), km = wm(km1, kids), minutes = wm(total, kids), boardings = wm(boardings, kids), bus_share = 100 * (1 - wm(p_walk, kids)), two_share = 100 * wm(p_two, kids), late = wm(late, kids), up_out = wm(up_out, kids), up_back = wm(up_back, kids), wait = wm(wait, kids)), by = .(URN = school)]
sd <- merge(sq, frame, by = "URN")[children >= 100]; sd[, `:=`(l2 = log2(km), selective = grepl("Selective", ADMPOL_PT))]; sd <- sd[selective == FALSE & !is.na(overall)]
SC <- "fsm + wbri + sen_sup + ks2_c"
school_terms <- c(`Straight-line distance (doubling)` = "l2", `Share of children needing a bus` = "bus_share", `Share needing two or more buses` = "two_share", `Buses per journey` = "boardings", `Minutes late if the last bus is missed` = "late", `Metres climbed to school` = "up_out", `Metres climbed home` = "up_back", `Minutes on the journey` = "minutes")
school_models <- rbindlist(lapply(c("overall", "unauth", "illness", "persistent"), function(y) rbindlist(lapply(names(school_terms), function(lb) { x <- school_terms[[lb]]; dd <- sd[!is.na(get(y)) & !is.na(get(x))]
  m1 <- feols(as.formula(paste0(y, " ~ ", x, " + ", SC, " | la")), dd, cluster = ~la); xk <- if (x == "l2") x else paste0(x, " + l2"); m2 <- feols(as.formula(paste0(y, " ~ ", xk, " + ", SC, " | la")), dd, cluster = ~la)
  data.table(outcome = y, term = lb, sd_x = sd(dd[[x]]), b = coef(m1)[[x]], se = se(m1)[[x]], b_km = coef(m2)[[x]], se_km = se(m2)[[x]], n = m1$nobs, schools_la = uniqueN(dd$la)) }))))
# And by tenth of home neighbourhood within authority, all pupils
cell2 <- jo[, .(boardings = wm(boardings, kids), two_share = 100 * wm(p_two, kids), bus_share = 100 * (1 - wm(p_walk, kids)), late = wm(late, kids), up_back = wm(up_back, kids), km = wm(km1, kids)), by = .(la = lad, dec = idaci_dec)]
cell2 <- merge(as.data.table(rs), cell2, by = c("la", "dec"))
cell_terms <- c("two_share", "boardings", "bus_share", "late", "up_back")
cell_struct <- rbindlist(lapply(c("overall", "unauth", "persistent"), function(y) rbindlist(lapply(cell_terms, function(x) { m <- feols(as.formula(paste0(y, " ~ ", x, " + km | la^tp + dec^tp")), cell2, weights = ~enrol, cluster = ~la); data.table(outcome = y, term = x, b = coef(m)[[x]], se = se(m)[[x]], sd_x = sd(cell2[[x]]), n = m$nobs, las = uniqueN(cell2$la)) }))))

# ---- 6. Missing the school bus (R/24) -----------------------------------------------
pfs <- list.files(J, pattern = "^penalty_.*\\.rds$", full.names = TRUE); PP <- Filter(function(p) !is.null(p$pairs), lapply(pfs, readRDS))
sbus <- NULL
if (length(PP)) {
  pr <- rbindlist(lapply(PP, function(p) { x <- unique(p$link[, .(oa21cd, URN, kids, lad, walk_to_stop)][order(walk_to_stop)][, .SD[1], by = .(oa21cd, URN)]); merge(p$pairs[, URN := as.integer(URN)], x, by.x = c("from_id", "URN"), by.y = c("oa21cd", "URN"))[, net := p$id] }), fill = TRUE)
  names_ <- as.data.table(FR$sch)[, .(URN, name, la)]
  pr <- merge(pr, names_, by = "URN")
  by_sch <- pr[, .(oas = .N, children = sum(kids), uses_sb = 100 * wm(uses_sb, kids), late = wm(late, kids), late25 = wm(late25, kids), late_sb = wm(late[uses_sb], kids[uses_sb]), late_other = wm(late[!uses_sb], kids[!uses_sb]), over15 = 100 * wm(late >= 15, kids), walk_t = wm(walk_t, kids), bell = bell[1], leave = wm(last_leave, kids), place = lad[1]), by = .(net, URN, name)]
  by_net <- pr[, .(schools = uniqueN(URN), oas = uniqueN(from_id), uses_sb = 100 * wm(uses_sb, kids), late_sb = wm(late[uses_sb], kids[uses_sb]), late_other = wm(late[!uses_sb], kids[!uses_sb]), over15_sb = 100 * wm(late[uses_sb] >= 15, kids[uses_sb]), over15_other = 100 * wm(late[!uses_sb] >= 15, kids[!uses_sb]), walk_t = wm(walk_t, kids)), by = net]
  ab <- frame[, .(URN = as.integer(URN), overall, unauth, persistent, fsm)]; by_sch <- merge(by_sch, ab, by = "URN", all.x = TRUE)
  rt <- rbindlist(lapply(PP, function(p) p$routes[, .(net = p$id, route, URN, am_trips, pm_trips, first_arr, last_arr, named, head)]))
  sbus <- list(pairs = pr[net %in% c("bh", "plymouth"), .(net, from_id, URN, name, kids, late, uses_sb, walk_t, bell, last_leave, n_arrivals)], by_school = by_sch, by_net = by_net, routes = merge(rt, names_, by = "URN"))
}

# Which bus file serves which place? Stops within 3 km of each centre, in every regional file
CENTRES <- list(`Brighton and Hove` = c(-0.137, 50.828), Hastings = c(0.573, 50.858), Portsmouth = c(-1.085, 50.805), Southampton = c(-1.404, 50.909), Bristol = c(-2.587, 51.455), Plymouth = c(-4.142, 50.375), Blackpool = c(-3.05, 53.815), Newham = c(0.03, 51.525), `Tower Hamlets` = c(-0.03, 51.515))
audit <- bind_rows(lapply(c("south_east", "south_west", "north_west", "london"), function(r) { td <- tempfile(); dir.create(td); unzip(here::here("data-raw", "r5", "src", paste0("gtfs_", r, ".zip")), files = "stops.txt", exdir = td); s <- fread(file.path(td, "stops.txt"), select = c("stop_lat", "stop_lon"))
  tibble(feed = r, place = names(CENTRES), stops = vapply(CENTRES, function(p) s[sqrt(((stop_lon - p[1]) * 70)^2 + ((stop_lat - p[2]) * 111)^2) < 3, .N], 1L)) }))

out <- list(built = Sys.Date(), audit = audit, scenarios = SCENARIOS, str_third = str_third, str_tenth = str_tenth, interchanges = interchanges, school_models = school_models, cell_struct = cell_struct, sbus = sbus, n_school_model = nrow(sd), meta = meta_set("flat"), places = places, tenths = tenths, cells = cells, cell_models = cell_models, horse = horse,
            abs_la = abs_la, cors = cors, lsoa_bh = lsoa_bh, schools_used = schools_used, validate = validate, missing = missing, unreach = unreach)
saveRDS(out, here::here("data", "journey_times.rds"))
message("Saved data/journey_times.rds")
