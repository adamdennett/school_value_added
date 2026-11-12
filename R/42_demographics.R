# 42_demographics.R - Pupil numbers, cohorts, and schools opened and closed
# ---------------------------------------------------------------------------
# Where pupil numbers have grown or fallen consistently, what the primary cohorts
# already in school mean for secondary intakes, and where state schools have opened
# and closed. For demographics.qmd (and later the School Information System).
#
# Sources (all published):
#   Pupils by year group: DfE "Schools, pupils and their characteristics" 2025/26,
#     data set "School characteristics by year, sex and attendance pattern"
#     (EES API data set 019e7403-0e46-7122-9bc2-3252d775517c), January census
#     2015/16 to 2025/26, national, regional and local authority. Downloaded to
#     data-raw/ees/spc_2526/yeargroup.csv.gz and cut to the all-schools rows
#     (every breakdown "Total") as yeargroup_totals.csv; see README.
#   Openings and closures: Get Information about Schools, all establishments and
#     links, 10 June 2026 (pipeline/07_gias.R: data/gias/*.rds, data-raw/gias/).
#   National pupil projections (a check on the roll-forward): DfE "National pupil
#     projections" 2025, principal projection (school_attainment_tool data/place).
#
# Calculations:
#   Pupils are counted by year group across state-funded primary and secondary
#   schools together (so middle schools fall where their pupils' ages put them):
#   primary-age = Reception to Year 6, secondary-age = Years 7 to 11. Nursery
#   classes and sixth forms are left out, as they vary with provision, not births.
#   Areas: local authorities, with three reorganisations combined so every series
#   runs unbroken (Dorset with Bournemouth, Christchurch and Poole; North and West
#   Northamptonshire; Cumberland and Westmorland and Furness).
#   Trend class, primary-age pupils, 2018/19 to 2025/26 (seven year-on-year changes):
#   consistent decline = fell in at least six of seven years and by 5% or more
#   overall; decline = fell by 3% or more; consistent growth and growth likewise
#   upwards; otherwise stable. The same for secondary-age pupils.
#   Year 7 roll-forward: each cohort in school in January 2026 is moved up a year
#   at a time by the area's average progression ratio for that step over the last
#   three years (pupils in year g+1 one year on / pupils in year g), so moves in and
#   out of the area and into independent schools carry on as recently. This gives
#   Year 7 intakes to 2032/33 and secondary-age totals to 2030/31 with no births
#   assumption at all.
#   Openings: state-funded mainstream schools (primary, middle, secondary,
#   all-through; not special, AP, nursery, 16-19, studio or UTC) opened from
#   September 2015 with no predecessor in GIAS's links (genuinely new places, not
#   academy conversions, fresh starts or amalgamations). Closures: the same schools
#   closed from September 2015 with no successor ("closed"), or merged into another
#   ("merged"), from pipeline/07_gias.R's classification. An academic year runs
#   September to August; a school closing on 31 August 2024 last taught in 2023/24.
#
# Output: data/demographics.rds.   Rscript R/42_demographics.R  (about a minute)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(sf) })

# ---- areas: local authorities, reorganisations combined -------------------------
COMBINE <- c("Bournemouth" = "Dorset and BCP", "Poole" = "Dorset and BCP", "Dorset" = "Dorset and BCP",
             "Bournemouth, Christchurch and Poole" = "Dorset and BCP",
             "Northamptonshire" = "Northamptonshire", "North Northamptonshire" = "Northamptonshire",
             "West Northamptonshire" = "Northamptonshire",
             "Cumbria" = "Cumbria", "Cumberland" = "Cumbria", "Westmorland and Furness" = "Cumbria")
area_of <- function(la) ifelse(la %in% names(COMBINE), COMBINE[la], la)

# ---- 1. pupils by year group ------------------------------------------------------
yg_file <- here::here("data-raw", "ees", "spc_2526", "yeargroup_totals.csv")
if (!file.exists(yg_file)) stop("Download the year-group data set first (see the header and README)")
Y <- fread(yg_file)[phase_type_grouping %in% c("State-funded primary", "State-funded secondary")]
ycols <- c(R = "year_group_reception_pupil_count", setNames(paste0("year_group_", 1:11, "_pupil_count"), paste0("Y", 1:11)))
Y <- Y[, c(list(year = paste0(substr(time_period, 1, 4), "-", substr(time_period, 5, 6)), level = geographic_level,
               region = region_name, la = la_name), lapply(.SD, as.numeric)), .SDcols = unname(ycols)]
setnames(Y, unname(ycols), names(ycols))
Y[level == "Local authority", area := area_of(la)][level == "National", area := "England"][level == "Regional", area := region]
Y <- Y[!is.na(area) & la != "Isles of Scilly"]                     # 1 school; too small to read as a trend
yg <- Y[, lapply(.SD, sum, na.rm = TRUE), by = .(level, area, year), .SDcols = names(ycols)]
reg_of <- unique(Y[level == "Local authority", .(area, region)])[, .SD[1], by = area]
yg <- merge(yg, reg_of, by = "area", all.x = TRUE)
yg[, `:=`(primary = R + Y1 + Y2 + Y3 + Y4 + Y5 + Y6, secondary = Y7 + Y8 + Y9 + Y10 + Y11)]
YEARS <- sort(unique(yg$year))

# ---- 2. trend classes -------------------------------------------------------------
classify <- function(x, years) {
  x <- x[order(years)]; if (anyNA(x) || x[1] <= 0) return(NA_character_)
  d <- diff(x); chg <- 100 * (tail(x, 1) / x[1] - 1)
  down <- sum(d < 0); up <- sum(d > 0); n <- length(d)
  if (down >= n - 1 && chg <= -5) "Consistent decline" else if (up >= n - 1 && chg >= 5) "Consistent growth"
  else if (chg <= -3) "Decline" else if (chg >= 3) "Growth" else "Stable"
}
CLASSES <- c("Consistent decline", "Decline", "Stable", "Growth", "Consistent growth")
T0 <- "2018-19"
trend <- yg[year >= T0, .(primary_class = classify(primary, year), secondary_class = classify(secondary, year),
                         primary_chg = 100 * (primary[year == max(year)] / primary[year == T0] - 1),
                         secondary_chg = 100 * (secondary[year == max(year)] / secondary[year == T0] - 1),
                         reception_chg = 100 * (R[year == max(year)] / R[year == T0] - 1),
                         primary_falls = sum(diff(primary[order(year)]) < 0)), by = .(level, area)]
peak <- yg[, .(primary_peak_year = year[which.max(primary)], primary_peak = max(primary),
               primary_now = primary[year == max(year)]), by = .(level, area)][
  , primary_from_peak := 100 * (primary_now / primary_peak - 1)]
trend <- merge(trend, peak, by = c("level", "area"))
for (v in c("primary_class", "secondary_class")) trend[[v]] <- factor(trend[[v]], CLASSES)

# ---- 3. rolling the cohorts in school forward ---------------------------------------
steps <- c("R", paste0("Y", 1:11))
ratio <- rbindlist(lapply(seq_len(length(steps) - 1), function(k) {
  a <- steps[k]; b <- steps[k + 1]
  x <- merge(yg[, .(level, area, year, from = get(a))],
             yg[, .(level, area, year_prev = c(NA, YEARS)[match(year, YEARS)], to = get(b))],
             by.x = c("level", "area", "year"), by.y = c("level", "area", "year_prev"))
  x[year %in% tail(YEARS, 4)[1:3], .(step = b, r = sum(to) / sum(from)), by = .(level, area)]   # the last three transitions
}))
now <- yg[year == max(YEARS)]
roll <- rbindlist(lapply(seq_len(nrow(now)), function(i) {
  a <- now[i]; rr <- ratio[level == a$level & area == a$area]
  cur <- unlist(a[, ..steps]); out <- list(); yr <- max(YEARS)
  for (h in 1:7) {                                            # 2026/27 to 2032/33
    nxt <- c(R = NA_real_, setNames(cur[-length(cur)] * rr$r[match(steps[-1], rr$step)], steps[-1]))
    yr <- paste0(as.integer(substr(yr, 1, 4)) + 1, "-", sprintf("%02d", (as.integer(substr(yr, 6, 7)) + 1) %% 100))
    out[[h]] <- data.table(level = a$level, area = a$area, year = yr, Y7 = nxt[["Y7"]],
                           secondary = if (h <= 5) sum(nxt[paste0("Y", 7:11)]) else NA_real_)
    cur <- nxt
  }
  rbindlist(out)
}))
proj <- merge(roll, now[, .(level, area, Y7_now = Y7, secondary_now = secondary)], by = c("level", "area"))
proj_sum <- proj[, .(y7_2030 = Y7[year == "2030-31"], y7_2032 = Y7[year == "2032-33"], sec_2030 = secondary[year == "2030-31"],
                     Y7_now = Y7_now[1], secondary_now = secondary_now[1]), by = .(level, area)][
  , `:=`(y7_chg_2032 = 100 * (y7_2032 / Y7_now - 1), sec_chg_2030 = 100 * (sec_2030 / secondary_now - 1))]
trend <- merge(trend, proj_sum, by = c("level", "area"), all.x = TRUE)

# DfE's national projection, as a check on England's roll-forward
P <- tryCatch(readRDS(file.path(SAT, "data", "place", "pupil_projections.rds")), error = function(e) NULL)
dfe_check <- NULL
if (!is.null(P)) dfe_check <- as.data.table(P)[establishment_type_group == "State secondary" & pupil_status == "FTE" & pupil_age %in% as.character(11:15),   # ages 11-15, about Years 7-11
  .(pupils = sum(as.numeric(pupil_count))), by = .(model_year, population_measure)][order(model_year)]

# ---- 4. schools opened and closed --------------------------------------------------
G <- as.data.table(readRDS(here::here("data", "gias", "gias_establishments.rds")))
C <- as.data.table(readRDS(here::here("data", "gias", "school_closures.rds")))
L <- fread(here::here("data-raw", "gias", "links_edubasealldata20260610.csv"), select = c("URN", "LinkURN", "LinkType"))
MAIN_PHASE <- c("Primary", "Secondary", "Middle deemed primary", "Middle deemed secondary", "All-through")
NOT_MAIN <- c("Academy 16-19 converter", "Academy 16 to 19 sponsor led", "Academy secure 16 to 19", "Free schools 16 to 19",
              "Studio schools", "University technical college", "Local authority nursery school", "Pupil referral unit",
              "Academy alternative provision converter", "Academy alternative provision sponsor led", "Free schools alternative provision",
              "Academy special converter", "Academy special sponsor led", "Free schools special")
main <- G[phase %in% MAIN_PHASE & type_group %in% c("Academies", "Free Schools", "Local authority maintained schools") &
          !establishment_type %in% NOT_MAIN]
acyear <- function(d, start) { y <- as.integer(format(d, "%Y")); m <- as.integer(format(d, "%m"))
  s <- if (start) ifelse(m >= 8, y, y - 1) else ifelse(m >= 9, y, y - 1); paste0(s, "-", sprintf("%02d", (s + 1) %% 100)) }
pred <- unique(L[grepl("^Predecessor", LinkType), URN])
opened <- main[!is.na(open_date) & open_date >= as.Date("2015-08-01") & !(urn %in% pred) &
               !reason_opened %in% c("Academy Converter", "Fresh Start", "Result of Amalgamation", "Result of Closure",
                                     "Split school", "Change in status", "Change Religious Character", "Former Independent")]
opened <- opened[, .(urn, name = establishment_name, la = la_name, phase, type = establishment_type, event = "Opened",
                     date = open_date, year = acyear(open_date, TRUE), reason = reason_opened, pupils = number_of_pupils,
                     capacity = school_capacity, easting, northing)]
closed <- C[urn %in% main$urn | (phase %in% MAIN_PHASE & is_state_school)][
  !is.na(close_date) & close_date >= as.Date("2015-09-01") & closure_class %in% c("Closure (no successor)", "Amalgamation/merger") &
  !establishment_type %in% NOT_MAIN]
closed <- closed[, .(urn, name = establishment_name, la = la_name, phase, type = establishment_type,
                     event = ifelse(closure_class == "Amalgamation/merger", "Merged", "Closed"), date = close_date,
                     year = acyear(close_date, FALSE), reason = reason_closed, pupils = number_of_pupils,
                     capacity = school_capacity, easting, northing)]
events <- rbind(opened, closed)[, `:=`(area = area_of(la), stage = fifelse(phase %in% c("Primary", "Middle deemed primary"), "Primary", "Secondary"))]
events <- events[year %in% YEARS]
ll <- st_coordinates(st_transform(st_as_sf(events[!is.na(easting)], coords = c("easting", "northing"), crs = 27700), 4326))
events[!is.na(easting), `:=`(lon = ll[, 1], lat = ll[, 2])][, c("easting", "northing") := NULL]
# Schools open in each area in January 2026, by stage, for rates
open_now <- main[status %in% c("Open", "Open, but proposed to close")][, .(area = area_of(la_name),
  stage = fifelse(phase %in% c("Primary", "Middle deemed primary"), "Primary", "Secondary"))][, .N, by = .(area, stage)]
ev_area <- events[, .N, by = .(area, stage, event)] |> dcast(area ~ stage + event, value.var = "N", fill = 0)
ev_area <- merge(ev_area, dcast(open_now, area ~ stage, value.var = "N", fill = 0)[, .(area, primary_open = Primary, secondary_open = Secondary)],
                 by = "area", all = TRUE)
for (v in setdiff(names(ev_area), "area")) set(ev_area, which(is.na(ev_area[[v]])), v, 0L)
trend <- merge(trend, ev_area, by = "area", all.x = TRUE)
trend <- merge(trend, reg_of, by = "area", all.x = TRUE)

# ---- 5. boundaries for the combined areas ------------------------------------------
B <- readRDS(here::here("data", "geography.rds"))$bounds
B <- st_make_valid(st_transform(B, 27700)); B$area <- area_of(B$LANAME)   # union on the National Grid (planar)
bounds <- B |> group_by(area) |> summarise(geometry = st_union(geometry), .groups = "drop") |> st_make_valid() |> st_transform(4326)

res <- list(years = YEARS, t0 = T0, yeargroups = yg, trend = trend, proj = proj, ratio = ratio, dfe_check = dfe_check,
            events = events, bounds = bounds, combine = COMBINE, built = Sys.Date(),
            n = list(areas = sum(trend$level == "Local authority"), opened = nrow(opened), closed = nrow(closed)))
saveRDS(res, here::here("data", "demographics.rds"))
message(sprintf("demographics.rds: %d areas, %s; %d openings, %d closures and mergers, %s",
                res$n$areas, paste(range(YEARS), collapse = " to "), nrow(opened[year %in% YEARS]), nrow(closed[year %in% YEARS]),
                paste(capture.output(print(table(trend[level == "Local authority", primary_class]))), collapse = " ")))
