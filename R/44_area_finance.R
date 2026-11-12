# 44_area_finance.R - School finances by area: England, regions, authorities, Sussex districts
# ---------------------------------------------------------------------------
# Area-level summaries of state mainstream schools' finance returns, for the area finance
# pages (finance_areas/*.qmd, from templates/finance_area.qmd by R/45_finance_pages.R).
# Only aggregates are written: no school is named or can be picked out, as agreed for the
# finance work (see finance.qmd).
#
# Source: data/cache/finance_panel.rds, written by R/43_finance_closures.R from DfE's CFR
#   (maintained schools, financial years 2014-15 to 2025-26) and AAR (academies, 2016-17
#   to 2024-25) returns, one identity per school across URN changes.
# Areas: England; regions (GIAS); local authorities as in R/42 (three reorganised councils
#   combined); and the 13 Sussex districts (Brighton and Hove, East Sussex's five, West
#   Sussex's seven), placing each school in its district by its GIAS easting and northing
#   (data/sussex_lads.rds, ONS boundaries).
#
# For each area, year, stage (primary, secondary, both) and sector (maintained, academy,
# both): schools with a return; pupils (FTE), income, expenditure, in-year balance and
# revenue reserve summed; the share of schools with a negative revenue reserve (in
# deficit) and with an in-year deficit; median reserve and in-year balance as % of income;
# income per pupil (total income / total pupils); staff costs as % of expenditure. Plus,
# for the latest year, how many schools fall in each band of reserve as % of income, and
# the spread (10th to 90th percentiles).
# Academies' reserves are as the AAR reports them for the academy: in a multi-academy
# trust that is the academy's share, without the trust's central reserve, and some trusts
# report none, so the maintained schools carry the clearest reserve series. The years
# with both sectors are 2016-17 to 2024-25; 2014-15, 2015-16 and 2025-26 are maintained only.
#
# Also the Sussex districts' warning-sign counts (from R/43's private list of open schools,
# counted by district; no school is named or written out).
#
# Output: data/area_finance.rds.   Rscript R/44_area_finance.R  (under a minute)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(sf) })
F <- readRDS(here::here("data", "cache", "finance_panel.rds"))
F <- F[!is.na(income) & income > 0 & !is.na(pupils) & pupils > 0]

# Sussex districts from coordinates
lads <- st_transform(readRDS(here::here("data", "sussex_lads.rds")), 27700)
sx <- F[area %in% unique(lads$la) & !is.na(easting)][, .(cid, easting, northing)] |> unique()
pts <- st_as_sf(sx, coords = c("easting", "northing"), crs = 27700)
sx$lad <- lads$lad[st_nearest_feature(pts, lads)]           # nearest, so a school on a boundary still lands
F <- merge(F, sx[, .(cid, lad)], by = "cid", all.x = TRUE)

# Long by area: each return counts in England, its region, its authority and (Sussex) its district
L <- rbindlist(list(
  F[, .(level = "England", unit = "England", cid, year, stage, academy, pupils, income, expenditure, in_year, reserve, staff, reserve_pct, in_year_pct)],
  F[!is.na(region), .(level = "Region", unit = region, cid, year, stage, academy, pupils, income, expenditure, in_year, reserve, staff, reserve_pct, in_year_pct)],
  F[, .(level = "Authority", unit = area, cid, year, stage, academy, pupils, income, expenditure, in_year, reserve, staff, reserve_pct, in_year_pct)],
  F[!is.na(lad), .(level = "District", unit = lad, cid, year, stage, academy, pupils, income, expenditure, in_year, reserve, staff, reserve_pct, in_year_pct)]))
# every stage and sector, and both together
L <- rbindlist(list(L, copy(L)[, stage := "All"]))
L[, sector := fifelse(academy, "Academy", "Maintained")]
L <- rbindlist(list(L, copy(L)[, sector := "All"]))

summ <- function(d) d[, .(schools = uniqueN(cid), pupils = sum(pupils), income = sum(income), expenditure = sum(expenditure),
                          in_year = sum(in_year), reserve = sum(reserve, na.rm = TRUE),
                          deficit_pct = 100 * mean(reserve < 0, na.rm = TRUE), iy_deficit_pct = 100 * mean(in_year < 0, na.rm = TRUE),
                          med_reserve = median(reserve_pct, na.rm = TRUE), med_in_year = median(in_year_pct, na.rm = TRUE),
                          income_pp = sum(income) / sum(pupils), staff_pct = 100 * sum(staff, na.rm = TRUE) / sum(expenditure[!is.na(staff)])),
                      by = .(level, unit, year, stage, sector)]
stats <- summ(L)

# The latest year by sector: maintained 2025-26, academies 2024-25
latest <- c(Maintained = max(F$year[F$source == "CFR"]), Academy = max(F$year[F$source == "AAR"]))
latest["All"] <- min(latest)
BANDS <- c(-Inf, -10, -5, 0, 5, 10, 20, Inf)
BAND_LAB <- c("Below -10%", "-10% to -5%", "-5% to 0%", "0% to 5%", "5% to 10%", "10% to 20%", "Over 20%")
lat <- rbindlist(lapply(names(latest), function(s) L[sector == s & year == latest[[s]] & !is.na(reserve_pct)]))
bands <- lat[, .(schools = .N), by = .(level, unit, stage, sector, band = cut(reserve_pct, BANDS, BAND_LAB, right = FALSE))]
spread <- lat[, as.list(quantile(reserve_pct, c(0.1, 0.25, 0.5, 0.75, 0.9))), by = .(level, unit, stage, sector)]
setnames(spread, c("10%", "25%", "50%", "75%", "90%"), c("p10", "p25", "p50", "p75", "p90"))

# Warning-sign counts for the Sussex districts (counts only). R/43 gives them by authority;
# here the same open schools (its private list) are placed in districts as above, and the
# five signs recomputed from the list's columns with R/43's thresholds. Kept from the last
# build if the private list is not on this machine.
pv <- here::here("data-raw", "private", "closure_risk_open_schools.csv")
warn_district <- if (file.exists(pv)) {
  w <- merge(fread(pv)[, cid := as.integer(URN)], sx[, .(cid, lad)], by = "cid")
  w[, n_signs := (pupils < ifelse(stage == "Secondary", 600, 100)) + (pupils_chg3 <= -15) + (in_year_pct < -3) +
      (reserve_pct < 0) + (util < 70)]
  w[, .(schools = .N, flagged = sum(flag), three_plus = sum(n_signs >= 3)), by = .(area = lad, stage)]
} else tryCatch(readRDS(here::here("data", "area_finance.rds"))$warn_district, error = function(e) NULL)

units <- unique(rbind(F[, .(level = "Authority", unit = area, parent = region)],
                      F[!is.na(lad), .(level = "District", unit = lad, parent = area)],
                      F[!is.na(region), .(level = "Region", unit = region, parent = "England")]))[, .SD[1], by = .(level, unit)]
res <- list(stats = stats, bands = bands, spread = spread, units = units, latest = latest, band_labels = BAND_LAB,
            years = sort(unique(F$year)), both_years = sort(unique(F$year[F$source == "AAR"])),
            warn_district = warn_district, built = Sys.Date())
saveRDS(res, here::here("data", "area_finance.rds"))
b <- stats[unit == "Brighton and Hove" & level == "Authority" & stage == "All" & sector == "Maintained" & year == latest[["Maintained"]]]
message(sprintf("area_finance.rds: %d areas; Brighton and Hove maintained schools %s: %d schools, %.0f%% in deficit, net reserves £%.1fm",
                uniqueN(stats[, paste(level, unit)]), latest[["Maintained"]], b$schools, b$deficit_pct, b$reserve / 1e6))
if (!is.null(warn_district)) {   # check: Brighton and Hove as a district should match R/43's authority counts
  fc <- readRDS(here::here("data", "finance_closures.rds"))$by_area[area == "Brighton and Hove", .(schools = sum(schools), flagged = sum(flagged), three = sum(three_plus))]
  wd <- warn_district[area == "Brighton and Hove", .(schools = sum(schools), flagged = sum(flagged), three = sum(three_plus))]
  message(sprintf("warning signs, Brighton and Hove: by district %d/%d/%d, R/43 by authority %d/%d/%d",
                  wd$schools, wd$flagged, wd$three, fc$schools, fc$flagged, fc$three))
}
