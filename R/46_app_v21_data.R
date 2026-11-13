# 46_app_v21_data.R - Pupils and places, and school finances, for the app (v2.1)
# ---------------------------------------------------------------------------
# The School Information System v2.1 adds two tabs: "Pupils and Places" (from the
# demographics page) and "School Finances" (from the finance pages). This writes the
# small tables they need to app/data/v21.rds, from files the site already builds:
#
#   pupils    pupils by year group, Reception to Year 11, England, regions and local
#             authorities, January 2016 to January 2026 (R/42: data/demographics.rds,
#             from DfE "Schools, pupils and their characteristics")
#   trend     each area's trend in primary and secondary-age pupils since 2018/19, and the
#             Year 7 roll-forward to 2032/33 (R/42)
#   proj      the roll-forward year by year: Year 7 intakes and secondary-age totals (R/42)
#   events    state mainstream schools opened (new places) and closed or merged since
#             2015/16 (R/42, from GIAS). These are past events, from GIAS's public register.
#   fin       finance aggregates by England, region, authority and Sussex district: share
#             in deficit, median reserve and in-year balance, income per pupil, reserve
#             bands (R/44: data/area_finance.rds, from DfE CFR and AAR returns)
#   warn      counts of open schools showing three or more of the five warning signs, and
#             in the closure model's riskiest 2%, by authority and Sussex district (R/43 and
#             R/44). Counts only: no school is named, as agreed for the finance work.
#   model     the closure model's headline test results (R/43)
#
# Names: R/42 and R/44 combine three reorganised councils (Dorset with BCP, North and
# West Northamptonshire, Cumberland with Westmorland and Furness); `combine` maps the
# app's authorities to those combined areas. R/42's "Yorkshire and The Humber" is
# renamed to the app's "Yorkshire and the Humber".
#
# Rscript R/46_app_v21_data.R   (a few seconds; run R/42, R/43 and R/44 first)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(data.table))

dm <- readRDS(here::here("data", "demographics.rds"))
af <- readRDS(here::here("data", "area_finance.rds"))
fc <- readRDS(here::here("data", "finance_closures.rds"))

LEV <- c("National" = "England", "Regional" = "Region", "Local authority" = "Authority")
fix_area <- function(x) sub("Yorkshire and The Humber", "Yorkshire and the Humber", x, fixed = TRUE)
tidy <- function(d) { d <- as.data.table(d); d[, level := unname(LEV[level])]; d[, area := fix_area(area)]
  if ("region" %in% names(d)) d[, region := fix_area(region)]; d[] }

pupils <- tidy(dm$yeargroups)[, c("level", "area", "year", "R", paste0("Y", 1:11), "primary", "secondary"), with = FALSE]
trend <- tidy(dm$trend)
proj <- tidy(dm$proj)[, .(level, area, year, Y7, secondary)]
events <- as.data.table(dm$events)[, .(urn, name, area, la, stage, event, year, date, reason, type, pupils, capacity, lon, lat)]
events[, area := sub("^Pre-LGR [0-9]+ ", "", area)]       # GIAS labels closed schools of the old councils this way
events[, region := trend[level == "Authority"][match(events$area, area), region]]
stopifnot(!anyNA(events$region))

# Warning signs: authority counts from R/43, Sussex districts from R/44; regions and
# England summed from the authorities
reg_of <- setNames(af$units$parent, af$units$unit)[af$units$level == "Authority"]
wa <- as.data.table(fc$by_area)[, level := "Authority"]
wa[, region := reg_of[area]]
stopifnot(!anyNA(wa$region))
warn <- rbind(
  wa[, .(level, area, stage, schools, flagged, three_plus)],
  wa[, .(level = "Region", schools = sum(schools), flagged = sum(flagged), three_plus = sum(three_plus)), by = .(area = region, stage)],
  wa[, .(level = "England", area = "England", schools = sum(schools), flagged = sum(flagged), three_plus = sum(three_plus)), by = stage],
  as.data.table(af$warn_district)[, .(level = "District", area, stage, schools, flagged, three_plus)], use.names = TRUE)
stopifnot(warn[level == "England", sum(flagged)] == fc$now$flagged)

combine <- dm$combine
df <- function(x) as.data.frame(x)                  # plain data frames: the app does not load data.table
out <- list(
  pupils = df(pupils), trend = df(trend), proj = df(proj), events = df(events), t0 = dm$t0, years = dm$years,
  fin = list(stats = df(af$stats), bands = df(af$bands), band_labels = af$band_labels,
             latest = af$latest, years = af$years, units = df(af$units)),
  warn = df(warn),
  model = list(auc = fc$test$auc, top2 = fc$test$top2, top5 = fc$test$top5, closures = fc$test$closures,
               schools = fc$now$schools, flagged = fc$now$flagged, cut = unname(fc$now$cut), years = fc$model$years),
  combine = combine, built = Sys.Date())

dir.create(here::here("app", "data"), showWarnings = FALSE)
saveRDS(out, here::here("app", "data", "v21.rds"), compress = "xz")
message(sprintf("app/data/v21.rds: %d pupil rows, %d openings and closures, finance %d rows, warnings %d rows (%.1f MB)",
                nrow(pupils), nrow(events), nrow(out$fin$stats), nrow(warn),
                file.size(here::here("app", "data", "v21.rds")) / 1e6))
