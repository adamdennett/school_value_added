# 36_workforce.R - The school workforce as levers: retention, sickness, staffing ratios
# ---------------------------------------------------------------------------
# The "Pulling the Right Lever" model found teacher retention and sickness worth a little each.
# This builds, for every school in England, its workforce measures from DfE's School Workforce
# in England (school level, 2021/22 to 2024/25, the value-added model's years):
#
#   retention      % of teachers (FTE) in post at the census who were in the same school a year
#                  later (remained_in_the_same_school / teacher_fte_in_census_year)
#   sick_days      average days of sickness absence per teacher, all teachers
#   sick_share     % of teachers taking any sickness absence
#   ptr            pupils per qualified teacher
#   ta_ratio       teaching assistants (FTE) per teacher (FTE)
#   leadership     % of teachers on the leadership pay range
#
# and how each goes with Value Added v2.1 across England, all at once, so the pieces can be
# added without counting the same thing twice. Value added already allows for intake and
# absence, so these are associations beyond both. They are not proof that changing one would
# move results: a well-run school both keeps its teachers and gets good results.
#
# Sources (Explore Education Statistics, School Workforce in England, school level):
#   turnover 1df9bfbf-b573-4f1d-ba87-06a36387f2e5, pay 760ad614-44ac-4e8a-8fdd-a8080c762770,
#   sickness aa13e99e-534c-4f70-9c43-c7f9544296b3, workforce size 6cb459c2-5cc5-41be-87c8-01c0986514e8,
#   pupil ratios 12849f4a-1427-4dc2-9f7a-e069087824aa. Downloaded to data-raw/ees/workforce/.
# Output: data/workforce_levers.rds (committed). Rscript R/36_workforce.R
# ---------------------------------------------------------------------------

source(here::here("R", "site_helpers.R"))
suppressPackageStartupMessages({ library(data.table); library(sandwich) })
D <- here::here("data-raw", "ees", "workforce"); dir.create(D, showWarnings = FALSE, recursive = TRUE)
IDS <- c(turnover = "1df9bfbf-b573-4f1d-ba87-06a36387f2e5", pay = "760ad614-44ac-4e8a-8fdd-a8080c762770", sickness = "aa13e99e-534c-4f70-9c43-c7f9544296b3",
         workforce_size = "6cb459c2-5cc5-41be-87c8-01c0986514e8", pupil_ratios = "12849f4a-1427-4dc2-9f7a-e069087824aa")
for (k in names(IDS)) { f <- file.path(D, paste0(k, ".csv"))
  if (!file.exists(f)) download.file(sprintf("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/%s/csv", IDS[[k]]), f, mode = "wb", quiet = TRUE) }
YRS <- c(202122L, 202223L, 202324L, 202425L)
rd <- function(k, cols) { x <- fread(file.path(D, paste0(k, ".csv")), select = c("time_period", "geographic_level", "school_urn", cols), na.strings = c("", "z", "x", "c", "u", ":", "NA", "low"))
  x[geographic_level == "School" & time_period %in% YRS][, geographic_level := NULL][, URN := as.integer(school_urn)][, school_urn := NULL][] }
num <- function(x) suppressWarnings(as.numeric(x))
to <- rd("turnover", c("teacher_fte_in_census_year", "remained_in_the_same_school"))[, .(URN, time_period, retention = 100 * num(remained_in_the_same_school) / num(teacher_fte_in_census_year))]
sk <- rd("sickness", c("average_number_of_days_all_teachers", "percentage_taking_absence"))[, .(URN, time_period, sick_days = num(average_number_of_days_all_teachers), sick_share = num(percentage_taking_absence))]
pr <- rd("pupil_ratios", c("pupil_to_qual_teacher_ratio"))[, .(URN, time_period, ptr = num(pupil_to_qual_teacher_ratio))]
ws <- rd("workforce_size", c("fte_all_teachers", "fte_teaching_assistants"))[, .(URN, time_period, ta_ratio = num(fte_teaching_assistants) / num(fte_all_teachers))]
py <- rd("pay", c("teachers_on_leadership_pay_range_percent"))[, .(URN, time_period, leadership = num(teachers_on_leadership_pay_range_percent))]
W <- Reduce(function(a, b) merge(a, b, by = c("URN", "time_period"), all = TRUE), list(to, sk, pr, ws, py))
for (v in setdiff(names(W), c("URN", "time_period"))) set(W, which(!is.finite(W[[v]])), v, NA_real_)
W[retention > 100, retention := NA]
wf <- W[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = URN, .SDcols = c("retention", "sick_days", "sick_share", "ptr", "ta_ratio", "leadership")]
for (v in names(wf)[-1]) set(wf, which(!is.finite(wf[[v]])), v, NA_real_)

# ---- How each goes with value added, beyond intake and absence, all together --------------------------
V <- load_va(); va <- dcast(as.data.table(V$va)[group %in% c("all", "disadvantaged", "non_disadvantaged")], URN ~ group, value.var = "va_shrunk")
M <- merge(va, wf, by = "URN")
# Levers: retention, sickness days and staffing. Teaching assistants per teacher and leadership pay mostly
# reflect who a school serves (more pupils with special needs, more senior posts), and the share of
# teachers taking any sickness is only meaningful given total days, so they are kept as context only.
LV <- c("retention", "sick_days", "ptr")
LV_ALL <- c("retention", "sick_days", "sick_share", "ptr", "ta_ratio", "leadership")
fit_g <- function(y) { d <- M[complete.cases(M[, c(y, LV), with = FALSE])]; m <- lm(reformulate(LV, y), d); s <- sqrt(diag(vcovHC(m, "HC3")))
  data.table(group = y, lever = LV, b = coef(m)[LV], se = s[LV], n = nrow(d), sd = sapply(LV, function(v) sd(d[[v]]))) }
coefs <- rbindlist(lapply(c("all", "disadvantaged", "non_disadvantaged"), fit_g))[, t := b / se]
one <- rbindlist(lapply(c("all", "disadvantaged", "non_disadvantaged"), function(y) rbindlist(lapply(LV_ALL, function(v) { d <- M[!is.na(get(y)) & !is.na(get(v))]
  m <- lm(reformulate(v, y), d); s <- sqrt(diag(vcovHC(m, "HC3"))); data.table(group = y, lever = v, b = coef(m)[[v]], se = s[[v]]) }))))
out <- list(built = Sys.Date(), years = YRS, schools = wf, coefs = coefs, one_at_a_time = one, n = nrow(M),
            labels = c(retention = "Teacher retention (% staying a year)", sick_days = "Teacher sickness (days per teacher)", sick_share = "Teachers taking sickness absence (%)",
                       ptr = "Pupils per qualified teacher", ta_ratio = "Teaching assistants per teacher", leadership = "Teachers on leadership pay (%)"))
saveRDS(out, here::here("data", "workforce_levers.rds")); message("Saved data/workforce_levers.rds: ", nrow(wf), " schools")
print(coefs[, .(group, lever, b = round(b, 3), t = round(t, 1), per_sd = round(b * sd, 2), n)])
