# 43_finance_closures.R - School finances and pupil numbers before closure: an early warning
# ---------------------------------------------------------------------------
# For every state mainstream school that closed or merged, its pupils and finances in
# the years before it closed, against the schools that stayed open; and a model of
# closure within two years on what a school's own returns show. For finance.qmd.
#
# Sources (all published):
#   School finances: DfE Financial Benchmarking and Insights Tool, data sources
#     (https://financial-benchmarking-and-insights-tool.education.gov.uk/data-sources):
#     CFR full data workbooks for local authority maintained schools, financial years
#     2014-15 to 2025-26 (April to March), and AAR downloads for academies, academic
#     years 2016-17 to 2024-25 (September to August), in data-raw/finance/ (see README).
#   Schools, openings and closures, predecessor links and capacity: GIAS, 10 June 2026
#     (pipeline/07_gias.R; data-raw/gias/ links).
#   Area trends: data/demographics.rds (R/42_demographics.R).
#
# Calculations:
#   One identity per school: a school's earlier URNs are joined through GIAS's
#   one-to-one "Predecessor" links (academy conversion, a change of type), not through
#   mergers, so its finances run unbroken. Where a year has two returns (a part-year CFR
#   and an AAR around conversion), the one with the larger income is kept.
#   Each return gives pupils (FTE), total income, total expenditure, staff costs and the
#   revenue reserve; the in-year balance is income minus expenditure. Academies'
#   reserves are as the AAR reports them for the academy (in multi-academy trusts, the
#   academy's own share, without the trust's central reserve). A CFR year (April to
#   March) is labelled with the academic year it mostly falls in (2021-22 for April 2021
#   to March 2022), as an AAR year is.
#   Measures: pupils; change in pupils over three years (%); pupils as % of capacity
#   (GIAS's latest recorded capacity); revenue reserve and in-year balance as % of income;
#   income per pupil; staff costs as % of expenditure. Each is also put as a percentile
#   among all state mainstream schools of the same stage (primary, secondary) that year.
#   Closure: from R/42 (closed with no successor, or merged), with the last academic year
#   the school taught. A return for year t "closes within two years" if the school's last
#   year is t+1 or t+2; returns in or after the last year are left out of the model.
#   Model: logistic regression of closing within two years on the measures, fitted on
#   returns for 2017-18 to 2020-21 (2016-17 has no three-year change in pupils, so adds no
#   rows) and tested on 2021-22 to 2023-24 (AUC with a bootstrap-by-school interval,
#   calibration by score band, refits on rolls, finance or governance alone and without
#   the area trend, and how many
#   closures the highest-risk 2% and 5% of schools catch).
#   Current schools: the model applied to each open school's latest return. Only counts
#   by authority are published; the school-level scores are written to
#   data-raw/private/ (not committed), so no open school is named as at risk.
#
# Output: data/finance_closures.rds.   Rscript R/43_finance_closures.R  (a few minutes)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(readxl) })
FIN_DIR <- here::here("data-raw", "finance")
num <- function(x) suppressWarnings(as.numeric(x))

# ---- 1. read every return -------------------------------------------------------
header_of <- function(path) {
  best <- NULL
  for (s in excel_sheets(path)) {
    raw <- tryCatch(read_excel(path, sheet = s, col_names = FALSE, n_max = 6, .name_repair = "minimal"), error = function(e) NULL)
    if (is.null(raw) || !nrow(raw)) next
    r <- which(apply(raw, 1, function(x) "URN" %in% trimws(x)))[1]
    if (!is.na(r) && (is.null(best) || ncol(raw) > best$n)) best <- list(sheet = s, skip = r - 1, n = ncol(raw))
  }
  best
}
pick <- function(nms, pats) { for (p in pats) { m <- grep(p, nms, ignore.case = TRUE, value = TRUE); if (length(m)) return(m[1]) }; NA_character_ }
read_return <- function(src, yr) {
  path <- file.path(FIN_DIR, sprintf("%s_%s.xlsx", tolower(src), yr)); if (!file.exists(path)) return(NULL)
  h <- header_of(path); if (is.null(h)) { warning("no data sheet in ", path); return(NULL) }
  if (src == "AAR") { s <- grep("^Academies", excel_sheets(path), value = TRUE)[1]; if (!is.na(s)) h$sheet <- s
    raw <- read_excel(path, sheet = h$sheet, col_names = FALSE, n_max = 6, .name_repair = "minimal")
    h$skip <- which(apply(raw, 1, function(x) "URN" %in% trimws(x)))[1] - 1 }
  # readxl drops leading empty rows when peeking, so the header can sit a row or two lower
  for (extra in 0:3) {
    df <- read_excel(path, sheet = h$sheet, skip = h$skip + extra, .name_repair = "minimal", guess_max = 5000)
    if ("URN" %in% trimws(names(df))) break
  }
  nm <- gsub("\\s+", " ", trimws(names(df))); names(df) <- make.unique(nm)
  cols <- c(URN = pick(nm, "^URN$"),
            pupils = pick(nm, c("^No pupils", "^Number of pupils in academy", "^Number of Pupils \\(FTE\\)", "^Full time equivalent number of pupils")),
            phase = pick(nm, c("^Overall Phase", "^Phase of education")),
            type = pick(nm, "^Type$"),
            period = pick(nm, "^Period covered"),
            income = pick(nm, "^Total Income"),
            expenditure = pick(nm, "^Total Expenditure"),
            staff = pick(nm, c("^Staff Total", "^Total Staff Costs")),
            reserve = pick(nm, "^Revenue Reserve$|^Revenue Reserve:"),
            mat = pick(nm, "^MAT SAT or Central"), trust = pick(nm, "^Trust or Company Name"))
  cols <- cols[!is.na(cols)]
  out <- as.data.table(df)[, unname(cols), with = FALSE]; setnames(out, names(cols))
  for (v in intersect(c("pupils", "period", "income", "expenditure", "staff", "reserve"), names(out))) set(out, j = v, value = num(out[[v]]))
  out[, `:=`(URN = as.integer(num(URN)), year = yr, source = src)]
  out[!is.na(URN)]
}
CFR_YEARS <- sprintf("%d-%02d", 2014:2025, (15:26) %% 100); AAR_YEARS <- sprintf("%d-%02d", 2016:2024, (17:25) %% 100)
fin <- rbindlist(c(lapply(CFR_YEARS, function(y) read_return("CFR", y)), lapply(AAR_YEARS, function(y) read_return("AAR", y))), fill = TRUE)
fin <- fin[is.na(mat) | mat != "Central Services"]
fin[, in_year := income - expenditure]
read_log <- fin[, .(returns = .N, with_income = sum(!is.na(income) & income > 0), with_reserve = sum(!is.na(reserve))), by = .(source, year)][order(source, year)]

# ---- 2. one identity per school --------------------------------------------------
G <- as.data.table(readRDS(here::here("data", "gias", "gias_establishments.rds")))
L <- fread(here::here("data-raw", "gias", "links_edubasealldata20260610.csv"), select = c("URN", "LinkURN", "LinkType"))
succ <- unique(L[LinkType == "Successor", .(from = URN, to = LinkURN)])          # one-to-one: conversion, change of type
succ <- succ[!from %in% succ[duplicated(from), from]]                            # never split a school
final_of <- function(u) { f <- as.integer(u); for (i in 1:10) { nx <- succ$to[match(f, succ$from)]; f <- ifelse(is.na(nx), f, nx) }; f }
fin[, cid := final_of(URN)]
fin <- fin[order(cid, year, -income)][, .SD[1], by = .(cid, year)]              # one return a school-year

# state mainstream schools, as R/42 defines them; stage from the latest URN
DEM <- readRDS(here::here("data", "demographics.rds"))
MAIN_PHASE <- c("Primary", "Secondary", "Middle deemed primary", "Middle deemed secondary", "All-through")
NOT_MAIN <- c("Academy 16-19 converter", "Academy 16 to 19 sponsor led", "Academy secure 16 to 19", "Free schools 16 to 19",
              "Studio schools", "University technical college", "Local authority nursery school", "Pupil referral unit",
              "Academy alternative provision converter", "Academy alternative provision sponsor led", "Free schools alternative provision",
              "Academy special converter", "Academy special sponsor led", "Free schools special")
main <- G[phase %in% MAIN_PHASE & type_group %in% c("Academies", "Free Schools", "Local authority maintained schools") & !establishment_type %in% NOT_MAIN]
area_of <- function(la) ifelse(la %in% names(DEM$combine), DEM$combine[la], la)
ident <- main[, .(cid = as.integer(urn), name = establishment_name, la = la_name, area = area_of(la_name), region = gor, status, easting, northing,
                  stage = fifelse(phase %in% c("Primary", "Middle deemed primary"), "Primary", "Secondary"),
                  capacity = school_capacity, academy = type_group != "Local authority maintained schools")]
fin <- merge(fin, ident, by = "cid")
# closures (R/42), keyed on the closing URN's identity
cl <- DEM$events[event %in% c("Closed", "Merged"), .(cid = final_of(urn), last_year = year, event, close_date = date)]
cl <- cl[, .SD[1], by = cid]
fin <- merge(fin, cl, by = "cid", all.x = TRUE)

# ---- 3. measures -------------------------------------------------------------------
YEARS <- sort(unique(fin$year)); yi <- function(y) match(y, YEARS)
setorder(fin, cid, year)
fin[, `:=`(t = yi(year))]
fin[, pupils3 := pupils[match(t - 3, t)], by = cid]
fin[, `:=`(pupils_chg3 = 100 * (pupils / pupils3 - 1),
           util = 100 * pupils / capacity,
           reserve_pct = 100 * reserve / income, in_year_pct = 100 * in_year / income,
           income_pp = income / pupils, staff_pct = 100 * staff / expenditure,
           deficit = reserve < 0)]
fin[!is.finite(util) | capacity <= 0, util := NA][!is.finite(income_pp) | pupils < 5, income_pp := NA]
for (v in c("reserve_pct", "in_year_pct")) fin[abs(get(v)) > 200, (v) := NA]   # a return with almost no income
fin[, k := ifelse(is.na(last_year), NA_integer_, t - yi(last_year))]              # 0 = the last year taught
pct_rank <- function(x) { r <- frank(x, na.last = "keep", ties.method = "average"); 100 * (r - 0.5) / sum(!is.na(x)) }
MEAS <- c(pupils = "Pupils", pupils_chg3 = "Change in pupils over three years (%)", util = "Pupils as % of capacity",
          reserve_pct = "Revenue reserve, % of income", in_year_pct = "In-year balance, % of income",
          income_pp = "Income per pupil", staff_pct = "Staff costs, % of expenditure")
# percentiles among schools still open that year (not in or after their last year)
fin[, open_yr := is.na(k) | k < 0]
for (v in names(MEAS)) fin[open_yr == TRUE, (paste0("p_", v)) := pct_rank(get(v)), by = .(stage, year)]

# ---- 4. the years before closure ------------------------------------------------------
closers <- fin[!is.na(k) & last_year >= "2017-18" & k %between% c(-6, 0)]
profile <- rbindlist(lapply(names(MEAS), function(v) closers[!is.na(get(v)), .(measure = v, n = .N,
  median = median(get(v)), q1 = quantile(get(v), 0.25), q3 = quantile(get(v), 0.75),
  p_median = median(get(paste0("p_", v)), na.rm = TRUE)), by = .(stage, k)]))
benchmark <- rbindlist(lapply(names(MEAS), function(v) fin[open_yr == TRUE & is.na(k) & !is.na(get(v)),
  .(measure = v, median = median(get(v)), q1 = quantile(get(v), 0.25), q3 = quantile(get(v), 0.75)), by = stage]))
n_closers <- closers[, .(schools = uniqueN(cid)), by = .(stage)]
deficit_k <- closers[!is.na(deficit), .(deficit = 100 * mean(deficit), n = .N), by = .(stage, k)]
deficit_all <- fin[open_yr == TRUE & is.na(k) & !is.na(deficit), .(deficit = 100 * mean(deficit)), by = .(stage, year)]

# ---- 5. a model of closure within two years ------------------------------------------
md <- fin[open_yr == TRUE & year <= "2023-24" & !is.na(pupils) & pupils >= 10 & !is.na(income) & income > 0]
md[, y := as.integer(!is.na(k) & k %in% c(-1, -2))]
md <- merge(md, DEM$trend[level == "Local authority", .(area, area_primary_chg = primary_chg)], by = "area", all.x = TRUE)
md[, `:=`(log_pupils = log(pupils), chg3 = pmax(pmin(pupils_chg3, 50), -50), util_c = pmax(pmin(util, 150), 20),
          res = pmax(pmin(reserve_pct, 40), -40), iy = pmax(pmin(in_year_pct, 20), -20),
          secondary = stage == "Secondary", ipp = log(income_pp))]
VARS <- c("log_pupils", "chg3", "util_c", "res", "iy", "ipp", "area_primary_chg", "academy", "secondary")
VAR_LAB <- c(log_pupils = "Size (log pupils)", chg3 = "Change in pupils over three years (%)", util_c = "Pupils as % of capacity",
             res = "Revenue reserve, % of income", iy = "In-year balance, % of income", ipp = "Income per pupil (log)",
             area_primary_chg = "Area's change in primary-age pupils since 2018/19 (%)", academy = "Academy", secondary = "Secondary school")
mm <- md[complete.cases(md[, ..VARS])]
form <- reformulate(VARS, "y")
TRAIN <- c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21"); TEST <- c("2021-22", "2022-23", "2023-24")
fit_tr <- glm(form, family = binomial, data = mm[year %in% TRAIN])
fit_all <- glm(form, family = binomial, data = mm)
auc <- function(p, y) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
te <- mm[year %in% TEST]; te[, p := predict(fit_tr, te, type = "response")]
catch <- function(d, top) { d <- d[order(-p)]; cut <- ceiling(top * nrow(d)); c(top = top, flagged = cut, caught = sum(d$y[seq_len(cut)]), closures = sum(d$y)) }
test_res <- list(auc = auc(te$p, te$y), n = nrow(te), closures = sum(te$y), schools_closing = uniqueN(te[y == 1, cid]),
                 top2 = catch(te, 0.02), top5 = catch(te, 0.05), base = mean(te$y))
# An earlier warning: closing two or three years on (leaving out the last year before
# closure, when a decision may already be known and rolls fall because of it)
early <- local({
  d <- copy(mm)[!(k %in% -1) | is.na(k)][, y := as.integer(!is.na(k) & k %in% c(-2, -3))]
  f <- glm(form, family = binomial, data = d[year %in% TRAIN]); e <- d[year %in% TEST]
  e[, p := predict(f, e, type = "response")]
  list(auc = auc(e$p, e$y), closures = sum(e$y), top2 = catch(e, 0.02), top5 = catch(e, 0.05))
})
test_res$early <- early
# Finance alone and rolls alone, to see which carries the warning
part_auc <- function(vars) { f <- glm(reformulate(vars, "y"), family = binomial, data = mm[year %in% TRAIN])
  e <- mm[year %in% TEST]; auc(predict(f, e, type = "response"), e$y) }
test_res$parts <- c(rolls = part_auc(c("log_pupils", "chg3", "util_c", "area_primary_chg", "secondary")),
                    finance = part_auc(c("res", "iy", "ipp", "secondary")),
                    governance = part_auc(c("academy", "secondary")),
                no_area = part_auc(setdiff(VARS, "area_primary_chg")))   # without the area trend (which looks ahead)
# effects per SD (yes/no terms as they are), from the model on every year
sds <- sapply(VARS, function(v) if (is.logical(mm[[v]])) 1 else sd(mm[[v]]))
co <- summary(fit_all)$coefficients
term_of <- function(v) if (is.logical(mm[[v]])) paste0(v, "TRUE") else v
effects <- rbindlist(lapply(VARS, function(v) { b <- co[term_of(v), ]; s <- sds[[v]]
  data.table(term = v, label = VAR_LAB[[v]], per = if (is.logical(mm[[v]])) "yes vs no" else "per SD", sd = s,
             or = exp(b[["Estimate"]] * s), lo = exp((b[["Estimate"]] - 1.96 * b[["Std. Error"]]) * s),
             hi = exp((b[["Estimate"]] + 1.96 * b[["Std. Error"]]) * s), z = b[["z value"]]) }))
model_info <- list(n = nrow(mm), events = sum(mm$y), schools = uniqueN(mm$cid), closing_schools = uniqueN(mm[y == 1, cid]),
                   years = range(mm$year), call = deparse(form), aic = AIC(fit_all))
# For the methodology section (method.qmd, _method_finance.qmd): the coefficients per unit,
# how the sample narrows, calibration on the test years, and an interval for the test AUC
model_info$coefs <- data.table(term = rownames(co), est = co[, 1], se = co[, 2], z = co[, 3], p = co[, 4])
model_info$flow <- c(returns = nrow(fin), open_to_2324 = nrow(fin[open_yr == TRUE & year <= "2023-24"]),
                     eligible = nrow(md), complete = nrow(mm))
model_info$null_dev <- fit_all$null.deviance; model_info$dev <- fit_all$deviance
test_res$calib <- te[, .(band = cut(frank(p) / .N, c(0, 0.5, 0.8, 0.9, 0.95, 0.98, 1),
                                    c("Lowest 50%", "50-80%", "80-90%", "90-95%", "95-98%", "Riskiest 2%"), include.lowest = TRUE), p, y)][
  , .(returns = .N, closing = sum(y), predicted = 100 * mean(p), observed = 100 * mean(y)), by = band][order(band)]
set.seed(43)   # bootstrap by school, so a school's several years stay together
ids <- unique(te$cid); te_by <- split(seq_len(nrow(te)), te$cid)
boot_auc <- replicate(500, { s <- unlist(te_by[as.character(sample(ids, replace = TRUE))], use.names = FALSE); auc(te$p[s], te$y[s]) })
test_res$auc_ci <- quantile(boot_auc, c(0.025, 0.975))
# simple warning signs, and how often closing schools showed them
signs <- function(d) d[, .(small = pupils < ifelse(secondary, 600, 100), falling = chg3 <= -15, deficit_in_year = iy < -3,
                          reserve_neg = res < 0, under_capacity = util_c < 70)]
sg <- cbind(mm[, .(y, stage, year)], signs(mm))
sg[, n_signs := small + falling + deficit_in_year + reserve_neg + under_capacity]
sign_rates <- sg[, .(returns = .N, closing = sum(y), rate = 100 * mean(y)), by = .(stage, n_signs = pmin(n_signs, 4))][order(stage, n_signs)]
sign_share <- rbindlist(lapply(c("small", "falling", "deficit_in_year", "reserve_neg", "under_capacity"), function(s)
  sg[, .(sign = s, closing = 100 * mean(get(s)[y == 1], na.rm = TRUE), open = 100 * mean(get(s)[y == 0], na.rm = TRUE)), by = stage]))

# ---- 6. open schools now: counts by authority, no names --------------------------------
latest <- fin[status %in% c("Open", "Open, but proposed to close") & is.na(last_year)][order(cid, -t)][, .SD[1], by = cid]
latest <- merge(latest, DEM$trend[level == "Local authority", .(area, area_primary_chg = primary_chg)], by = "area", all.x = TRUE)
latest[, `:=`(log_pupils = log(pupils), chg3 = pmax(pmin(pupils_chg3, 50), -50), util_c = pmax(pmin(util, 150), 20),
              res = pmax(pmin(reserve_pct, 40), -40), iy = pmax(pmin(in_year_pct, 20), -20),
              secondary = stage == "Secondary", ipp = log(income_pp))]
lt <- latest[complete.cases(latest[, ..VARS]) & pupils >= 10 & year >= "2023-24"]
lt[, p := predict(fit_all, lt, type = "response")]
cut2 <- quantile(mm[, predict(fit_all, mm, type = "response")], 0.98)                    # the top 2% of returns, all years
lt[, flag := p >= cut2]
lt_signs <- cbind(lt[, .(area, stage)], signs(lt))[, n_signs := small + falling + deficit_in_year + reserve_neg + under_capacity]
by_area <- merge(lt[, .(schools = .N, flagged = sum(flag)), by = .(area, stage)],
                 lt_signs[, .(three_plus = sum(n_signs >= 3)), by = .(area, stage)], by = c("area", "stage"))
now_summary <- list(schools = nrow(lt), flagged = sum(lt$flag), cut = cut2, years = table(lt$year),
                    by_stage = lt[, .(schools = .N, flagged = sum(flag), mean_p = mean(p)), by = stage])
dir.create(here::here("data-raw", "private"), showWarnings = FALSE, recursive = TRUE)
fwrite(lt[order(-p), .(URN = cid, name, la, stage, year, pupils, pupils_chg3, util, reserve_pct, in_year_pct, p, flag)],
       here::here("data-raw", "private", "closure_risk_open_schools.csv"))           # private: not committed

# Examples: closed schools in Sussex, named (they have closed)
sussex <- c("Brighton and Hove", "East Sussex", "West Sussex")
examples <- fin[!is.na(k) & area %in% sussex & last_year >= "2019-20" & k %between% c(-6, 0),
                .(cid, name, area, stage, event, last_year, year, k, pupils, util, reserve_pct, in_year_pct, income_pp)]

res <- list(read_log = read_log, measures = MEAS, profile = profile, benchmark = benchmark, n_closers = n_closers,
            deficit_k = deficit_k, deficit_all = deficit_all, effects = effects, model = model_info, test = test_res,
            sign_rates = sign_rates, sign_share = sign_share, by_area = by_area, now = now_summary, examples = examples,
            years = YEARS, built = Sys.Date())
saveRDS(res, here::here("data", "finance_closures.rds"))
# The school-year panel, for the area finance pages (R/44): local only (data/cache is not committed)
saveRDS(fin[, .(cid, name, la, area, region, stage, academy, status, easting, northing, year, source, mat, trust, pupils, income,
                expenditure, in_year, reserve, staff, capacity, util, pupils_chg3, reserve_pct, in_year_pct, income_pp,
                staff_pct, deficit, last_year, k)], here::here("data", "cache", "finance_panel.rds"))
message(sprintf("finance_closures.rds: %d returns, %d school-years in the model (%d closing within two years); test AUC %.2f; %d of %d open schools in the top-2%% band",
                nrow(fin), nrow(mm), sum(mm$y), test_res$auc, sum(lt$flag), nrow(lt)))
