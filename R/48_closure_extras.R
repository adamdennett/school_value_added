# 48_closure_extras.R - Do Ofsted, SEN and disadvantage add to the closure model?
# ---------------------------------------------------------------------------
# Follows up the House of Commons Library briefing "Declining pupil numbers in England"
# (CBP-12207, September 2026), which cites London Councils (January 2026: closure
# decisions weigh parental preference, Ofsted ratings, travel routes, projected demand
# and financial sustainability), the BBC (schools that closed in 2020 to 2025 had more
# pupils with special educational needs than average) and the NAO (April 2026: look at
# the effect of falling rolls on disadvantaged pupils). R/43's model of closure within
# two years uses rolls and finance only. This tests three more terms on the same schools:
#
#   fsm   % of pupils known to be eligible for free school meals (school census)
#   sen   % of pupils with an EHC plan or on SEN support (SEN in England)
#   ofsted  the latest graded overall effectiveness at the start of the year (Ofsted's
#           year-end management information as at 31 August), Good or Outstanding /
#           Requires improvement / Inadequate / not yet graded
#
# The years with all three: 2019-20 to 2023-24 (the school-level census and SEN files
# start in 2019-20). Compared on identical rows: R/43's terms alone and with the three
# added, by a likelihood-ratio test (all years) and by the AUC on 2022-23 and 2023-24 for
# a model fitted on 2019-20 to 2021-22. Also: how schools in their last two years before
# closing compare with schools staying open, and the share of pupils who are disadvantaged.
#
# Sources (all published):
#   data/cache/finance_panel.rds (R/43: CFR and AAR returns, GIAS identities, closures)
#   School census, school-level underlying data, 2019/20 to 2023/24: data-raw/ees/spc_all/
#     (EES "Schools, pupils and their characteristics" releases, as R/14)
#   Special educational needs in England, school-level underlying data, 2019/20 to
#     2023/24: data-raw/ees/sen_all/ (EES release files; release ids in the README)
#   Ofsted, State-funded schools inspections and outcomes as at 31 August 2018 to 2023
#     (gov.uk official statistics; the CSV of each school's latest inspection):
#     data-raw/ofsted/ofsted_as_at_31_aug_YYYY.csv
#   GIAS links (data-raw/gias/links_edubasealldata20260610.csv), to follow URNs to the
#     school's identity as R/43 does
#
# Output: data/closure_extras.rds.   Rscript R/48_closure_extras.R   (about a minute)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(data.table))
num <- function(x) suppressWarnings(as.numeric(x))
F <- readRDS(here::here("data", "cache", "finance_panel.rds")); setDT(F)
DEM <- readRDS(here::here("data", "demographics.rds"))

# identities across URN changes, as R/43
L <- fread(here::here("data-raw", "gias", "links_edubasealldata20260610.csv"), select = c("URN", "LinkURN", "LinkType"))
succ <- unique(L[LinkType == "Successor", .(from = URN, to = LinkURN)]); succ <- succ[!from %in% succ[duplicated(from), from]]
final_of <- function(u) { f <- as.integer(u); for (i in 1:10) { nx <- succ$to[match(f, succ$from)]; f <- ifelse(is.na(nx), f, nx) }; f }

# ---- R/43's model data, rebuilt from the panel --------------------------------------
md <- F[(is.na(k) | k < 0) & year <= "2023-24" & !is.na(pupils) & pupils >= 10 & !is.na(income) & income > 0]
md[, y := as.integer(!is.na(k) & k %in% c(-1, -2))]
md <- merge(md, as.data.table(DEM$trend)[level == "Local authority", .(area, area_primary_chg = primary_chg)], by = "area", all.x = TRUE)
md[, `:=`(log_pupils = log(pupils), chg3 = pmax(pmin(pupils_chg3, 50), -50), util_c = pmax(pmin(util, 150), 20),
          res = pmax(pmin(reserve_pct, 40), -40), iy = pmax(pmin(in_year_pct, 20), -20),
          secondary = stage == "Secondary", ipp = log(income_pp))]
VARS <- c("log_pupils", "chg3", "util_c", "res", "iy", "ipp", "area_primary_chg", "academy", "secondary")

# ---- the three extra terms ----------------------------------------------------------
YRS <- c("2019-20", "2020-21", "2021-22", "2022-23", "2023-24")
pick <- function(nms, pats) { for (p in pats) { m <- grep(p, nms, ignore.case = TRUE, value = TRUE); if (length(m)) return(m[1]) }; NA_character_ }
read_zip_csv <- function(z, pattern) {
  f <- grep(pattern, unzip(z, list = TRUE)$Name, value = TRUE, ignore.case = TRUE)[1]
  tmp <- tempfile(); dir.create(tmp); unzip(z, files = f, exdir = tmp, junkpaths = TRUE)
  fread(file.path(tmp, basename(f)), showProgress = FALSE)
}
fsm <- rbindlist(lapply(YRS, function(yr) {
  x <- read_zip_csv(here::here("data-raw", "ees", "spc_all", paste0("spc_", yr, ".zip")), "school_level_underlying")
  n <- names(x); u <- pick(n, "^urn$"); hc <- pick(n, "^headcount of pupils$"); fe <- pick(n, "^number of pupils known to be eligible for free school meals$")
  x[, .(URN = as.integer(get(u)), year = yr, headcount = num(get(hc)), fsm_n = num(get(fe)))]
}))
sen <- rbindlist(lapply(YRS, function(yr) {
  x <- read_zip_csv(here::here("data-raw", "ees", "sen_all", paste0("sen_", yr, ".zip")), "sen_school_level_u")
  n <- names(x); u <- pick(n, c("^urn$", "school_urn")); tp <- pick(n, c("^total pupils$", "^total_pupils$"))
  sp <- pick(n, c("^sen support$", "^sen_support$")); eh <- pick(n, c("^ehc plan$", "^ehc_plan$"))
  if (anyNA(c(u, tp, sp, eh))) stop("SEN columns not found for ", yr, ": ", paste(head(n, 40), collapse = ", "))
  x[, .(URN = as.integer(get(u)), year = yr, sen_total = num(get(tp)), sen_n = num(get(sp)) + num(get(eh)))]
}))
ofs <- rbindlist(lapply(YRS, function(yr) {
  y0 <- as.integer(substr(yr, 1, 4))                      # graded as at 31 August before the year starts
  x <- fread(here::here("data-raw", "ofsted", sprintf("ofsted_as_at_31_aug_%d.csv", y0)), showProgress = FALSE)
  oe <- pick(names(x), "^Overall effectiveness$")
  x[, .(URN = as.integer(URN), year = yr, grade = num(get(oe)))]
}))
for (d in list(fsm, sen, ofs)) d[, cid := final_of(URN)]
fsm <- fsm[, .(fsm = 100 * sum(fsm_n, na.rm = TRUE) / sum(headcount, na.rm = TRUE)), by = .(cid, year)]
sen <- sen[, .(sen = 100 * sum(sen_n, na.rm = TRUE) / sum(sen_total, na.rm = TRUE)), by = .(cid, year)]
ofs <- ofs[, .(grade = min(grade, na.rm = TRUE)), by = .(cid, year)][!is.finite(grade), grade := NA]
mx <- merge(merge(merge(md[year %in% YRS], fsm, by = c("cid", "year"), all.x = TRUE), sen, by = c("cid", "year"), all.x = TRUE),
            ofs, by = c("cid", "year"), all.x = TRUE)
mx[, ofsted := factor(fcase(grade %in% 1:2, "Good or Outstanding", grade == 3, "Requires improvement", grade == 4, "Inadequate", default = "Not graded"),
                      c("Good or Outstanding", "Requires improvement", "Inadequate", "Not graded"))]
mx[, `:=`(fsm = pmin(fsm, 80), sen = pmin(sen, 60))]
cover <- mx[, .(rows = .N, fsm = mean(!is.na(fsm)), sen = mean(!is.na(sen)), ofsted_graded = mean(ofsted != "Not graded")), by = year]

EXTRA <- c("fsm", "sen", "ofsted")
mm <- mx[complete.cases(mx[, c(VARS, "fsm", "sen"), with = FALSE])]
f0 <- reformulate(VARS, "y"); f1 <- reformulate(c(VARS, EXTRA), "y")
auc <- function(p, y) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
TRAIN <- c("2019-20", "2020-21", "2021-22"); TEST <- c("2022-23", "2023-24")
fit_auc <- function(form) { f <- glm(form, family = binomial, data = mm[year %in% TRAIN]); te <- mm[year %in% TEST]
  p <- predict(f, te, type = "response"); c(auc = auc(p, te$y), top5 = { o <- order(-p); n <- ceiling(0.05 * length(p)); sum(te$y[o][1:n]) / sum(te$y) }) }
m0 <- glm(f0, family = binomial, data = mm); m1 <- glm(f1, family = binomial, data = mm)
lr <- anova(m0, m1, test = "LRT")
one_at <- sapply(EXTRA, function(v) { m <- glm(reformulate(c(VARS, v), "y"), family = binomial, data = mm); a <- anova(m0, m, test = "LRT"); a$`Pr(>Chi)`[2] })
co <- as.data.table(summary(m1)$coefficients, keep.rownames = "term")
setnames(co, c("term", "est", "se", "z", "p"))
sds <- c(fsm = sd(mm$fsm), sen = sd(mm$sen))
co[term %in% names(sds), `:=`(or_per_sd = exp(est * sds[term]), lo = exp((est - 1.96 * se) * sds[term]), hi = exp((est + 1.96 * se) * sds[term]))]
co[term %like% "^ofsted", `:=`(or_per_sd = exp(est), lo = exp(est - 1.96 * se), hi = exp(est + 1.96 * se))]
test <- rbind(data.table(model = "R/43 terms", t(fit_auc(f0))), data.table(model = "plus FSM, SEN and Ofsted", t(fit_auc(f1))))

# ---- schools in their last two years against the rest ---------------------------------
desc <- mx[!is.na(fsm) & !is.na(sen), .(rows = .N, schools = uniqueN(cid), fsm = median(fsm), sen = median(sen),
  fsm_pupilw = sum(fsm * pupils) / sum(pupils), sen_pupilw = sum(sen * pupils) / sum(pupils),
  good_outst = 100 * mean(ofsted == "Good or Outstanding"), ri = 100 * mean(ofsted == "Requires improvement"),
  inadequate = 100 * mean(ofsted == "Inadequate"), not_graded = 100 * mean(ofsted == "Not graded")),
  by = .(stage, group = ifelse(y == 1, "Closing within two years", "Staying open"))][order(stage, group)]
# the same within each school's own region and phase: is a closing school more disadvantaged than its neighbours?
mx[!is.na(fsm), fsm_rel := fsm - median(fsm), by = .(region, stage, year)]
mx[!is.na(sen), sen_rel := sen - median(sen), by = .(region, stage, year)]
rel <- mx[y == 1, .(schools = uniqueN(cid), fsm_rel = median(fsm_rel, na.rm = TRUE), sen_rel = median(sen_rel, na.rm = TRUE)), by = stage]

# ---- is it London? inner London has both high FSM and steep falls ----------------------
mm[, london := region == "London"]
or_tab <- function(m, d) { s <- as.data.table(summary(m)$coefficients, keep.rownames = "term"); setnames(s, c("term", "est", "se", "z", "p"))
  s <- s[term %in% c("londonTRUE", "fsm", "sen", "ofstedRequires improvement", "ofstedInadequate")]
  s[, or := ifelse(term %in% c("fsm", "sen"), exp(est * sapply(term, function(v) sd(d[[v]]))), exp(est))][] }
robust <- list(
  with_london = or_tab(glm(reformulate(c(VARS, "london", EXTRA), "y"), family = binomial, data = mm), mm),
  outside_london = or_tab(glm(reformulate(c(VARS, EXTRA), "y"), family = binomial, data = mm[london == FALSE]), mm[london == FALSE]),
  closures = mm[, .(closures = sum(y)), by = london])

res <- list(cover = cover, n = c(rows = nrow(mm), closures = sum(mm$y), schools = uniqueN(mm$cid)), lr = lr, one_at = one_at,
            coefs = co, test = test, desc = desc, rel = rel, robust = robust, built = Sys.Date())
saveRDS(res, here::here("data", "closure_extras.rds"))

options(width = 170)
cat("\n== Coverage\n"); print(cover, digits = 3)
cat("\n== Rows, closures, schools\n"); print(res$n)
cat("\n== Likelihood ratio: all three\n"); print(lr)
cat("\n== Each alone (p)\n"); print(signif(one_at, 3))
cat("\n== Coefficients (odds ratios per SD, or against Good or Outstanding)\n"); print(co[, .(term, est, p = signif(p, 3), or_per_sd, lo, hi)], digits = 3)
cat("\n== Test years 2022-23 and 2023-24 (fitted on 2019-20 to 2021-22)\n"); print(test, digits = 3)
cat("\n== Closing within two years against staying open\n"); print(desc, digits = 3)
cat("\n== Closing schools against the median school of their phase, region and year (percentage points)\n"); print(rel, digits = 3)
