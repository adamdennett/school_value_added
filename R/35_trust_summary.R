# 35_trust_summary.R - A trust's schools: value added, absence, place and intake-aware targets
# ---------------------------------------------------------------------------
# Gathers, for one multi-academy trust's secondary schools, everything the site already
# measures, and adds targets that start from each school's own intake:
#
#   value added      v2.1 for all three groups, four-year shrunk, and year by year, with the
#                    model's expected Attainment 8 for the intake each year
#   absence          the school's absence against what its intake predicts (R/10)
#   place            the local authority's own effect on the v2.1 model
#   look-alikes      the ten schools in England most like each one on intake and setting, as on
#                    the Find a twin page: a weighted Gower-type distance, weights from how much
#                    each characteristic moves Attainment 8
#   targets          expected Attainment 8 for the latest intake, plus value added at three
#                    levels: the school's own, the upper quartile of its look-alikes, and England's
#                    top tenth; absence at what the intake predicts and at the look-alikes' best
#                    quarter, with what that is worth in Attainment 8 through the v2.1 elasticity
#
# Sources and formulas: the trust page's "How these figures are made". Uses only published data.
# TRUST="ALDRIDGE EDUCATION" Rscript R/35_trust_summary.R  ->  data/trust_<slug>.rds (committed)
# ---------------------------------------------------------------------------

source(here::here("R", "site_helpers.R"))
suppressPackageStartupMessages(library(data.table))
TRUST <- Sys.getenv("TRUST", "ALDRIDGE EDUCATION"); SLUG <- gsub("[^a-z]+", "_", tolower(TRUST))
G3 <- c("all", "disadvantaged", "non_disadvantaged")

# ---- The trust's schools (GIAS) ------------------------------------------------------------------
gias <- fread(here::here("data-raw", "gias", "edubasealldata20260610.csv"), encoding = "Latin-1",
              select = c("URN", "EstablishmentName", "Trusts (name)", "EstablishmentStatus (name)", "PhaseOfEducation (name)", "StatutoryLowAge", "StatutoryHighAge", "TypeOfEstablishment (name)", "OpenDate"))
setnames(gias, c("URN", "name", "trust", "status", "phase", "low", "high", "type", "opened"))
ts <- gias[trust == TRUST & status == "Open"]
V <- load_va(); w <- as.data.table(va_wide(V)); w[, URN := as.integer(URN)]
S <- merge(ts[, .(URN, low, high, type, opened)], w, by = "URN")
message(TRUST, ": ", nrow(ts), " open schools; ", nrow(S), " with value added (", paste(setdiff(ts$name, S$SCHNAME), collapse = ", "), " without)")

# Value added, interval and expected Attainment 8 (four years), per group
va <- as.data.table(V$va)[URN %in% S$URN & group %in% G3, .(URN, group, years, att8, base, va = va_shrunk, lo = va_lo, hi = va_hi, reliability, sig)]
# Year by year: actual, expected for that year's intake, value added
yr <- as.data.table(V$school_years)[URN %in% S$URN & group %in% G3, .(URN, group, year = year_label, obs, exp = exp_nat, va = va_nat)]
pv <- load_prov(); if (!is.null(pv)) yr <- rbind(yr[, prov := FALSE], as.data.table(pv$va)[URN %in% S$URN & group %in% G3, .(URN, group, year = pv$year, obs = NA_real_, exp = NA_real_, va = va_2526, prov = TRUE)], fill = TRUE)
# England, for comparison: every school's four-year value added and absence against intake
pa <- readRDS(here::here("data", "place_absence.rds")); ab <- as.data.table(pa$abs_schools)[, URN := as.integer(URN)]
eng <- merge(as.data.table(V$va)[group %in% G3, .(URN, group, va = va_shrunk)], ab[, .(URN, abs_excess)], by = "URN", all.x = TRUE)
pct <- eng[, .(p50 = quantile(va, 0.5), p75 = quantile(va, 0.75), p90 = quantile(va, 0.9)), by = group]

# ---- Look-alikes, as the Find a twin page ----------------------------------------------------------
AC <- readRDS(here::here("data", "absence_causes.rds")); Lx <- as.data.table(AC$local$schools)[, URN := as.integer(URN)]
Lx <- merge(Lx, w[, .(URN, att8, va_all = va_shrunk_all, va_dis = va_shrunk_disadvantaged, va_non = va_shrunk_non_disadvantaged)], by = "URN")
Lx <- merge(Lx, gias[, .(URN, late = fifelse(low >= 13, "Starts at 13 or 14", "Starts at 11"))], by = "URN", all.x = TRUE)
Lx <- merge(Lx, ab[, .(URN, abs_obs, abs_exp, abs_excess)], by = "URN", all.x = TRUE)
NUM <- c("fsm", "ks2_c", "eal", "wbri", "mobile", "girls", "sen_sup", "ehcp", "semh", "z_l4", "size"); CAT <- c("place", "ADMPOL_PT", "sixth", "faith", "late")
Lx[, (CAT) := lapply(.SD, as.character), .SDcols = CAT]
cc <- Lx[complete.cases(Lx[, c("att8", NUM, CAT), with = FALSE])]
fit <- lm(reformulate(c(NUM, CAT), "att8"), data = cc); sdc <- apply(predict(fit, type = "terms"), 2, sd)[c(NUM, CAT)]
WT <- pmax(sdc / mean(sdc), 0.05)                           # mean 1; nothing quite ignored (as twins.qmd)
SDn <- sapply(NUM, function(v) sd(Lx[[v]], na.rm = TRUE))
gower <- function(a, B) { t <- 0; n <- 0
  for (v in NUM) { ok <- !is.na(B[[v]]) & !is.na(a[[v]]); d <- pmin(abs(B[[v]] - a[[v]]) / SDn[[v]] / 3, 1); t <- t + ifelse(ok, WT[[v]] * d, 0); n <- n + ifelse(ok, WT[[v]], 0) }
  for (v in CAT) { ok <- !is.na(B[[v]]) & !is.na(a[[v]]); t <- t + ifelse(ok, WT[[v]] * (B[[v]] != a[[v]]), 0); n <- n + ifelse(ok, WT[[v]], 0) }
  t / n }
twins <- rbindlist(lapply(S$URN, function(u) { a <- Lx[URN == u]; if (!nrow(a)) return(NULL)
  B <- Lx[URN != u]; B[, dist := gower(a, B)]
  B[order(dist)][1:10, .(URN = u, twin = URN, twin_name = SCHNAME, twin_la = as.character(la), dist, att8, va_all, va_dis, va_non, abs_obs, abs_exp, abs_excess, fsm, ks2_c)] }))
tw_sum <- twins[, .(tw_va75_all = quantile(va_all, 0.75, na.rm = TRUE), tw_va75_dis = quantile(va_dis, 0.75, na.rm = TRUE), tw_va75_non = quantile(va_non, 0.75, na.rm = TRUE),
                    tw_abs25 = quantile(abs_obs, 0.25, na.rm = TRUE), tw_abs_med = median(abs_obs, na.rm = TRUE)), by = URN]

# ---- Targets -----------------------------------------------------------------------------------------
# Expected Attainment 8 for the latest intake, then value added at each level
latest <- yr[prov == FALSE | is.na(prov)][order(year), .SD[.N], by = .(URN, group)][, .(URN, group, year, exp_latest = exp, obs_latest = obs)]
TW <- c(all = "tw_va75_all", disadvantaged = "tw_va75_dis", non_disadvantaged = "tw_va75_non")
tg <- merge(merge(va, latest, by = c("URN", "group")), tw_sum, by = "URN", all.x = TRUE)
tg[, tw75 := mapply(function(u, g) tw_sum[URN == u][[TW[[g]]]], URN, group)]
tg <- merge(tg, pct, by = "group")
tg[, `:=`(t_own = exp_latest + va, t_twin = exp_latest + pmax(va, tw75), t_top = exp_latest + pmax(va, p90))]
# Year-to-year spread of the school's own value added: a target inside it is within ordinary variation
sdy <- yr[prov == FALSE | is.na(prov), .(va_sd = sd(va)), by = .(URN, group)]; tg <- merge(tg, sdy, by = c("URN", "group"), all.x = TRUE)
# Absence: what the intake predicts, the look-alikes' best quarter, and what each is worth in Attainment 8
EL <- setNames(as.data.table(V$coefs)[term == "log(PERCTOT)"]$estimate, as.data.table(V$coefs)[term == "log(PERCTOT)"]$group)
abs_t <- merge(ab[URN %in% S$URN, .(URN, abs_obs, abs_exp, abs_excess)], tw_sum[, .(URN, tw_abs25, tw_abs_med)], by = "URN", all.x = TRUE)
abs_t <- abs_t[, .(group = G3, el = EL[G3]), by = .(URN, abs_obs, abs_exp, abs_excess, tw_abs25, tw_abs_med)]
abs_t <- merge(abs_t, va[, .(URN, group, att8)], by = c("URN", "group"))
gain <- function(a8, el, from, to) ifelse(is.na(to) | to >= from, 0, a8 * ((to / from)^el - 1))
abs_t[, `:=`(gain_exp = gain(att8, el, abs_obs, abs_exp), gain_tw = gain(att8, el, abs_obs, tw_abs25))]

# ---- Levers: where a school could gain, and similar schools that do better on each -------------------
# Things a school can work on, each put into Attainment 8 points as "if this school matched the
# better quarter of the schools most like it":
#   attendance       overall absence, through the v2.1 absence elasticity (all pupils)
#   subject areas    Progress 8 by element, mean of the years DfE published it: a point of Progress 8
#                    in an element is a grade a slot, so the Attainment 8 gain is the gap times the
#                    element's slots (English 2, maths 2, EBacc 3, open 3)
#   filling slots    EBacc and open slots filled (out of 3 each): an empty slot scores nothing; worth
#                    is shown as slots, not points, because what a filled slot scores varies
# Persistent absence is shown with attendance, as part of the same lever. Exemplars are the
# look-alikes (from the 25 closest) best on that lever whose value added for all pupils is not
# below England's median, so a conversation is with a school doing well overall.
num <- function(x) suppressWarnings(as.numeric(x))
fmt_lev <- function(v, x) if (v %in% c("abs_obs", "persistent")) sprintf("%.1f%%", x) else if (v %in% c("ebac_fill", "open_fill")) sprintf("%.2f slots", x) else sprintf("%+.2f", x)
pnl <- as.data.table(readRDS(here::here("data", "panel_data.rds")))[, URN := as.integer(URN)]
el <- pnl[, .(p8_eng = mean(num(P8MEAENG), na.rm = TRUE), p8_mat = mean(num(P8MEAMAT), na.rm = TRUE), p8_ebac = mean(num(P8MEAEBAC), na.rm = TRUE),
              p8_open = mean(num(P8MEAOPEN), na.rm = TRUE), ebac_fill = mean(num(AVGEBACFILL), na.rm = TRUE), open_fill = mean(num(AVGOPENFILL), na.rm = TRUE)), by = URN]
for (v in names(el)[-1]) set(el, which(!is.finite(el[[v]])), v, NA_real_)
Lx2 <- merge(Lx, el, by = "URN", all.x = TRUE)
Lx2 <- merge(Lx2, as.data.table(AC$local$schools)[, .(URN = as.integer(URN), persistent)], by = "URN", all.x = TRUE, suffixes = c("", ".y"))
near25 <- rbindlist(lapply(S$URN, function(u) { a <- Lx2[URN == u]; if (!nrow(a)) return(NULL); B <- Lx2[URN != u]; B[, dist := gower(a, B)]
  B[order(dist)][1:25][, `:=`(URN0 = u, rank = .I)] }))
LEV <- data.table(lever = c("abs_obs", "persistent", "p8_eng", "p8_mat", "p8_ebac", "p8_open", "ebac_fill", "open_fill"),
                  label = c("Attendance: overall absence", "Attendance: persistent absence", "English", "Maths", "EBacc subjects", "Open subjects", "EBacc slots filled", "Open slots filled"),
                  kind = c("Attendance", "Attendance", "Subject areas", "Subject areas", "Subject areas", "Subject areas", "Filling slots", "Filling slots"),
                  better = c("low", "low", "high", "high", "high", "high", "high", "high"), slots = c(NA, NA, 2, 2, 3, 3, NA, NA),
                  unit = c("% of sessions", "% of pupils", "Progress 8 a slot", "Progress 8 a slot", "Progress 8 a slot", "Progress 8 a slot", "slots of 3", "slots of 3"))
med_va <- pct[group == "all", p50]
a8_all <- setNames(va[group == "all", att8], va[group == "all", URN])
levers <- rbindlist(lapply(S$URN, function(u) { me <- Lx2[URN == u]; nb <- near25[URN0 == u]; tw10 <- nb[rank <= 10]
  rbindlist(lapply(seq_len(nrow(LEV)), function(k) { L <- LEV[k]; v <- L$lever; own <- me[[v]]; q <- if (L$better == "low") 0.25 else 0.75
    tq <- quantile(tw10[[v]], q, na.rm = TRUE); md <- median(tw10[[v]], na.rm = TRUE)
    gap <- if (L$better == "low") own - tq else tq - own
    worth <- if (is.na(gap) || gap <= 0) 0 else if (v == "abs_obs") a8_all[[as.character(u)]] * ((tq / own)^EL[["all"]] - 1) else if (!is.na(L$slots)) gap * L$slots else NA_real_
    ex <- nb[!is.na(get(v)) & va_all >= med_va][order(if (L$better == "low") get(v) else -get(v))][1:3][!is.na(URN)]
    data.table(URN = u, lever = v, label = L$label, kind = L$kind, unit = L$unit, own = own, twin_med = md, twin_better = tq, gap = gap, worth = worth,
               ex_names = paste(sprintf("%s (%s, %s)", ex$SCHNAME, ex$la, fmt_lev(v, ex[[v]])), collapse = "; "),
               ex_urns = paste(ex$URN, collapse = ";")) })) }))

# ---- Marginal gains: attendance and the workforce, each a little, added up ---------------------------
# Attendance through the v2.1 absence elasticity; retention, teacher sickness and pupils per qualified
# teacher through their joint association with value added across England (R/36), which is beyond
# intake and absence, so the pieces can be added. A piece counts for a group only where its national
# link is clear (|t| >= 2). Each is what matching the better quarter of the ten look-alikes would be worth.
WF <- readRDS(here::here("data", "workforce_levers.rds"))
Lx3 <- merge(Lx2, WF$schools, by = "URN", all.x = TRUE)
near25 <- merge(near25, WF$schools, by = "URN", all.x = TRUE)
WFL <- c(retention = "Teacher retention (% staying a year)", sick_days = "Teacher sickness (days per teacher)", ptr = "Pupils per qualified teacher")
mg <- rbindlist(lapply(S$URN, function(u) { me <- Lx3[URN == u]; nb <- near25[URN0 == u]; tw10 <- nb[rank <= 10]
  rbindlist(lapply(c("all", "disadvantaged"), function(g) {
    a8 <- va[URN == u & group == g, att8]
    att <- data.table(lever = "abs_obs", label = "Attendance: overall absence", own = me$abs_obs, target = quantile(tw10$abs_obs, 0.25, na.rm = TRUE), b = NA_real_, t = NA_real_)
    att[, worth := if (target < own) a8 * ((target / own)^EL[[g]] - 1) else 0]
    wfr <- rbindlist(lapply(names(WFL), function(v) { cf <- WF$coefs[group == g & lever == v]
      tq <- quantile(tw10[[v]], if (cf$b > 0) 0.75 else 0.25, na.rm = TRUE); own <- me[[v]]
      data.table(lever = v, label = WFL[[v]], own = own, target = tq, b = cf$b, t = cf$t, worth = if (abs(cf$t) < 2 || is.na(own)) NA_real_ else max(0, cf$b * (tq - own))) }))
    rbind(att, wfr)[, `:=`(URN = u, group = g)] })) }))
# Exemplars for each workforce lever: look-alikes best on it, adding value overall
wf_ex <- rbindlist(lapply(S$URN, function(u) { nb <- near25[URN0 == u]
  rbindlist(lapply(names(WFL), function(v) { up <- WF$coefs[group == "all" & lever == v, b] > 0
    ex <- nb[!is.na(get(v)) & va_all >= med_va][order(if (up) -get(v) else get(v))][1:3][!is.na(URN)]
    data.table(URN = u, lever = v, ex_names = paste(sprintf("%s (%s, %s)", ex$SCHNAME, ex$la, sprintf(if (v == "retention") "%.0f%%" else "%.1f", ex[[v]])), collapse = "; ")) })) }))

# ---- Boundaries for the map: the districts the schools are in (ONS, December 2024, ultra generalised)
lad_names <- unique(na.omit(S$lad))
lads <- tryCatch({
  q <- paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2024_Boundaries_UK_BUC/FeatureServer/0/query?where=",
              utils::URLencode(paste0("LAD24NM IN (", paste0("'", gsub("'", "''", lad_names), "'", collapse = ","), ")"), reserved = TRUE), "&outFields=LAD24CD,LAD24NM&outSR=4326&f=geojson")
  x <- sf::st_read(q, quiet = TRUE); x$lad <- x$LAD24NM; x$la <- S$LANAME[match(x$lad, S$lad)]; x }, error = function(e) NULL)
message("Boundaries: ", if (is.null(lads)) "none (download failed)" else paste(nrow(lads), "districts"))

# ---- Place: the authority's effect on the v2.1 model -------------------------------------------------
la_eff <- as.data.table(V$la)[LANAME %in% S$LANAME & group %in% G3, .(LANAME, group, effect, se, lo, hi, rank, n_la)]

out <- list(built = Sys.Date(), trust = TRUST, schools_open = ts, schools = S, va = va, years = yr, pct = pct, eng = eng, twins = twins, twin_weights = WT,
            targets = tg, absence = abs_t, la_effects = la_eff, levers = levers, lever_defs = LEV, near25 = near25[, .(URN0, rank, URN, SCHNAME, la, dist, va_all, abs_obs, retention, sick_days, ptr)], marginal = mg, wf_examples = wf_ex, wf_coefs = WF$coefs, lads = lads, prov_note = if (!is.null(pv)) PROV_NOTE else NULL)
saveRDS(out, here::here("data", paste0("trust_", SLUG, ".rds"))); message("Saved data/trust_", SLUG, ".rds")
print(tg[group == "all", .(URN, att8, exp_latest, va, tw75, p90, t_own, t_twin, t_top, va_sd)], digits = 3)
print(levers[order(URN, -worth)][, .(URN, label, own = round(own, 2), better = round(twin_better, 2), worth = round(worth, 2))]); print(levers[lever == "abs_obs", ex_names][1:2])
print(mg[, .(URN, group, label, own = round(own, 1), target = round(target, 1), worth = round(worth, 2))])
print(abs_t[group == "disadvantaged", .(URN, abs_obs, abs_exp, tw_abs25, gain_exp, gain_tw)], digits = 3)
