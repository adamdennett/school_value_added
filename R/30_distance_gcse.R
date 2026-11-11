# 30_distance_gcse.R - From distance to absence to GCSE results, and the size of the mixing estimate
# ---------------------------------------------------------------------------
# R/29 finds more absence among disadvantaged pupils where an authority's deprived
# neighbourhoods are further from a school. This asks what that could mean for GCSE results,
# and sets it beside Gorard and Siddiqui's (2019) estimate of what more mixed intakes could
# gain, on the same scale.
#
#   1. The chain. Points of absence per km (R/29's authority frame) times Attainment 8 points
#      per point of absence. Two values for the second link: the Value Added v2.1 absence
#      elasticity for each group (method.qmd#sec-add-take), at the group's mean Attainment 8
#      and absence, and the authority-level slope of the group's Attainment 8 on its own
#      absence fitted here.
#   2. The direct check. Authority Attainment 8 (and Progress 8) for each group on the same
#      distance, with the same controls, and between schools within authority.
#   3. Gorard and Siddiqui's 0.05 to 0.15 of a standard deviation, in Attainment 8 points.
#
# Sources and formulas: method.qmd#sec-gcse-method. Inputs: data/journey_la.rds (R/29),
# data/panel_data.rds (KS4 performance tables, 2021/22 to 2024/25), Value Added v2.1.
# Output: data/distance_gcse.rds (committed). Rscript R/30_distance_gcse.R
# ---------------------------------------------------------------------------

source(here::here("R", "site_helpers.R"))
suppressPackageStartupMessages({ library(data.table); library(fixest) })
setFixest_notes(FALSE)
num <- function(x) suppressWarnings(as.numeric(x)); BH <- "Brighton and Hove"
wm <- function(x, w) { ok <- !is.na(x) & !is.na(w) & w > 0; if (!any(ok)) return(NA_real_); sum(x[ok] * w[ok]) / sum(w[ok]) }
J <- readRDS(here::here("data", "journey_la.rds")); stopifnot(!is.null(J$la_frame))
A <- J$la_frame; ACT <- J$la_controls; S <- J$school_frame; SCT <- J$school_controls

# ---- Attainment 8 and Progress 8 by authority, year and group (KS4 performance tables) ---------
p <- as.data.table(readRDS(here::here("data", "panel_data.rds")))
p[, tp := as.integer(paste0(substr(academic_year, 1, 4), substr(academic_year, 8, 9)))]
cols <- c("ATT8SCR", "ATT8SCR_FSM6CLA1A", "ATT8SCR_NFSM6CLA1A", "P8MEA_FSM6CLA1A", "P8MEA_NFSM6CLA1A", "TPUP", "TFSM6CLA1A", "TNOTFSM6CLA1A", "P8PUP_FSM6CLA1A", "P8PUP_NFSM6CLA1A")
p[, (cols) := lapply(.SD, num), .SDcols = cols]
# Pupil-weighted means of school figures; schools with suppressed figures drop out of that group
K <- p[, .(att8_dis = wm(ATT8SCR_FSM6CLA1A, TFSM6CLA1A), att8_non = wm(ATT8SCR_NFSM6CLA1A, TNOTFSM6CLA1A), att8_all = wm(ATT8SCR, TPUP),
           p8_dis = wm(P8MEA_FSM6CLA1A, P8PUP_FSM6CLA1A), p8_non = wm(P8MEA_NFSM6CLA1A, P8PUP_NFSM6CLA1A), p8_all = NA_real_), by = .(la = LANAME, tp)]
K <- melt(K, id.vars = c("la", "tp"), measure.vars = patterns(att8 = "^att8_", p8 = "^p8_"), variable.name = "g")
K[, g := c("dis", "non", "all")[as.integer(g)]]
F <- A[phase == "sec" & !is.na(eth_wb)]
miss <- setdiff(unique(F$la), unique(K$la)); if (length(miss)) stop("Authorities with no KS4 figures: ", paste(miss, collapse = ", "))
D <- merge(F, K, by = c("la", "tp", "g"))
message("Authorities: ", uniqueN(D$la), "; years: ", paste(sort(unique(D$tp)), collapse = ", "))

# ---- 1 and 2. Authorities ------------------------------------------------------------------------
SAMP <- list(`All 109` = function(d) d, `Without London` = function(d) d[london == FALSE],
             `Without Brighton and Hove` = function(d) d[la != BH], `Without London or Brighton and Hove` = function(d) d[london == FALSE & la != BH])
fit <- function(y, x, dd, ctl) { dd <- dd[complete.cases(dd[, c(y, x), with = FALSE])]
  m <- feols(as.formula(paste0(y, " ~ ", paste(c(x, ctl), collapse = " + "), " | tp")), dd, cluster = ~la)
  data.table(b = coef(m)[[x[1]]], se = se(m)[[x[1]]], n = m$nobs, las = uniqueN(dd$la)) }
MODELS <- list(`Absence on km` = list(y = "overall", x = "km1_dep"), `Attainment 8 on km` = list(y = "att8", x = "km1_dep"),
               `Attainment 8 on km, given absence` = list(y = "att8", x = c("km1_dep", "overall")), `Attainment 8 on absence` = list(y = "att8", x = "overall"),
               `Progress 8 on km` = list(y = "p8", x = "km1_dep"))
la_models <- rbindlist(lapply(c("dis", "non", "all"), function(gr) rbindlist(lapply(names(SAMP), function(sa) rbindlist(lapply(names(ACT), function(ct) {
  dd <- SAMP[[sa]](D[g == gr])
  rbindlist(lapply(names(MODELS), function(mo) { M <- MODELS[[mo]]; if (all(is.na(dd[[M$y]]))) return(NULL); fit(M$y, M$x, dd, ACT[[ct]])[, model := mo] }))[, `:=`(group = gr, sample = sa, ctrl = ct)] }))))))

# The chain. Value Added v2.1 models log Attainment 8 on log absence, so one point of absence
# at absence a is worth Att8 * elasticity / a points, at the group's means.
V <- load_va()
el <- setNames(V$coefs$estimate[V$coefs$term == "log(PERCTOT)"], V$coefs$group[V$coefs$term == "log(PERCTOT)"])
el <- c(dis = el[["disadvantaged"]], non = el[["non_disadvantaged"]], all = el[["all"]])
means <- D[, .(absence = mean(overall), att8 = mean(att8, na.rm = TRUE), p8 = mean(p8, na.rm = TRUE)), by = g]
means[, `:=`(elasticity = el[g], per_point_va = att8 * -el[g] / absence)]
ab <- la_models[model == "Absence on km", .(group, sample, ctrl, b_abs = b, se_abs = se)]
al <- la_models[model == "Attainment 8 on absence", .(group, sample, ctrl, per_point_la = -b, se_pp_la = se)]
dr <- la_models[model == "Attainment 8 on km", .(group, sample, ctrl, direct = b, se_direct = se)]
chain <- Reduce(function(x, y) merge(x, y, by = c("group", "sample", "ctrl")), list(ab, al, dr))
chain <- merge(chain, means[, .(group = g, per_point_va)], by = "group")
# Interval for the chain from the first link alone; the second is estimated far more precisely
chain[, `:=`(chain_va = -b_abs * per_point_va, se_chain_va = se_abs * per_point_va, chain_la = -b_abs * per_point_la)]

# ---- 1b. Does it flatten? Distance in bands, and as a spline -----------------------------------------
# The same models with distance in 0.1 km bands (against 0.8 to 0.9 km, where both samples have
# authorities) and as a natural cubic spline with three degrees of freedom. London's boroughs sit
# almost all under 0.8 km, so the shape across all 109 partly compares London with elsewhere.
BRK <- c(0, 0.8, 0.9, 1.0, 1.1, 1.2, Inf); BLAB <- c("Under 0.8", "0.8 to 0.9", "0.9 to 1.0", "1.0 to 1.1", "1.1 to 1.2", "1.2 and over")
D[, band := factor(as.character(cut(km1_dep, BRK, labels = BLAB, right = FALSE)), BLAB)]
EVERY <- ACT[["And every control"]]; SHAPE_S <- SAMP[c("All 109", "Without London")]
shape_bands <- rbindlist(lapply(c("dis", "non", "all"), function(gr) rbindlist(lapply(names(SHAPE_S), function(sa) rbindlist(lapply(c("overall", "att8"), function(y) {
  dd <- SHAPE_S[[sa]](D[g == gr]); dd <- dd[complete.cases(dd[, c(y, "km1_dep", EVERY), with = FALSE])]
  m <- feols(as.formula(paste0(y, " ~ i(band, ref = '0.8 to 0.9') + ", paste(EVERY, collapse = " + "), " | tp")), dd, cluster = ~la)
  k <- grep("^band::", names(coef(m)))
  cnt <- unique(dd[, .(la, band, km1_dep)])[, .(las = .N, km_mid = median(km1_dep)), by = band]
  rbind(data.table(band = sub("^band::", "", names(coef(m))[k]), b = coef(m)[k], se = se(m)[k]), data.table(band = "0.8 to 0.9", b = 0, se = 0))[
    cnt, on = "band"][, `:=`(group = gr, sample = sa, outcome = y)] }))))))
spline_fit <- function(y, dd, x0 = 0.85) {
  dd <- dd[complete.cases(dd[, c(y, "km1_dep", EVERY), with = FALSE])]
  basis <- splines::ns(dd$km1_dep, df = 3); dd[, paste0("s", 1:3) := as.data.table(unclass(basis)[, 1:3])]
  m <- feols(as.formula(paste0(y, " ~ s1 + s2 + s3 + ", paste(EVERY, collapse = " + "), " | tp")), dd, cluster = ~la)
  grid <- seq(quantile(dd$km1_dep, 0.02), quantile(dd$km1_dep, 0.98), length.out = 60)
  B <- predict(basis, grid) - matrix(predict(basis, x0), nrow = length(grid), ncol = 3, byrow = TRUE)
  bb <- coef(m)[c("s1", "s2", "s3")]; V <- vcov(m)[c("s1", "s2", "s3"), c("s1", "s2", "s3")]
  data.table(km = grid, fit = as.vector(B %*% bb), se = sqrt(rowSums((B %*% V) * B))) }
shape_spline <- rbindlist(lapply(c("dis", "non", "all"), function(gr) rbindlist(lapply(names(SHAPE_S), function(sa) rbindlist(lapply(c("overall", "att8"), function(y)
  spline_fit(y, SHAPE_S[[sa]](D[g == gr]))[, `:=`(group = gr, sample = sa, outcome = y)]))))))

# ---- 2b. Schools within authority -----------------------------------------------------------------
KS <- p[, .(att8_all = mean(ATT8SCR, na.rm = TRUE), att8_dis = wm(ATT8SCR_FSM6CLA1A, TFSM6CLA1A), att8_non = wm(ATT8SCR_NFSM6CLA1A, TNOTFSM6CLA1A),
            p8_dis = mean(P8MEA_FSM6CLA1A, na.rm = TRUE), p8_non = mean(P8MEA_NFSM6CLA1A, na.rm = TRUE)), by = .(URN = as.character(URN))]
S[, URN := as.character(URN)]; DS <- merge(S, KS, by = "URN")
SAMP_S <- list(`All schools` = DS, `Without London` = DS[region != "London"], `Without Brighton and Hove` = DS[la != BH])
fit_s <- function(y, x, dd, extra = NULL) { dd <- dd[!is.na(get(y))]; rhs <- paste(c(x, extra, SCT), collapse = " + ")
  m <- feols(as.formula(paste0(y, " ~ ", rhs, " | la")), dd, cluster = ~la); data.table(b = coef(m)[[x]], se = se(m)[[x]], n = m$nobs, las = uniqueN(dd$la)) }
SM <- list(`Absence, all pupils` = c("overall", "l2"), `Attainment 8, all pupils` = c("att8_all", "l2"), `Attainment 8, disadvantaged` = c("att8_dis", "l2"),
           `Attainment 8, non-disadvantaged` = c("att8_non", "l2"), `Progress 8, disadvantaged` = c("p8_dis", "l2"), `Progress 8, non-disadvantaged` = c("p8_non", "l2"),
           `Attainment 8, all pupils, per point of absence` = c("att8_all", "overall"), `Attainment 8, disadvantaged, per point of absence` = c("att8_dis", "overall"),
           `Attainment 8, non-disadvantaged, per point of absence` = c("att8_non", "overall"))
school_models <- rbindlist(lapply(names(SAMP_S), function(sa) rbindlist(lapply(names(SM), function(mo) {
  x <- SM[[mo]]; fit_s(x[1], x[2], SAMP_S[[sa]], if (x[2] == "overall") "l2")[, `:=`(model = mo, sample = sa)] }))))

# ---- 3. Gorard and Siddiqui (2019) on the Attainment 8 scale -------------------------------------
# Their outcome is capped KS4 points (best eight GCSEs or equivalent, 2015 cohort): mean 311,
# SD 97.77 (their Table 1 note). On that scale grades are 6 points apart (C = 40, B = 46), and
# eight slots make 48 points a grade across the board. Attainment 8 has ten slot weights, so a
# grade across the board is 10 points. One Attainment 8 point is therefore 4.8 capped points.
# DfE's 2016 key figures give the pupil SD of Progress 8 as 1.0652 a slot, 10.65 Attainment 8
# points, the spread left once prior attainment is allowed for.
gor <- list(cite = "Gorard, S. and Siddiqui, N. (2019) How trajectories of disadvantage help explain school attainment. SAGE Open 9(1). doi:10.1177/2158244018825171",
            n = 549186L, cohort = 2015L, mean_capped = 311, sd_capped = 97.77, lo = 0.05, hi = 0.15, capped_per_att8 = 4.8, p8_sd_2016 = 1.0652,
            coefs_y11 = c(`School mean points score` = -0.133, `Always-FSM segregation for school` = -0.077, `Mean always-FSM in school` = -0.015, `FSM level in school` = -0.028))
gor$sd_att8 <- gor$sd_capped / gor$capped_per_att8
gor$att8 <- c(lo = gor$lo, hi = gor$hi) * gor$sd_att8
gor$att8_p8scale <- c(lo = gor$lo, hi = gor$hi) * 10 * gor$p8_sd_2016

# ---- Brighton and Hove ----------------------------------------------------------------------------
PL <- J$places[in_models == TRUE]
bh <- PL[la == BH, .(km_dep = km1_dep, km_rest = km1_rest, km_gap = km1_gap)]
bh[, `:=`(med_km_dep = median(PL$km1_dep, na.rm = TRUE), med_km_dep_nol = median(PL[london == FALSE]$km1_dep, na.rm = TRUE))]
# How mixed its schools already are: Gorard's segregation index of disadvantaged pupils across an
# authority's schools (pipeline/04_derived.R), latest year in the panel
sg <- unique(p[!is.na(gorard_segregation), .(la = as.character(LANAME), academic_year, g = gorard_segregation)])[academic_year == max(academic_year)]
bh[, `:=`(seg_year = sg$academic_year[1], seg = sg[la == BH, g], seg_median = median(sg$g), seg_rank = sg[, frank(g)][sg$la == BH], seg_n = nrow(sg))]

out <- list(built = Sys.Date(), n_las = uniqueN(D$la), years = sort(unique(D$tp)), la_models = la_models, chain = chain, means = means, shape_bands = shape_bands, shape_spline = shape_spline,
            school_models = school_models, n_schools = nrow(DS), school_las = uniqueN(DS$la), gorard = gor, bh = bh)
saveRDS(out, here::here("data", "distance_gcse.rds")); message("Saved data/distance_gcse.rds")
print(chain[ctrl == "And every control", .(group, sample, b_abs = round(b_abs, 2), chain_va = round(chain_va, 2), chain_la = round(chain_la, 2), direct = round(direct, 2), lo = round(direct - 1.96 * se_direct, 1), hi = round(direct + 1.96 * se_direct, 1))][order(sample, group)])
print(round(unlist(gor[c("sd_att8", "att8", "att8_p8scale")]), 2))
