# 55_fertility_fall.R - What went with the fall in fertility, 2013 to 2025?
# ---------------------------------------------------------------------------
# The last step of the Brighton and Hove falling-rolls question (writeup/bh_falling_rolls_backlog.md).
# R/49 to R/54 explain the LEVEL of fertility (who lives where). This asks about the FALL.
#
# A. By age. The change in the total fertility rate from 2013-15 to 2023-25 split by mother's age:
#    TFR = 5 x sum of age-specific rates / 1,000, so each 5-year age group's change in rate x 5 / 1,000
#    is its part of the change. Brighton and Hove, England, university cities and the neighbours.
#    A fall under 30 with no rise at 30+ is forgone births, not just later ones.
# B. Over time, within districts. District-year panel 2013 to 2025 (292 districts), district and year
#    fixed effects, standard errors clustered by district. Measures that change each year:
#      log ratio      ONS house price to residence-based earnings ratio (table 5c), same year and 3 years earlier
#      young          women 20-24 as % of women 15-44 (ONS mid-year estimates, MYEB2)
#      internal 25-39 net migration of women aged 25-39 within the UK per 1,000 (MYEB2), same year
#      intl 25-39     net international migration of women aged 25-39 per 1,000 (MYEB2), same year
#    The year effects carry the national fall; the measures can only explain why some districts fell
#    more than others.
# C. Long differences. The change in TFR (2013-15 to 2023-25) on the change in each measure.
#    Brighton and Hove's own fall against what its changes predict.
# Inputs: data-raw/ons/birthrates_lad_2013_2025_nomis_NM_207_1.csv, data-raw/ons/affordability/...xlsx (5c),
#   data-raw/ons/myeb/myebtablesuk20112025.xlsx (MYEB2)
# Output: data/fertility_fall.rds, writeup/figures/fertility_fall_*.png     Rscript R/55_fertility_fall.R
# ---------------------------------------------------------------------------
source(here::here("R", "00_config.R")); source(here::here("R", "site_helpers.R"))
suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(fixest) })
BH <- "E06000043"

# ---- fertility -------------------------------------------------------------------------------
fr <- fread(here::here("data-raw", "ons", "birthrates_lad_2013_2025_nomis_NM_207_1.csv"))
setnames(fr, c("year", "name", "code", "measure", "value"))
key <- c("Total fertility rate (TFR)" = "tfr", "Age specific fertility rate : Aged under 20" = "u20", "Age specific fertility rate : Aged 20-24" = "a20_24",
         "Age specific fertility rate : Aged 25-29" = "a25_29", "Age specific fertility rate : Aged 30-34" = "a30_34",
         "Age specific fertility rate : Aged 35-39" = "a35_39", "Age specific fertility rate : Aged 40-44" = "a40_44",
         "Age specific fertility rate : Aged 45 and over" = "a45")
fr <- fr[measure %in% names(key)][, measure := key[measure]]
F <- dcast(fr, year + code + name ~ measure, value.var = "value")[substr(code, 1, 1) == "E"]
ages <- c(u20 = "Under 20", a20_24 = "20-24", a25_29 = "25-29", a30_34 = "30-34", a35_39 = "35-39", a40_44 = "40-44", a45 = "45+")
for (a in names(ages)) F[is.na(get(a)), (a) := 0]

# ---- A. the change by age ---------------------------------------------------------------------
per <- function(y) fcase(y %between% c(2013, 2015), "p0", y %between% c(2023, 2025), "p1", default = NA_character_)
Fa <- melt(F[!is.na(per(year))][, p := per(year)], id.vars = c("code", "name", "year", "p"), measure.vars = names(ages), variable.name = "age", value.name = "rate")
Fa <- Fa[, .(rate = mean(rate)), by = .(code, name, p, age)]
Fa <- dcast(Fa, code + name + age ~ p, value.var = "rate")[, part := 5 * (p1 - p0) / 1000]
tf <- F[!is.na(per(year)), .(tfr = mean(tfr)), by = .(code, name, p = per(year))][, dcast(.SD, code + name ~ p, value.var = "tfr")]
setnames(tf, c("p0", "p1"), c("tfr0", "tfr1"))
cmp <- c("England", "Brighton and Hove", "Cambridge", "Oxford", "Norwich", "Exeter", "York", "Bristol, City of", "Lewes", "Mid Sussex", "Wealden", "Hackney")
AGE <- merge(Fa[name %in% cmp], tf, by = c("code", "name"))
AGE[, `:=`(age_lab = factor(ages[as.character(age)], ages), under30 = age %in% c("u20", "a20_24", "a25_29"))]
U30 <- AGE[, .(under30 = sum(part[under30]), over30 = sum(part[!under30]), total = sum(part), tfr0 = tfr0[1], tfr1 = tfr1[1]), by = name]
U30_all <- Fa[, .(under30 = sum(part[age %in% c("u20", "a20_24", "a25_29")]), over30 = sum(part[!age %in% c("u20", "a20_24", "a25_29")])), by = .(code, name)]

# ---- measures that change each year ----------------------------------------------------------------
x <- as.data.table(suppressMessages(readxl::read_excel(here::here("data-raw", "ons", "affordability", "aff2ratioofhousepricetoresidencebasedearnings.xlsx"), sheet = "5c", skip = 1)))
setnames(x, 1:4, c("region_code", "region", "code", "name"))
yrs <- grep("^(Year ending Sep )?[0-9]{4}$", names(x), value = TRUE)
R <- melt(x[, c("code", yrs), with = FALSE], id.vars = "code", variable.name = "year", value.name = "ratio")
R[, `:=`(year = as.integer(sub("\\D*(\\d{4})$", "\\1", year)), ratio = suppressWarnings(as.numeric(ratio)))]
R <- R[!is.na(ratio) & ratio > 0]
X <- as.data.table(suppressMessages(readxl::read_excel(here::here("data-raw", "ons", "myeb", "myebtablesuk20112025.xlsx"), sheet = "MYEB2", skip = 1)))
X <- X[country == "E" & sex %in% c("Female", "F", "2") & as.integer(age) %between% c(15, 44)]
X[, age := as.integer(age)]
long <- melt(X, id.vars = c("ladcode23", "laname23", "country", "sex", "age"), variable.name = "var", value.name = "n")
long[, `:=`(year = as.integer(sub(".*_", "", var)), comp = sub("_[0-9]{4}$", "", var), n = as.numeric(n))]
long <- long[comp %in% c("population", "internal_net", "international_net")]
M <- long[, .(w15_44 = sum(n[comp == "population"]), w20_24 = sum(n[comp == "population" & age %between% c(20, 24)]),
              w25_39 = sum(n[comp == "population" & age %between% c(25, 39)]),
              int25_39 = sum(n[comp == "internal_net" & age %between% c(25, 39)]), intl25_39 = sum(n[comp == "international_net" & age %between% c(25, 39)])),
          by = .(code = ladcode23, year)]
setorder(M, code, year)
M[, w25_39_prev := shift(w25_39), by = code]
M[, `:=`(young = 100 * w20_24 / w15_44, internal = 1000 * int25_39 / w25_39_prev, intl = 1000 * intl25_39 / w25_39_prev)]

Pn <- merge(F[code != "E92000001", .(code, name, year, tfr)], M[, .(code, year, young, internal, intl)], by = c("code", "year"))
Pn <- merge(Pn, R[, .(code, year, lr = log(ratio))], by = c("code", "year"), all.x = TRUE)
Pn <- merge(Pn, R[, .(code, year = year + 3L, lr_lag3 = log(ratio))], by = c("code", "year"), all.x = TRUE)
Pn <- Pn[year %between% c(2013, 2025)]

# ---- B. within-district panel -----------------------------------------------------------------------
pm <- list(
  prices = feols(tfr ~ lr | code + year, data = Pn, cluster = ~code),
  all = feols(tfr ~ lr + young + internal + intl | code + year, data = Pn, cluster = ~code),
  all_lag = feols(tfr ~ lr_lag3 + young + internal + intl | code + year, data = Pn, cluster = ~code))
PC <- rbindlist(lapply(names(pm), function(k) { ct <- as.data.table(coeftable(pm[[k]]), keep.rownames = "term"); setnames(ct, 2:3, c("b", "se"))
  ct[, .(model = k, term, b, se, lo = b - 1.96 * se, hi = b + 1.96 * se, n = nobs(pm[[k]]), wr2 = r2(pm[[k]], "wr2"))] }))
fy <- fixef(pm$all)$year; yfe <- data.table(year = as.integer(names(fy)), fe = unname(fy))

# ---- C. long differences ----------------------------------------------------------------------------
Pp <- Pn[!is.na(per(year)), .(tfr = mean(tfr), lr = mean(lr, na.rm = TRUE), young = mean(young), internal = mean(internal), intl = mean(intl)), by = .(code, name, p = per(year))]
LD <- dcast(Pp, code + name ~ p, value.var = c("tfr", "lr", "young", "internal", "intl"))
for (v in c("tfr", "lr", "young", "internal", "intl")) LD[, (paste0("d_", v)) := get(paste0(v, "_p1")) - get(paste0(v, "_p0"))]
LD <- LD[is.finite(d_lr)]
ld <- lm(d_tfr ~ d_lr + d_young + d_internal + d_intl, data = LD)
ld_one <- rbindlist(lapply(c("d_lr", "d_young", "d_internal", "d_intl"), function(v) data.table(var = v, r2 = summary(lm(as.formula(paste("d_tfr ~", v)), data = LD))$r.squared)))
LD[, pred := predict(ld, newdata = LD)]
bh_ld <- LD[code == BH]
bh_parts <- data.table(var = c("d_lr", "d_young", "d_internal", "d_intl"))[, part := coef(ld)[var] * (unlist(bh_ld[, var, with = FALSE]) - colMeans(LD[, var, with = FALSE]))]

res <- list(age = AGE, u30 = U30, u30_all = U30_all, share_under30_larger = U30_all[, mean(under30 < over30)], panel = PC, year_fe = yfe, ld = LD, ld_coef = coef(summary(ld)), ld_r2 = summary(ld)$r.squared,
            ld_one = ld_one, bh_ld = bh_ld, bh_parts = bh_parts, mean_d_tfr = mean(LD$d_tfr), n_ld = nrow(LD), n_panel = nrow(Pn), built = Sys.Date())
saveRDS(res, here::here("data", "fertility_fall.rds"))
options(width = 150)
print(U30[order(total)]); print(PC[, .(model, term, b = round(b, 3), lo = round(lo, 3), hi = round(hi, 3), wr2 = round(wr2, 3), n)])
print(yfe); print(round(res$ld_coef, 4)); cat("LD R2", round(res$ld_r2, 3), "\n"); print(ld_one)
print(bh_ld[, .(name, d_tfr, pred, d_lr, d_young, d_internal, d_intl)]); print(bh_parts); cat("mean d_tfr", mean(LD$d_tfr), "\n")
cat("share of districts where under-30 fall exceeds over-30 change:", U30_all[, mean(under30 < 0 & over30 < -under30 * 0 + 0.0001)], "\n")

# ---- figures -------------------------------------------------------------------------------------------
fig <- here::here("writeup", "figures")
AG <- AGE[name %in% c("England", "Brighton and Hove", "Cambridge", "Oxford", "Lewes", "Mid Sussex")]
AG[, name := factor(name, c("Brighton and Hove", "England", "Cambridge", "Oxford", "Lewes", "Mid Sussex"))]
g1 <- ggplot(AG, aes(age_lab, part, fill = part < 0)) + geom_hline(yintercept = 0, colour = GREY) + geom_col(width = 0.7) + facet_wrap(~name, nrow = 2) +
  scale_fill_manual(values = c(`TRUE` = DPURPLE, `FALSE` = MPURPLE), guide = "none") +
  labs(x = "Mother's age", y = "Change in total fertility rate (children per woman)",
       title = "Where the fall in fertility came from, by mother's age, 2013-15 to 2023-25",
       caption = "ONS birth rates by local authority (Nomis NM_207_1). Each bar: 5 x the change in that age group's birth rate per 1,000 women.") + theme_note(11)
ggsave(file.path(fig, "fertility_fall_by_age.png"), g1, width = 10, height = 5.5, dpi = 150)
