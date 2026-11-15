# 54_msoa_children.R - Young children by neighbourhood: what goes with fewer under-fives, and where in Brighton and Hove
# ---------------------------------------------------------------------------
# The within-city step of the Brighton and Hove falling-rolls question (writeup/bh_falling_rolls_backlog.md).
# Census 2021 has no women-by-age table at MSOA on Nomis, so the outcome is a child-adult ratio:
#   children aged 0-4 per 1,000 residents aged 20-44 (both sexes), TS007a (age, 5-year bands), MSOA
#   (and per 1,000 aged 25-44, leaving out the 20-24s who are most often students, as a check)
# Explanatory measures, MSOA, Census 2021 unless stated:
#   level4      % of residents 16+ with Level 4 qualifications or above (TS067)
#   students    % of residents 16+ who are full-time students (TS062 NS-SeC L15)
#   young       residents 20-24 as % of those 20-44 (TS007a)
#   married     % of residents 16+ married or in a civil partnership (TS002)
#   privrent    % of households privately renting (TS054)
#   abroad      % of residents born outside the UK (TS004)
#   lprice      log median price paid, mean of the four years ending December 2019 to December 2022
#               (ONS House Price Statistics for Small Areas, dataset 2, table 1a)
# Models (England, MSOAs; each measure standardised across MSOAs):
#   national    ratio ~ all measures
#   within LA   ratio ~ all measures | local authority (compares neighbourhoods within the same authority)
# Brighton and Hove: each MSOA's ratio, its national-model prediction and residual, and how much of the
# city's gap from the England MSOA mean each measure accounts for.
# Inputs: data-raw/census2021 (zips and Nomis MSOA % files), data-raw/ons/hpssa, data-raw/msoa_names
# Outputs: data/msoa_children.rds, writeup/figures/msoa_children_*.png     Rscript R/54_msoa_children.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(fixest); library(ggplot2); library(sf); library(httr) })
sf_use_s2(FALSE)
cz <- function(z, f) fread(text = readLines(unz(here::here("data-raw", "census2021", z), f), warn = FALSE), check.names = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))

A <- cz("census2021-ts007a.zip", "census2021-ts007a-msoa.csv")
setnames(A, "geography code", "msoa")
ag <- function(lbl) A[[grep(lbl, names(A), fixed = TRUE)]]
D <- data.table(msoa = A$msoa, name = A$geography, pop = A[["Age: Total"]], age0_4 = ag("4 years and under"),
                a20_24 = ag("20 to 24"), a25_29 = ag("25 to 29"), a30_34 = ag("30 to 34"), a35_39 = ag("35 to 39"), a40_44 = ag("40 to 44"))
D[, `:=`(a20_44 = a20_24 + a25_29 + a30_34 + a35_39 + a40_44, a25_44 = a25_29 + a30_34 + a35_39 + a40_44)]
D[, `:=`(ratio = 1000 * age0_4 / a20_44, ratio2544 = 1000 * age0_4 / a25_44, young = 100 * a20_24 / a20_44)]
Q <- cz("census2021-ts067.zip", "census2021-ts067-msoa.csv"); setnames(Q, "geography code", "msoa")
Q[, level4 := 100 * get(grep("Level 4", names(Q), value = TRUE)) / get(grep("Total", names(Q), value = TRUE))]
N <- cz("census2021-ts062.zip", "census2021-ts062-msoa.csv"); setnames(N, "geography code", "msoa")
N[, students := 100 * get(grep("L15", names(N), value = TRUE)) / get(grep("Total", names(N), value = TRUE))]
nom <- function(f, keep) { x <- fread(here::here("data-raw", "census2021", f)); setnames(x, c("msoa", "category", "v")); x[category == keep, .(msoa, v)] }
mar <- nom("ts002_msoa_pct_nomis.csv", "Married or in a registered civil partnership"); setnames(mar, "v", "married")
prv <- nom("ts054_msoa_pct_nomis.csv", "Private rented"); setnames(prv, "v", "privrent")
uk <- nom("ts004_msoa_pct_nomis.csv", "Europe: United Kingdom"); uk[, abroad := 100 - v][, v := NULL]
H <- as.data.table(readxl::read_excel(here::here("data-raw", "ons", "hpssa", "HPSSA Dataset 2 - Median price paid by MSOA.xls"), sheet = "1a", skip = 5))
yrs <- paste("Year ending Dec", 2019:2022)
H <- H[, c("MSOA code", yrs), with = FALSE]; setnames(H, 1, "msoa")
H[, lprice := rowMeans(log(sapply(.SD, num)), na.rm = TRUE), .SDcols = yrs]
# HPSSA uses 2011 MSOA codes: MSOAs whose code changed in 2021 (splits and merges) have no price, so the
# main model leaves price out and keeps every MSOA; a second model adds price on the MSOAs that match.
D <- Reduce(function(x, y) merge(x, y, by = "msoa"), list(D, Q[, .(msoa, level4)], N[, .(msoa, students)], mar, prv, uk))
D <- merge(D, H[, .(msoa, lprice)], by = "msoa", all.x = TRUE)
D[!is.finite(lprice), lprice := NA_real_]
nm <- fread(here::here("data-raw", "msoa_names", "MSOA-Names-2.2.csv"), select = c("msoa21cd", "msoa21hclnm", "localauthorityname"))
D <- merge(D, nm[, .(msoa = msoa21cd, hcl = msoa21hclnm, la = localauthorityname)], by = "msoa", all.x = TRUE)
D <- D[grepl("^E02", msoa) & a20_44 > 0]
vars <- c(level4 = "Level 4+ qualifications", students = "Full-time students", young = "Aged 20-24 (of 20-44)", married = "Married or civil partnership",
          privrent = "Private renting", abroad = "Born outside the UK", lprice = "House prices (log)")
for (v in names(vars)) D[, (paste0(v, "_z")) := (get(v) - mean(get(v), na.rm = TRUE)) / sd(get(v), na.rm = TRUE)]
vars_main <- setdiff(names(vars), "lprice")
rhs <- paste(paste0(vars_main, "_z"), collapse = " + ")
m_nat <- feols(as.formula(paste("ratio ~", rhs)), data = D, vcov = "hetero")
m_la <- feols(as.formula(paste("ratio ~", rhs, "| la")), data = D, cluster = ~la)
m_nat2 <- feols(as.formula(paste("ratio2544 ~", rhs)), data = D, vcov = "hetero")
m_price <- feols(as.formula(paste("ratio ~", rhs, "+ lprice_z")), data = D[!is.na(lprice)], vcov = "hetero")
m_price_la <- feols(as.formula(paste("ratio ~", rhs, "+ lprice_z | la")), data = D[!is.na(lprice)], cluster = ~la)
m_one <- rbindlist(lapply(names(vars), function(v) { m <- feols(as.formula(sprintf("ratio ~ %s_z", v)), data = D[!is.na(get(v))]); data.table(var = v, r2 = r2(m, "r2"), n = nobs(m)) }))
coefs <- rbindlist(lapply(list(national = m_nat, within_la = m_la, national_25_44 = m_nat2, with_price = m_price, with_price_within_la = m_price_la), function(m) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[term != "(Intercept)"]; setnames(ct, 2:3, c("b", "se")); ct[, .(term, b, se)] }), idcol = "model")
coefs[, `:=`(var = sub("_z$", "", term), lo = b - 1.96 * se, hi = b + 1.96 * se)]

# Brighton and Hove: prediction, residual and parts of its gap from the England MSOA mean
D[, `:=`(pred = predict(m_nat, newdata = D), resid = ratio - predict(m_nat, newdata = D))]
B <- D[la == "Brighton and Hove"]
bz <- colMeans(B[, paste0(vars_main, "_z"), with = FALSE])
parts <- data.table(var = vars_main, part = coef(m_nat)[paste0(vars_main, "_z")] * bz)
gap <- data.table(bh_mean = mean(B$ratio), eng_mean = mean(D$ratio), gap = mean(B$ratio) - mean(D$ratio), explained = sum(parts$part), resid = mean(B$resid))
la_sum <- D[, .(msoas = .N, ratio = sum(age0_4) / sum(a20_44) * 1000, resid = mean(resid)), by = la][order(ratio)]

res <- list(D = D, coefs = coefs, one_at_a_time = m_one, n = c(all = nrow(D), with_price = D[!is.na(lprice), .N], bh = nrow(B)),
            r2 = sapply(list(national = r2(m_nat, "r2"), within_la = r2(m_la, "wr2"), national_25_44 = r2(m_nat2, "r2"), with_price = r2(m_price, "r2"), with_price_within_la = r2(m_price_la, "wr2")), unname),
            bh = B[order(ratio)], parts = parts, gap = gap, la_sum = la_sum, vars = vars, built = Sys.Date())
saveRDS(res, here::here("data", "msoa_children.rds"))
options(width = 150)
print(coefs[, .(model, var, b = round(b, 1), lo = round(lo, 1), hi = round(hi, 1))]); print(res$r2); print(m_one[order(-r2)])
print(gap); print(parts[order(part)]); print(head(la_sum, 15))
print(B[order(ratio), .(hcl, ratio = round(ratio), pred = round(pred), resid = round(resid), level4 = round(level4), students = round(students), privrent = round(privrent), married = round(married), price = round(exp(lprice) / 1000))]); print(res$n)

# ---- figures ------------------------------------------------------------------------------------
fig <- here::here("writeup", "figures")
C <- coefs[model %in% c("national", "within_la")][, `:=`(model = factor(model, c("national", "within_la"), c("Across England", "Within the same local authority")), lab = factor(vars[var], rev(vars)))]
g1 <- ggplot(C, aes(b, lab, colour = model)) + geom_vline(xintercept = 0, colour = "#6b6680") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge2(width = 0.5, reverse = TRUE), size = 0.35) +
  scale_colour_manual(values = c("Across England" = "#c9a2f5", "Within the same local authority" = "#361a54"), name = NULL) +
  labs(x = "Children 0-4 per 1,000 adults aged 20-44, per standard deviation (95% interval)", y = NULL,
       title = "What goes with fewer young children in a neighbourhood", subtitle = sprintf("%s MSOAs in England, Census 2021; all measures together", format(nrow(D), big.mark = ",")),
       caption = "ONS Census 2021 (TS002, TS004, TS007a, TS054, TS062, TS067); ONS House Price Statistics for Small Areas") +
  theme_minimal(base_size = 11) + theme(legend.position = "top", plot.title.position = "plot", panel.grid.minor = element_blank())
ggsave(file.path(fig, "msoa_children_drivers.png"), g1, width = 9, height = 5, dpi = 150)

# Brighton and Hove MSOA map (ONS MSOA 2021 boundaries, super generalised, from the ONS ArcGIS service)
bf <- here::here("data-raw", "boundaries", "msoa21_bsc_brighton.gpkg")
if (!file.exists(bf)) {
  dir.create(dirname(bf), showWarnings = FALSE, recursive = TRUE)
  r <- POST("https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/MSOA_2021_EW_BSC_V3_RUC/FeatureServer/0/query",
            body = list(where = sprintf("MSOA21CD IN (%s)", paste0("'", B$msoa, "'", collapse = ",")), outFields = "MSOA21CD", outSR = "27700", f = "geojson"), encode = "form")
  st_write(st_read(content(r, "text", encoding = "UTF-8"), quiet = TRUE), bf, quiet = TRUE, delete_dsn = TRUE)
}
M <- st_read(bf, quiet = TRUE); st_crs(M) <- 27700; names(M)[1] <- "msoa"
M <- merge(M, B[, .(msoa, hcl, ratio, resid)], by = "msoa")
lab <- M[M$hcl %in% B[order(ratio)][c(1:4, (.N - 2):.N), hcl], ]
g2 <- ggplot(M) + geom_sf(aes(fill = ratio), colour = "white", linewidth = 0.3) +
  ggrepel::geom_label_repel(data = cbind(st_drop_geometry(lab), st_coordinates(st_point_on_surface(lab))), aes(X, Y, label = hcl), size = 2.5, colour = "#1d1b20", fill = "white", label.size = 0.1, min.segment.length = 0, box.padding = 0.6, seed = 1) +
  scale_fill_gradient(low = "#efe5fb", high = "#361a54", name = "Children 0-4\nper 1,000\naged 20-44") +
  labs(title = "Young children by neighbourhood, Brighton and Hove", subtitle = sprintf("Census 2021, MSOAs. England MSOA average: %.0f", mean(D$ratio)),
       caption = "ONS Census 2021 TS007a; MSOA 2021 boundaries (ONS, OGL); House of Commons Library MSOA names") +
  theme_void(base_size = 10) + theme(plot.title.position = "plot")
ggsave(file.path(fig, "msoa_children_brighton_map.png"), g2, width = 9, height = 5.5, dpi = 150)
g3 <- ggplot(B, aes(pred, ratio)) + geom_abline(slope = 1, intercept = 0, colour = "#6b6680", linetype = 2) +
  geom_point(data = D[sample(.N, 3000)], colour = "#d9d4e0", size = 0.6) + geom_point(colour = "#993bff", size = 2) +
  ggrepel::geom_text_repel(data = B[abs(resid) > quantile(abs(resid), 0.8) | ratio < quantile(ratio, 0.15)], aes(label = hcl), size = 2.6, max.overlaps = 30) +
  labs(x = "Predicted from the neighbourhood's make-up (national model)", y = "Actual: children 0-4 per 1,000 aged 20-44",
       title = "Brighton and Hove neighbourhoods against what their make-up predicts", subtitle = "Purple: Brighton and Hove MSOAs. Grey: a sample of England's MSOAs. Dashed: actual = predicted") +
  theme_minimal(base_size = 10) + theme(plot.title.position = "plot")
ggsave(file.path(fig, "msoa_children_brighton_pred.png"), g3, width = 9, height = 5.5, dpi = 150)
