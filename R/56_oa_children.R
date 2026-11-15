# 56_oa_children.R - Young children by Output Area: the within-city picture at street level
# ---------------------------------------------------------------------------
# Follows R/54 (MSOAs) down to Census 2021 Output Areas (about 130 households each), the finest
# geography the Census publishes. Same outcome and measures as R/54:
#   ratio       children aged 0-4 per 1,000 residents aged 20-44 (TS007a)
#   level4      % of residents 16+ with Level 4 qualifications or above (TS067)
#   students    % of residents 16+ who are full-time students (TS062 NS-SeC L15)
#   young       residents 20-24 as % of those 20-44 (TS007a)
#   married     % of residents 16+ married or in a civil partnership (TS002)
#   privrent    % of households privately renting (TS054)
#   abroad      % of residents born outside the UK (TS004)
# House prices are not published for OAs, so they are left out.
# Models (England, 178,000 OAs; measures standardised across OAs; standard errors clustered by MSOA):
#   national     ratio ~ all measures
#   within LA    + local authority fixed effects
#   within MSOA  + MSOA fixed effects: compares OAs in the same neighbourhood of about 8,000 people
# Brighton and Hove: the ratio for each OA, mapped, and the city's gap from the England OA mean split
# by measure (national model). OAs with fewer than 30 residents aged 20-44 are left out of the map's
# colour scale (shown grey): their ratios rest on a handful of children.
# Census counts at OA level have small random adjustments (cell key perturbation) for confidentiality,
# so single-OA ratios are noisy; patterns across groups of OAs are reliable.
# Inputs: data-raw/census2021 (OA files in the ONS/Nomis bulk zips), E:/school_active_travel outputs
#   (origins: OA to MSOA and local authority; schools: locations), ONS OA 2021 boundaries (BFE, ArcGIS)
# Outputs: data/oa_children.rds, writeup/figures/oa_children_*.png      Rscript R/56_oa_children.R
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(data.table); library(fixest); library(ggplot2); library(sf); library(httr); library(arrow) })
sf_use_s2(FALSE)
source(here::here("R", "site_helpers.R"))
cz <- function(z, f) fread(text = readLines(unz(here::here("data-raw", "census2021", z), f), warn = FALSE), check.names = FALSE)
col <- function(x, lbl) { k <- names(x)[sub("; measures: Value$", "", names(x)) == lbl]; stopifnot(length(k) == 1); x[[k]] }
SAT <- normalizePath(Sys.getenv("SCHOOL_ACTIVE_TRAVEL", file.path(here::here(), "..", "school_active_travel")), "/")

A <- cz("census2021-ts007a.zip", "census2021-ts007a-oa.csv")
ag <- function(lbl) A[[grep(lbl, names(A), fixed = TRUE)]]
D <- data.table(oa = A[["geography code"]], pop = A[["Age: Total"]], age0_4 = ag("4 years and under"),
                a20_24 = ag("20 to 24"), a25_44 = ag("25 to 29") + ag("30 to 34") + ag("35 to 39") + ag("40 to 44"))
D[, a20_44 := a20_24 + a25_44][, `:=`(ratio = 1000 * age0_4 / a20_44, young = 100 * a20_24 / a20_44)]
Q <- cz("census2021-ts067.zip", "census2021-ts067-oa.csv")
q <- data.table(oa = Q[["geography code"]], level4 = 100 * Q[[grep("Level 4", names(Q))]] / Q[[grep("Total", names(Q))]])
N <- cz("census2021-ts062.zip", "census2021-ts062-oa.csv")
n <- data.table(oa = N[["geography code"]], students = 100 * N[[grep("L15", names(N))]] / N[[grep("Total", names(N))]])
M2 <- cz("census2021-ts002.zip", "census2021-ts002-oa.csv")
m2 <- data.table(oa = M2[["geography code"]], married = 100 * col(M2, "Marital and civil partnership status: Married or in a registered civil partnership") /
                   col(M2, "Marital and civil partnership status: Total"))
Te <- cz("census2021-ts054.zip", "census2021-ts054-oa.csv")
te <- data.table(oa = Te[["geography code"]], privrent = 100 * col(Te, "Tenure of household: Private rented") / col(Te, "Tenure of household: Total: All households"))
Cb <- cz("census2021-ts004.zip", "census2021-ts004-oa.csv")
cb <- data.table(oa = Cb[["geography code"]], abroad = 100 - 100 * col(Cb, "Country of birth: Europe: United Kingdom") / col(Cb, "Country of birth: Total"))
O <- as.data.table(read_parquet(file.path(SAT, "data", "out", "origins.parquet"), col_select = c("oa21cd", "msoa21cd", "lad25nm", "e", "n")))
D <- Reduce(function(x, y) merge(x, y, by = "oa"), list(D, q, n, m2, te, cb))
D <- merge(D, O[, .(oa = oa21cd, msoa = msoa21cd, la = lad25nm, e, n)], by = "oa")
D <- D[grepl("^E00", oa) & a20_44 > 0]

vars <- c(level4 = "Level 4+ qualifications", students = "Full-time students", young = "Aged 20-24 (of 20-44)",
          married = "Married or civil partnership", privrent = "Private renting", abroad = "Born outside the UK")
for (v in names(vars)) D[, (paste0(v, "_z")) := (get(v) - mean(get(v))) / sd(get(v))]
rhs <- paste(paste0(names(vars), "_z"), collapse = " + ")
mods <- list(national = feols(as.formula(paste("ratio ~", rhs)), data = D, cluster = ~msoa),
             within_la = feols(as.formula(paste("ratio ~", rhs, "| la")), data = D, cluster = ~msoa),
             within_msoa = feols(as.formula(paste("ratio ~", rhs, "| msoa")), data = D, cluster = ~msoa))
coefs <- rbindlist(lapply(mods, function(m) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[term != "(Intercept)"]
  setnames(ct, 2:3, c("b", "se")); ct[, .(var = sub("_z$", "", term), b, se, lo = b - 1.96 * se, hi = b + 1.96 * se)] }), idcol = "model")
r2s <- c(national = unname(r2(mods$national, "r2")), within_la = unname(r2(mods$within_la, "wr2")), within_msoa = unname(r2(mods$within_msoa, "wr2")))
# how much of the OA variation lies between MSOAs (neighbourhoods) rather than within them
D[, msoa_mean := mean(ratio), by = msoa]
share_between <- D[, var(msoa_mean) / var(ratio)]

D[, pred := predict(mods$national, newdata = D)]
B <- D[la == "Brighton and Hove"]
bz <- colMeans(B[, paste0(names(vars), "_z"), with = FALSE])
parts <- data.table(var = names(vars), part = coef(mods$national)[paste0(names(vars), "_z")] * bz)
gap <- data.table(bh_ratio = 1000 * sum(B$age0_4) / sum(B$a20_44), eng_ratio = 1000 * sum(D$age0_4) / sum(D$a20_44),
                  bh_mean = mean(B$ratio), eng_mean = mean(D$ratio), explained = sum(parts$part))
nm <- fread(here::here("data-raw", "msoa_names", "MSOA-Names-2.2.csv"), select = c("msoa21cd", "msoa21hclnm"))
B <- merge(B, nm[, .(msoa = msoa21cd, hcl = msoa21hclnm)], by = "msoa", all.x = TRUE)
# distribution of Brighton and Hove OAs against England's
bands <- c(0, 60, 120, 180, 240, Inf); blab <- c("Under 60", "60-119", "120-179", "180-239", "240 and over")
dist <- rbind(B[a20_44 >= 30, .(area = "Brighton and Hove", band = cut(ratio, bands, blab, right = FALSE))],
              D[a20_44 >= 30, .(area = "England", band = cut(ratio, bands, blab, right = FALSE))])[, .N, by = .(area, band)][, pct := 100 * N / sum(N), by = area]
# within each Brighton and Hove MSOA: the spread between its OAs
spread <- B[a20_44 >= 30, .(oas = .N, msoa_ratio = 1000 * sum(age0_4) / sum(a20_44), p10 = quantile(ratio, 0.1), p90 = quantile(ratio, 0.9)), by = .(msoa, hcl)][order(msoa_ratio)]

# ---- OA boundaries for Brighton and Hove (ONS OA 2021, full resolution clipped to the coastline) ------
bf <- here::here("data-raw", "boundaries", "oa21_bfe_brighton.gpkg")
if (!file.exists(bf)) {
  url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/OA_2021_EW_BFE_V9_RUC/FeatureServer/3/query"
  chunks <- split(B$oa, ceiling(seq_along(B$oa) / 150))
  G <- do.call(rbind, lapply(chunks, function(ids) {
    for (k in 1:4) { r <- try(POST(url, body = list(where = sprintf("OA21CD IN (%s)", paste0("'", ids, "'", collapse = ",")), outFields = "OA21CD",
                                                    outSR = "27700", returnGeometry = "true", f = "geojson"), encode = "form", timeout(120)), silent = TRUE)
                     if (!inherits(r, "try-error") && status_code(r) == 200) break; Sys.sleep(5) }
    st_read(content(r, "text", encoding = "UTF-8"), quiet = TRUE) }))
  st_write(G, bf, quiet = TRUE, delete_dsn = TRUE)
}
G <- st_read(bf, quiet = TRUE); st_crs(G) <- 27700; names(G)[1] <- "oa"
stopifnot(nrow(G) == nrow(B))
G <- merge(G, B[, .(oa, msoa, ratio, a20_44)], by = "oa")
G$fill <- ifelse(G$a20_44 >= 30, pmin(G$ratio, 300), NA)
MS <- aggregate(G["msoa"], by = list(m = G$msoa), FUN = function(x) x[1])   # MSOA outlines from their OAs, so the lines match
S <- as.data.table(read_parquet(file.path(SAT, "data", "out", "schools.parquet"), col_select = c("urn", "name", "phase", "la", "e", "n", "independent")))
two <- S[name %in% c("Brunswick Primary School", "Coldean Primary School") & la == "Brighton and Hove"]
stopifnot(nrow(two) == 2)
res <- list(D_summary = D[, .(oas = .N, mean_ratio = mean(ratio))], coefs = coefs, r2 = r2s, share_between = share_between, n = nrow(D),
            bh = B[, .(oa, msoa, hcl, ratio, a20_44, age0_4, level4, students, young, married, privrent, abroad, pred)], parts = parts, gap = gap,
            dist = dist, spread = spread, n_bh = nrow(B), n_bh_small = B[a20_44 < 30, .N], built = Sys.Date())
saveRDS(res, here::here("data", "oa_children.rds"))
options(width = 150)
print(coefs[, .(model, var, b = round(b, 1), lo = round(lo, 1), hi = round(hi, 1))]); print(r2s); cat("share of OA variance between MSOAs:", round(share_between, 3), "\n")
print(gap); print(parts); print(dcast(dist, band ~ area, value.var = "pct")); print(spread)

# ---- figures ------------------------------------------------------------------------------------------
fig <- here::here("writeup", "figures")
g1 <- ggplot() + geom_sf(data = G, aes(fill = fill), colour = NA) +
  geom_sf(data = MS, fill = NA, colour = "white", linewidth = 0.35) +
  geom_point(data = two, aes(e, n), shape = 21, fill = "#ffd23f", colour = "#1d1b20", size = 3, stroke = 0.6) +
  ggrepel::geom_label_repel(data = two, aes(e, n, label = sub(" School$", "", name)), size = 2.8, fill = "white", label.size = 0.15, box.padding = 0.8, seed = 2) +
  scale_fill_gradient(low = "#efe5fb", high = "#361a54", na.value = "#d9d4e0", limits = c(0, 300), breaks = c(0, 100, 200, 300), labels = c("0", "100", "200", "300+"),
                      name = "Children 0-4\nper 1,000\naged 20-44") +
  labs(title = "Young children by Output Area, Brighton and Hove", subtitle = sprintf("Census 2021, %s Output Areas; white lines: MSOAs; grey: fewer than 30 residents aged 20-44. England OA ratio: %.0f",
                                                                                   format(nrow(B), big.mark = ","), gap$eng_ratio),
       caption = "ONS Census 2021 TS007a; OA and MSOA 2021 boundaries (ONS, OGL); school locations: DfE Get Information about Schools") +
  coord_sf(datum = NA) + theme_void(base_size = 10) + theme(plot.title.position = "plot")
ggsave(file.path(fig, "oa_children_brighton_map.png"), g1, width = 9, height = 5.6, dpi = 150)
dist[, `:=`(band = factor(band, blab), area = factor(area, c("Brighton and Hove", "England")))]
g2 <- ggplot(dist, aes(band, pct, fill = area)) + geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = c("Brighton and Hove" = DPURPLE, England = MPURPLE), name = NULL) +
  labs(x = "Children 0-4 per 1,000 residents aged 20-44", y = "% of Output Areas") + theme_note(11)
ggsave(file.path(fig, "oa_children_distribution.png"), g2, width = 8, height = 4, dpi = 150)
