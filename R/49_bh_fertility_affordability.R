# 49_bh_fertility_affordability.R - Brighton and Hove: fertility over time, against affordability
# ---------------------------------------------------------------------------
# Step 1 of writeup/bh_falling_rolls_backlog.md. Councillors say Brighton and Hove's
# falling births and rolls come from housing costs (Brighton and Hove News, 13 November
# 2026). This sets fertility, not just births, against house prices relative to earnings,
# over time, for Brighton and Hove, England and every district:
#
#  A. Births split into the number of women aged 15 to 44 and the general fertility rate
#     (births per 1,000 women 15 to 44); the total fertility rate (TFR) removes age structure.
#  B. Age-specific fertility: at which ages has Brighton and Hove's fertility fallen, against
#     England (postponement shows as falls in the 20s with steady or rising rates at 35 to 44).
#  C. Affordability over time: median house price / median residence-based earnings,
#     Brighton and Hove against England, 2002 to 2025.
#  D. Across districts, 2013 to 2025: does fertility fall faster where affordability worsens?
#     A panel with district and year effects, so only each district's own change over time
#     counts (log ratio, same year and three years earlier); the change from 2013-15 to
#     2023-25 in TFR against the change in log ratio; and where Brighton and Hove sits against
#     the level its affordability predicts in 2023-25.
#
# Sources (all published):
#   ONS, Live births in England and Wales: birth rates down to local authority areas, via
#     Nomis NM_207_1, districts and unitaries (April 2023), 2013 to 2025: births, GFR, TFR,
#     age-specific rates, standardised mean age of mother. Downloaded one year at a time
#     (Nomis caps a query at 25,000 rows) to
#     data-raw/ons/birthrates_lad_2013_2025_nomis_NM_207_1.csv (README has the command).
#   ONS, House price to residence-based earnings ratio (Housing affordability in England and
#     Wales, 2025 release), table 5c (median ratio by local authority district, 2002 to
#     2025): data-raw/ons/affordability/aff2ratioofhousepricetoresidencebasedearnings.xlsx.
#     House prices are for the year ending September; earnings are ASHE full-time,
#     residence-based.
#
# Output: data/bh_fertility_affordability.rds and figures in writeup/figures/.
#   Rscript R/49_bh_fertility_affordability.R   (under a minute)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
BH <- "E06000043"

# ---- fertility --------------------------------------------------------------------------
fr <- fread(here::here("data-raw", "ons", "birthrates_lad_2013_2025_nomis_NM_207_1.csv"))
setnames(fr, c("year", "name", "code", "measure", "value"))
key <- c("Live births" = "births", "General fertility rate (GFR)" = "gfr", "Total fertility rate (TFR)" = "tfr",
         "Standardised mean age of mother" = "mean_age",
         "Age specific fertility rate : Aged under 20" = "a_u20", "Age specific fertility rate : Aged 20-24" = "a_20_24",
         "Age specific fertility rate : Aged 25-29" = "a_25_29", "Age specific fertility rate : Aged 30-34" = "a_30_34",
         "Age specific fertility rate : Aged 35-39" = "a_35_39", "Age specific fertility rate : Aged 40-44" = "a_40_44")
fr <- fr[measure %in% names(key)][, measure := key[measure]]
F <- dcast(fr, year + code + name ~ measure, value.var = "value")
F[, women := 1000 * births / gfr]                        # women aged 15 to 44, implied by births / GFR
F <- F[substr(code, 1, 1) == "E"]                        # England (the country row and English districts)

# ---- A. births = women x rate ------------------------------------------------------------
decomp <- function(cd, y0, y1) {
  a <- F[code == cd & year == y0]; b <- F[code == cd & year == y1]
  data.table(code = cd, name = a$name, from = y0, to = y1, births = 100 * (b$births / a$births - 1),
             women_15_44 = 100 * (b$women / a$women - 1), gfr = 100 * (b$gfr / a$gfr - 1), tfr = 100 * (b$tfr / a$tfr - 1),
             tfr_from = a$tfr, tfr_to = b$tfr, mean_age_from = a$mean_age, mean_age_to = b$mean_age)
}
A <- rbind(decomp(BH, 2013, 2025), decomp("E92000001", 2013, 2025), decomp(BH, 2015, 2025), decomp("E92000001", 2015, 2025))

# ---- B. age-specific fertility -------------------------------------------------------------
ages <- c("a_u20", "a_20_24", "a_25_29", "a_30_34", "a_35_39", "a_40_44")
B <- melt(F[code %in% c(BH, "E92000001"), c("year", "code", "name", ages), with = FALSE], id.vars = c("year", "code", "name"),
          variable.name = "age", value.name = "rate")
B[, age := factor(age, ages, c("Under 20", "20-24", "25-29", "30-34", "35-39", "40-44"))]
B_chg <- dcast(B[year %in% c(2013, 2025)], name + age ~ year, value.var = "rate")[, chg := 100 * (`2025` / `2013` - 1)][]

# ---- affordability -------------------------------------------------------------------------
x <- suppressMessages(readxl::read_excel(here::here("data-raw", "ons", "affordability", "aff2ratioofhousepricetoresidencebasedearnings.xlsx"),
                                         sheet = "5c", skip = 1))
x <- as.data.table(x)
setnames(x, 1:4, c("region_code", "region", "code", "name"))
yrs <- grep("^(Year ending Sep )?[0-9]{4}$", names(x), value = TRUE)
R <- melt(x[, c("code", "name", yrs), with = FALSE], id.vars = c("code", "name"), variable.name = "year", value.name = "ratio")
R[, `:=`(year = as.integer(sub("\\D*(\\d{4})$", "\\1", year)), ratio = suppressWarnings(as.numeric(ratio)))]
R <- R[!is.na(ratio)]
eng_ratio <- suppressMessages(readxl::read_excel(here::here("data-raw", "ons", "affordability", "aff2ratioofhousepricetoresidencebasedearnings.xlsx"),
                                                 sheet = "1c", skip = 1)) |> as.data.table()
setnames(eng_ratio, 1:2, c("code", "name"))
eng_ratio <- melt(eng_ratio[code == "E92000001", c("code", "name", grep("^[0-9]{4}$", names(eng_ratio), value = TRUE)), with = FALSE],
                  id.vars = c("code", "name"), variable.name = "year", value.name = "ratio")[, `:=`(year = as.integer(as.character(year)), ratio = as.numeric(ratio))]
C <- rbind(R[code == BH], eng_ratio)

# ---- D. across districts ---------------------------------------------------------------------
P <- merge(F[code != "E92000001", .(year, code, name, tfr, gfr, births)], R[, .(code, year, ratio)], by = c("code", "year"))
P[, lr := log(ratio)]
setorder(P, code, year)
P[, lr_lag3 := shift(lr, 3), by = code]
lag_from_R <- R[, .(code, year = year + 3L, lr_lag3_full = log(ratio))]                # three years earlier, from the full 2002- series
P <- merge(P, lag_from_R, by = c("code", "year"), all.x = TRUE)
fe <- function(form) { m <- lm(form, data = P); s <- summary(m)$coefficients; s[grep("^lr", rownames(s)), , drop = FALSE] }
D_panel <- list(same_year = fe(tfr ~ lr + factor(code) + factor(year)),
                lag3 = fe(tfr ~ lr_lag3_full + factor(code) + factor(year)),
                both = fe(tfr ~ lr + lr_lag3_full + factor(code) + factor(year)),
                districts = uniqueN(P$code), years = range(P$year))
# change from 2013-15 to 2023-25
avg <- function(y) P[year %in% y, .(tfr = mean(tfr), lr = mean(lr), ratio = mean(ratio)), by = .(code, name)]
D_chg <- merge(avg(2013:2015), avg(2023:2025), by = c("code", "name"), suffixes = c("_0", "_1"))
D_chg[, `:=`(d_tfr = tfr_1 - tfr_0, d_lr = lr_1 - lr_0, pct_tfr = 100 * (tfr_1 / tfr_0 - 1), pct_ratio = 100 * (ratio_1 / ratio_0 - 1))]
m_chg <- lm(d_tfr ~ d_lr, data = D_chg)
D_chg[, pred_d_tfr := predict(m_chg)][, resid_d := d_tfr - pred_d_tfr]
# level in 2023-25: TFR against log ratio
m_lvl <- lm(tfr_1 ~ log(ratio_1), data = D_chg)
D_chg[, pred_tfr_1 := predict(m_lvl)][, resid_lvl := tfr_1 - pred_tfr_1]
D_chg[, `:=`(rank_tfr_1 = frank(tfr_1), rank_resid_lvl = frank(resid_lvl), rank_resid_d = frank(resid_d))]
bh <- D_chg[code == BH]

res <- list(A = A, B = B, B_chg = B_chg, C = C, D_panel = D_panel, D_chg = D_chg, m_chg = summary(m_chg)$coefficients,
            r2_chg = summary(m_chg)$r.squared, m_lvl = summary(m_lvl)$coefficients, r2_lvl = summary(m_lvl)$r.squared,
            bh = bh, n_districts = nrow(D_chg), tfr_series = F[code %in% c(BH, "E92000001"), .(year, name, tfr, gfr, births, women, mean_age)],
            built = Sys.Date())
saveRDS(res, here::here("data", "bh_fertility_affordability.rds"))

# ---- figures (two charts, no dual axes) -------------------------------------------------------
dir.create(here::here("writeup", "figures"), showWarnings = FALSE, recursive = TRUE)
th <- theme_minimal(base_size = 12) + theme(legend.position = "top", panel.grid.minor = element_blank(), plot.title.position = "plot")
col2 <- c("Brighton and Hove" = "#361a54", "England" = "#9e9eb0")
f1 <- rbind(F[code %in% c(BH, "E92000001"), .(year, name, value = tfr, panel = "Total fertility rate (children per woman)")],
            C[year >= 2002, .(year, name, value = ratio, panel = "House price to earnings ratio (median)")])
f1[, name := ifelse(name == "England", "England", "Brighton and Hove")]
g1 <- ggplot(f1, aes(year, value, colour = name)) + geom_line(linewidth = 1.1) + geom_point(size = 1.4) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") + scale_colour_manual(values = col2, name = NULL) +
  labs(x = NULL, y = NULL, title = "Brighton and Hove's fertility fell in step with England's: the gap is in the level",
       caption = "ONS birth rates (Nomis NM_207_1); ONS house price to residence-based earnings ratio, table 5c") + th
ggsave(here::here("writeup", "figures", "bh_tfr_affordability_over_time.png"), g1, width = 8, height = 6.5, dpi = 150)
g2 <- ggplot(D_chg, aes(pct_ratio, pct_tfr)) + geom_hline(yintercept = median(D_chg$pct_tfr), colour = "#d3cde0") +
  geom_point(colour = "#9e9eb0", alpha = 0.7, size = 1.6) + geom_smooth(method = "lm", se = TRUE, colour = "#6b6680", linewidth = 0.8, formula = y ~ x) +
  geom_point(data = bh, colour = "#361a54", size = 3.5) +
  geom_text(data = bh, aes(label = "Brighton and Hove"), colour = "#361a54", hjust = -0.1, vjust = -0.6, size = 3.8) +
  labs(x = "Change in house price to earnings ratio, 2013-15 to 2023-25 (%)", y = "Change in total fertility rate (%)",
       title = "Fertility fell about as much where housing got less affordable as where it didn't",
       subtitle = sprintf("%d districts in England", nrow(D_chg)),
       caption = "ONS birth rates (Nomis NM_207_1); ONS house price to residence-based earnings ratio, table 5c") + th
ggsave(here::here("writeup", "figures", "district_change_tfr_vs_affordability.png"), g2, width = 8, height = 6, dpi = 150)

options(width = 150)
cat("\n== A. Births split into women 15-44 and the fertility rate (% change)\n"); print(A, digits = 3)
cat("\n== B. Age-specific fertility, 2013 and 2025 (births per 1,000 women)\n"); print(B_chg, digits = 3)
cat("\n== C. Affordability, Brighton and Hove and England\n"); print(dcast(C[year %in% c(2002, 2008, 2013, 2015, 2019, 2022, 2025)], name ~ year, value.var = "ratio"))
cat("\n== D. Panel, district and year effects: TFR on log ratio\n"); print(D_panel)
cat("\n== D. Change 2013-15 to 2023-25: d TFR on d log ratio\n"); print(res$m_chg); cat("R2", res$r2_chg, "\n")
cat("\n== D. Level 2023-25: TFR on log ratio\n"); print(res$m_lvl); cat("R2", res$r2_lvl, "\n")
cat("\n== Brighton and Hove among", nrow(D_chg), "districts\n"); print(bh, digits = 3)
cat("\nLowest TFR 2023-25:\n"); print(D_chg[order(tfr_1)][1:12, .(name, tfr_1, ratio_1, resid_lvl, pct_tfr, pct_ratio)], digits = 3)
