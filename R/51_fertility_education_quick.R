# 51_fertility_education_quick.R - Quick first test: fertility against Level 4+ qualifications (13 Nov 2026)
# Census 2021 TS067, % of residents aged 16+ with Level 4+ (all adults, not yet women 25-44), districts,
#   Nomis NM_2084_1: curl "https://www.nomisweb.co.uk/api/v01/dataset/NM_2084_1.data.csv?date=latest&geography=TYPE424&measures=20301&select=geography_code,geography_name,c2021_hiqual_8_name,obs_value"
#   saved as data-raw/census2021/ts067_lad_pct_nomis_NM_2084_1.csv; fertility and affordability from R/49.
# Next (backlog): women 25-44 by sex and age, students, renting, mothers born abroad; 2011-2021 change.

suppressMessages({library(data.table); library(ggplot2)})
setwd(here::here())
q <- fread("data-raw/census2021/ts067_lad_pct_nomis_NM_2084_1.csv")
setnames(q, c("code", "name", "cat", "pct"))
l4 <- q[cat == "Level 4 qualifications or above", .(code, l4 = pct)]
FA <- readRDS("data/bh_fertility_affordability.rds"); D <- FA$D_chg
d <- merge(D, l4, by = "code")
cat("districts:", nrow(d), "\n")
m1 <- lm(tfr_1 ~ log(ratio_1), d); m2 <- lm(tfr_1 ~ l4, d); m3 <- lm(tfr_1 ~ l4 + log(ratio_1), d)
m4 <- lm(pct_tfr ~ l4, d)
r2 <- function(m) round(summary(m)$r.squared, 3)
cat("R2: affordability", r2(m1), " L4+", r2(m2), " both", r2(m3), "\n")
print(round(summary(m3)$coefficients, 4))
cat("change in TFR (%) on L4+: R2", r2(m4), "\n"); print(round(summary(m4)$coefficients, 4))
d[, `:=`(res_aff = resid(m1), res_both = resid(m3))]
bh <- d[name == "Brighton and Hove"]
cat("BH L4+", bh$l4, " rank", d[, frank(-l4)][d$name == "Brighton and Hove"], "of", nrow(d), "\n")
cat("BH gap below prediction: affordability only", round(bh$res_aff, 3), " with L4+ and affordability", round(bh$res_both, 3), "\n")
print(d[name %in% c("Brighton and Hove","Lewes","Mid Sussex","Wealden","Cambridge","Oxford","Norwich","York","Exeter")][order(tfr_1), .(name, tfr = round(tfr_1, 2), l4, ratio = round(ratio_1, 1), gap_aff = round(res_aff, 2), gap_both = round(res_both, 2))])
g <- ggplot(d, aes(l4, tfr_1)) + geom_point(colour = "#c9a2f5", size = 1.7) + geom_smooth(method = "lm", se = FALSE, colour = "#6b6680", formula = y ~ x) +
  geom_point(data = d[name %in% c("Brighton and Hove","Lewes","Mid Sussex","Wealden","Cambridge","Oxford","Norwich")], colour = "#361a54", size = 2.6) +
  ggrepel::geom_text_repel(data = d[name %in% c("Brighton and Hove","Lewes","Mid Sussex","Wealden","Cambridge","Oxford","Norwich")], aes(label = name), colour = "#361a54", size = 3.2, seed = 1, min.segment.length = 0) +
  labs(x = "Residents aged 16+ with Level 4+ qualifications, Census 2021 (%)", y = "Total fertility rate, 2023-25",
       title = sprintf("Fertility against Level 4+ qualifications, %d districts (R² = %.2f; house prices alone %.2f)", nrow(d), r2(m2), r2(m1)),
       caption = "ONS Census 2021 TS067 (Nomis NM_2084_1); ONS birth rates (Nomis NM_207_1)") +
  theme_minimal(base_size = 12) + theme(plot.title.position = "plot", panel.grid.minor = element_blank())
ggsave("writeup/figures/tfr_vs_level4_quick.png", g, width = 8.5, height = 6, dpi = 150)
