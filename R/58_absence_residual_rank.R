# 58_absence_residual_rank.R - How unusual is Brighton and Hove's absence once intake is allowed for?
# ---------------------------------------------------------------------------
# Uses R/04's absence-against-intake model (data/place_absence.rds): log(school absence) on the v2
# intake terms with year and region effects and no local authority effect, 2021/22 to 2024/25,
# state-funded mainstream secondary schools in the Value Added v2.1 frame. A school's residual is its
# actual absence minus the model's prediction, in percentage points; an authority's is the mean over
# its schools (each school counted equally), averaged over the four years.
# Reports, for Brighton and Hove:
#   rank of 152 authorities, overall and in each year; z-score against the authorities' spread;
#   a check against chance: the share of 20,000 random sets of the same number of English secondaries
#   whose mean residual is at least as large; a one-sample t-test on the city's school residuals.
# Output: data/absence_residual_rank.rds, writeup/figures/absence_residual_la.png
#   Rscript R/58_absence_residual_rank.R
# ---------------------------------------------------------------------------
source(here::here("R", "site_helpers.R"))
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
x <- readRDS(here::here("data", "place_absence.rds"))
L <- as.data.table(x$abs_la)[order(-excess)]
SY <- as.data.table(x$abs_school_years)
bh <- L[LANAME == "Brighton and Hove"]
bs <- SY[LANAME == "Brighton and Hove", .(excess = mean(excess)), by = URN]
sx <- SY[, .(excess = mean(excess)), by = .(URN, LANAME)]
set.seed(58); sims <- replicate(20000, mean(sample(sx$excess, nrow(bs))))
Y <- SY[, .(excess = mean(excess)), by = .(year_label, LANAME)][, rank := frank(-excess, ties.method = "min"), by = year_label]
res <- list(la = L, bh = bh, z = (bh$excess - mean(L$excess)) / sd(L$excess), pct_below = mean(L$excess < bh$excess),
            p_random = mean(sims >= mean(bs$excess)), p_ttest = t.test(bs$excess)$p.value, schools = bs[order(-excess)],
            by_year = Y[LANAME == "Brighton and Hove"][order(year_label)], r2 = x$abs_r2, built = Sys.Date())
saveRDS(res, here::here("data", "absence_residual_rank.rds"))
print(L[1:8, .(LANAME, excess = round(excess, 2), rank_excess)]); cat("z", round(res$z, 2), " p_random", res$p_random, " p_t", signif(res$p_ttest, 2), "\n"); print(res$by_year)

L[, `:=`(ord = .N - .I + 1, hl = LANAME == "Brighton and Hove")]
lab <- L[rank_excess <= 5 | hl]
g <- ggplot(L, aes(ord, excess)) + geom_hline(yintercept = 0, colour = GREY) +
  geom_segment(aes(xend = ord, y = 0, yend = excess, colour = hl), linewidth = 0.6) +
  geom_point(aes(colour = hl, size = hl)) +
  ggrepel::geom_text_repel(data = lab, aes(label = sprintf("%s %+.1f", LANAME, excess)), size = 3.3, direction = "y", nudge_x = -25, hjust = 1, segment.colour = GREY, seed = 1) +
  scale_colour_manual(values = c(`TRUE` = BPURPLE, `FALSE` = "#c9c2d6"), guide = "none") + scale_size_manual(values = c(`TRUE` = 3, `FALSE` = 1), guide = "none") +
  scale_x_continuous(breaks = NULL) +
  labs(x = "152 local authorities, ordered", y = "Absence above what the intake predicts\n(percentage points, 2021/22 to 2024/25)",
       caption = "Value Added v2.1 school frame; DfE school absence and census. Model: R/04, log absence on intake with year and region effects.") + theme_note(11)
ggsave(here::here("writeup", "figures", "absence_residual_la.png"), g, width = 9, height = 4.8, dpi = 150)
