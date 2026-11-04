# 11_absence_reasons.R - What each school's absence is, school-year by school-year
# ---------------------------------------------------------------------------
# The school-level absence data from Explore Education Statistics, which
# 03_build_panel.R downloads for overall absence, also splits every
# school-year's absence by reason. This keeps the split for secondary
# schools: illness, the other authorised reasons, and unauthorised absence,
# each as a percentage of possible sessions, and the illness share of all
# absence. Value Added v2.1 tests whether the kind of absence, not just its
# amount, belongs in the expectation.
#
# Output: data/absence_reasons.rds (URN, year_label, overall, illness,
# auth_other, unauthorised, ill_share).   Rscript pipeline/11_absence_reasons.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr); library(readr) })

raw <- here::here("data", "cache", "absence_api_raw.csv")
if (!file.exists(raw)) stop("Missing ", raw, ": run pipeline/03_build_panel.R first")

num <- function(x) suppressWarnings(as.numeric(x))
out <- read_csv(raw, show_col_types = FALSE, col_types = cols(.default = "c")) |>
  filter(geographic_level == "School", education_phase == "State-funded secondary") |>
  transmute(URN = school_urn,
            year_label = paste0(substr(time_period, 1, 4), "-", substr(time_period, 5, 6)),
            overall = num(sess_overall_percent),
            illness = num(sess_auth_illness_rate),
            auth_other = num(sess_authorised_percent) - num(sess_auth_illness_rate),
            unauthorised = num(sess_unauthorised_percent)) |>
  filter(!is.na(overall), overall > 0) |>
  mutate(ill_share = 100 * illness / overall) |>
  distinct(URN, year_label, .keep_all = TRUE)

saveRDS(out, here::here("data", "absence_reasons.rds"))
message(sprintf("Saved absence_reasons.rds: %d school-years, %s", nrow(out),
                paste(sort(unique(out$year_label)), collapse = ", ")))
