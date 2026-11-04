# 02c_ofsted_dates.R - Say when each Ofsted rating in the panel is from
# ---------------------------------------------------------------------------
# The panel's Ofsted rating (OFSTEDRATING / OFSTEDRATING_1) comes from two
# sources, and neither is current:
#
#   2021/22, 2022/23  the performance tables' school information file, which
#                     carries the rating and the date of the inspection it
#                     came from (OFSTEDLASTINSP);
#   2023/24 onward    that file no longer carries Ofsted at all, so
#                     02_build_panel.R takes the overall effectiveness of each
#                     school's latest graded inspection from Ofsted's
#                     statistics "as at 31 August 2024" (OFSTED_URL).
#
# Neither date reached the panel, so nothing could say how old a rating is:
# a school inspected in 2025 still shows the grade from its previous
# inspection, undated. This adds two columns and changes nothing else:
#
#   OFSTEDLASTINSP  start date of the inspection the rating comes from
#   OFSTED_SOURCE   which of the two sources the rating came from
#
# A date is only attached where the source's rating agrees with the
# panel's, so a date can never describe a different rating. It is run after
# 02_build_panel.R (and 02b), and is safe to rerun.
#
#   Rscript R/02c_ofsted_dates.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr); library(readr) })
source(here::here("pipeline", "helpers.R"))

GRADES <- c("1" = "Outstanding", "2" = "Good", "3" = "Requires Improvement", "4" = "Inadequate")
TEXT   <- c("Outstanding" = "Outstanding", "Good" = "Good",
            "Requires improvement" = "Requires Improvement",
            "Requires Improvement" = "Requires Improvement", "Inadequate" = "Inadequate",
            "Serious Weaknesses" = "Inadequate", "Special Measures" = "Inadequate")
STATS_SOURCE <- "Ofsted statistics as at 31 August 2024"

#' Rating and inspection date from one year's school information file,
#' or NULL if that year's file carries no Ofsted columns
tables_dates <- function(year_label) {
  dir <- sub("^(\\d{4})-(\\d{2})$", "\\1-20\\2", year_label)
  f <- here::here("data", "extracted", dir, "england_school_information.csv")
  if (!file.exists(f)) return(NULL)
  if (!all(c("OFSTEDRATING", "OFSTEDLASTINSP") %in% names(read_csv(f, n_max = 0, show_col_types = FALSE))))
    return(NULL)
  read_dfe_csv(f, col_types = cols(.default = col_character())) |>
    transmute(URN, year_label = year_label, pt_grade = unname(TEXT[OFSTEDRATING]),
              pt_date = suppressWarnings(lubridate::dmy(OFSTEDLASTINSP))) |>
    filter(!is.na(pt_grade))
}

#' Rating and inspection date from Ofsted's statistics, one row a school
stats_dates <- function() {
  read_csv(here::here("data", "cache", "ofsted_raw.csv"), show_col_types = FALSE,
           col_types = cols(.default = col_character())) |>
    transmute(URN, st_grade = unname(GRADES[`Overall effectiveness`]),
              st_date = suppressWarnings(lubridate::dmy(`Inspection start date`))) |>
    filter(!is.na(st_grade)) |>
    distinct(URN, .keep_all = TRUE)
}

date_ratings <- function(panel) {
  pt <- bind_rows(lapply(unique(panel$year_label), tables_dates))
  st <- stats_dates()
  out <- panel |>
    select(-any_of(c("OFSTEDLASTINSP", "OFSTED_SOURCE"))) |>
    mutate(.grade = as.character(OFSTEDRATING_1)) |>
    left_join(pt, by = c("URN", "year_label")) |>
    left_join(st, by = "URN") |>
    mutate(
      OFSTED_SOURCE = case_when(
        !is.na(.grade) & !is.na(pt_grade) & .grade == pt_grade ~
          paste0("Performance tables school information, ", sub("-", "/", year_label)),
        !is.na(.grade) & !is.na(st_grade) & .grade == st_grade ~ STATS_SOURCE,
        TRUE ~ NA_character_),
      OFSTEDLASTINSP = case_when(
        startsWith(coalesce(OFSTED_SOURCE, ""), "Performance") ~ pt_date,
        OFSTED_SOURCE %in% STATS_SOURCE ~ st_date)) |>
    select(-.grade, -pt_grade, -pt_date, -st_grade, -st_date)
  stopifnot(nrow(out) == nrow(panel), identical(out$OFSTEDRATING_1, panel$OFSTEDRATING_1))
  out
}

report <- function(p) {
  p |> filter(!is.na(OFSTEDRATING_1)) |>
    group_by(year_label) |>
    summarise(rated = n(), dated = sum(!is.na(OFSTEDLASTINSP)),
              from_tables = sum(startsWith(coalesce(OFSTED_SOURCE, ""), "Performance")),
              from_stats = sum(OFSTED_SOURCE %in% STATS_SOURCE),
              newest = max(OFSTEDLASTINSP, na.rm = TRUE), .groups = "drop") |>
    print()
}

if (sys.nframe() == 0) {
  for (f in c("panel_data.rds", "panel_data_5yr.rds")) {
    path <- here::here("data", f)
    if (!file.exists(path)) next
    message(f)
    p <- date_ratings(readRDS(path))
    report(p)
    saveRDS(p, path)
  }
}
