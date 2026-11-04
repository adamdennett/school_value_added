# 01b_extract_ees_2526.R - Build the 2025/26 panel year from EES sources
# ---------------------------------------------------------------------------
# 01_extract_data.R reads the CSCP performance-table CSVs, which stop at
# 2024/25. This script does the same job for 2025/26 from the EES downloads
# fetched by 00b_download_ees_api.R, and returns a frame with the same column
# names so 02_build_panel.R can bind it on without knowing the difference.
#
# Three things are genuinely different about 2025/26, and all three are
# recorded in the output rather than papered over:
#
#   P8MEA is NA.  Progress 8 is not published for 2024/25 or 2025/26 - those
#   cohorts have no KS2 baseline, because the 2019/20 and 2020/21 primary
#   assessments were cancelled. DfE have said there will be no replacement.
#   PTPRIORLO / PTPRIORHI are NA for the same reason.
#
#   PERCTOT is NA.  Full-year absence for 2025/26 publishes around March 2027.
#   In its place this script builds PERCTOT_2term - overall absence across the
#   autumn and spring terms - for EVERY panel year, from a single file that
#   runs 2016/17 to 2025/26. That makes a consistent five-year absence series
#   possible without touching the full-year PERCTOT the published analysis
#   was fitted on. See ees_absence_2term().
#
#   School attributes are carried forward.  Ofsted rating, admissions policy,
#   school type and coordinates are taken from the school's most recent
#   existing panel row. For Ofsted this is the only defensible option: single
#   headline grades were abolished in September 2024, so no new overall grade
#   exists to read.
#
# Usage:
#   source("R/01b_extract_ees_2526.R")
#   year_2526 <- build_ees_year()
#
# Run from project root.
# ---------------------------------------------------------------------------

source(here::here("pipeline", "helpers.R"))
source(here::here("pipeline", "ees_api.R"))

EES_YEAR_CONFIG <- list(
  academic_year = "2025-2026",
  year_label    = "2025-26",
  year_numeric  = 4L,
  time_period   = 202526L
)

# EES reports suppressed or not-applicable cells with single letters. NA_CODES
# in helpers.R already covers them; this keeps the intent visible at the
# point of use.
as_num <- function(x) suppressWarnings(as.numeric(ifelse(x %in% NA_CODES, NA, x)))

# Special schools are not in scope: 01_extract_data.R filters the performance
# tables to RECTYPE == 1 (mainstream), and these are the EES type groups that
# fall outside it.
EES_SPECIAL_TYPES <- c(
  "Community special school", "Foundation special school",
  "Converter academies - special school", "Free schools - special school",
  "Sponsored academies - special school", "Independent special schools",
  "Non-maintained special schools"
)


#' Read the KS4 institution-level data for one academic year
#'
#' The EES file is long: one row per school x time period x breakdown, where
#' the breakdown is Total, or a level of sex / disadvantage status / first
#' language / prior attainment / mobility. The panel wants one row per school
#' with the breakdowns spread across columns, so this pivots the three
#' breakdowns the model uses and drops the rest.
#'
#' @param time_period EES time period code, e.g. 202526L
#' @return A tibble, one row per school
ees_ks4_year <- function(time_period = EES_YEAR_CONFIG$time_period) {

  path <- ees_path("ks4_institution")
  message("Reading KS4 institution-level data: ", basename(path))

  raw <- readr::read_csv(
    path, col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  ) |>
    dplyr::filter(.data$time_period == !!as.character(time_period))

  message("  ", format(nrow(raw), big.mark = ","), " rows for ", time_period)

  raw <- raw |>
    dplyr::filter(!.data$establishment_type_group %in% EES_SPECIAL_TYPES)

  pick <- function(topic, level, cols) {
    raw |>
      dplyr::filter(.data$breakdown_topic == topic, .data$breakdown == level) |>
      dplyr::select(URN = "school_urn", dplyr::all_of(names(cols))) |>
      dplyr::rename(!!!stats::setNames(names(cols), unname(cols))) |>
      dplyr::distinct(.data$URN, .keep_all = TRUE)
  }

  # All pupils
  total <- raw |>
    dplyr::filter(.data$breakdown_topic == "Total") |>
    dplyr::transmute(
      URN         = .data$school_urn,
      SCHNAME     = .data$school_name,
      LEA         = .data$old_la_code,
      LANAME      = .data$la_name,
      ESTAB_GROUP = .data$establishment_type_group,
      VERSION     = .data$version,
      ATT8SCR     = as_num(.data$attainment8_average),
      ATT8SCRENG  = as_num(.data$attainment8eng_average),
      ATT8SCRMAT  = as_num(.data$attainment8mat_average),
      TPUP        = as_num(.data$pupil_count)
    ) |>
    dplyr::distinct(.data$URN, .keep_all = TRUE)

  dis <- pick("Disadvantage status", "Disadvantaged",
              c(attainment8_average = "ATT8SCR_FSM6CLA1A",
                pupil_percent       = "PTFSM6CLA1A",
                pupil_count         = "TFSM6CLA1A"))

  ndis <- pick("Disadvantage status", "Not known to be disadvantaged",
               c(attainment8_average = "ATT8SCR_NFSM6CLA1A",
                 pupil_percent       = "PTNOTFSM6CLA1A"))

  boys  <- pick("Sex", "Boys",  c(attainment8_average = "ATT8SCR_BOYS",
                                  pupil_count         = "NORB"))
  girls <- pick("Sex", "Girls", c(attainment8_average = "ATT8SCR_GIRLS",
                                  pupil_count         = "NORG"))

  out <- total |>
    dplyr::left_join(dis,   by = "URN") |>
    dplyr::left_join(ndis,  by = "URN") |>
    dplyr::left_join(boys,  by = "URN") |>
    dplyr::left_join(girls, by = "URN") |>
    dplyr::mutate(dplyr::across(
      c("PTFSM6CLA1A", "TFSM6CLA1A", "PTNOTFSM6CLA1A",
        "ATT8SCR_FSM6CLA1A", "ATT8SCR_NFSM6CLA1A",
        "ATT8SCR_BOYS", "ATT8SCR_GIRLS", "NORB", "NORG"), as_num))

  # Not published for this cohort - carried as NA so the gap is explicit
  # rather than a silently missing column.
  out$P8MEA             <- NA_real_
  out$P8MEA_FSM6CLA1A   <- NA_real_
  out$P8MEA_NFSM6CLA1A  <- NA_real_
  out$PTPRIORLO         <- NA_real_
  out$PTPRIORHI         <- NA_real_

  message("  ", format(nrow(out), big.mark = ","), " mainstream schools; ",
          sum(!is.na(out$ATT8SCR)), " with an Attainment 8 score")
  out
}


#' School-level absence on an autumn-and-spring basis, every year available
#'
#' One file covers 2016/17 to 2025/26, so the same measure can be put against
#' every year in the panel. This is NOT the same quantity as PERCTOT in the
#' performance tables, which is a full academic year - summer term absence is
#' excluded here. Treat the two as separate variables, never as a join.
#'
#' @return A tibble: URN, time_period, PERCTOT_2term, PPERSABS10_2term
ees_absence_2term <- function() {

  path <- ees_path("absence_2term_school")
  message("Reading two-term absence: ", basename(path))

  con <- if (grepl("\\.zip$", path)) {
    unz(path, "data/Absence_2term_school.csv")
  } else {
    path
  }

  abs_raw <- readr::read_csv(
    con, col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )

  out <- abs_raw |>
    dplyr::filter(.data$geographic_level == "School") |>
    dplyr::transmute(
      URN              = .data$school_urn,
      time_period      = as.integer(.data$time_period),
      PERCTOT_2term    = as_num(.data$sess_overall_percent),
      PPERSABS10_2term = as_num(.data$enrolments_pa_10_exact_percent)
    ) |>
    dplyr::filter(!is.na(.data$URN)) |>
    dplyr::distinct(.data$URN, .data$time_period, .keep_all = TRUE)

  message("  ", format(nrow(out), big.mark = ","), " school-years, ",
          min(out$time_period), " to ", max(out$time_period))
  out
}


#' School characteristics from the January 2026 school census
#'
#' Supplies the first-language percentages the model uses as PNUMEAL, plus
#' headcount, postcode and region, which the KS4 file does not carry.
ees_census_2526 <- function() {

  path <- ees_path("school_census")
  message("Reading school census: ", basename(path))

  cen <- readr::read_csv(
    path, col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )

  eal_pct <- "% of pupils whose first language is known or believed to be other than English"
  eng_pct <- "% of pupils whose first language is known or believed to be English"
  head_n  <- "headcount of pupils"

  out <- cen |>
    dplyr::filter(.data$geographic_level == "School") |>
    dplyr::transmute(
      URN       = .data$urn,
      PNUMEAL   = as_num(.data[[eal_pct]]),
      PNUMENGFL = as_num(.data[[eng_pct]]),
      TOTPUPS   = as_num(.data[[head_n]]),
      POSTCODE  = .data$school_postcode,
      gor_name  = .data$region_name,
      ADMPOL    = .data$admissions_policy
    ) |>
    dplyr::filter(!is.na(.data$URN)) |>
    dplyr::distinct(.data$URN, .keep_all = TRUE)

  message("  ", format(nrow(out), big.mark = ","), " schools")
  out
}


#' Attributes carried forward from the school's most recent existing panel row
#'
#' Ofsted rating, admissions policy band, school type and coordinates do not
#' come with the new release. For Ofsted there is no alternative: single
#' headline grades were abolished in September 2024.
#'
#' @param panel An existing panel (defaults to data/panel_data.rds)
carry_forward_attrs <- function(panel = NULL) {

  if (is.null(panel)) {
    p <- here::here("data", "panel_data.rds")
    if (!file.exists(p)) {
      stop("No existing panel at ", p, " - run 01-02 first, or pass panel =")
    }
    panel <- readRDS(p)
  }

  keep <- c("OFSTEDRATING", "SCHOOLTYPE", "MINORGROUP", "RELCHAR",
            "ADMPOL_PT", "TOWN", "easting", "northing", "gender_name")
  keep <- intersect(keep, names(panel))

  out <- panel |>
    dplyr::arrange(.data$URN, dplyr::desc(.data$year_numeric)) |>
    dplyr::group_by(.data$URN) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select("URN", dplyr::all_of(keep)) |>
    dplyr::mutate(URN = as.character(.data$URN))

  message("Carried forward ", length(keep), " attributes for ",
          format(nrow(out), big.mark = ","), " schools")
  out
}


#' Build the 2025/26 year, shaped like a year from 01_extract_data.R
#'
#' @param panel Existing panel used for carry-forward (default: on disk)
#' @return A tibble, one row per mainstream school
build_ees_year <- function(panel = NULL) {

  cfg <- EES_YEAR_CONFIG
  message("\n=== Building ", cfg$academic_year, " from EES ===")

  ks4   <- ees_ks4_year(cfg$time_period)
  cen   <- ees_census_2526()
  absn  <- ees_absence_2term() |>
    dplyr::filter(.data$time_period == cfg$time_period) |>
    dplyr::select(-"time_period")
  attrs <- carry_forward_attrs(panel)

  out <- ks4 |>
    dplyr::left_join(cen,   by = "URN") |>
    dplyr::left_join(absn,  by = "URN") |>
    dplyr::left_join(attrs, by = "URN") |>
    dplyr::mutate(
      # Full-year absence does not exist for this year yet.
      PERCTOT       = NA_real_,
      PPERSABS10    = NA_real_,
      academic_year = cfg$academic_year,
      year_label    = cfg$year_label,
      year_numeric  = cfg$year_numeric,
      time_period   = cfg$time_period
    )

  covered <- function(v) if (v %in% names(out)) sum(!is.na(out[[v]])) else NA_integer_
  message("\nCoverage of the model's variables:")
  for (v in c("ATT8SCR", "ATT8SCR_FSM6CLA1A", "ATT8SCR_NFSM6CLA1A",
              "PTFSM6CLA1A", "PNUMEAL", "PERCTOT_2term", "ADMPOL_PT",
              "OFSTEDRATING", "gor_name")) {
    message(sprintf("  %-20s %5s of %s", v, format(covered(v), big.mark = ","),
                    format(nrow(out), big.mark = ",")))
  }

  out
}
