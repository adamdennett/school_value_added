# 02b_extend_panel_2526.R - Add 2025/26 to the panel, without disturbing it
# ---------------------------------------------------------------------------
# Takes the existing four-year panel built by 01-04 and returns a five-year
# one with 2025/26 bound on from EES. Writes to data/panel_data_5yr.rds, NOT
# over data/panel_data.rds: the published analysis is fitted on the four-year
# panel and must keep reproducing from it until the new year is adopted
# deliberately.
#
# What this script adds beyond binding the rows:
#
#   PERCTOT_2term / PPERSABS10_2term - overall absence and persistent absence
#   across autumn and spring only, for EVERY year, from a single EES file
#   covering 2016/17 to 2025/26. Full-year absence (PERCTOT) does not exist
#   for 2025/26 and will not until around March 2027, so a consistent series
#   is the only way to model five years. Where both bases exist they agree
#   closely - r = 0.984 across 12,958 school-years, with the two-term figure
#   running 0.28 percentage points lower - and swapping one for the other
#   moves the fitted absence coefficient from -0.2616 to -0.2613.
#
#   gorard_segregation - recomputed over five years, because the index is a
#   within-LA-within-year quantity and the new year needs its own value.
#
#   OFSTEDRATING_1 - re-derived after the bind, as 02_build_panel.R does.
#
# Usage:
#   source("R/02b_extend_panel_2526.R")
#   p5 <- extend_panel_2526()          # also writes data/panel_data_5yr.rds
#
# Run from project root.
# ---------------------------------------------------------------------------

source(here::here("pipeline", "helpers.R"))
source(here::here("pipeline", "ees_2526.R"))

# academic_year -> EES time_period, for joining the absence series on
PANEL_TIME_PERIODS <- c(
  "2021-2022" = 202122L, "2022-2023" = 202223L, "2023-2024" = 202324L,
  "2024-2025" = 202425L, "2025-2026" = 202526L
)


#' Align a new year's frame to an existing panel's columns and types
#'
#' Columns the new year does not have are filled with NA rather than dropped,
#' so the panel keeps its shape and every gap is visible as a gap.
align_to_panel <- function(new_year, panel) {

  shared <- intersect(names(panel), names(new_year))
  message("  ", length(shared), " of ", ncol(panel),
          " panel columns are present in the new year")

  out <- new_year[, shared, drop = FALSE]
  for (v in setdiff(names(panel), shared)) out[[v]] <- NA
  out <- out[, names(panel), drop = FALSE]

  # Match types column by column; bind_rows refuses on ordered-vs-unordered
  # factors, and the panel carries several ordered ones.
  for (v in names(panel)) {
    if (is.factor(panel[[v]])) {
      out[[v]] <- factor(as.character(out[[v]]), levels = levels(panel[[v]]),
                         ordered = is.ordered(panel[[v]]))
    } else if (is.numeric(panel[[v]]) && !is.numeric(out[[v]])) {
      out[[v]] <- suppressWarnings(as.numeric(out[[v]]))
    } else if (is.character(panel[[v]]) && !is.character(out[[v]])) {
      out[[v]] <- as.character(out[[v]])
    }
  }
  out
}


#' Attach the autumn-and-spring absence series to every year of a panel
add_two_term_absence <- function(panel) {

  abs2 <- ees_absence_2term()

  panel |>
    dplyr::mutate(.tp = unname(PANEL_TIME_PERIODS[.data$academic_year])) |>
    dplyr::select(-dplyr::any_of(c("PERCTOT_2term", "PPERSABS10_2term"))) |>
    dplyr::left_join(abs2, by = c("URN" = "URN", ".tp" = "time_period")) |>
    dplyr::select(-".tp")
}


#' Compute the Gorard segregation index for the NEW year only
#'
#' Mirrors compute_gorard_index() in 04_compute_derived.R. The index is a
#' within-LA, within-year quantity, so the new year needs its own value and
#' the published years need to keep theirs untouched. Recomputing all five
#' years from the extended panel moves 815 of the published rows - the
#' original index was computed at a different point in the pipeline, over a
#' slightly different set of schools - and every one of those rows feeds a
#' published model prediction. So this fills 2025/26 and nothing else.
add_gorard_new_year <- function(panel, new_year = "2025-2026") {

  gor <- panel |>
    dplyr::filter(.data$academic_year == new_year,
                  !is.na(.data$TFSM6CLA1A), !is.na(.data$TPUP), .data$TPUP > 0) |>
    dplyr::group_by(.data$LANAME, .data$academic_year) |>
    dplyr::mutate(F_total = sum(.data$TFSM6CLA1A, na.rm = TRUE),
                  T_total = sum(.data$TPUP, na.rm = TRUE)) |>
    dplyr::filter(.data$F_total > 0, .data$T_total > 0) |>
    dplyr::summarise(
      gorard_new = 0.5 * sum(abs(.data$TFSM6CLA1A / .data$F_total -
                                 .data$TPUP / .data$T_total), na.rm = TRUE),
      .groups = "drop")

  out <- panel |>
    dplyr::left_join(gor, by = c("LANAME", "academic_year")) |>
    dplyr::mutate(gorard_segregation = dplyr::if_else(
      .data$academic_year == new_year & !is.na(.data$gorard_new),
      .data$gorard_new, .data$gorard_segregation)) |>
    dplyr::select(-"gorard_new")

  message("  Gorard index for ", new_year, ": ",
          dplyr::n_distinct(gor$LANAME), " local authorities")
  out
}


#' Derive the harmonised Ofsted factor for rows that do not have one
#'
#' Published rows keep the factor 02_build_panel.R gave them; only the new
#' year is derived here, for the same reason as the Gorard index.
derive_ofsted_1 <- function(panel, new_year = "2025-2026") {
  ofsted_map <- c("Outstanding" = "1", "Good" = "2",
                  "Requires Improvement" = "3", "Requires improvement" = "3",
                  "Inadequate" = "4", "Serious Weaknesses" = "4",
                  "Special Measures" = "4")
  lv <- c("Outstanding", "Good", "Requires Improvement", "Inadequate")
  panel |>
    dplyr::mutate(
      OFSTEDRATING = as.character(.data$OFSTEDRATING),
      .new = dplyr::case_when(
        .data$OFSTEDRATING %in% names(ofsted_map) ~ ofsted_map[.data$OFSTEDRATING],
        .data$OFSTEDRATING %in% c("1", "2", "3", "4") ~ .data$OFSTEDRATING,
        TRUE ~ NA_character_),
      .new = as.character(factor(.data$.new, levels = c("1", "2", "3", "4"),
                                 labels = lv)),
      OFSTEDRATING_1 = factor(
        dplyr::if_else(.data$academic_year == new_year,
                       .data$.new, as.character(.data$OFSTEDRATING_1)),
        levels = lv, ordered = TRUE)) |>
    dplyr::select(-".new")
}


#' Build and save the five-year panel
#'
#' @param panel Existing four-year panel (default: data/panel_data.rds)
#' @param save Write data/panel_data_5yr.rds
#' @return The extended panel
extend_panel_2526 <- function(panel = NULL, save = TRUE) {

  if (is.null(panel)) panel <- readRDS(here::here("data", "panel_data.rds"))
  panel <- panel |> dplyr::mutate(URN = as.character(.data$URN))

  message("Existing panel: ", format(nrow(panel), big.mark = ","), " rows, ",
          dplyr::n_distinct(panel$academic_year), " years")

  new_year <- build_ees_year(panel) |> dplyr::mutate(URN = as.character(.data$URN))

  message("\nAligning and binding ...")
  p5 <- dplyr::bind_rows(panel, align_to_panel(new_year, panel))
  message("  ", format(nrow(p5), big.mark = ","), " rows")

  message("\nAttaching the autumn+spring absence series ...")
  p5 <- add_two_term_absence(p5)

  message("Deriving Gorard segregation and the Ofsted factor for the new year ...")
  p5 <- add_gorard_new_year(p5)
  p5 <- derive_ofsted_1(p5)

  cov <- p5 |>
    dplyr::group_by(.data$academic_year) |>
    dplyr::summarise(schools = dplyr::n(),
                     att8 = sum(!is.na(.data$ATT8SCR)),
                     absence_full = sum(!is.na(.data$PERCTOT)),
                     absence_2term = sum(!is.na(.data$PERCTOT_2term)),
                     .groups = "drop")
  message("")
  print(as.data.frame(cov))

  if (save) {
    out <- here::here("data", "panel_data_5yr.rds")
    saveRDS(p5, out)
    message("\nWritten: ", out)
    message("NOTE: data/panel_data.rds is unchanged - the published four-year ",
            "analysis still reproduces from it.")
  }

  invisible(p5)
}


if (sys.nframe() == 0) extend_panel_2526()
