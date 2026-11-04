# 04_derived.R - Compute derived variables: Gorard index, imputation, ks2_c, log transforms
# -------------------------------------------------------------------------------
# This script:
#   1. Computes the Gorard segregation index per LA per year
#   2. Creates log-transformed versions of skewed variables
#   3. Creates the school lookup table (URN -> name, LA, region, coords)
#   4. Saves the final panel_data.rds and school_lookup.rds
#
# From the original project (R/04_compute_derived.R). Depends on
# pipeline/03_build_panel.R (data/panel_raw.rds).
#   Rscript pipeline/04_derived.R    # writes data/panel_data.rds
# -------------------------------------------------------------------------------

source(here::here("pipeline", "helpers.R"))


#' Compute Gorard Segregation Index for each LA in each year
#'
#' Formula: G_s = 0.5 * sum_i |F_i/F_total - T_i/T_total|
#' where F_i = FSM-eligible pupils in school i
#'       F_total = total FSM-eligible pupils in the LA
#'       T_i = total pupils in school i
#'       T_total = total pupils in the LA
#'
#' Source: Gorard & Taylor (2002), replicating the approach in week8_lecture.qmd
#'
#' @param panel Dataframe with TFSM6CLA1A (total FSM pupils), TPUP (total pupils),
#'              LANAME, and academic_year columns
#' @return Dataframe with LANAME, academic_year, gorard_segregation columns
compute_gorard_index <- function(panel) {

  message("Computing Gorard segregation index per LA per year ...")

  # Need: school-level FSM counts and total pupils
  # TFSM6CLA1A = total number of disadvantaged pupils in the school

  # TPUP = total pupils in the school

  gorard <- panel %>%
    filter(!is.na(TFSM6CLA1A), !is.na(TPUP), TPUP > 0) %>%
    group_by(LANAME, academic_year) %>%
    mutate(
      F_total = sum(TFSM6CLA1A, na.rm = TRUE),
      T_total = sum(TPUP, na.rm = TRUE)
    ) %>%
    filter(F_total > 0, T_total > 0) %>%
    summarise(
      gorard_segregation = 0.5 * sum(
        abs(TFSM6CLA1A / F_total - TPUP / T_total),
        na.rm = TRUE
      ),
      n_schools = n(),
      .groups = "drop"
    )

  message("  Gorard index computed for ", n_distinct(gorard$LANAME), " LAs across ",
          n_distinct(gorard$academic_year), " years")
  message("  Index range: ", round(min(gorard$gorard_segregation), 3), " - ",
          round(max(gorard$gorard_segregation), 3))

  gorard %>% select(LANAME, academic_year, gorard_segregation)
}


#' Impute missing predictor variables for 2024-25
#'
#' Four variables are unavailable for 2024-25:
#'   - KS2ASS    (mean KS2 scaled score — cancelled due to COVID)
#'   - PTPRIORLO (KS2 prior attainment — cancelled due to COVID)
#'   - remained_in_the_same_school (workforce turnover)
#'   - teachers_on_leadership_pay_range_percent (workforce pay)
#'   - average_number_of_days_taken (workforce sickness)
#'
#' Method: carry-forward from 2023-24. If a school has no 2023-24 value,
#' fall back to the mean of all available years for that school.
#'
#' A flag column `has_imputed_predictors` is set to TRUE for any 2024-25
#' row that received at least one imputed value.
#'
#' @param panel Dataframe with URN, year_label, and the 4 target variables
#' @return Panel with imputed values and flag column
impute_missing_predictors <- function(panel) {

  message("Imputing missing predictors for 2024-25 ...")

  impute_vars <- c(
    "KS2ASS",
    "PTPRIORLO",
    "remained_in_the_same_school",
    "teachers_on_leadership_pay_range_percent",
    "average_number_of_days_taken"
  )

  # Identify 2024-25 rows
  is_2425 <- panel$year_label == "2024-25"
  n_2425 <- sum(is_2425)
  message("  2024-25 rows: ", n_2425)

  # Initialise flag: FALSE for all rows

  panel$has_imputed_predictors <- FALSE

  # Build lookup tables from historical data (non-2024-25 rows)
  historical <- panel %>% filter(year_label != "2024-25")

  for (v in impute_vars) {
    if (!v %in% names(panel)) {
      message("  WARNING: ", v, " not found in panel — skipping")
      next
    }

    n_missing_before <- sum(is.na(panel[[v]][is_2425]))

    # 1. Carry-forward from 2023-24 (preferred)
    carry_forward <- historical %>%
      filter(year_label == "2023-24", !is.na(!!sym(v))) %>%
      select(URN, !!v) %>%
      rename(imputed_value = !!v)

    # 2. Fallback: school-level mean across all available years
    school_mean <- historical %>%
      filter(!is.na(!!sym(v))) %>%
      group_by(URN) %>%
      summarise(mean_value = mean(!!sym(v), na.rm = TRUE), .groups = "drop")

    # Apply imputation only where value is NA in 2024-25
    for (i in which(is_2425 & is.na(panel[[v]]))) {
      urn_i <- panel$URN[i]

      # Try carry-forward first
      cf <- carry_forward$imputed_value[carry_forward$URN == urn_i]
      if (length(cf) == 1 && !is.na(cf)) {
        panel[[v]][i] <- cf
        panel$has_imputed_predictors[i] <- TRUE
        next
      }

      # Fallback to school mean
      sm <- school_mean$mean_value[school_mean$URN == urn_i]
      if (length(sm) == 1 && !is.na(sm)) {
        panel[[v]][i] <- sm
        panel$has_imputed_predictors[i] <- TRUE
      }
    }

    n_missing_after <- sum(is.na(panel[[v]][is_2425]))
    n_imputed <- n_missing_before - n_missing_after
    message("  ", v, ": ", n_imputed, " values imputed (",
            n_missing_after, " still missing)")
  }

  n_flagged <- sum(panel$has_imputed_predictors)
  message("  Rows flagged as has_imputed_predictors: ", n_flagged, " / ", n_2425,
          " (2024-25 rows)")

  panel
}


#' Add log transforms for model variables
#'
#' Only log-transforms variables with |skewness| > 1 (substantially skewed).
#' Guards against zero/negative values by setting to NA before log transform.
#' Uses natural log (base e) consistent with the model specification.
#'
#' Outcomes (ATT8SCR variants) are always log-transformed as a modelling choice
#' (log-linear specification), independent of skewness.
add_log_transforms <- function(panel) {

  message("Adding log transforms ...")

  # Outcome variables: always log-transformed (modelling choice, not skewness)
  outcome_log_vars <- c("ATT8SCR", "ATT8SCR_FSM6CLA1A", "ATT8SCR_NFSM6CLA1A")

  # Predictor candidates: only log if |skewness| > 1
  predictor_candidates <- c(
    "PTFSM6CLA1A", "PERCTOT", "PNUMEAL",
    "remained_in_the_same_school", "average_number_of_days_taken"
  )

  skew_threshold <- 1.0
  predictor_log_vars <- character(0)

  message("  Checking predictor skewness (threshold = ", skew_threshold, "):")
  for (v in predictor_candidates) {
    if (!v %in% names(panel)) {
      message("    ", v, ": NOT FOUND — skipping")
      next
    }
    x <- panel[[v]][!is.na(panel[[v]])]
    if (length(x) < 3) next
    sk <- (sum((x - mean(x))^3) / length(x)) / (sd(x)^3)
    message(sprintf("    %-45s skew = %6.3f  %s",
                    v, sk, ifelse(abs(sk) > skew_threshold, "-> LOG", "-> raw")))
    if (abs(sk) > skew_threshold) {
      predictor_log_vars <- c(predictor_log_vars, v)
    }
  }

  log_vars <- c(outcome_log_vars, predictor_log_vars)

  for (v in log_vars) {
    if (v %in% names(panel)) {
      new_name <- paste0("log_", v)
      panel[[new_name]] <- safe_log(panel[[v]])
      n_valid <- sum(!is.na(panel[[new_name]]))
      n_total <- nrow(panel)
      message("  log_", v, ": ", n_valid, "/", n_total, " valid")
    }
  }

  panel
}


#' Create a lookup table for schools
create_school_lookup <- function(panel) {

  message("Creating school lookup table ...")

  lookup <- panel %>%
    group_by(URN) %>%
    summarise(
      SCHNAME = first(na.omit(SCHNAME)),
      LANAME = first(na.omit(LANAME)),
      LEA = first(na.omit(LEA)),
      gor_name = first(na.omit(gor_name)),
      TOWN = first(na.omit(TOWN)),
      POSTCODE = first(na.omit(POSTCODE)),
      easting = first(na.omit(easting)),
      northing = first(na.omit(northing)),
      OFSTEDRATING = first(na.omit(OFSTEDRATING)),
      OFSTEDRATING_1 = first(na.omit(OFSTEDRATING_1)),
      MINORGROUP = first(na.omit(MINORGROUP)),
      n_years_present = n(),
      years_present = paste(sort(unique(year_label)), collapse = ", "),
      .groups = "drop"
    )

  message("  Lookup: ", nrow(lookup), " unique schools")
  message("  With coordinates: ", sum(!is.na(lookup$easting)))
  message("  With region: ", sum(!is.na(lookup$gor_name)))

  lookup
}


#' Create a lookup table for LAs
create_la_lookup <- function(panel) {

  message("Creating LA lookup table ...")

  la_lookup <- panel %>%
    group_by(LANAME) %>%
    summarise(
      LEA = first(na.omit(LEA)),
      gor_name = first(na.omit(gor_name)),
      n_schools = n_distinct(URN),
      .groups = "drop"
    ) %>%
    filter(!is.na(LANAME))

  message("  LA lookup: ", nrow(la_lookup), " local authorities")
  la_lookup
}


#' Final cleaning and validation of the panel
clean_panel <- function(panel) {

  message("\nFinal panel cleaning ...")

  # Ensure OFSTEDRATING is a factor
  if ("OFSTEDRATING" %in% names(panel)) {
    panel <- panel %>%
      mutate(OFSTEDRATING = factor(OFSTEDRATING))
  }

  # Ensure OFSTEDRATING_1 retains its ordered factor levels
  if ("OFSTEDRATING_1" %in% names(panel)) {
    panel <- panel %>%
      mutate(OFSTEDRATING_1 = factor(OFSTEDRATING_1,
                                     levels = c("Outstanding", "Good",
                                                "Requires Improvement", "Inadequate"),
                                     ordered = TRUE))
  }

  # Ensure gor_name is a factor
  if ("gor_name" %in% names(panel)) {
    panel <- panel %>%
      mutate(gor_name = factor(gor_name))
  }

  # Ensure LANAME is a factor
  if ("LANAME" %in% names(panel)) {
    panel <- panel %>%
      mutate(LANAME = factor(LANAME))
  }

  # Filter out rows with missing outcome (ATT8SCR) - can't model these
  n_before <- nrow(panel)
  panel <- panel %>% filter(!is.na(ATT8SCR), ATT8SCR > 0)
  n_after <- nrow(panel)
  message("  Dropped ", n_before - n_after, " rows with missing/zero ATT8SCR")

  # Filter out rows with missing key predictors for log transform
  panel <- panel %>%
    filter(PTFSM6CLA1A > 0 | is.na(PTFSM6CLA1A))

  message("  Final panel: ", nrow(panel), " school-year observations")

  panel
}


# ---- Main execution ----

if (sys.nframe() == 0) {

  # The panel from 03_build_panel.R. The original project joined workforce
  # and finance data here first (its R/03, R/03a); v2.0 uses neither, and
  # both were left joins, so the rows are the same without them.
  panel_path <- here::here("data", "panel_raw.rds")
  if (!file.exists(panel_path)) stop("panel_raw.rds not found. Run pipeline/03_build_panel.R first.")

  panel <- readRDS(panel_path)
  message("Loaded panel: ", nrow(panel), " rows, ", ncol(panel), " cols\n")

  # 1. Compute and join Gorard segregation index
  gorard <- compute_gorard_index(panel)
  panel <- panel %>%
    left_join(gorard, by = c("LANAME", "academic_year"))

  # 2. Impute missing predictors for 2024-25
  panel <- impute_missing_predictors(panel)

  # 2b. Centre mean KS2 scaled score at the national standard (100) so the
  #     model intercept is interpretable. ks2_c is the prior-attainment
  #     control used in the models (it fits materially better than the
  #     low-prior-attainment share; see revisions/r23b_output.txt).
  if ("KS2ASS" %in% names(panel)) {
    panel$ks2_c <- panel$KS2ASS - 100
    message("  Added ks2_c (KS2ASS centred at 100): ",
            sum(!is.na(panel$ks2_c)), " non-missing")
  }

  # 3. Add log transforms
  panel <- add_log_transforms(panel)

  # 4. Clean panel
  panel <- clean_panel(panel)

  # 5. Create lookups
  school_lookup <- create_school_lookup(panel)
  la_lookup <- create_la_lookup(panel)

  # 6. Save outputs
  saveRDS(panel, here::here("data", "panel_data.rds"))
  saveRDS(school_lookup, here::here("data", "school_lookup.rds"))
  saveRDS(la_lookup, here::here("data", "la_lookup.rds"))

  message("\n=== Final Panel Summary ===")
  message("panel_data.rds: ", nrow(panel), " rows, ", ncol(panel), " cols")
  message("school_lookup.rds: ", nrow(school_lookup), " schools")
  message("la_lookup.rds: ", nrow(la_lookup), " local authorities")

  message("\nPer-year breakdown:")
  panel %>%
    group_by(year_label) %>%
    summarise(
      n = n(),
      mean_ATT8 = round(mean(ATT8SCR, na.rm = TRUE), 1),
      pct_absence = round(mean(!is.na(PERCTOT)) * 100, 1),
      pct_ofsted = round(mean(!is.na(OFSTEDRATING)) * 100, 1),
      pct_ofsted_1 = round(mean(!is.na(OFSTEDRATING_1)) * 100, 1),
      pct_gorard = round(mean(!is.na(gorard_segregation)) * 100, 1),
      .groups = "drop"
    ) %>%
    print()
}
