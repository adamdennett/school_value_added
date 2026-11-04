# helpers.R - Shared utility functions for school attainment tool
# ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(janitor)
library(sf)
library(here)

# Standard NA codes used in DfE performance tables
NA_CODES <- c("", "NA", "SUPP", "NP", "NE", "SP", "SN", "LOWCOV", "NEW", "SUPPMAT", "x", "z", "c", "u")

# Edubase download URL (Oct 2024 snapshot)
EDUBASE_URL <- "https://www.dropbox.com/scl/fi/fhzafgt27v30lmmuo084y/edubasealldata20241003.csv?rlkey=uorw43s44hnw5k9js3z0ksuuq&raw=1"

# Ofsted official statistics CSV (as at 31 Aug 2024 - last with overall grades)
OFSTED_URL <- "https://assets.publishing.service.gov.uk/media/673f1b064a6dd5b06db95a5b/State_funded_schools_inspections_and_outcomes_as_at_31_August_2024.csv"

# Absence data from Explore Education Statistics (school-level, 2016/17-2024/25)
ABSENCE_API_URL <- "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/4eac8e90-3401-4108-b130-bfed3b65a4ee/csv"

# Workforce data URLs from Explore Education Statistics
WORKFORCE_URLS <- list(
  turnover = "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/1df9bfbf-b573-4f1d-ba87-06a36387f2e5/csv",
  pay = "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/760ad614-44ac-4e8a-8fdd-a8080c762770/csv",
  sickness = "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/aa13e99e-534c-4f70-9c43-c7f9544296b3/csv",
  workforce_size = "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/6cb459c2-5cc5-41be-87c8-01c0986514e8/csv",
  pupil_ratios = "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/12849f4a-1427-4dc2-9f7a-e069087824aa/csv"
)

# Academic years and their time_period codes for API filtering
YEAR_CONFIG <- tibble::tribble(
  ~academic_year, ~year_label, ~year_numeric, ~time_period, ~ks4_file,
  "2021-2022",    "2021-22",   0L,            202122L,      "england_ks4final.csv",
  "2022-2023",    "2022-23",   1L,            202223L,      "england_ks4final.csv",
  "2023-2024",    "2023-24",   2L,            202324L,      "england_ks4provisional.csv",
  "2024-2025",    "2024-25",   3L,            202425L,      "england_ks4provisional.csv"
)

# Variables to keep in the final panel dataset
PANEL_VARS <- c(
  # Identifiers
  "URN", "SCHNAME", "LEA", "LANAME", "TOWN", "POSTCODE",
  "gor_name", "easting", "northing",
  "academic_year", "year_label", "year_numeric",
  # Outcomes
  "ATT8SCR", "ATT8SCR_FSM6CLA1A", "ATT8SCR_NFSM6CLA1A",
  "ATT8SCRENG", "ATT8SCRMAT",
  "ATT8SCR_BOYS", "ATT8SCR_GIRLS",
  "P8MEA", "P8MEA_FSM6CLA1A", "P8MEA_NFSM6CLA1A",
  # Predictors
  "PTFSM6CLA1A", "PTNOTFSM6CLA1A",
  "PNUMEAL", "PNUMENGFL",
  "PTPRIORLO", "PTPRIORHI",
  "PERCTOT", "PPERSABS10",
  "TOTPUPS", "NORB", "NORG",
  "PNUMFSMEVER", "TFSM6CLA1A", "TPUP",
  # School characteristics
  "OFSTEDRATING",
  "SCHOOLTYPE", "MINORGROUP", "RELCHAR",
  "ADMPOL", "ADMPOL_PT",
  "gender_name",
  # Workforce (added later)
  "remained_in_the_same_school",
  "teachers_on_leadership_pay_range_percent",
  "average_number_of_days_taken",
  "teacher_fte_in_census_year",
  # Financial (added in step 03a)
  "fin_total_income", "fin_total_expenditure",
  "fin_in_year_balance", "fin_revenue_reserve",
  "fin_grant_funding", "fin_self_generated",
  "fin_staff_costs", "fin_teaching_staff",
  "fin_premises_costs", "fin_energy", "fin_pupil_premium",
  "fin_total_income_pp", "fin_total_expenditure_pp",
  "fin_staff_costs_pp", "fin_teaching_staff_pp", "fin_grant_funding_pp",
  "fin_staff_income_ratio", "fin_teaching_expenditure_ratio",
  "fin_balance_income_ratio", "fin_source",
  # Derived (computed later)
  "gorard_segregation",
  "has_imputed_predictors"
)


#' Download and cache a file
#' @param url URL to download
#' @param cache_path Local path to cache the file
#' @param force Re-download even if cached
#' @return The cache_path
download_cached <- function(url, cache_path, force = FALSE) {
  if (!file.exists(cache_path) || force) {
    message("Downloading: ", basename(cache_path), " ...")
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    download.file(url, cache_path, mode = "wb", quiet = FALSE)
    message("  Done.")
  } else {
    message("Using cached: ", basename(cache_path))
  }
  cache_path
}


#' Read a CSV with standard DfE NA handling and URN as character
#' @param path File path
#' @param ... Additional arguments passed to read_csv
#' @return A tibble
read_dfe_csv <- function(path, ...) {
  read_csv(path, na = NA_CODES, show_col_types = FALSE, ...) %>%
    mutate(URN = as.character(URN))
}


#' Safely log-transform a numeric vector, returning NA for non-positive values
#' @param x Numeric vector
#' @return log(x) with NA where x <= 0
safe_log <- function(x) {
  ifelse(x > 0, log(x), NA_real_)
}


#' Which variables a fitted model takes the log of
#'
#' Read off the model's own formula, response included, so the answer cannot
#' drift from the specification. Used to decide which variables a row has to
#' be positive on before it can be predicted: applying that test to every
#' numeric variable instead is wrong for anything centred or legitimately
#' zero (ks2_c is KS2ASS - 100, so it is negative for a below-average
#' intake), and silently drops those rows.
#'
#' @param model A fitted model, or a formula
#' @return Character vector of variable names appearing inside log()
model_log_vars <- function(model) {
  f <- if (inherits(model, "formula")) model else stats::formula(model)
  parts <- c(if (length(f) == 3L) deparse(f[[2]]) else character(0),
             attr(stats::terms(f), "term.labels"))
  hits <- unlist(regmatches(parts, gregexpr("log\\([^()]+\\)", parts)))
  unique(trimws(gsub("^log\\(|\\)$", "", hits)))
}


# ===========================================================================
# Variable labels - Human-readable descriptions from DfE meta files
# ===========================================================================
# Sourced from:
#   - KS4 meta (ks4_meta.xlsx)        : Column | Metafile heading | Metafile description
#   - Census meta (census_meta.csv)    : Field Reference | Field Name
#   - Absence meta (abs_meta.csv)      : Variable | Label
#   - School info meta (school_information_meta.csv) : Field Name | Description
#   - Workforce variables: labelled manually (from Explore Education Statistics)

VAR_LABELS <- c(
  # --- Outcomes (KS4 meta) ---
  "ATT8SCR"               = "Avg. Attainment 8 score per pupil",
  "ATT8SCR_FSM6CLA1A"     = "Avg. Attainment 8 score (disadvantaged)",
  "ATT8SCR_NFSM6CLA1A"    = "Avg. Attainment 8 score (non-disadvantaged)",
  "ATT8SCRENG"             = "Avg. Attainment 8 score (English element)",
  "ATT8SCRMAT"             = "Avg. Attainment 8 score (maths element)",
  "ATT8SCR_BOYS"           = "Avg. Attainment 8 score (boys)",
  "ATT8SCR_GIRLS"          = "Avg. Attainment 8 score (girls)",
  "P8MEA"                  = "Progress 8 measure (adjusted)",
  "P8MEA_FSM6CLA1A"        = "Progress 8 measure (disadvantaged)",
  "P8MEA_NFSM6CLA1A"       = "Progress 8 measure (non-disadvantaged)",

  # --- KS4 predictor/descriptor variables ---
  "TOTPUPS"                = "Total pupils on roll (all ages)",
  "TPUP"                   = "No. pupils at end of KS4",
  "NORB"                   = "No. boys on roll",
  "NORG"                   = "No. girls on roll",
  "PTFSM6CLA1A"            = "% KS4 pupils who are disadvantaged",
  "PTNOTFSM6CLA1A"         = "% KS4 pupils who are not disadvantaged",
  "TFSM6CLA1A"             = "No. disadvantaged pupils at end of KS4",
  "TNOTFSM6CLA1A"          = "No. non-disadvantaged pupils at end of KS4",
  "PTPRIORLO"              = "% KS4 pupils with low prior attainment (KS2)",
  "PTPRIORAV"              = "% KS4 pupils with middle prior attainment (KS2)",
  "PTPRIORHI"              = "% KS4 pupils with high prior attainment (KS2)",
  "ADMPOL"                 = "Admissions policy (self-declared)",
  "ADMPOL_PT"              = "Admissions policy (new definition from 2019)",
  "ADMPOL_PTOTHER NON SEL" = "Admissions: Other non-selective (ref: non-sel. in highly sel. area)",
  "ADMPOL_PTSEL"           = "Admissions: Selective (ref: non-sel. in highly sel. area)",
  "RECTYPE"                = "Record type (1=mainstream, 2=special, 4=LA, 5=national)",

  # --- Census meta ---
  "NOR"                    = "Total no. pupils on roll",
  "PNUMEAL"                = "% pupils with English not first language",
  "PNUMENGFL"              = "% pupils with English as first language",
  "NUMEAL"                 = "No. pupils with English not first language",
  "NUMENGFL"               = "No. pupils with English as first language",
  "NUMFSM"                 = "No. pupils eligible for free school meals",
  "NUMFSMEVER"             = "No. pupils eligible for FSM in past 6 years",
  "PNUMFSMEVER"            = "% pupils eligible for FSM in past 6 years",
  "TSENELSE"               = "No. pupils with EHC plan",
  "PSENELSE"               = "% pupils with EHC plan",
  "TSENELK"                = "No. pupils with SEN support",
  "PSENELK"                = "% pupils with SEN support",

  # --- Absence meta ---
  "PERCTOT"                = "% overall absence",
  "PPERSABS10"             = "% persistent absentees (10%+ sessions missed)",

  # --- School information meta ---
  "URN"                    = "School unique reference number",
  "LANAME"                 = "Local authority name",
  "SCHNAME"                = "School name",
  "TOWN"                   = "School town",
  "POSTCODE"               = "School postcode",
  "MINORGROUP"             = "Type of school/college",
  "SCHOOLTYPE"             = "School type (e.g. Voluntary Aided)",
  "OFSTEDRATING"           = "Ofsted rating",
  "OFSTEDRATING_1"         = "Ofsted rating (harmonised)",
  "RELCHAR"                = "Religious character",
  "GENDER"                 = "Mixed or single sex",

  # --- Workforce variables (Explore Education Statistics) ---
  "remained_in_the_same_school"              = "Teachers remaining in same school (FTE)",
  "entrants_to_the_school"                   = "Teachers entering the school (FTE)",
  "leavers_from_the_school"                  = "Teachers leaving the school (FTE)",
  "teacher_fte_in_census_year"               = "Teacher FTE in census year",
  "teachers_on_leadership_pay_range_percent" = "% teachers on leadership pay range",
  "teachers_on_main_pay_range_percent"       = "% teachers on main pay range",
  "teachers_on_upper_pay_range_percent"      = "% teachers on upper pay range",
  "average_number_of_days_taken"             = "Avg. teacher sickness days",
  "total_number_of_days_lost"                = "Total teacher sickness days lost",
  "percentage_taking_absence"                = "% teachers taking sickness absence",
  "fte_workforce"                            = "Total workforce (FTE)",
  "fte_all_teachers"                         = "All teachers (FTE)",
  "fte_classroom_teachers"                   = "Classroom teachers (FTE)",
  "fte_leadership_teachers"                  = "Leadership teachers (FTE)",
  "fte_teaching_assistants"                  = "Teaching assistants (FTE)",
  "pupil_to_qual_teacher_ratio"              = "Pupil-to-qualified-teacher ratio",
  "pupil_to_adult_ratio"                     = "Pupil-to-adult ratio",

  # --- Financial variables (CFR/AAR) ---
  "fin_total_income"          = "Total income (£)",
  "fin_total_expenditure"     = "Total expenditure (£)",
  "fin_in_year_balance"       = "In-year balance (£)",
  "fin_revenue_reserve"       = "Revenue reserve (£)",
  "fin_grant_funding"         = "Grant funding (£)",
  "fin_self_generated"        = "Self-generated income (£)",
  "fin_staff_costs"           = "Total staff costs (£)",
  "fin_teaching_staff"        = "Teaching staff costs (£)",
  "fin_premises_costs"        = "Premises costs (£)",
  "fin_energy"                = "Energy costs (£)",
  "fin_pupil_premium"         = "Pupil premium income (£)",
  "fin_total_income_pp"       = "Total income per pupil (£)",
  "fin_total_expenditure_pp"  = "Total expenditure per pupil (£)",
  "fin_staff_costs_pp"        = "Staff costs per pupil (£)",
  "fin_teaching_staff_pp"     = "Teaching staff costs per pupil (£)",
  "fin_grant_funding_pp"      = "Grant funding per pupil (£)",
  "fin_staff_income_ratio"    = "Staff costs as % of income",
  "fin_teaching_expenditure_ratio" = "Teaching staff as % of expenditure",
  "fin_balance_income_ratio"  = "In-year balance as % of income",
  "fin_source"                = "Financial data source (CFR/AAR)",

  # --- Derived variables ---
  "gorard_segregation"        = "Gorard segregation index",
  "retention_rate"            = "Teacher retention rate",
  "has_imputed_predictors"    = "2024-25 predictors are estimates (carry-forward)",
  "gap_driver_index"          = "Gap driver index: z(FSM ATT8) - z(non-FSM ATT8); neg = low FSM driven, pos = high non-FSM driven",

  # --- Geographic / identifiers ---
  "gor_name"               = "Government Office Region",
  "easting"                = "Easting (OS grid)",
  "northing"               = "Northing (OS grid)",
  "LEA"                    = "Local authority code",

  # --- Year / time ---
  "academic_year"          = "Academic year",
  "year_label"             = "Academic year (short)",
  "year_numeric"           = "Year index (0 = 2021-22)",
  "time_period"            = "DfE time period code"
)


# Null-or-NA coalescing operator: returns y if x is NULL, length-0, or all-NA
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}


#' Get a human-readable label for a variable
#'
#' Returns the label from VAR_LABELS if available, otherwise returns the
#' original variable name. Optionally wraps long labels.
#'
#' @param var_name Character: the raw variable name (e.g. "PTFSM6CLA1A")
#' @param wrap_width Integer or NULL: if set, wraps the label to this width
#' @return Character: the human-readable label
var_label <- function(var_name, wrap_width = NULL) {
  label <- ifelse(
    var_name %in% names(VAR_LABELS),
    VAR_LABELS[var_name],
    var_name
  )
  if (!is.null(wrap_width)) {
    label <- str_wrap(label, width = wrap_width)
  }
  label
}


#' Get labels for multiple variables at once
#'
#' @param var_names Character vector of variable names
#' @param wrap_width Integer or NULL: if set, wraps labels
#' @return Named character vector: names are original var_names, values are labels
var_labels <- function(var_names, wrap_width = NULL) {
  labels <- vapply(var_names, var_label, character(1), wrap_width = wrap_width)
  names(labels) <- var_names
  labels
}
