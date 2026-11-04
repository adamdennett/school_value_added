# 02_extract.R - Load each year's DfE performance-table CSVs into one list
# -------------------------------------------------------------------------------
# From the original project's R/01_extract_data.R. This script:
#   1. Reads and joins KS4, absence, census and school information per year,
#      from data/extracted/{year}/ as 01_download_cscp.R lays them out
#   2. Filters to open secondary non-special schools
#   3. Returns a list of per-year dataframes ready for panel harmonisation
#
# The original also unzipped hand-downloaded performance-table ZIPs and
# copied 2022-23 from a sibling repository; here every year comes from the
# CSCP download service instead (01_download_cscp.R), in the same layout.
#
#   Rscript pipeline/02_extract.R    # writes data/year_data_list.rds
# -------------------------------------------------------------------------------

source(here::here("pipeline", "helpers.R"))

extracted_dir <- here::here("data", "extracted")


# ---- Step 2: Load a single year's data ----

load_year_data <- function(year) {

  year_dir <- file.path(extracted_dir, year)
  config <- YEAR_CONFIG %>% filter(academic_year == year)

  if (nrow(config) == 0) stop("Unknown year: ", year)

  ks4_file <- config$ks4_file
  ks4_path <- file.path(year_dir, ks4_file)

  if (!file.exists(ks4_path)) {
    # Fallback: try provisional if final not available
    ks4_path <- file.path(year_dir, "england_ks4provisional.csv")
    if (!file.exists(ks4_path)) {
      stop("No KS4 file found for ", year, " in ", year_dir)
    }
    message("  Using provisional KS4 for ", year)
  }

  message("Loading ", year, " ...")
  message("  KS4: ", basename(ks4_path))

  # --- Read KS4 ---
  ks4 <- read_dfe_csv(ks4_path) %>%
    # Coerce numeric columns: everything from TOTPUPS onwards
    {
      totpups_idx <- which(names(.) == "TOTPUPS")
      if (length(totpups_idx) > 0) {
        mutate(., across(
          all_of(names(.)[totpups_idx:ncol(.)]),
          ~ suppressWarnings(readr::parse_number(as.character(.)))
        ))
      } else {
        .
      }
    }

  message("  KS4: ", nrow(ks4), " rows, ", ncol(ks4), " cols")

  # --- Read Absence (if available in performance tables) ---
  abs_path <- file.path(year_dir, "england_abs.csv")
  if (file.exists(abs_path)) {
    abs_data <- read_dfe_csv(abs_path) %>%
      mutate(across(c(PERCTOT, PPERSABS10), ~ suppressWarnings(as.numeric(as.character(.)))))
    message("  Absence: ", nrow(abs_data), " rows")
  } else {
    message("  Absence: NOT AVAILABLE in performance tables for ", year)
    abs_data <- NULL
  }

  # --- Read Census ---
  census_path <- file.path(year_dir, "england_census.csv")
  if (file.exists(census_path)) {
    census <- read_dfe_csv(census_path) %>%
      mutate(across(5:ncol(.), ~ suppressWarnings(readr::parse_number(as.character(.)))))
    message("  Census: ", nrow(census), " rows")
  } else {
    warning("  Census file not found for ", year)
    census <- NULL
  }

  # --- Read School Information ---
  schinfo_path <- file.path(year_dir, "england_school_information.csv")
  if (file.exists(schinfo_path)) {
    # Check if OFSTEDLASTINSP column exists before trying to parse dates
    schinfo_headers <- names(read_csv(schinfo_path, n_max = 0, show_col_types = FALSE))

    if ("OFSTEDLASTINSP" %in% schinfo_headers) {
      schinfo <- read_dfe_csv(schinfo_path) %>%
        mutate(OFSTEDLASTINSP = suppressWarnings(
          lubridate::dmy(as.character(OFSTEDLASTINSP))
        ))
    } else {
      schinfo <- read_dfe_csv(schinfo_path)
    }
    message("  School info: ", nrow(schinfo), " rows, ",
            ifelse("OFSTEDRATING" %in% names(schinfo), "HAS", "NO"), " OFSTEDRATING")
  } else {
    warning("  School information file not found for ", year)
    schinfo <- NULL
  }

  # --- Join all by URN ---
  joined <- ks4

  if (!is.null(abs_data)) {
    joined <- joined %>% left_join(abs_data, by = "URN")
  }

  if (!is.null(census)) {
    joined <- joined %>% left_join(census, by = "URN")
  }

  if (!is.null(schinfo)) {
    joined <- joined %>% left_join(schinfo, by = "URN")
  }

  # --- Filter to open secondary non-special schools ---
  # RECTYPE == 1 means mainstream school in KS4 data
  if ("RECTYPE" %in% names(joined)) {
    joined <- joined %>% filter(RECTYPE == 1)
    message("  After RECTYPE==1 filter: ", nrow(joined), " rows")
  }

  # --- Add year identifiers ---
  joined <- joined %>%
    mutate(
      academic_year = config$academic_year,
      year_label = config$year_label,
      year_numeric = config$year_numeric,
      time_period = config$time_period
    )

  # --- Standardise key column names ---
  # Handle .x/.y suffixes from joins and inconsistencies
  joined <- standardise_column_names(joined)

  message("  Final: ", nrow(joined), " rows, ", ncol(joined), " cols")
  message("")

  joined
}


#' Standardise column names after joins
#' Resolves .x/.y suffixes and inconsistent naming across years
standardise_column_names <- function(df) {

  # Prefer .x version (from KS4) over .y (from school_info) for shared columns
  # Common duplicates: SCHOOLTYPE, LEA, ESTAB, LA
  resolve_xy <- function(df, base_name) {
    x_name <- paste0(base_name, ".x")
    y_name <- paste0(base_name, ".y")

    if (x_name %in% names(df) && y_name %in% names(df)) {
      # Coerce both sides to character before coalescing to avoid type mismatches
      # (e.g. ESTAB read as numeric from one CSV and character from another)
      df <- df %>%
        mutate(
          across(all_of(c(x_name, y_name)), ~ as.character(.)),
          !!base_name := coalesce(!!sym(x_name), !!sym(y_name))
        ) %>%
        select(-all_of(c(x_name, y_name)))
    }
    df
  }

  # Resolve common duplicated columns
  for (col in c("SCHOOLTYPE", "LEA", "ESTAB", "LA", "SCHNAME", "TOWN")) {
    df <- resolve_xy(df, col)
  }

  df
}


# ---- Step 3: Load all years ----

load_all_years <- function() {
  years <- YEAR_CONFIG$academic_year

  year_data_list <- map(years, ~ {
    tryCatch(
      load_year_data(.x),
      error = function(e) {
        warning("Failed to load ", .x, ": ", e$message)
        NULL
      }
    )
  })

  names(year_data_list) <- years

  # Remove any NULL entries
  year_data_list <- compact(year_data_list)

  message("\nSuccessfully loaded ", length(year_data_list), " years: ",
          paste(names(year_data_list), collapse = ", "))

  year_data_list
}


# ---- Main execution ----

if (sys.nframe() == 0) {
  # Running as script (not sourced)
  message("=== Loading all years ===")
  year_data <- load_all_years()

  message("\n=== Summary ===")
  for (yr in names(year_data)) {
    df <- year_data[[yr]]
    message(yr, ": ", nrow(df), " schools, ", ncol(df), " variables")
    message("  ATT8SCR range: ",
            round(min(df$ATT8SCR, na.rm = TRUE), 1), " - ",
            round(max(df$ATT8SCR, na.rm = TRUE), 1))
    message("  Has PERCTOT: ", "PERCTOT" %in% names(df) && sum(!is.na(df$PERCTOT)) > 0)
    message("  Has OFSTEDRATING: ", "OFSTEDRATING" %in% names(df) && sum(!is.na(df$OFSTEDRATING)) > 0)
  }

  # Save intermediate result
  saveRDS(year_data, here::here("data", "year_data_list.rds"))
  message("\nSaved year_data_list.rds")
}
