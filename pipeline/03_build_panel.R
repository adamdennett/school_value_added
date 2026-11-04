# 02_build_panel.R - Harmonise and stack all years into a panel dataset
# ---------------------------------------------------------------------
# This script:
#   1. Downloads Edubase data (region, coordinates)
#   2. Downloads Ofsted ratings (from official statistics CSV)
#   3. Downloads school-level absence data (from Explore Education Statistics API)
#   4. Joins external data to each year
#   5. Harmonises column names and stacks into a single panel dataframe
#
# Depends on: R/01_extract_data.R (produces year_data_list.rds)
# Run from project root: source("R/02_build_panel.R")
# ---------------------------------------------------------------------

source(here::here("pipeline", "helpers.R"))

cache_dir <- here::here("data", "cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)


# ---- 1. Download and prepare Edubase data ----

load_edubase <- function(force = FALSE) {
  cache_path <- file.path(cache_dir, "edubase.rds")

  if (file.exists(cache_path) && !force) {
    message("Loading cached Edubase data ...")
    return(readRDS(cache_path))
  }

  message("Downloading Edubase data ...")
  edubase <- read_csv(
    EDUBASE_URL,
    show_col_types = FALSE,
    locale = locale(encoding = "latin1")
  ) %>%
    janitor::clean_names() %>%
    mutate(urn = as.character(urn)) %>%
    # Keep only columns we need
    select(
      urn,
      establishment_name,
      phase_of_education_name,
      establishment_status_name,
      gor_name = gor_name,
      easting, northing,
      postcode
    ) %>%
    # Filter to secondary schools that are open
    filter(
      phase_of_education_name == "Secondary",
      establishment_status_name == "Open"
    )

  message("  Edubase: ", nrow(edubase), " open secondary schools with locations")
  saveRDS(edubase, cache_path)
  edubase
}


# ---- 2. Download and prepare Ofsted ratings ----

load_ofsted_ratings <- function(force = FALSE) {
  cache_path <- file.path(cache_dir, "ofsted_ratings.rds")

  if (file.exists(cache_path) && !force) {
    message("Loading cached Ofsted ratings ...")
    return(readRDS(cache_path))
  }

  message("Downloading Ofsted official statistics (as at 31 Aug 2024) ...")
  csv_path <- file.path(cache_dir, "ofsted_raw.csv")
  download_cached(OFSTED_URL, csv_path, force = force)

  ofsted_raw <- read_csv(csv_path, show_col_types = FALSE)

  message("  Raw Ofsted data: ", nrow(ofsted_raw), " rows, ", ncol(ofsted_raw), " cols")
  message("  Columns: ", paste(head(names(ofsted_raw), 20), collapse = ", "), " ...")

  # The overall effectiveness grade should be in the graded inspection columns

  # Find the relevant columns - they may vary in exact name
  ofsted_cols <- names(ofsted_raw)

  # Look for "Overall effectiveness" column
  oe_col <- ofsted_cols[grepl("overall.*effectiveness|Overall.*effectiveness", ofsted_cols, ignore.case = TRUE)]
  message("  Overall effectiveness columns found: ", paste(oe_col, collapse = ", "))

  # Also check for individual judgement columns as fallback
  quality_col <- ofsted_cols[grepl("quality.*education", ofsted_cols, ignore.case = TRUE)]
  message("  Quality of education columns: ", paste(quality_col, collapse = ", "))

  # Extract URN and overall rating
  ofsted <- ofsted_raw %>%
    mutate(URN = as.character(URN))

  # Try to extract overall effectiveness rating
  if (length(oe_col) > 0) {
    # Use the overall effectiveness column directly
    ofsted <- ofsted %>%
      rename(ofsted_overall = !!oe_col[1]) %>%
      mutate(OFSTEDRATING_external = suppressWarnings(as.numeric(ofsted_overall))) %>%
      select(URN, OFSTEDRATING_external) %>%
      filter(!is.na(OFSTEDRATING_external))

    message("  Ofsted ratings extracted: ", nrow(ofsted), " schools with numeric ratings")
  } else if (length(quality_col) > 0) {
    # Fallback: use Quality of Education as a proxy for overall effectiveness
    message("  WARNING: No 'Overall effectiveness' column found.")
    message("  Using 'Quality of education' as proxy for OFSTEDRATING")
    ofsted <- ofsted %>%
      rename(quality_ed = !!quality_col[1]) %>%
      mutate(OFSTEDRATING_external = suppressWarnings(as.numeric(quality_ed))) %>%
      select(URN, OFSTEDRATING_external) %>%
      filter(!is.na(OFSTEDRATING_external))

    message("  Ofsted ratings (proxy): ", nrow(ofsted), " schools")
  } else {
    warning("Could not find any Ofsted rating columns in the downloaded data!")
    message("  Available columns: ", paste(head(ofsted_cols, 30), collapse = ", "))
    ofsted <- tibble(URN = character(), OFSTEDRATING_external = numeric())
  }

  saveRDS(ofsted, cache_path)
  ofsted
}


# ---- 3. Download and prepare absence data from API ----

load_absence_api <- function(force = FALSE) {
  cache_path <- file.path(cache_dir, "absence_api.rds")

  if (file.exists(cache_path) && !force) {
    message("Loading cached absence API data ...")
    return(readRDS(cache_path))
  }

  message("Downloading school-level absence data from Explore Education Statistics ...")
  message("  (This is ~68MB, may take a few minutes)")
  csv_path <- file.path(cache_dir, "absence_api_raw.csv")
  download_cached(ABSENCE_API_URL, csv_path, force = force)

  absence_raw <- read_csv(csv_path, show_col_types = FALSE)
  message("  Raw absence data: ", nrow(absence_raw), " rows, ", ncol(absence_raw), " cols")

  # Explore column names to understand structure
  message("  Columns: ", paste(head(names(absence_raw), 20), collapse = ", "))
  message("  Time periods: ", paste(unique(absence_raw$time_period), collapse = ", "))
  message("  Geographic levels: ", paste(unique(absence_raw$geographic_level), collapse = ", "))

  # Filter to school-level data and relevant time periods
  our_time_periods <- YEAR_CONFIG$time_period

  absence <- absence_raw %>%
    filter(
      geographic_level == "School",
      time_period %in% our_time_periods
    )

  message("  After filtering: ", nrow(absence), " school-year rows")

  # Identify the URN column (might be school_urn, urn, or URN)
  urn_col <- intersect(names(absence), c("school_urn", "urn", "URN"))
  if (length(urn_col) == 0) {
    # Search more broadly
    urn_col <- names(absence)[grepl("urn", names(absence), ignore.case = TRUE)]
  }
  message("  URN column: ", paste(urn_col, collapse = ", "))

  # Identify absence rate columns
  # The API dataset may use different names than the performance tables
  # Performance tables use: PERCTOT, PPERSABS10
  # API might use: sess_overall_percent, sess_overall_percent_pa_10_exact, etc.
  message("  Absence-related columns: ",
          paste(names(absence)[grepl("percent|rate|overall|persistent", names(absence), ignore.case = TRUE)],
                collapse = ", "))

  # Standardise: rename to match performance table convention
  if (length(urn_col) > 0) {
    absence <- absence %>%
      rename(URN = !!urn_col[1]) %>%
      mutate(URN = as.character(URN))
  }

  # Select and rename key variables - adapt column names based on what's available
  # Common API column names:
  #   sess_overall_percent -> PERCTOT (overall absence rate)
  #   enrolments_pa_10_exact_percent -> PPERSABS10 (persistent absence %)
  abs_cols <- names(absence)

  # Map API columns to standard names
  overall_col <- abs_cols[grepl("^sess_overall_percent$|^overall_absence_rate", abs_cols)]
  persist_col <- abs_cols[grepl("enrolments_pa_10_exact_percent|persistent.*percent", abs_cols)]

  if (length(overall_col) > 0) {
    message("  Mapping '", overall_col[1], "' -> PERCTOT_api")
    absence <- absence %>%
      mutate(PERCTOT_api = suppressWarnings(as.numeric(!!sym(overall_col[1]))))
  } else {
    message("  WARNING: Could not find overall absence rate column. Will need manual mapping.")
    absence <- absence %>% mutate(PERCTOT_api = NA_real_)
  }

  if (length(persist_col) > 0) {
    message("  Mapping '", persist_col[1], "' -> PPERSABS10_api")
    absence <- absence %>%
      mutate(PPERSABS10_api = suppressWarnings(as.numeric(!!sym(persist_col[1]))))
  } else {
    message("  WARNING: Could not find persistent absence rate column. Will need manual mapping.")
    absence <- absence %>% mutate(PPERSABS10_api = NA_real_)
  }

  absence_slim <- absence %>%
    select(URN, time_period, PERCTOT_api, PPERSABS10_api) %>%
    distinct(URN, time_period, .keep_all = TRUE)

  message("  Final absence dataset: ", nrow(absence_slim), " school-year rows")

  saveRDS(absence_slim, cache_path)
  absence_slim
}


# ---- 4. Build the panel ----

build_panel <- function(year_data_list, force_download = FALSE) {

  message("\n=== Building Panel Dataset ===\n")

  # Load external data sources
  edubase <- load_edubase(force = force_download)
  ofsted <- load_ofsted_ratings(force = force_download)
  absence_api <- load_absence_api(force = force_download)

  message("\n--- Joining external data to each year ---\n")

  panel_years <- map(names(year_data_list), function(year) {
    df <- year_data_list[[year]]
    config <- YEAR_CONFIG %>% filter(academic_year == year)
    tp <- config$time_period

    message("Processing ", year, " (", nrow(df), " schools) ...")

    # --- Join Edubase (region + coordinates) ---
    df <- df %>%
      left_join(
        edubase %>% select(urn, gor_name, easting, northing),
        by = c("URN" = "urn")
      )

    n_with_region <- sum(!is.na(df$gor_name))
    message("  Edubase join: ", n_with_region, "/", nrow(df), " schools matched (gor_name)")

    # --- Backfill missing gor_name from LA/region lookup ---
    # Some schools (especially newer academies) are missing from Edubase.
    # Use the DfE performance tables LA-to-region lookup to fill gaps via LEA code.
    if (any(is.na(df$gor_name)) && "LEA" %in% names(df)) {
      la_region_lookup <- read.csv(
        # DfE performance-tables metadata, 2022-23 (committed: the download service
        # cannot fetch metadata)
        here::here("data", "lookups", "la_and_region_codes_meta.csv"),
        stringsAsFactors = FALSE
      )
      # Map sub-region names to the broad gor_name categories used in Edubase
      la_region_lookup <- la_region_lookup %>%
        mutate(
          gor_name_broad = case_when(
            grepl("^North East",   REGION.NAME) ~ "North East",
            grepl("^North West",   REGION.NAME) ~ "North West",
            grepl("^North Yorkshire|^South and West Yorkshire", REGION.NAME) ~ "Yorkshire and the Humber",
            grepl("^East Midlands", REGION.NAME) ~ "East Midlands",
            grepl("^West Midlands", REGION.NAME) ~ "West Midlands",
            grepl("^East of England", REGION.NAME) ~ "East of England",
            grepl("^London",       REGION.NAME) ~ "London",
            grepl("^South East",   REGION.NAME) ~ "South East",
            grepl("^South West",   REGION.NAME) ~ "South West",
            TRUE ~ NA_character_
          )
        )

      n_before <- sum(is.na(df$gor_name))
      df <- df %>%
        left_join(
          la_region_lookup %>% select(LEA, gor_name_broad),
          by = "LEA"
        ) %>%
        mutate(
          gor_name = coalesce(gor_name, gor_name_broad)
        ) %>%
        select(-gor_name_broad)

      n_after <- sum(is.na(df$gor_name))
      message("  Region backfill: fixed ", n_before - n_after, " of ",
              n_before, " missing gor_name via LA lookup (",
              n_after, " still missing)")
    }

    # --- Within-school backfill for remaining missing gor_name ---
    # Schools in reorganised LAs (e.g. Westmorland and Furness from Cumbria)
    # may be missing in the LA lookup for earlier years but present in later years
    if (any(is.na(df$gor_name))) {
      n_before_urn <- sum(is.na(df$gor_name))
      urn_region <- df %>%
        filter(!is.na(gor_name)) %>%
        distinct(URN, gor_name) %>%
        group_by(URN) %>%
        slice(1) %>%
        ungroup() %>%
        rename(gor_name_urn = gor_name)
      df <- df %>%
        left_join(urn_region, by = "URN") %>%
        mutate(gor_name = coalesce(gor_name, gor_name_urn)) %>%
        select(-gor_name_urn)
      n_after_urn <- sum(is.na(df$gor_name))
      message("  Within-school region backfill: fixed ", n_before_urn - n_after_urn,
              " of ", n_before_urn, " remaining missing gor_name (",
              n_after_urn, " still missing)")
    }

    # --- Join Ofsted ratings ---
    # Use OFSTEDRATING from school_information if available, else from external source
    # Coerce both to character first to avoid type mismatch in coalesce
    if (!"OFSTEDRATING" %in% names(df) || all(is.na(df$OFSTEDRATING))) {
      df <- df %>%
        left_join(ofsted, by = "URN") %>%
        mutate(OFSTEDRATING = as.character(OFSTEDRATING_external)) %>%
        select(-OFSTEDRATING_external)
      message("  Ofsted: using EXTERNAL ratings (", sum(!is.na(df$OFSTEDRATING)), " matched)")
    } else {
      # Still join external as supplement for any NAs
      df <- df %>%
        left_join(ofsted, by = "URN") %>%
        mutate(
          OFSTEDRATING = as.character(OFSTEDRATING),
          OFSTEDRATING_external = as.character(OFSTEDRATING_external),
          OFSTEDRATING = coalesce(OFSTEDRATING, OFSTEDRATING_external)
        ) %>%
        select(-OFSTEDRATING_external)
      message("  Ofsted: using INTERNAL ratings, supplemented with external (",
              sum(!is.na(df$OFSTEDRATING)), " total)")
    }

    # --- Join Absence API data ---
    # Use performance table absence if available, supplement with API data
    absence_year <- absence_api %>% filter(time_period == tp)

    if (!"PERCTOT" %in% names(df) || all(is.na(df$PERCTOT))) {
      # No absence in performance tables - use API data entirely
      df <- df %>%
        left_join(absence_year %>% select(URN, PERCTOT_api, PPERSABS10_api), by = "URN") %>%
        mutate(
          PERCTOT = suppressWarnings(as.numeric(PERCTOT_api)),
          PPERSABS10 = suppressWarnings(as.numeric(PPERSABS10_api))
        ) %>%
        select(-PERCTOT_api, -PPERSABS10_api)
      message("  Absence: using API data (", sum(!is.na(df$PERCTOT)), " matched)")
    } else {
      # Performance table absence available - use API as supplement
      # Coerce to numeric before coalescing to avoid type mismatches
      df <- df %>%
        left_join(absence_year %>% select(URN, PERCTOT_api, PPERSABS10_api), by = "URN") %>%
        mutate(
          PERCTOT = coalesce(
            suppressWarnings(as.numeric(PERCTOT)),
            suppressWarnings(as.numeric(PERCTOT_api))
          ),
          PPERSABS10 = coalesce(
            suppressWarnings(as.numeric(PPERSABS10)),
            suppressWarnings(as.numeric(PPERSABS10_api))
          )
        ) %>%
        select(-PERCTOT_api, -PPERSABS10_api)
      message("  Absence: using perf tables + API supplement (",
              sum(!is.na(df$PERCTOT)), " total)")
    }

    # --- Derive harmonised OFSTEDRATING_1 ---
    # In 2021-22 and 2022-23 the performance tables report text labels
    # (Outstanding, Good, Requires Improvement, Inadequate, etc.)
    # In 2023-24 and 2024-25 they report numeric codes (1, 2, 3, 4).
    # Map both to a single numeric 1-4 scale, then label as an ordered factor.
    if ("OFSTEDRATING" %in% names(df)) {
      ofsted_text_to_num <- c(
        "Outstanding"          = "1",
        "Good"                 = "2",
        "Requires Improvement" = "3",
        "Requires improvement" = "3",
        "Inadequate"           = "4",
        "Serious Weaknesses"   = "4",
        "Special Measures"     = "4"
      )

      df <- df %>%
        mutate(
          OFSTEDRATING   = as.character(OFSTEDRATING),
          # Recode: if the value is text, map it; if already 1-4, keep it
          OFSTEDRATING_1 = case_when(
            OFSTEDRATING %in% names(ofsted_text_to_num) ~
              ofsted_text_to_num[OFSTEDRATING],
            OFSTEDRATING %in% c("1", "2", "3", "4") ~ OFSTEDRATING,
            TRUE ~ NA_character_
          ),
          OFSTEDRATING_1 = factor(OFSTEDRATING_1,
                                  levels = c("1", "2", "3", "4"),
                                  labels = c("Outstanding", "Good",
                                             "Requires Improvement", "Inadequate"),
                                  ordered = TRUE)
        )

      n_harmonised <- sum(!is.na(df$OFSTEDRATING_1))
      message("  OFSTEDRATING_1 (harmonised): ", n_harmonised, "/", nrow(df), " schools")
    }

    # Ensure OFSTEDRATING is a factor for the model
    if ("OFSTEDRATING" %in% names(df)) {
      df <- df %>%
        mutate(OFSTEDRATING = factor(OFSTEDRATING))
    }

    df
  })

  names(panel_years) <- names(year_data_list)

  # --- Stack all years ---
  message("\n--- Stacking all years ---")

  # Find common columns across all years for safe binding
  common_cols <- Reduce(intersect, map(panel_years, names))
  message("  Common columns across all years: ", length(common_cols))

  # Also keep any columns that are in PANEL_VARS even if not common
  # (they'll be NA for years that don't have them)
  all_cols <- unique(unlist(map(panel_years, names)))
  keep_cols <- unique(c(
    intersect(all_cols, PANEL_VARS),
    common_cols[!common_cols %in% c(
      # Drop columns we definitely don't need in the panel
      grep("^ADDRESS|^PCODE$|^TELNUM$|^PCON_|^CONTFLAG$|^SCHNAME_AC$",
           common_cols, value = TRUE)
    )]
  ))

  # Ensure all dataframes have the same columns (fill with NA where missing)
  # and align column types to avoid bind_rows failures
  panel_years_aligned <- map(panel_years, function(df) {
    missing_cols <- setdiff(keep_cols, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    df %>% select(any_of(keep_cols))
  })

  # Detect and resolve type conflicts across years before binding
  # (e.g. a column is numeric in one year but character in another)
  shared_cols <- Reduce(intersect, map(panel_years_aligned, names))
  for (col in shared_cols) {
    types <- map_chr(panel_years_aligned, ~ class(.x[[col]])[1])
    if (n_distinct(types) > 1) {
      message("  Resolving type conflict for '", col, "': ",
              paste(names(types), types, sep = "=", collapse = ", "),
              " -> coercing to character")
      panel_years_aligned <- map(panel_years_aligned, function(df) {
        df[[col]] <- as.character(df[[col]])
        df
      })
    }
  }

  panel <- bind_rows(panel_years_aligned)

  message("  Panel: ", nrow(panel), " total school-year observations")
  message("  Years: ", paste(unique(panel$year_label), collapse = ", "))
  message("  Unique schools: ", n_distinct(panel$URN))

  # --- Summary statistics ---
  message("\n--- Panel Summary ---")
  panel %>%
    group_by(year_label) %>%
    summarise(
      n_schools = n(),
      pct_ATT8SCR = round(mean(!is.na(ATT8SCR)) * 100, 1),
      pct_PERCTOT = round(mean(!is.na(PERCTOT)) * 100, 1),
      pct_OFSTED = round(mean(!is.na(OFSTEDRATING)) * 100, 1),
      pct_OFSTED_1 = round(mean(!is.na(OFSTEDRATING_1)) * 100, 1),
      pct_gor = round(mean(!is.na(gor_name)) * 100, 1),
      .groups = "drop"
    ) %>%
    print()

  panel
}


# ---- Main execution ----

if (sys.nframe() == 0) {

  # Load the year data from step 01
  year_data_path <- here::here("data", "year_data_list.rds")
  expected_years <- YEAR_CONFIG$academic_year
  reload_needed <- FALSE

  if (!file.exists(year_data_path)) {
    message("year_data_list.rds not found. Running 01_extract_data.R first ...")
    reload_needed <- TRUE
  } else {
    year_data <- readRDS(year_data_path)
    cached_years <- names(year_data)
    missing_years <- setdiff(expected_years, cached_years)
    if (length(missing_years) > 0) {
      message("year_data_list.rds is stale — missing years: ",
              paste(missing_years, collapse = ", "))
      message("Re-running 01_extract_data.R to rebuild ...")
      reload_needed <- TRUE
    } else {
      message("year_data_list.rds has all ", length(expected_years), " expected years")
    }
  }

  if (reload_needed) {
    source(here::here("pipeline", "02_extract.R"))
    year_data <- load_all_years()
    saveRDS(year_data, year_data_path)
    message("Saved updated year_data_list.rds with ", length(year_data), " years")
  }

  # Build the panel
  panel <- build_panel(year_data)

  # Save
  saveRDS(panel, here::here("data", "panel_raw.rds"))
  message("\nSaved panel_raw.rds (", nrow(panel), " rows)")
}
