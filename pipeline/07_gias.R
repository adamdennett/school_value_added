# 07_gias.R - GIAS register: establishment spine, closures and successions
# -------------------------------------------------------------------------------
# From the original project (R/10_gias_closures.R).
# Cross-phase GIAS infrastructure (scripts 10-19). Downloads the full daily GIAS
# register (every establishment ever recorded, open and closed) plus the links
# file (predecessor/successor relationships), and builds:
#
#   data/gias/gias_establishments.rds - slim national spine (~50k schools, all
#                                       statuses) with open/close dates, reasons,
#                                       phase, type, capacity, NOR, coordinates
#   data/gias/school_closures.rds     - one row per closed school with close date,
#                                       reason, successor URN(s) and a closure
#                                       classification distinguishing real
#                                       closures from academisations and mergers
#
# Sources (daily extracts, no auth needed):
#   https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/edubasealldata<yyyymmdd>.csv
#   https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/links_edubasealldata<yyyymmdd>.csv
#
# Why this matters for the panels:
#   * URN succession: a school that converts/merges gets a NEW URN - panel
#     attrition is otherwise confounded with closure
#   * Closure analysis (falling rolls work): real closures must be separated
#     from administrative URN changes
#
# add_closure_flags(panel) joins closure fields onto any URN-keyed panel
# (used by 20_extract_ks2.R when this data is present).
#
# Run from project root:  Rscript R/10_gias_closures.R
# -------------------------------------------------------------------------------

source(here::here("pipeline", "helpers.R"))

GIAS_BASE_URL <- "https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/"

gias_raw_dir <- here::here("data-raw", "gias")
gias_dir     <- here::here("data", "gias")
dir.create(gias_raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(gias_dir, showWarnings = FALSE, recursive = TRUE)


#' Download the GIAS daily extracts (establishments + links), date-stamped.
#' Re-uses any extract already downloaded for `snapshot_date`. With no date,
#' the newest pair already downloaded is used, so a rebuild reproduces the
#' last one; today's is fetched only when there is none (or with --force).
gias_download <- function(snapshot_date = NULL) {
  if (is.null(snapshot_date)) {
    have <- sub("^edubasealldata(\\d{8})\\.csv$", "\\1",
                list.files(gias_raw_dir, "^edubasealldata\\d{8}\\.csv$"))
    have <- have[file.exists(file.path(gias_raw_dir, paste0("links_edubasealldata", have, ".csv")))]
    snapshot_date <- if (length(have)) as.Date(max(have), "%Y%m%d") else Sys.Date()
  }
  stamp <- format(as.Date(snapshot_date), "%Y%m%d")
  files <- c(
    establishments = paste0("edubasealldata", stamp, ".csv"),
    links          = paste0("links_edubasealldata", stamp, ".csv")
  )
  paths <- file.path(gias_raw_dir, files)
  names(paths) <- names(files)

  for (i in seq_along(files)) {
    if (file.exists(paths[i])) {
      message("Using cached GIAS extract: ", files[i])
      next
    }
    message("Downloading GIAS extract: ", files[i], " ...")
    utils::download.file(paste0(GIAS_BASE_URL, files[i]), paths[i],
                         mode = "wb", quiet = TRUE)
    message("  Done (", format(file.size(paths[i]), big.mark = ","), " bytes)")
  }
  paths
}


#' Read and slim the establishment register
#' GIAS CSVs are Windows-1252 encoded; dates are dd-mm-yyyy.
load_gias_establishments <- function(path) {
  message("Reading GIAS register (this is a ~60MB CSV) ...")
  est <- read_csv(
    path,
    locale = locale(encoding = "windows-1252"),
    na = c("", "NA"),
    show_col_types = FALSE,
    guess_max = 10000
  ) %>%
    janitor::clean_names()

  message("  ", nrow(est), " establishments, ", ncol(est), " columns")

  est %>%
    transmute(
      urn                = as.character(urn),
      la_code            = as.character(la_code),
      la_name,
      establishment_name,
      establishment_type = type_of_establishment_name,
      type_group         = establishment_type_group_name,
      phase              = phase_of_education_name,
      status             = establishment_status_name,
      open_date          = suppressWarnings(lubridate::dmy(open_date)),
      close_date         = suppressWarnings(lubridate::dmy(close_date)),
      reason_opened      = reason_establishment_opened_name,
      reason_closed      = reason_establishment_closed_name,
      statutory_low_age  = suppressWarnings(as.integer(statutory_low_age)),
      statutory_high_age = suppressWarnings(as.integer(statutory_high_age)),
      number_of_pupils   = suppressWarnings(as.integer(number_of_pupils)),
      school_capacity    = suppressWarnings(as.integer(school_capacity)),
      gor                = gor_name,
      trust_name         = trusts_name,
      easting, northing
    )
}


#' Read the links file and summarise successor relationships per URN
load_gias_successors <- function(path) {
  links <- read_csv(
    path,
    locale = locale(encoding = "windows-1252"),
    na = c("", "NA"),
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    mutate(urn = as.character(urn), link_urn = as.character(link_urn))

  message("  Links file: ", nrow(links), " rows; link types: ",
          paste(head(sort(unique(links$link_type)), 12), collapse = " | "))

  links %>%
    filter(grepl("^Successor|Merged", link_type)) %>%
    group_by(urn) %>%
    summarise(
      successor_urn       = first(link_urn),
      successor_link_type = first(link_type),
      n_successors        = n(),
      .groups = "drop"
    )
}


#' Classify each closure
#' "Real" closures (school ceases to exist) vs administrative URN changes
#' (academisation, amalgamation) where provision continues under a new URN.
classify_closure <- function(reason_closed, successor_link_type, successor_urn,
                             successor_type_group) {
  reason <- tolower(coalesce(reason_closed, ""))
  link   <- tolower(coalesce(successor_link_type, ""))
  s_type <- tolower(coalesce(successor_type_group, ""))

  case_when(
    grepl("academy", reason) | grepl("academ", s_type) & !is.na(successor_urn)
      ~ "Academisation (URN change)",
    grepl("amalgam|merg", reason) | grepl("amalgam|merg", link)
      ~ "Amalgamation/merger",
    grepl("fresh start", reason)
      ~ "Fresh start (relaunch)",
    !is.na(successor_urn)
      ~ "Reorganisation (other successor)",
    TRUE
      ~ "Closure (no successor)"
  )
}


#' Build establishment spine + closures dataset
build_gias_closures <- function(snapshot_date = NULL, save = TRUE) {

  paths <- gias_download(snapshot_date)
  est <- load_gias_establishments(paths["establishments"])
  successors <- load_gias_successors(paths["links"])

  # Successor type (is the successor an academy? still open?)
  successor_info <- est %>%
    select(successor_urn = urn,
           successor_name = establishment_name,
           successor_type_group = type_group,
           successor_status = status)

  closures <- est %>%
    filter(status == "Closed", !is.na(close_date)) %>%
    left_join(successors, by = "urn") %>%
    left_join(successor_info, by = "successor_urn") %>%
    mutate(
      close_year = lubridate::year(close_date),
      closure_class = classify_closure(reason_closed, successor_link_type,
                                       successor_urn, successor_type_group),
      is_state_school = type_group %in% c(
        "Academies", "Free Schools", "Local authority maintained schools",
        "State-funded schools"  # group label varies across GIAS vintages
      )
    )

  message("Closures (all time, all establishment types): ", nrow(closures))

  if (save) {
    saveRDS(est, file.path(gias_dir, "gias_establishments.rds"))
    saveRDS(closures, file.path(gias_dir, "school_closures.rds"))
    message("Saved gias_establishments.rds (", nrow(est), " rows) and ",
            "school_closures.rds (", nrow(closures), " rows)")
  }

  closures
}


#' Join closure fields onto a URN-keyed panel (e.g. the KS2 panel):
#' whether/when the school subsequently closed, why, and its successor.
add_closure_flags <- function(panel,
                              closures_path = here::here("data", "gias", "school_closures.rds")) {
  if (!file.exists(closures_path)) {
    message("Closure data not found (run 10_gias_closures.R) - skipping closure flags")
    return(panel)
  }
  closures <- readRDS(closures_path) %>%
    select(URN = urn, closed_date = close_date, closure_reason = reason_closed,
           closure_class, successor_urn)

  panel <- panel %>% left_join(closures, by = "URN")
  n_closed <- panel %>% filter(!is.na(closed_date)) %>% distinct(URN) %>% nrow()
  message("Closure flags joined: ", n_closed, " panel schools have since closed")
  panel
}


#' Recent primary closures summary (the falling-rolls policy question)
summarise_primary_closures <- function(closures, from_year = 2018) {
  closures %>%
    filter(
      phase %in% c("Primary", "Middle deemed primary", "All-through"),
      is_state_school,
      close_year >= from_year
    ) %>%
    count(close_year, closure_class) %>%
    pivot_wider(names_from = closure_class, values_from = n, values_fill = 0) %>%
    arrange(close_year)
}


# ---- Main execution (Rscript only) ----
if (sys.nframe() == 0) {
  closures <- build_gias_closures(if ("--force" %in% commandArgs(trailingOnly = TRUE)) Sys.Date() else NULL)

  message("\n=== State-funded primary closures by year and class (2018+) ===")
  print(summarise_primary_closures(closures), n = 30, width = Inf)

  message("\n=== Type groups in register (for is_state_school check) ===")
  print(count(closures, type_group, sort = TRUE), n = 15)
}
