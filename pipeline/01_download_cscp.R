# 01_download_cscp.R - Automated downloads from Compare School and College Performance
# -------------------------------------------------------------------------------------
# From the original project (R/00_download_cscp.R), set up for the KS4 files
# Value Added v2.0 needs.
# Scripted access to the CSCP download service, replacing the manual download wizard:
#   https://www.compare-school-performance.service.gov.uk/download-data
#
# The service accepts direct GET requests of the form:
#   /download-data?download=true&regions=0&filters=<type1,type2,...>&fileformat=csv&year=<YYYY-YYYY>&meta=false
# With >=2 datatypes it returns a ZIP containing <year>/england_*.csv files in
# exactly the layout used by data/extracted/<year>/ - so downloads drop straight
# into the existing pipeline structure. Existing files are NEVER overwritten
# (the published analyses depend on them); only missing files are added.
#
# Notes discovered while building this (June 2026):
#   * meta=true returns an error page - metadata zips still need the manual wizard
#     (they are documentation, not data; data-raw/ holds copies for past years)
#   * KS2 school-level data exists only from 2022-2023 onwards (COVID gap:
#     2021-22 KS2 results were not published at school level)
#   * vaqual / vasubj (16-18 value added) exist only from 2023-2024 onwards
#   * the service requires a browser User-Agent header
#
# Usage:
#   source("pipeline/01_download_cscp.R")           # load functions
#   cscp_update()                            # fetch any missing files, all years/phases
#   cscp_download("2024-2025", c("ks4prov", "Census"))   # targeted download
#
# Run from project root. Standalone execution (Rscript pipeline/01_download_cscp.R)
# runs cscp_update().
# -------------------------------------------------------------------------------------

source(here::here("pipeline", "helpers.R"))

CSCP_BASE_URL <- "https://www.compare-school-performance.service.gov.uk/download-data"
CSCP_USER_AGENT <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/126.0 Safari/537.36"

# Datatype token -> filename the service delivers inside the per-year folder.
# Tokens come from the wizard's datatypes step; filenames verified by download.
CSCP_DATATYPE_FILES <- c(
  "gias"             = "england_school_information.csv",
  "ks2"              = "england_ks2final.csv",
  "ks4"              = "england_ks4final.csv",
  "ks4prov"          = "england_ks4provisional.csv",
  "ks5"              = "england_ks5final.csv",
  "vaqual"           = "england_vaqual.csv",
  "vasubj"           = "england_vasubj.csv",
  "ks4destination"   = "england_ks4-pupdest.csv",
  "ks5destination"   = "england_ks5-studest.csv",
  "ks5destinationhe" = "england_ks5-studest-he.csv",
  "pupilabsence"     = "england_abs.csv",
  "Census"           = "england_census.csv",
  "ks2mats"          = "england_ks2-mats-performance.csv",
  "ks4mats"          = "england_ks4-mats-performance.csv",
  "ks5mats"          = "england_ks5-mats-performance.csv"
)

# What Value Added v2.0 needs on disk, per year: the KS4 file YEAR_CONFIG
# names for that year (final or provisional, so the panel keeps the release
# it was built on), the school census, absence and school information. The
# original project fetched KS2, KS5 and destinations here too; v2.0 needs
# none of them. A new year is added in YEAR_CONFIG (helpers.R).
ks4_token <- function(file) names(CSCP_DATATYPE_FILES)[match(file, CSCP_DATATYPE_FILES)]
CSCP_PHASE_CONFIG <- lapply(seq_len(nrow(YEAR_CONFIG)), function(i) list(
  datatypes = c(ks4_token(YEAR_CONFIG$ks4_file[i]), "Census", "pupilabsence", "gias"),
  years     = YEAR_CONFIG$academic_year[i]))
names(CSCP_PHASE_CONFIG) <- paste0("ks4_", YEAR_CONFIG$academic_year)

CSCP_EXTRACTED_DIR <- here::here("data", "extracted")


#' Build a CSCP direct-download URL
cscp_url <- function(year, datatypes) {
  paste0(
    CSCP_BASE_URL,
    "?download=true&regions=0",
    "&filters=", paste(datatypes, collapse = ","),
    "&fileformat=csv&year=", year, "&meta=false"
  )
}


#' Download one or more datatypes for a year and place files in data/extracted/<year>/
#'
#' Always requests at least two datatypes (single-type requests return a bare CSV
#' with no filename; pairing guarantees a ZIP with canonical filenames).
#' Existing destination files are skipped unless force = TRUE.
#'
#' @return (invisibly) character vector of file paths newly placed
cscp_download <- function(year, datatypes, dest_root = CSCP_EXTRACTED_DIR, force = FALSE) {

  datatypes <- unique(datatypes)
  # Pad single requests so the service returns a ZIP (gias is small and always available)
  padded <- if (length(datatypes) == 1) unique(c(datatypes, "gias")) else datatypes

  url <- cscp_url(year, padded)
  tmp <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp), add = TRUE)

  message("CSCP download: ", year, " [", paste(datatypes, collapse = ", "), "]")
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE,
                       headers = c("User-Agent" = CSCP_USER_AGENT))

  # Validate response: ZIPs start "PK"; error pages start "<"
  magic <- readBin(tmp, "raw", n = 2)
  if (!identical(magic, as.raw(c(0x50, 0x4B)))) {
    head_txt <- suppressWarnings(readLines(tmp, n = 1, warn = FALSE))
    stop("CSCP returned a non-ZIP response for ", year,
         " [", paste(padded, collapse = ","), "] - the datatype(s) are probably ",
         "not available for this year. Response begins: ",
         substr(paste(head_txt, collapse = " "), 1, 80))
  }

  staging <- tempfile("cscp_unzip_")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  unzip(tmp, exdir = staging)

  # ZIP contains <year>/england_*.csv
  extracted <- list.files(staging, recursive = TRUE, full.names = TRUE)
  placed <- character(0)

  for (src in extracted) {
    src_norm <- normalizePath(src, winslash = "/")
    rel <- if (grepl(paste0("/", year, "/"), src_norm, fixed = TRUE)) {
      sub(paste0("^.*/", year, "/"), paste0(year, "/"), src_norm)
    } else {
      file.path(year, basename(src_norm))
    }
    dest <- file.path(dest_root, rel)

    if (file.exists(dest) && !force) {
      message("  exists, skipped: ", rel)
      next
    }
    dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
    file.copy(src, dest, overwrite = force)
    message("  placed: ", rel, " (", format(file.size(dest), big.mark = ","), " bytes)")
    placed <- c(placed, dest)
  }

  invisible(placed)
}


#' Which target files are missing for a phase-config entry and year?
cscp_missing_datatypes <- function(year, datatypes, dest_root = CSCP_EXTRACTED_DIR) {
  targets <- CSCP_DATATYPE_FILES[datatypes]
  missing <- datatypes[!file.exists(file.path(dest_root, year, targets))]
  missing
}


#' Fetch everything in CSCP_PHASE_CONFIG that is not already on disk.
#'
#' Idempotent: safe to re-run any time; when DfE publishes a new year, add it to
#' the relevant `years` vectors above (and YEAR_CONFIG equivalents) and re-run.
cscp_update <- function(config = CSCP_PHASE_CONFIG, dest_root = CSCP_EXTRACTED_DIR) {
  for (phase in names(config)) {
    cfg <- config[[phase]]
    message("\n== Phase set: ", phase, " ==")
    for (year in cfg$years) {
      missing <- cscp_missing_datatypes(year, cfg$datatypes, dest_root)
      if (length(missing) == 0) {
        message("Complete: ", year, " (all ", length(cfg$datatypes), " files present)")
        next
      }
      tryCatch(
        cscp_download(year, missing, dest_root),
        error = function(e) warning("FAILED ", phase, " / ", year, ": ",
                                    conditionMessage(e), call. = FALSE)
      )
      Sys.sleep(2)  # be polite to the service
    }
  }
  invisible(NULL)
}


# ---- Main execution (Rscript only; sourcing just loads the functions) ----
if (sys.nframe() == 0) {
  cscp_update()
}
