# 00b_download_ees_api.R - Downloads from Explore Education Statistics
# ---------------------------------------------------------------------------
# Successor to 00_download_cscp.R.
#
# The Compare School and College Performance service is closing. The 2025/26
# KS4 release states plainly: "CSCP will close soon. From autumn 2026, you will
# be able to use a new service. Data on CSCP will not be updated past the
# 2024/25 academic year." So 00_download_cscp.R can fetch 2021/22-2024/25 and
# nothing after it. That script is kept exactly as it is, because every
# published figure for those four years reproduces from the files it fetched.
#
# From 2025/26 onward the data comes from EES instead, by two routes:
#
#   1. The statistics API, for data sets that have been API-enabled.
#        GET {EES_API_BASE}/data-sets/{dataSetId}/csv
#      Returns the whole data set as a gzipped CSV in EES long format
#      (one row per school x time period x breakdown).
#
#   2. The content API, for data sets that are download-only.
#        GET {EES_CONTENT_BASE}/releases/{releaseId}/files?fileIds={fileId}
#      Returns either a ZIP (data/ + data-guidance/) or a bare CSV, depending
#      on the file. ees_download() sniffs the first bytes and names it right.
#      Note the query-string form: /files/{fileId} answers 200 with an empty
#      body, and /files with no query returns the entire release (129 MB for
#      KS4 2025/26), so neither is what we want.
#
# Discovering ids: open the data set in the EES data catalogue and read
# "API data set ID" for route 1, or the Download link's href for route 2.
#
# Files are NEVER overwritten - pass force = TRUE if a release is revised.
# The KS4 2025/26 figures here are PROVISIONAL; revised land February 2027.
#
# Usage:
#   source("R/00b_download_ees_api.R")
#   ees_update()                      # fetch anything missing
#   ees_download("ks4_institution", force = TRUE)   # refresh one source
#
# Run from project root. Standalone execution runs ees_update().
# ---------------------------------------------------------------------------

source(here::here("pipeline", "helpers.R"))

EES_API_BASE     <- "https://api.education.gov.uk/statistics/v1"
EES_CONTENT_BASE <- "https://content.explore-education-statistics.service.gov.uk/api"
EES_DIR          <- here::here("data-raw", "ees")

# What we need on disk, and where each piece comes from. All are Open
# Government Licence v3.0.
EES_SOURCES <- list(

  ks4_institution = list(
    kind        = "api_csv",
    data_set_id = "19e39901-a96c-be76-b9c2-6af54ae076d2",
    file        = "ks4_institution_performance_2025-26.csv.gz",
    publication = "Key stage 4 performance",
    data_set    = "Key stage 4 institution level - Performance",
    coverage    = "2023/24 to 2025/26, school level, 137,975 rows",
    note        = paste("Attainment 8 and English/maths measures by sex,",
                        "disadvantage status, first language, prior attainment",
                        "and mobility. Progress 8 columns are all 'z' for",
                        "2024/25 and 2025/26 - no KS2 baseline after COVID.")
  ),

  absence_2term_school = list(
    kind       = "release_file",
    release_id = "bc053a9f-7963-4523-9e7b-08df18a929bb",
    file_id    = "8ce59885-8402-49b6-ddcf-08df188ac684",
    file       = "absence_2term_school_2025-26.zip",
    publication = "Pupil absence in schools in England",
    data_set   = "Absence - school level - autumn and spring terms combined",
    coverage   = "2016/17 to 2025/26 autumn+spring, school level, 191,017 rows",
    note       = paste("Autumn and spring terms only. Full-year 2025/26",
                       "absence is not published until around March 2027,",
                       "so this is the only absence basis that reaches the",
                       "new KS4 year - and it reaches every earlier year too,",
                       "which is what makes a consistent series possible.")
  ),

  school_census = list(
    kind       = "release_file",
    release_id = "420ab305-b770-4dfa-89a0-ac899f34ac46",
    file_id    = "3d6359e3-0733-4a4c-a829-66aec003966d",
    file       = "spc_school_level_underlying_data_2026.csv",
    publication = "Schools, pupils and their characteristics",
    data_set   = "School level underlying data 2026",
    coverage   = "One row per school at January 2026",
    note       = paste("Supplies first-language percentages (PNUMEAL),",
                       "free school meal shares and admissions policy for",
                       "the 2025/26 year.")
  ),

  exclusions_school = list(
    kind       = "release_file",
    release_id = "0e388e4d-3748-49a4-984c-08dece27c222",
    file_id    = "32c873cf-30bd-4ec4-22ab-08decde94ad4",
    file       = "exclusions_school_2024-25.zip",
    publication = "Suspensions and permanent exclusions in England",
    data_set   = "Suspensions and permanent exclusions - school level",
    coverage   = "2006/07 to 2024/25, school level, 405,529 rows",
    note       = paste("Headcount, permanent exclusions and suspensions with",
                       "their rates, plus the count of pupils receiving one or",
                       "more suspension. Runs one year behind the absence and",
                       "KS4 series: 2025/26 exclusions are not published until",
                       "summer 2027. Suppressed values appear as 'x'.")
  ),

  exclusions_la = list(
    kind       = "release_file",
    release_id = "0e388e4d-3748-49a4-984c-08dece27c222",
    file_id    = "3a5fc88a-dc90-4e71-22b3-08decde94ad4",
    file       = "exclusions_av_days_missed_2024-25.zip",
    publication = "Suspensions and permanent exclusions in England",
    data_set   = "Suspensions - average days missed",
    coverage   = "National, regional and LA level by phase",
    note       = paste("Carries sessions_susp and number_days_susp, which is",
                       "what turns a count of suspensions into a count of",
                       "missed sessions. The school-level absence file's",
                       "sess_auth_excluded is the same quantity measured from",
                       "the attendance register, and the two are compared in",
                       "15_exclusions.R.")
  )
)


#' Build the download URL for one configured source
ees_url <- function(src) {
  switch(src$kind,
    api_csv      = sprintf("%s/data-sets/%s/csv", EES_API_BASE, src$data_set_id),
    release_file = sprintf("%s/releases/%s/files?fileIds=%s",
                           EES_CONTENT_BASE, src$release_id, src$file_id),
    stop("Unknown source kind: ", src$kind)
  )
}


#' Identify what actually came back, so a mis-named file is caught here
#' rather than three steps downstream.
#' @return One of "gzip", "zip", "csv" or "unknown"
sniff_format <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  magic <- readBin(con, "raw", 4)
  if (length(magic) < 4) return("unknown")
  if (identical(as.integer(magic[1:2]), c(0x1f, 0x8b))) return("gzip")
  if (rawToChar(magic[1:2]) == "PK") return("zip")
  # CSVs from EES carry a UTF-8 BOM often enough to be worth allowing
  head_txt <- suppressWarnings(readLines(path, n = 1, warn = FALSE))
  if (length(head_txt) && grepl(",", head_txt)) return("csv")
  "unknown"
}


#' Download one configured EES source
#'
#' @param name A name from EES_SOURCES
#' @param force Re-download even if the file is already on disk
#' @return The path written, invisibly
ees_download <- function(name, force = FALSE) {
  src <- EES_SOURCES[[name]]
  if (is.null(src)) stop("Unknown EES source: ", name)

  dir.create(EES_DIR, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(EES_DIR, src$file)

  if (file.exists(dest) && !force) {
    message("Already present: ", src$file,
            " (", format(file.size(dest) / 1e6, digits = 3), " MB)")
    return(invisible(dest))
  }

  url <- ees_url(src)
  message("Downloading ", name, " ...")
  message("  ", src$publication, " / ", src$data_set)
  message("  ", url)

  tmp <- tempfile(fileext = ".part")
  on.exit(unlink(tmp), add = TRUE)
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)

  fmt <- sniff_format(tmp)
  if (fmt == "unknown") {
    stop("Download for ", name, " is neither gzip, zip nor CSV - ",
         "the endpoint or the file id has probably changed.")
  }

  # The content API serves some data sets as a bare CSV and others as a ZIP,
  # and does not say which in advance. Make the extension tell the truth.
  expected <- c(gzip = "csv.gz", zip = "zip", csv = "csv")[[fmt]]
  if (!grepl(paste0("\\.", gsub("\\.", "\\\\.", expected), "$"), src$file)) {
    dest <- file.path(EES_DIR,
                      paste0(tools::file_path_sans_ext(src$file, compression = TRUE),
                             ".", expected))
    message("  Served as ", fmt, "; writing ", basename(dest))
  }

  file.copy(tmp, dest, overwrite = TRUE)
  message("  Done: ", format(file.size(dest) / 1e6, digits = 3), " MB")
  invisible(dest)
}


#' Fetch every configured source that is not already on disk
ees_update <- function(force = FALSE) {
  for (name in names(EES_SOURCES)) ees_download(name, force = force)
  message("\nEES downloads up to date in ", EES_DIR)
  invisible(TRUE)
}


#' Where a configured source landed, whatever extension it ended up with
#' @param name A name from EES_SOURCES
ees_path <- function(name) {
  src <- EES_SOURCES[[name]]
  if (is.null(src)) stop("Unknown EES source: ", name)
  stem <- tools::file_path_sans_ext(src$file, compression = TRUE)
  hits <- list.files(EES_DIR, pattern = paste0("^", stem, "\\."), full.names = TRUE)
  if (!length(hits)) {
    stop("Not downloaded yet: ", name, " - run ees_download(\"", name, "\")")
  }
  hits[1]
}


if (sys.nframe() == 0) ees_update()
