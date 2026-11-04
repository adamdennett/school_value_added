# 00_fetch_data.R - Build Value Added v2.0's inputs from public sources
# ---------------------------------------------------------------------------
# Everything R/01_inputs.R needs for the model is built here, from DfE,
# GIAS, ONS, nomis, postcodes.io and Ofsted, by the scripts in pipeline/.
# They come from the original project (school_attainment_tool), cut to what
# v2.0 uses: no workforce or finance steps, no KS2 or KS5 panels.
#
#   Rscript R/00_fetch_data.R               # build whatever is missing
#   Rscript R/00_fetch_data.R --dry-run     # say what would run, run nothing
#   Rscript R/00_fetch_data.R --all         # rebuild every step regardless
#   Rscript R/00_fetch_data.R ofsted_latest # just these steps (missing or not)
#
# Each step runs in its own Rscript process, so one script's packages and
# globals cannot leak into the next; a failure is reported and the rest
# carry on. Downloads are kept in data-raw/ and data/, both git-ignored, and
# a file already on disk is never fetched again unless its step is named.
#
# Two things still come from the original project, and say so where they
# are read: the Sussex area analyses (data/area/, a snapshot) and the
# Sussex governance in R/01_inputs.R. v1 value added, the published
# measure, is a fixed snapshot in data/lookups/v1_published.csv.
# ---------------------------------------------------------------------------

source(here::here("pipeline", "helpers.R"))   # YEAR_CONFIG: the years and their KS4 files

extracted <- function(file) file.path("data", "extracted", YEAR_CONFIG$academic_year, file)
ees_files <- c("ks4_institution_performance_2025-26.csv.gz", "absence_2term_school_2025-26.zip",
               "exclusions_school_2024-25.zip")

# What each step writes, and which steps it needs first. A step must also
# be LISTED after everything it needs.
STEPS <- list(
  gias = list(scripts = "pipeline/07_gias.R",
              outputs = "data/gias/gias_establishments.rds"),
  cscp = list(scripts = "pipeline/01_download_cscp.R",
              outputs = c(file.path("data", "extracted", YEAR_CONFIG$academic_year, YEAR_CONFIG$ks4_file),
                          extracted("england_census.csv"), extracted("england_abs.csv"),
                          extracted("england_school_information.csv"))),
  extract = list(scripts = "pipeline/02_extract.R", needs = "cscp",
              outputs = "data/year_data_list.rds"),
  panel_raw = list(scripts = "pipeline/03_build_panel.R", needs = "extract",
              outputs = "data/panel_raw.rds"),
  # derived variables, then the Ofsted dates (which patch the panel in place)
  panel = list(scripts = c("pipeline/04_derived.R", "pipeline/05_ofsted_dates.R"), needs = "panel_raw",
              outputs = "data/panel_data.rds"),
  ees = list(scripts = "pipeline/ees_api.R",
              outputs = file.path("data-raw", "ees", ees_files)),
  panel_5yr = list(scripts = c("pipeline/06_extend_2526.R", "pipeline/05_ofsted_dates.R"), needs = c("panel", "ees"),
              outputs = "data/panel_data_5yr.rds"),
  census = list(scripts = "pipeline/08_census.R", needs = c("gias", "panel"),
              outputs = c("data/census/school_census_context.rds", "data/cache/oa_pwc_2021.rds")),
  school_lad = list(scripts = "pipeline/09_school_lad.R", needs = "panel",
              outputs = "data/cache/school_lad.rds"),
  # absence by reason, from the EES file the panel build downloads
  absence_reasons = list(scripts = "pipeline/11_absence_reasons.R", needs = "panel_raw",
              outputs = "data/absence_reasons.rds"),
  # Ofsted's newest monthly release. Only fetched when missing: to move to a
  # newer month run it by name, or R/refresh_ofsted.R
  ofsted_latest = list(scripts = "pipeline/10_ofsted_latest.R",
              outputs = "data/ofsted/ofsted_latest.rds"))

root <- function(...) here::here(...)
missing_outputs <- function(step) STEPS[[step]]$outputs[!file.exists(root(STEPS[[step]]$outputs))]

#' Run one step's scripts in order, each in its own process; TRUE if all
#' succeeded and the step's outputs are there
run_step <- function(name, args = character(0)) {
  s <- STEPS[[name]]
  t0 <- Sys.time()
  status <- 0
  for (sc in s$scripts) {
    status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(root(sc)), args))
    if (status != 0) break
  }
  still <- missing_outputs(name)
  mins <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  if (status != 0 || length(still)) {
    message(sprintf("  FAILED %s after %s min (exit %s)%s", name, mins, status,
                    if (length(still)) paste0("; still missing: ", paste(still, collapse = ", ")) else ""))
    return(FALSE)
  }
  message(sprintf("  done %s in %s min", name, mins))
  TRUE
}

fetch_data <- function(only = NULL, all = FALSE, dry_run = FALSE) {
  unknown <- setdiff(only, names(STEPS))
  if (length(unknown)) stop("Unknown step(s): ", paste(unknown, collapse = ", "),
                            ". Steps: ", paste(names(STEPS), collapse = ", "))
  wanted <- names(STEPS)
  if (length(only)) wanted <- only
  else if (!all) wanted <- wanted[vapply(wanted, function(w) length(missing_outputs(w)) > 0, logical(1))]
  # Pull in anything a wanted step needs whose outputs are missing, and
  # what that needs in turn, until nothing new is added
  repeat {
    add <- unique(unlist(lapply(STEPS[wanted], `[[`, "needs")))
    add <- add[!add %in% wanted & vapply(add, function(n) length(missing_outputs(n)) > 0, logical(1))]
    if (!length(add)) break
    wanted <- c(add, wanted)
  }
  wanted <- names(STEPS)[names(STEPS) %in% wanted]          # keep the dependency order

  if (!length(wanted)) { message("Nothing to fetch: every output is present."); return(invisible(TRUE)) }
  message("Steps to run, in order:")
  for (w in wanted) message(sprintf("  %-13s %s -> %s", w, paste(STEPS[[w]]$scripts, collapse = " + "),
                                    paste(STEPS[[w]]$outputs, collapse = ", ")))
  if (dry_run) return(invisible(TRUE))

  ok <- vapply(wanted, function(w) {
    blocked <- STEPS[[w]]$needs[vapply(STEPS[[w]]$needs, function(n) length(missing_outputs(n)) > 0, logical(1))]
    if (length(blocked)) { message("  SKIPPED ", w, ": needs ", paste(blocked, collapse = ", ")); return(FALSE) }
    message("\n== ", w)
    # a step named on the command line fetches afresh (GIAS today, Ofsted's newest)
    run_step(w, if (w %in% only) "--force" else character(0))
  }, logical(1))
  message("\n", sum(ok), " of ", length(ok), " steps succeeded",
          if (any(!ok)) paste0("; failed or skipped: ", paste(names(ok)[!ok], collapse = ", ")) else "")
  invisible(all(ok))
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  flags <- args[startsWith(args, "--")]
  bad <- setdiff(flags, c("--all", "--dry-run"))
  if (length(bad)) stop("Unknown option(s): ", paste(bad, collapse = ", "))
  ok <- fetch_data(only = setdiff(args, flags), all = "--all" %in% flags, dry_run = "--dry-run" %in% flags)
  if (!ok) quit(status = 1)
}
