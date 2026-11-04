# 10_ofsted_latest.R - Each school's latest Ofsted judgement, whatever the framework
# ---------------------------------------------------------------------------
# From the original project (R/15b_ofsted_latest.R).
# The panel's Ofsted rating is the last graded inspection before September
# 2024 (R/02c_ofsted_dates.R), which is right for the model years but not
# for saying where a school stands now. Since then Ofsted has inspected
# under two more schemes:
#
#   Sep 2024 - Nov 2025  graded inspections with no overall grade ("Not
#                        judged"), only the four sub-judgements, 1 to 4;
#   Nov 2025 onward      report cards: up to eight evaluation areas on five
#                        points (Exceptional, Strong standard, Expected
#                        standard, Needs attention, Urgent improvement) and
#                        safeguarding met / not met.
#
# Ofsted's monthly management information ("latest inspections", one row a
# school by URN) holds all of these side by side. This downloads the newest
# release, found from GOV.UK's content API, and writes one row a school:
#
#   ofsted_kind     "Report card", "No overall grade", "Graded" or
#                   "Ungraded only"
#   ofsted_latest   the judgement as one line, chosen in that order:
#                   a report card's achievement and curriculum grades; for
#                   an inspection with no overall grade, its quality of
#                   education; otherwise the last graded inspection, with
#                   any later ungraded outcome ("remains Good"); and for a
#                   school last graded before 2019, its latest ungraded
#                   outcome alone
#   ofsted_latest_date, and every report-card area as rc_*
#   oeif_modal      for an inspection with no overall grade, one grade on
#                   the old scale: the most common of its key judgements,
#                   ties going to quality of education (modal_grade())
#
# Output: data/ofsted/ofsted_latest.rds (attribute "as_at": the date the
# release covers). Raw file kept in data-raw/ofsted/.
#
#   Rscript pipeline/10_ofsted_latest.R           # the newest release already downloaded
#   Rscript pipeline/10_ofsted_latest.R --force   # check GOV.UK for a newer one first
#
# Ofsted publishes a new release around the middle of each month.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr); library(readr) })

MI_PAGE <- paste0("https://www.gov.uk/api/content/government/statistical-data-sets/",
                  "monthly-management-information-ofsteds-school-inspections-outcomes")
OLD_GRADES <- c("1" = "Outstanding", "2" = "Good", "3" = "Requires Improvement", "4" = "Inadequate")
RC_AREAS <- c(rc_inclusion = "Inclusion", rc_curriculum = "Curriculum and teaching",
              rc_achievement = "Achievement", rc_attendance = "Attendance and behaviour",
              rc_personal = "Personal development and wellbeing",
              rc_leadership = "Leadership and governance",
              rc_early_years = "Early years (where applicable)",
              rc_post16 = "Post-16 provision (where applicable)",
              rc_safeguarding = "Safeguarding standards")

#' The newest "latest inspections" CSV listed on the GOV.UK page
latest_mi <- function() {
  a <- jsonlite::fromJSON(MI_PAGE)$details$attachments
  a <- a[grepl("latest_inspections", a$filename, ignore.case = TRUE) & grepl("\\.csv$", a$filename), ]
  a$as_at <- lubridate::dmy(sub(".*_(\\d{1,2}_[A-Za-z]+_\\d{4})\\.csv$", "\\1", a$filename), quiet = TRUE)
  a <- a[!is.na(a$as_at), ]
  a[which.max(a$as_at), c("filename", "url", "as_at")]
}

#' The release to build from. By default the newest one already downloaded,
#' so a rebuild reproduces the last; with newest = TRUE (--force), whatever
#' GOV.UK lists as newest, downloaded if it is not here yet.
fetch_mi <- function(newest = FALSE) {
  raw <- here::here("data-raw", "ofsted")
  as_at_of <- function(f) lubridate::dmy(sub(".*_(\\d{1,2}_[A-Za-z]+_\\d{4})\\.csv$", "\\1", f), quiet = TRUE)
  have <- list.files(raw, "latest_inspections.*\\.csv$", ignore.case = TRUE)
  if (!newest && length(have)) {
    f <- have[which.max(as_at_of(have))]
    message("Using ", f)
    return(list(path = file.path(raw, f), as_at = as_at_of(f)))
  }
  m <- latest_mi()
  path <- file.path(raw, m$filename)
  if (!file.exists(path)) {
    dir.create(raw, showWarnings = FALSE, recursive = TRUE)
    message("Downloading ", m$filename)
    download.file(m$url, path, mode = "wb", quiet = TRUE)
  } else message("Using ", m$filename, " (already the newest)")
  list(path = path, as_at = m$as_at)
}

#' One grade for an inspection that gave none overall: the most common of
#' its key judgements (1 to 4; 9, not applicable, and blanks ignored), ties
#' going to quality of education, the judgement the old overall grade
#' usually followed. Quality of education is the first argument.
modal_grade <- function(...) {
  g <- do.call(cbind, lapply(list(...), function(x) suppressWarnings(as.integer(x))))
  g[!g %in% 1:4] <- NA
  out <- apply(g, 1, function(r) {
    qoe <- r[1]; r <- r[!is.na(r)]
    if (!length(r)) return(NA_integer_)
    n <- tabulate(r, 4); top <- which(n == max(n))
    # a tie quality of education does not settle goes to the lower grade
    if (length(top) == 1) top else if (!is.na(qoe) && qoe %in% top) qoe else max(top)
  })
  unname(OLD_GRADES[as.character(out)])
}

d_m_y <- function(x) suppressWarnings(lubridate::dmy(x))
mon <- function(d) format(d, "%b %Y")

build_ofsted_latest <- function(newest = FALSE, save = TRUE) {
  f <- fetch_mi(newest)
  raw <- read_csv(f$path, locale = locale(encoding = "latin1"), show_col_types = FALSE,
                  col_types = cols(.default = col_character()), na = c("", "NULL"))
  rc <- raw[, RC_AREAS]; names(rc) <- names(RC_AREAS)
  out <- bind_cols(
    raw |> transmute(
      URN = as.integer(URN),
      rc_date   = d_m_y(`Inspection start date`),
      oeif_date = d_m_y(`Inspection start date of latest OEIF graded inspection`),
      oeif_overall = `Latest OEIF overall effectiveness`,
      oeif_qoe  = unname(OLD_GRADES[`Latest OEIF quality of education`]),
      oeif_modal = modal_grade(`Latest OEIF quality of education`, `Latest OEIF behaviour and attitudes`,
                               `Latest OEIF personal development`,
                               `Latest OEIF effectiveness of leadership and management`,
                               `Latest OEIF early years provision (where applicable)`,
                               `Latest OEIF sixth form provision (where applicable)`),
      ug_date   = d_m_y(`Date of latest ungraded inspection`),
      ug_outcome = `Ungraded inspection overall outcome`),
    rc) |>
    mutate(
      has_rc = !is.na(rc_curriculum) & !is.na(rc_date),
      # A school last graded before the 2019 framework has no grade in these
      # columns at all, only its later ungraded visits
      ofsted_kind = case_when(
        has_rc ~ "Report card",
        oeif_overall %in% "Not judged" ~ "No overall grade",
        oeif_overall %in% names(OLD_GRADES) ~ "Graded",
        !is.na(ug_outcome) & !is.na(ug_date) ~ "Ungraded only"),
      ofsted_latest_date = case_when(has_rc ~ rc_date, ofsted_kind == "Ungraded only" ~ ug_date,
                                     TRUE ~ oeif_date),
      # a later ungraded visit, as Ofsted words it, trimmed of its "S5 Next" codes
      ug_txt = if_else(!is.na(ug_outcome) & !is.na(ug_date) & !is.na(oeif_date) & ug_date > oeif_date,
                       paste0("; ", sub(" - S5 Next$", "", sub("^School remains", "remains", ug_outcome)),
                              " (", mon(ug_date), ")"), ""),
      ofsted_latest = case_when(
        ofsted_kind == "Report card" ~ paste0(
          "Report card (", mon(rc_date), "): achievement ", rc_achievement,
          ", curriculum and teaching ", rc_curriculum),
        ofsted_kind == "No overall grade" ~ paste0(
          "No overall grade (", mon(oeif_date), "): quality of education ", coalesce(oeif_qoe, "not known")),
        ofsted_kind == "Graded" ~ paste0(OLD_GRADES[oeif_overall], " (", mon(oeif_date), ")", ug_txt),
        ofsted_kind == "Ungraded only" ~ paste0(
          sub(" - S5 Next$", "", sub("^School r", "R", ug_outcome)), " (", mon(ug_date), ", ungraded visit)"))) |>
    filter(!is.na(URN)) |>
    select(URN, ofsted_kind, ofsted_latest, ofsted_latest_date, all_of(names(RC_AREAS)),
           oeif_date, oeif_overall, oeif_qoe, oeif_modal, ug_date, ug_outcome)
  attr(out, "as_at") <- f$as_at
  message(sprintf("  %d schools, as at %s: %s", nrow(out), format(f$as_at, "%d %B %Y"),
                  paste(names(table(out$ofsted_kind)), table(out$ofsted_kind), collapse = ", ")))
  if (save) {
    dir.create(here::here("data", "ofsted"), showWarnings = FALSE)
    saveRDS(out, here::here("data", "ofsted", "ofsted_latest.rds"))
  }
  invisible(out)
}

if (sys.nframe() == 0) build_ofsted_latest(newest = "--force" %in% commandArgs(trailingOnly = TRUE))
