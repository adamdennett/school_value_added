# 01_inputs.R - Assemble the modelling data for Value Added v2.0
# ---------------------------------------------------------------------------
# Reads the school panel and its companions, built from public sources by
# R/00_fetch_data.R, and writes one table, data/inputs.rds (git-ignored),
# with every variable the v2 model and the site need:
#
#   data/panel_data.rds                   Attainment 8 by group, intake, absence (DfE)
#   data/census/school_census_context.rds neighbourhood education, 150 nearest OAs
#   data/cache/school_lad.rds             each school's district (ONS, postcodes.io)
#   data/ofsted/ofsted_latest.rds         latest Ofsted judgement (monthly MI)
#   data/gias/gias_establishments.rds     locations the panel lacks
#   data-raw/gias/links_*.csv             predecessor / successor URNs
#
# and, from the original project (school_attainment_tool) if it is there,
# who ran each Sussex school (its trust analysis; display only).
#
#   Rscript R/01_inputs.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))

stop_if_missing <- function(f) if (!file.exists(f))
  stop("Missing ", f, ". Build it with Rscript R/00_fetch_data.R (see README.md).")

#' Each URN's family through GIAS links: URN = the panel school, urn = any
#' URN it has filed under. Used to find a re-opened school's Ofsted record.
urn_links <- function(urns) {
  f <- tail(sort(list.files(here::here("data-raw", "gias"), "^links_edubasealldata.*csv$", full.names = TRUE)), 1)
  self <- tibble(URN = urns, urn = urns)
  if (!length(f)) return(self)
  l <- readr::read_csv(f, locale = readr::locale(encoding = "latin1"), progress = FALSE,
                       col_types = readr::cols(.default = "c")) |>
    transmute(from = as.integer(URN), link = as.integer(LinkURN), type = LinkType) |>
    filter(grepl("Predecessor|Successor", type))
  out <- self
  for (i in 1:4) {
    nxt <- out |> inner_join(l, by = c("urn" = "from"), relationship = "many-to-many") |>
      transmute(URN, urn = link) |> anti_join(out, by = c("URN", "urn"))
    if (!nrow(nxt)) break
    out <- bind_rows(out, nxt)
  }
  out |> filter(!(urn %in% urns & URN != urn)) |> distinct(URN, urn)
}

#' One identity per school across a change of URN. An academy conversion,
#' re-brokering or fresh start gives a school a new URN; this follows GIAS's
#' successor links to the latest URN that is itself in the panel, so the
#' school's years are counted together under its current URN.
current_urn <- function(urns) {
  f <- tail(sort(list.files(here::here("data-raw", "gias"), "^links_edubasealldata.*csv$", full.names = TRUE)), 1)
  if (!length(f)) return(tibble(URN = urns, school_id = urns))
  succ <- readr::read_csv(f, locale = readr::locale(encoding = "latin1"), progress = FALSE,
                          col_types = readr::cols(.default = "c")) |>
    filter(grepl("Successor", LinkType)) |>
    transmute(from = as.integer(URN), to = as.integer(LinkURN)) |>
    filter(to %in% urns) |> distinct(from, .keep_all = TRUE)
  id <- urns
  for (i in 1:5) {
    nxt <- succ$to[match(id, succ$from)]
    moved <- !is.na(nxt) & nxt != id
    if (!any(moved)) break
    id[moved] <- nxt[moved]
  }
  tibble(URN = urns, school_id = id)
}

#' Each school's latest Ofsted judgement, found through its URN family (any
#' URN it has filed under), so a school that converted or re-opened keeps
#' its record. The family is saved to data/lookups/urn_family.csv, so the
#' monthly refresh (R/refresh_ofsted.R) can repeat this without GIAS.
ofsted_latest <- function(urns) {
  family <- urn_links(urns)
  write.csv(family |> arrange(URN, urn), here::here("data", "lookups", "urn_family.csv"), row.names = FALSE)
  ofsted_fields(family)
}

ofsted_fields <- function(family) {
  f <- here::here("data", "ofsted", "ofsted_latest.rds"); stop_if_missing(f)
  o <- readRDS(f)
  best <- family |>
    inner_join(o, by = c("urn" = "URN")) |>
    filter(!is.na(ofsted_latest)) |>
    arrange(URN, desc(ofsted_latest_date)) |>
    distinct(URN, .keep_all = TRUE) |>
    transmute(URN, ofsted_latest, ofsted_latest_date, ofsted_kind, ofsted_rc = rc_achievement,
              # one grade for an inspection with no overall grade: the most
              # common of its key judgements (pipeline/10_ofsted_latest.R)
              ofsted_nog = if ("oeif_modal" %in% names(o)) ifelse(ofsted_kind == "No overall grade", oeif_modal, NA_character_)
                           else NA_character_)
  attr(best, "as_at") <- attr(o, "as_at")
  best
}

#' Who ran each Sussex school in 2024/25, from the original project's trust
#' analysis (its R/19). Sussex only, and display only: without the file the
#' columns are left empty and nothing in the model changes.
sussex_governance <- function() {
  f <- sat("data", "trust_effects.rds")
  if (!file.exists(f)) {
    message("  no ", f, ": Sussex governance left empty")
    return(tibble(URN = integer(), route = character(), trust_kind = character(),
                  run_by = character(), gov = character()))
  }
  tr <- readRDS(f)
  tidy <- function(x) ifelse(is.na(x), NA_character_, sub(" Limited$", "", gsub("\\bOf\\b", "of",
    gsub("\\bAnd\\b", "and", tools::toTitleCase(tolower(x))))))
  tr$sussex_years |>
    filter(year_label == "2024-25") |>
    transmute(URN = as.integer(URN), route = as.character(route), trust_kind,
              run_by = case_when(trust_kind == "Maintained" ~ "Maintained by the council",
                                 trust_kind == "Single-academy trust" ~ "Single-academy trust",
                                 trust_kind == "Multi-academy trust" ~ tidy(trust),
                                 TRUE ~ "Academy, trust not recorded"),
              gov = case_when(trust_kind == "Maintained" ~ "Maintained",
                              trust_kind == "Multi-academy trust" & route == "Sponsor-led academy" ~
                                "Multi-academy trust, sponsored",
                              trust_kind == "Multi-academy trust" ~ "Multi-academy trust, converter or free school",
                              TRUE ~ "Single-academy trust or not recorded"))
}

build_inputs <- function() {
  src <- function(...) here::here("data", ...)
  for (f in c(src("panel_data.rds"), src("census", "school_census_context.rds"),
              src("cache", "school_lad.rds"))) stop_if_missing(f)
  panel <- readRDS(src("panel_data.rds")) |>
    filter(MINORGROUP %in% c("Academy", "Maintained school"), !is.na(ATT8SCR), ATT8SCR > 0) |>
    mutate(URN = as.integer(URN), SCHNAME = gsub("\\s+", " ", trimws(SCHNAME)))

  l4 <- readRDS(src("census", "school_census_context.rds")) |>
    transmute(URN = as.integer(URN), l4 = cens_l4plus_k150)
  # every school in the panel is placed (pipeline/09_school_lad.R)
  lad_map <- readRDS(src("cache", "school_lad.rds")) |> transmute(URN = as.integer(URN), lad)

  d <- panel |>
    left_join(l4, by = "URN") |>
    left_join(lad_map |> distinct(URN, .keep_all = TRUE), by = "URN") |>
    mutate(year_label = as.character(year_label),
           z_l4 = (l4 - mean(l4, na.rm = TRUE)) / sd(l4, na.rm = TRUE),
           mobile = 100 - PTNMOB, sen_support = PSENELK, sen_ehcp = PSENELSE,
           girls_school = GENDER %in% "Girls", boys_school = GENDER %in% "Boys",
           ADMPOL_PT = factor(ADMPOL_PT))

  # Location for the maps: the panel's own easting and northing, else GIAS
  gias <- readRDS(here::here("data", "gias", "gias_establishments.rds")) |>
    transmute(URN = as.integer(urn), e_g = as.numeric(easting), n_g = as.numeric(northing)) |>
    distinct(URN, .keep_all = TRUE)
  xy <- d |> select(URN, easting, northing) |>
    mutate(easting = as.numeric(easting), northing = as.numeric(northing)) |>
    left_join(gias, by = "URN") |>
    transmute(URN, easting = coalesce(easting, e_g), northing = coalesce(northing, n_g)) |>
    filter(!is.na(easting), !is.na(northing)) |> distinct(URN, .keep_all = TRUE)
  ll <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(xy, coords = c("easting", "northing"), crs = 27700), 4326))
  d <- d |> left_join(tibble(URN = xy$URN, lon = ll[, 1], lat = ll[, 2]), by = "URN")

  # One school, one identity: URN becomes the school's current URN, and the
  # URN each row was published under is kept as urn_published
  ids <- current_urn(unique(d$URN))
  d <- d |> left_join(ids, by = "URN") |>
    mutate(urn_published = URN, URN = school_id) |> select(-school_id)
  message(sprintf("  %d published URNs merged into a later URN of the same school",
                  sum(ids$URN != ids$school_id)))

  ofs <- ofsted_latest(unique(d$URN))
  d <- d |> left_join(ofs, by = "URN")
  d <- d |> left_join(sussex_governance(), by = "URN")

  # School-years where Attainment 8 does not measure what the school does,
  # by what was entered, not by how low the results were:
  #   - fewer than three GCSE entries per pupil: the school does not mainly
  #     take GCSEs (Europa School UK's pupils take the European Baccalaureate);
  #     UTCs and studio schools taking technical qualifications that count
  #     towards Attainment 8 sit well above this;
  #   - fewer than half of pupils achieving any qualification: an anomalous year.
  # They stay in this table, flagged, and are left out of every model.
  d <- d |> mutate(
    excl_reason = case_when(
      TAVENT_G_PTQ_EE < 3 ~ "Fewer than three GCSE entries per pupil",
      PTANYQ_PTQ_EE < 50 ~ "Fewer than half of pupils achieved any qualification"),
    excluded = !is.na(excl_reason))
  message(sprintf("  %d school-years excluded (%d schools)", sum(d$excluded), n_distinct(d$URN[d$excluded])))

  attr(d, "ofsted_as_at") <- attr(ofs, "as_at")
  attr(d, "built_at") <- Sys.time()
  message(sprintf("%d school-years, %d schools, years %s", nrow(d), n_distinct(d$URN),
                  paste(sort(unique(d$year_label)), collapse = ", ")))
  dir.create(here::here("data"), showWarnings = FALSE)
  saveRDS(d, here::here("data", "inputs.rds"))
  invisible(d)
}

if (sys.nframe() == 0) build_inputs()
