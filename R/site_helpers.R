# site_helpers.R - Colours, formats, charts and the map for the v2 site
# ---------------------------------------------------------------------------
# The palette is the current UCL one, led by its lighter purples. Charts
# need more contrast than the site's surfaces, so they use the dark, bright
# and mid purples with a deepened heritage blue for the "below" side of
# value added. Sourced by every page; not run on its own.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales); library(knitr); library(kableExtra)
  library(sf)    # so subsetting the district boundaries keeps their geometry
})
source(here::here("R", "00_config.R"))

INK <- "#1a1a2e"; DPURPLE <- "#361a54"; BPURPLE <- "#993bff"; MPURPLE <- "#ba82ff"
LPURPLE <- "#ddbdff"; PPURPLE <- "#eedeff"; HERITAGE <- "#30d6ff"; BLUE <- "#1f7fa0"
GREY <- "#6b6680"; LIGHT <- "#d3cde0"

# Names the review page (carried over from the original project) expects
PURPLE <- DPURPLE; TEAL <- DPURPLE; WARM <- BLUE; BAD <- BLUE
AREA_COL <- c("Brighton and Hove" = DPURPLE, "East Sussex" = BPURPLE, "West Sussex" = BLUE,
              "Mid Sussex" = MPURPLE, "South East" = GREY, "England" = INK, "Sussex" = INK)
GROUP_COL <- c("All pupils" = DPURPLE, "Disadvantaged" = BPURPLE, "Non-disadvantaged" = MPURPLE,
               "Girls" = BLUE, "Boys" = GREY)

theme_note <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.title = element_text(face = "bold", colour = DPURPLE),
          plot.subtitle = element_text(colour = GREY, size = rel(0.9)),
          plot.title.position = "plot", axis.title = element_text(colour = GREY, size = rel(0.9)),
          panel.grid.minor = element_blank(), legend.position = "bottom",
          strip.text = element_text(face = "bold", colour = DPURPLE))
}

kt <- function(df, ...) knitr::kable(df, format = "html", ...) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 12)

yr  <- function(x) paste0(substr(x, 1, 4), "/", substr(x, 5, 6))
yl  <- function(x) sub("-", "/", x)
ord <- function(n) { n <- round(n); s <- ifelse(n %% 100 %in% 11:13, "th",
  c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]); paste0(n, s) }
pts <- function(x, d = 1) sprintf(paste0("%.", d, "f"), x)
pp  <- pts
pct <- function(x) 100 * (exp(x) - 1)
sgn <- function(x, d = 1) sprintf(paste0("%+.", d, "f"), x)
and_list <- function(x) { x <- as.character(x)
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1)) }
slug <- function(x) gsub("[^a-z]+", "_", tolower(x))

# ---- Value added ----------------------------------------------------------
load_va <- function() readRDS(here::here("data", "va_v2.rds"))

#' One row a school: its attributes, v2 value added for every group (shrunk,
#' against England, with interval), and v1 for comparison
va_wide <- function(V) {
  w <- V$va |>
    select(URN, group, years, va_shrunk, va_lo, va_hi, va_nat, va_la, sig) |>
    pivot_wider(names_from = group, values_from = c(years, va_shrunk, va_lo, va_hi, va_nat, va_la, sig),
                names_glue = "{.value}_{group}")
  v1 <- V$v1 |> select(URN, group, va_v1) |>
    pivot_wider(names_from = group, values_from = va_v1, names_glue = "v1_{group}")
  out <- V$schools |> inner_join(w, by = "URN") |> left_join(v1, by = "URN")
  # Provisional 2025/26, predicted from the fitted model (R/05_provisional_2526.R)
  pv <- load_prov()
  if (!is.null(pv)) out <- out |> left_join(
    pv$va |> select(URN, group, va_2526) |>
      pivot_wider(names_from = group, values_from = va_2526, names_glue = "va_2526_{group}"), by = "URN")
  # One Ofsted column with no gaps: the report card where a school has one;
  # otherwise, for an inspection between September 2024 and November 2025
  # that gave no overall grade, the most common of its key judgements;
  # otherwise its last graded inspection. The report card's grade names and
  # the old scale's are disjoint, so one column can carry both and the grade
  # alone says which.
  if (!"ofsted_nog" %in% names(out)) out$ofsted_nog <- NA_character_
  out |> mutate(ofsted_combined = coalesce(ofsted_rc, ofsted_nog, ofsted_model))
}

#' Every school's value added year by year, against England (unshrunk), for
#' every group: the model years, then provisional 2025/26 flagged as such
va_years <- function(V = load_va()) {
  # Two schools that later merged can share a year under one identity: average them
  yrs <- V$school_years |> group_by(URN, group, year_label) |>
    summarise(va = mean(va_nat), .groups = "drop") |> mutate(provisional = FALSE)
  pv <- load_prov()
  if (!is.null(pv)) yrs <- bind_rows(yrs, pv$va |> transmute(URN, group, year_label = pv$year,
                                                             va = va_2526, provisional = TRUE))
  yrs
}

#' A moving average of yearly value added, centred on `centre`, over a
#' window of n years (odd), using whichever years in the window a school has.
#' Returns one row a school and group: the average and how many years it used.
centred_ma <- function(yrs, centre, n = 3) {
  all_years <- sort(unique(yrs$year_label))
  i <- match(centre, all_years); h <- (n - 1) %/% 2
  win <- all_years[max(1, i - h):min(length(all_years), i + h)]
  yrs |> filter(year_label %in% win) |>
    group_by(URN, group) |>
    summarise(ma = mean(va, na.rm = TRUE), years_used = sum(!is.na(va)),
              includes_provisional = any(provisional), .groups = "drop") |>
    mutate(centre = centre, window = n)
}

#' A fixed, national colour scale for each diverging map measure: the
#' 97.5th percentile of |value| across England's schools. Schools beyond it
#' take the end colour (legends read "<=" / ">="), so a single extreme
#' school cannot wash out an area, and a colour means the same everywhere.
colour_caps <- function() {
  V <- load_va()
  w <- va_wide(V)
  f <- here::here("data", "place_absence.rds")
  if (file.exists(f)) {
    pa <- readRDS(f)
    w <- w |> left_join(pa$abs_schools |> select(URN, abs_excess), by = "URN") |>
      left_join(pa$va_m2 |> filter(group == "all") |> select(URN, va_m2), by = "URN")
  }
  vars <- intersect(c(grep("^va_shrunk_|^va_2526_", names(w), value = TRUE), "v1_all", "abs_excess", "va_m2"), names(w))
  sapply(vars, function(v) round(unname(quantile(abs(w[[v]]), 0.975, na.rm = TRUE)), 1))
}

#' Provisional 2025/26 value added, or NULL before it has been built
load_prov <- function() {
  f <- here::here("data", "va_2526_provisional.rds")
  if (file.exists(f)) readRDS(f) else NULL
}

#' The sentence every provisional figure carries
PROV_NOTE <- paste("2025/26 is provisional: predicted from the v2.0 model using DfE's provisional KS4",
                   "results (October 2026), with absence bridged from autumn and spring and the intake",
                   "data not yet published carried forward. It will be updated with the revised KS4",
                   "release in February 2027.")

#' A school's value added as "+2.1 (+0.8 to +3.4)"
va_txt <- function(v, lo, hi) ifelse(is.na(v), "", sprintf("%+.1f (%+.1f to %+.1f)", v, lo, hi))

#' Each school's v2 value added with its interval, for chosen groups
fig_va <- function(w, groups = c("all", "disadvantaged", "non_disadvantaged"), title = NULL) {
  long <- bind_rows(lapply(groups, function(g) tibble(
    SCHNAME = w$SCHNAME, group = GROUP_LABELS[[g]], v = w[[paste0("va_shrunk_", g)]],
    lo = w[[paste0("va_lo_", g)]], hi = w[[paste0("va_hi_", g)]]))) |>
    filter(!is.na(v))
  ord_s <- long |> filter(group == GROUP_LABELS[[groups[1]]]) |> arrange(v) |> pull(SCHNAME)
  long |>
    mutate(SCHNAME = factor(SCHNAME, unique(c(ord_s, setdiff(long$SCHNAME, ord_s)))),
           group = factor(group, GROUP_LABELS[groups])) |>
    ggplot(aes(v, SCHNAME, colour = group)) +
    geom_vline(xintercept = 0, colour = GREY) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, alpha = 0.5,
                   position = position_dodge(width = 0.6)) +
    geom_point(size = 2.2, position = position_dodge(width = 0.6)) +
    scale_colour_manual(values = GROUP_COL, name = NULL) +
    labs(x = "Value added against England (Attainment 8 points, shrunk, 95% interval)", y = NULL,
         title = title) +
    theme_note(12) + theme(axis.text.y = element_text(size = 10.5, colour = INK))
}

# ---- Decomposition: why an area's average differs from England's ---------
COMP_ORDER <- c("Absence", "Prior attainment (KS2)", "Disadvantage", "English as an additional language",
                "Special educational needs", "Pupil mobility", "Neighbourhood education",
                "Single-sex school", "Admissions (selection)", "Region", "Local authority",
                "Year", "Unexplained (school)")

#' Each component's mean contribution in the area minus England's, in
#' approximate Attainment 8 points. Exact on the log scale; the pieces sum
#' to the gap in mean log Attainment 8.
decompose <- function(V, urns, area, groups = c("all", "disadvantaged")) {
  bind_rows(lapply(groups, function(g) {
    cg <- V$contrib |> filter(group == g)
    in_a <- cg$URN %in% urns
    cols <- intersect(COMP_ORDER, names(cg))
    gm <- exp(mean(cg$log_obs))
    tibble(area = area, group = GROUP_LABELS[[g]], component = cols,
           log_gap = sapply(cols, function(k) mean(cg[[k]][in_a], na.rm = TRUE) - mean(cg[[k]], na.rm = TRUE)),
           area_att8 = exp(mean(cg$log_obs[in_a])), nat_att8 = gm) |>
      mutate(points = log_gap * nat_att8, rows = sum(in_a))
  }))
}

fig_decomp <- function(dec, title = "What separates the area's results from England's") {
  lab <- dec |> distinct(area, group, area_att8, nat_att8) |>
    mutate(txt = sprintf("%.1f vs England %.1f", area_att8, nat_att8))
  dec |>
    mutate(component = factor(component, rev(COMP_ORDER)),
           dir = ifelse(points < 0, "Pulls below England", "Lifts above England")) |>
    ggplot(aes(points, component, fill = dir)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = GREY) +
    geom_label(data = lab, aes(x = Inf, y = 2, label = txt), inherit.aes = FALSE, hjust = 1.02,
               size = 2.8, colour = INK, fill = "white", label.size = 0) +
    facet_grid(area ~ group) +
    scale_fill_manual(values = c("Pulls below England" = BLUE, "Lifts above England" = DPURPLE), name = NULL) +
    labs(x = "Approximate Attainment 8 points", y = NULL, title = title,
         subtitle = "Each bar is one factor's contribution; they sum to the gap") +
    theme_note(10)
}

# ---- The map --------------------------------------------------------------
source(here::here("R", "map_js.R"))

MAP_VARS <- tribble(
  ~var,              ~label,                                                     ~group,           ~scale, ~good,  ~dp, ~unit,
  "va_shrunk_all",   "Value added v2, all pupils",                               "Value added",    "div",  "high", 1,   " pts",
  "va_shrunk_disadvantaged", "Value added v2, disadvantaged pupils",             "Value added",    "div",  "high", 1,   " pts",
  "va_shrunk_non_disadvantaged", "Value added v2, non-disadvantaged pupils",     "Value added",    "div",  "high", 1,   " pts",
  "va_shrunk_girls", "Value added v2, girls",                                    "Value added",    "div",  "high", 1,   " pts",
  "va_shrunk_boys",  "Value added v2, boys",                                     "Value added",    "div",  "high", 1,   " pts",
  "va_m2",           "Value added with no allowance for absence, all pupils",    "Value added",    "div",  "high", 1,   " pts",
  "va_2526_all",     "Value added 2025/26 PROVISIONAL, all pupils",              "Value added",    "div",  "high", 1,   " pts",
  "v1_all",          "Value added v1 (published, within the LA), all pupils",    "Value added",    "div",  "high", 1,   " pts",
  "abs_excess",      "Absence above or below what the intake predicts, 2021/22 to 2024/25", "Attendance", "div", "low", 1, " pp",
  "absence",         "Overall absence, 2024/25",                                 "Attendance",     "seq",  "",     1,   "%",
  "persistent",      "Persistent absence, 2024/25",                              "Attendance",     "seq",  "",     1,   "%",
  "illness",         "Illness absence, autumn and spring 2025/26",               "Attendance",     "seq",  "",     1,   "%",
  "unauth",          "Unauthorised absence, autumn and spring 2025/26",          "Attendance",     "seq",  "",     1,   "%",
  "reduced",         "Reduced timetables, autumn and spring 2025/26",            "Attendance",     "seq",  "",     2,   "%",
  "susp_rate",       "Suspensions per 100 pupils, 2024/25",                      "Attendance",     "seq",  "",     1,   "",
  "att8",            "Attainment 8, all pupils, 2024/25",                        "Results",        "seq",  "",     1,   "",
  "att8_dis",        "Attainment 8, disadvantaged pupils, 2024/25",              "Results",        "seq",  "",     1,   "",
  "att8_non",        "Attainment 8, other pupils, 2024/25",                      "Results",        "seq",  "",     1,   "",
  "fsm",             "Disadvantaged pupils (FSM6) %",                            "Intake",         "seq",  "",     1,   "%",
  "ks2_c",           "Prior attainment at KS2 (centred)",                        "Intake",         "seq",  "",     1,   "",
  "eal",             "English as an additional language, 2024/25",               "Intake",         "seq",  "",     1,   "%",
  "sen_support",     "SEN support, 2024/25",                                     "Intake",         "seq",  "",     1,   "%",
  "sen_ehcp",        "Education, health and care plans, 2024/25",               "Intake",         "seq",  "",     1,   "%",
  "mobile",          "Pupil mobility %",                                         "Intake",         "seq",  "",     1,   "%",
  "l4",              "Neighbourhood education (% level 4+)",                     "Intake",         "seq",  "",     1,   "%",
  "pupils_roll",     "Pupils on roll, 2024/25",                                  "Intake",         "seq",  "",     0,   "",
  "gov",             "Who runs it, 2024/25",                                     "The school",     "cat",  "",     0,   "",
  "ofsted_rc",       "Ofsted report card, achievement (since November 2025)",    "The school",     "cat",  "",     0,   "",
  "ofsted",          "Ofsted rating, last graded inspection before September 2024", "The school",  "cat",  "",     0,   "",
  "ofsted_combined", "Ofsted, report card where there is one, else the latest graded judgements", "The school", "cat", "", 0, "",
  "religion",        "Religious character",                                      "The school",     "cat",  "",     0,   "")

MAP_CATS <- list(
  gov = c("Maintained" = DPURPLE, "Multi-academy trust, converter or free school" = BPURPLE,
          "Multi-academy trust, sponsored" = BLUE, "Single-academy trust or not recorded" = GREY),
  ofsted_rc = c("Exceptional" = DPURPLE, "Strong standard" = BPURPLE, "Expected standard" = LPURPLE,
                "Needs attention" = HERITAGE, "Urgent improvement" = BLUE),
  ofsted = c("Outstanding" = DPURPLE, "Good" = BPURPLE, "Requires Improvement" = HERITAGE,
             "Inadequate" = BLUE),
  religion = c("None" = GREY, "Church of England" = BPURPLE, "Roman Catholic" = DPURPLE,
               "Christian" = HERITAGE, "Other faith" = BLUE),
  # Two regimes on one map, so two ramps: both run dark (best) to light
  # (worst), and the hue says which regime a school is being read under.
  # Purple is the current report card, teal the old graded inspection.
  ofsted_combined = c("Exceptional" = "#2a0f45", "Strong standard" = "#5b21a8",
                      "Expected standard" = "#8f5cf0", "Needs attention" = "#b98cff",
                      "Urgent improvement" = "#e0ccff",
                      "Outstanding" = "#07343a", "Good" = "#11636e",
                      "Requires Improvement" = "#3f9aa6", "Inadequate" = "#9fd3d9"))

# Which regime each grade belongs to, for the legend. Defined for every
# grade, not just the first of each block, so a regime whose top grade has
# no schools on this map still gets its heading.
MAP_CAT_HEADS <- list(ofsted_combined = c(
  "Exceptional" = "Report card, since November 2025",
  "Strong standard" = "Report card, since November 2025",
  "Expected standard" = "Report card, since November 2025",
  "Needs attention" = "Report card, since November 2025",
  "Urgent improvement" = "Report card, since November 2025",
  "Outstanding" = "No report card yet: most common key judgement of a Sep 2024 to Nov 2025 inspection, else the last graded inspection",
  "Good" = "No report card yet: most common key judgement of a Sep 2024 to Nov 2025 inspection, else the last graded inspection",
  "Requires Improvement" = "No report card yet: most common key judgement of a Sep 2024 to Nov 2025 inspection, else the last graded inspection",
  "Inadequate" = "No report card yet: most common key judgement of a Sep 2024 to Nov 2025 inspection, else the last graded inspection"))

MAP_NONE <- c(ofsted_rc = "no report card yet", gov = "not recorded",
              ofsted_combined = "never inspected, or not recorded")

#' The attendance, results and intake measures of the original area
#' documents (load_area_v1()), added to the area's schools for the map. The
#' original project keys schools by their published URN, so a school that
#' has since merged is matched on that.
map_extra <- function(w, X) {
  key <- function(src) {
    u <- as.integer(src$URN)
    i <- match(w$URN, u)
    if ("urn_published" %in% names(w)) i <- ifelse(is.na(i), match(w$urn_published, u), i)
    i
  }
  s <- X$schools |> transmute(URN, persistent, att8_dis, att8_non, eal, sen_support, sen_ehcp,
                              ofsted, religion = case_when(religion %in% c("Does not apply", "None") | is.na(religion) ~ "None",
                                                          religion %in% c("Church of England", "Roman Catholic", "Christian") ~ religion,
                                                          TRUE ~ "Other faith"),
                              pupils_roll = pupils)
  r <- X$sch_reasons |> transmute(URN = urn, illness, unauth = unauth_other, reduced)
  e <- X$excl_schools |> filter(time_period == max(time_period)) |> transmute(URN = urn, susp_rate)
  for (src in list(s, r, e)) {
    i <- key(src)
    for (v in setdiff(names(src), c("URN", names(w)))) w[[v]] <- src[[v]][i]
  }
  w
}

#' The area's schools within its district boundaries, coloured by a measure
#' chosen from a menu above the map (value added v2 by default)
map_schools <- function(w, lads, height = 520, X = NULL) {
  COLOUR_CAPS <- colour_caps()
  if (!is.null(X)) w <- map_extra(w, X)
  lad_c <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(lads)))
  bb <- sf::st_bbox(lads)
  dat <- w |>
    mutate(pupils = coalesce(pupils, 900),
           tip = sprintf(paste0("<b>%s</b> (%s)<br>Value added v2: all %s, disadvantaged %s, girls %s, boys %s",
                                "<br>Value added v1 (published): %s<br>Attainment 8 %.1f, FSM %.0f%%, absence %.1f%%%s",
                                "<br>Ofsted latest: %s%s"),
                         SCHNAME, coalesce(lad, LANAME),
                         ifelse(is.na(va_shrunk_all), "n/a", sgn(va_shrunk_all)),
                         ifelse(is.na(va_shrunk_disadvantaged), "n/a", sgn(va_shrunk_disadvantaged)),
                         ifelse(is.na(va_shrunk_girls), "n/a", sgn(va_shrunk_girls)),
                         ifelse(is.na(va_shrunk_boys), "n/a", sgn(va_shrunk_boys)),
                         ifelse(is.na(v1_all), "n/a", sgn(v1_all)), att8, fsm, absence,
                         if ("abs_excess" %in% names(w)) ifelse(is.na(abs_excess), "",
                           sprintf(" (%s pp against intake)", sgn(abs_excess))) else "",
                         coalesce(ofsted_latest, "not known"),
                         ifelse(is.na(run_by), "", paste0("<br>Run by (2024/25): ", run_by)))) |>
    select(SCHNAME, lon, lat, pupils, tip, any_of(MAP_VARS$var))
  payload <- list(
    schools = jsonlite::toJSON(dat, dataframe = "rows", na = "null", digits = 4),
    vars = jsonlite::toJSON(MAP_VARS |> filter(var %in% names(dat)) |>
                              mutate(cap = unname(COLOUR_CAPS[var])), dataframe = "rows", na = "null"),
    cats = jsonlite::toJSON(lapply(MAP_CATS, as.list), auto_unbox = TRUE),
    heads = jsonlite::toJSON(lapply(MAP_CAT_HEADS, as.list), auto_unbox = TRUE),
    none = jsonlite::toJSON(as.list(MAP_NONE), auto_unbox = TRUE))
  leaflet::leaflet(height = height, options = leaflet::leafletOptions(scrollWheelZoom = FALSE)) |>
    leaflet::addProviderTiles("Esri.WorldGrayCanvas") |>
    leaflet::addPolygons(data = lads, color = DPURPLE, weight = 1.5, fill = TRUE,
                         fillOpacity = 0.03, fillColor = DPURPLE, label = ~lad) |>
    leaflet::addLabelOnlyMarkers(lng = lad_c[, 1], lat = lad_c[, 2], label = lads$lad,
      labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE,
        style = list(color = DPURPLE, "font-weight" = "bold", "font-size" = "12px"))) |>
    leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) |>
    htmlwidgets::onRender(MAP_JS, data = payload)
}
