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
  V$schools |> inner_join(w, by = "URN") |> left_join(v1, by = "URN")
}

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
  "v1_all",          "Value added v1 (published, within the LA), all pupils",    "Value added",    "div",  "high", 1,   " pts",
  "att8",            "Attainment 8, all pupils",                                 "Results",        "seq",  "",     1,   "",
  "fsm",             "Disadvantaged pupils (FSM6) %",                            "Intake",         "seq",  "",     1,   "%",
  "ks2_c",           "Prior attainment at KS2 (centred)",                        "Intake",         "seq",  "",     1,   "",
  "absence",         "Overall absence %",                                        "Intake",         "seq",  "",     1,   "%",
  "mobile",          "Pupil mobility %",                                         "Intake",         "seq",  "",     1,   "%",
  "l4",              "Neighbourhood education (% level 4+)",                     "Intake",         "seq",  "",     1,   "%",
  "gov",             "Who runs it, 2024/25",                                     "The school",     "cat",  "",     0,   "",
  "ofsted_rc",       "Ofsted report card, achievement (since November 2025)",    "The school",     "cat",  "",     0,   "")

MAP_CATS <- list(
  gov = c("Maintained" = DPURPLE, "Multi-academy trust, converter or free school" = BPURPLE,
          "Multi-academy trust, sponsored" = BLUE, "Single-academy trust or not recorded" = GREY),
  ofsted_rc = c("Exceptional" = DPURPLE, "Strong standard" = BPURPLE, "Expected standard" = LPURPLE,
                "Needs attention" = HERITAGE, "Urgent improvement" = BLUE))
MAP_NONE <- c(ofsted_rc = "no report card yet", gov = "not recorded")

#' The area's schools within its district boundaries, coloured by a measure
#' chosen from a menu above the map (value added v2 by default)
map_schools <- function(w, lads, height = 520) {
  lad_c <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(lads)))
  bb <- sf::st_bbox(lads)
  dat <- w |>
    mutate(pupils = coalesce(pupils, 900),
           tip = sprintf(paste0("<b>%s</b> (%s)<br>Value added v2: all %s, disadvantaged %s, girls %s, boys %s",
                                "<br>Value added v1 (published): %s<br>Attainment 8 %.1f, FSM %.0f%%, absence %.1f%%",
                                "<br>Ofsted latest: %s%s"),
                         SCHNAME, coalesce(lad, LANAME),
                         ifelse(is.na(va_shrunk_all), "n/a", sgn(va_shrunk_all)),
                         ifelse(is.na(va_shrunk_disadvantaged), "n/a", sgn(va_shrunk_disadvantaged)),
                         ifelse(is.na(va_shrunk_girls), "n/a", sgn(va_shrunk_girls)),
                         ifelse(is.na(va_shrunk_boys), "n/a", sgn(va_shrunk_boys)),
                         ifelse(is.na(v1_all), "n/a", sgn(v1_all)), att8, fsm, absence,
                         coalesce(ofsted_latest, "not known"),
                         ifelse(is.na(run_by), "", paste0("<br>Run by (2024/25): ", run_by)))) |>
    select(SCHNAME, lon, lat, pupils, tip, any_of(MAP_VARS$var))
  payload <- list(
    schools = jsonlite::toJSON(dat, dataframe = "rows", na = "null", digits = 4),
    vars = jsonlite::toJSON(MAP_VARS |> filter(var %in% names(dat)), dataframe = "rows"),
    cats = jsonlite::toJSON(lapply(MAP_CATS, as.list), auto_unbox = TRUE),
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
