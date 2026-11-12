# 00_setup.R - School Information System v2.0: data, settings and shared helpers
# ---------------------------------------------------------------------------
# Runs once when the app starts (Shiny sources R/ in alphabetical order, so this comes first). Everything the app shows comes from one bundle,
# data/app_data.rds, written by R/40_app_data.R in the school_value_added project
# (see that script's header for what each table is and where it comes from).
# The modules are the other files in R/ (mod_*.R).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(leaflet); library(dplyr); library(tidyr)
  library(ggplot2); library(plotly); library(DT); library(sf); library(htmltools)
})

D <- readRDS("data/app_data.rds")
SCH <- D$schools

# Optional: the national walking and cycling routes summarised by school, from the
# school_active_travel project (R/41_app_travel_national.R). Partial until that run ends.
TN <- if (file.exists("data/travel_national.rds")) readRDS("data/travel_national.rds") else NULL

VERSION <- "v2.0"
VA_VERSION <- D$model$version                     # the value-added model, v2.1
SITE <- "https://adamdennett.github.io/school_value_added"

# ---- colours: the current UCL palette, led by its lighter purples --------------------
INK <- "#1a1a2e"; DPURPLE <- "#361a54"; BPURPLE <- "#993bff"; MPURPLE <- "#ba82ff"
LPURPLE <- "#ddbdff"; PPURPLE <- "#eedeff"; BLUE <- "#1f7fa0"; GREY <- "#6b6680"; LIGHT <- "#d3cde0"
# Better and worse on maps and charts: ColorBrewer PRGn (purple worse, green better),
# readable for those who cannot tell red from green, as on the site's maps
PRGN <- c("#762a83", "#9970ab", "#c2a5cf", "#e7d4e8", "#f7f7f7", "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837")
PRGN_SEQ <- PRGN[-5]
NEUTRAL <- c("#f1f1f5", "#9e9eb0", "#3a3a4e")
UP <- "#1b7837"; DOWN <- "#762a83"
GROUP_COL <- c("All pupils" = DPURPLE, "Disadvantaged" = BPURPLE, "Non-disadvantaged" = MPURPLE,
               "Girls" = BLUE, "Boys" = GREY)

# ---- groups, years and periods -------------------------------------------------------
GROUPS <- c("All pupils" = "all", "Disadvantaged" = "disadvantaged", "Non-disadvantaged" = "non_disadvantaged",
            "Girls" = "girls", "Boys" = "boys")
GLAB <- setNames(names(GROUPS), GROUPS)
MAIN3 <- c("all", "disadvantaged", "non_disadvantaged")
YEARS <- sort(unique(D$years$year))                 # model years, then provisional 2025/26
MODEL_YEARS <- D$model$years
PROV_YEAR <- D$model$prov_year
yl <- function(x) sub("-", "/", x)
YEAR_CHOICES <- setNames(YEARS, ifelse(!is.na(PROV_YEAR) & YEARS == PROV_YEAR, paste(yl(YEARS), "(provisional)"), yl(YEARS)))

# The years a window of n (odd) centred on `centre` covers, cut at the ends of the series
window_years <- function(centre, n) {
  i <- match(centre, YEARS); if (is.na(i)) i <- length(YEARS); h <- (n - 1) %/% 2
  YEARS[max(1, i - h):min(length(YEARS), i + h)]
}
period_label <- function(period, year, window) {
  switch(period,
    four = sprintf("four years, %s to %s, shrunk", yl(MODEL_YEARS[1]), yl(tail(MODEL_YEARS, 1))),
    year = paste0(yl(year), if (identical(year, PROV_YEAR)) " (provisional)" else ""),
    ma = { w <- window_years(year, window)
           sprintf("%d-year average centred on %s (%s)", window, yl(year),
                   if (length(w) > 1) paste(yl(w[1]), "to", yl(tail(w, 1))) else yl(w)) })
}

# ---- each school's measures for a group and period ----------------------------------------
# Four years: value added shrunk by reliability, with its 95% interval; mean Attainment 8
# and mean expected. One year or a moving average: the yearly (unshrunk) figures, averaged
# over whichever years in the window the school has. Absence against intake is for all
# pupils in every case (the absence model is not fitted by group), and has no 2025/26.
.mcache <- new.env()
metrics <- function(group = "all", period = "four", year = tail(MODEL_YEARS, 1), window = 3) {
  key <- paste(group, period, year, window); if (!is.null(.mcache[[key]])) return(.mcache[[key]])
  if (period == "four") {
    m <- D$four[D$four$group == group, ] |> transmute(URN, att8, exp, va, lo, hi, n_years = years, prov = FALSE)
    a <- D$absence$four |> transmute(URN, abs_obs = obs, abs_exp = exp, abs_excess = excess)
  } else {
    win <- window_years(year, if (period == "year") 1 else window)
    m <- D$years[D$years$group == group & D$years$year %in% win, ] |> group_by(URN) |>
      summarise(att8 = mean(att8), exp = mean(exp), va = mean(va), n_years = n(), prov = any(prov), .groups = "drop") |>
      mutate(lo = NA_real_, hi = NA_real_)
    a <- D$absence$years[D$absence$years$year %in% win, ] |> group_by(URN) |>
      summarise(abs_obs = mean(obs), abs_exp = mean(exp), abs_excess = mean(excess), .groups = "drop")
  }
  out <- SCH |> left_join(m, by = "URN") |> left_join(a, by = "URN")
  .mcache[[key]] <- out
  out
}

# ---- places ------------------------------------------------------------------------------
LEVELS <- c("England" = "eng", "Region" = "region", "Combined or mayoral authority" = "ca",
            "Local authority" = "la", "District" = "lad", "Academy trust" = "trust")
LEVEL_LAB <- setNames(names(LEVELS), LEVELS)
area_choices <- function(level) {
  if (level == "eng") return("England")
  x <- SCH[[level]]; x <- x[!is.na(x)]
  if (level == "trust") { n <- table(x); x <- names(n)[n >= 2] }
  sort(unique(x))
}
DEFAULT_AREA <- c(region = "South East", ca = "Sussex and Brighton (new)", la = "Brighton and Hove",
                  lad = "Brighton and Hove", trust = "United Learning Trust", eng = "England")
AREA_CHOICES <- lapply(LEVELS, area_choices) |> setNames(LEVELS)
# Is this area one of the level's? (Briefly not, while a new level's areas reach the browser)
area_ok <- function(level, area) !is.null(level) && (level == "eng" || (!is.null(area) && area %in% AREA_CHOICES[[level]]))
in_area <- function(df, level, area) {
  if (is.null(level) || level == "eng" || is.null(area) || !nzchar(area)) return(df)
  df[!is.na(df[[level]]) & df[[level]] == area, , drop = FALSE]
}
# An area picker: a level and an area within it; the server half keeps the areas in step
area_picker_ui <- function(ns, level = "la", levels = LEVELS, label = "Area") {
  tagList(selectInput(ns("level"), "Area type", levels, selected = level),
          selectizeInput(ns("area"), label, choices = area_choices(level), selected = DEFAULT_AREA[[level]]))
}
# `place` is the session's shared choice (reactiveValues level, area): a choice made in any
# tab's picker is written to it, and every other tab's picker follows it. A tab whose picker
# lacks a level (Area Overview has no "England") keeps its own choice until one it has comes.
area_picker_server <- function(input, session, place = NULL, levels = LEVELS) {
  # a new level: its areas, keeping the shared area when the shared level is this one
  observeEvent(input$level, {
    ch <- AREA_CHOICES[[input$level]]
    sel <- if (!is.null(place) && identical(place$level, input$level) && place$area %in% ch) place$area
           else if (DEFAULT_AREA[[input$level]] %in% ch) DEFAULT_AREA[[input$level]] else ch[1]
    updateSelectizeInput(session, "area", choices = ch, selected = sel)
  }, ignoreInit = TRUE)
  if (is.null(place)) return(invisible())
  # this tab -> everywhere (only once the level and area agree, so never half a change)
  observeEvent(list(input$level, input$area), {
    req(area_ok(input$level, input$area))
    if (!identical(place$level, input$level) || !identical(place$area, input$area)) {
      place$level <- input$level; place$area <- input$area }
  }, ignoreInit = TRUE)
  # everywhere -> this tab
  observeEvent(list(place$level, place$area), {
    lv <- place$level; ar <- place$area
    if (!(lv %in% levels)) return()
    if (!identical(input$level, lv)) updateSelectInput(session, "level", selected = lv)   # its observer above sets the area
    else if (!identical(input$area, ar)) updateSelectizeInput(session, "area", selected = ar)
  }, ignoreInit = TRUE)
}
# The local authority a shared place sits in, if it is one authority (a district's own), else NULL
place_la <- function(level, area) {
  if (identical(level, "la")) return(area)
  if (identical(level, "lad")) { la <- unique(SCH$la[SCH$lad == area]); if (length(la) == 1) return(la) }
  NULL
}
school_label <- function(s) paste0(s$name, " (", ifelse(s$lad == s$la, s$la, paste0(s$lad, ", ", s$la)), ")")
SCHOOL_CHOICES <- setNames(SCH$URN, school_label(SCH))[order(SCH$name)]
where_txt <- function(s) paste(c(if (s$lad != s$la) s$lad, s$la, if (!is.na(s$ca)) s$ca, s$region), collapse = " · ")

# ---- formats ----------------------------------------------------------------------------
f1 <- function(x, d = 1) ifelse(is.na(x), "–", formatC(round(x, d), format = "f", digits = d))
sg <- function(x, d = 1) ifelse(is.na(x), "–", sub("-", "−", sprintf(paste0("%+.", d, "f"), x)))
wmean <- function(x, w) { w <- ifelse(is.na(w) | w <= 0, 1, w); ok <- !is.na(x); if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok]) }

# ---- what a map or chart can show ---------------------------------------------------------
# scale: div (better/worse around zero), seq (better one way), neutral (no better or worse)
MEASURES <- tibble::tribble(
  ~key, ~label, ~short, ~unit, ~dp, ~scale, ~good,
  "va", "Value added (Attainment 8 points against England)", "Value added", " pts", 1, "div", "high",
  "att8", "Attainment 8", "Attainment 8", "", 1, "seq", "high",
  "exp", "Expected Attainment 8, given intake", "Expected Attainment 8", "", 1, "neutral", "",
  "abs_excess", "Absence against expected, all pupils (percentage points)", "Absence against expected", " pp", 1, "div", "low",
  "abs_obs", "Overall absence, all pupils (%)", "Absence", "%", 1, "seq", "low",
  "fsm", "Disadvantaged pupils (FSM6) %", "FSM6 %", "%", 1, "neutral", "",
  "ks2_c", "Prior attainment at KS2 (centred)", "KS2", "", 1, "neutral", "",
  "eal", "English as an additional language %", "EAL %", "%", 1, "neutral", "",
  "sen_support", "SEN support %", "SEN support %", "%", 1, "neutral", "",
  "pupils", "Pupils on roll", "Pupils", "", 0, "neutral", "")
MEASURE_CHOICES <- setNames(MEASURES$key, MEASURES$label)
mdef <- function(key) as.list(MEASURES[MEASURES$key == key, ])

# A fixed national cap for the diverging measures (97.5th percentile of |value| in
# England for the period shown), so one extreme school cannot wash out an area's colours
cap_for <- function(all, key) { v <- abs(all[[key]]); v <- v[!is.na(v)]; if (!length(v)) 1 else max(0.5, unname(quantile(v, 0.975))) }
measure_pal <- function(key, values, cap = NULL) {
  m <- mdef(key)
  if (m$scale == "div") {
    cap <- cap %||% max(abs(values), na.rm = TRUE); cols <- if (m$good == "high") PRGN else rev(PRGN)
    list(pal = colorNumeric(cols, c(-cap, cap), na.color = "#c9c9d6"), clamp = function(v) pmax(pmin(v, cap), -cap),
         domain = c(-cap, cap))
  } else {
    r <- range(values, na.rm = TRUE); if (!all(is.finite(r)) || diff(r) == 0) r <- c(0, 1) + (if (all(is.finite(r))) r[1] else 0)
    cols <- if (m$scale == "neutral") NEUTRAL else if (m$good == "high") PRGN_SEQ else rev(PRGN_SEQ)
    list(pal = colorNumeric(cols, r, na.color = "#c9c9d6"), clamp = identity, domain = r)
  }
}

# ---- maps: Esri's light grey canvas, as everywhere else in this work ------------------------
base_map <- function(lng = -1.5, lat = 52.8, zoom = 6) {
  leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
    addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(maxNativeZoom = 16, maxZoom = 18)) |>
    setView(lng, lat, zoom)
}
fit_to <- function(map, df) {
  df <- df[!is.na(df$lat) & !is.na(df$lon), ]
  if (!nrow(df)) return(map)
  if (nrow(df) == 1) return(setView(map, df$lon, df$lat, 13))
  fitBounds(map, min(df$lon) - 0.02, min(df$lat) - 0.02, max(df$lon) + 0.02, max(df$lat) + 0.02)
}
popup_school <- function(d, group_lab, period_txt) {
  paste0("<b>", htmlEscape(d$name), "</b><br><span style='color:#6b6680'>", htmlEscape(d$lad), ", ", htmlEscape(d$la), "</span><br>",
         group_lab, ", ", period_txt, "<br>Attainment 8 ", f1(d$att8), " (expected ", f1(d$exp), ")<br>",
         "Value added <b>", sg(d$va), "</b>",
         ifelse(is.na(d$lo), "", paste0(" (", sg(d$lo), " to ", sg(d$hi), ")")), "<br>",
         "Absence ", f1(d$abs_obs), "% against ", f1(d$abs_exp), "% expected")
}

# ---- charts ---------------------------------------------------------------------------------
theme_sis <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(plot.title = element_text(face = "bold", colour = DPURPLE), plot.title.position = "plot",
          plot.subtitle = element_text(colour = GREY, size = rel(0.85)),
          axis.title = element_text(colour = GREY, size = rel(0.85)), panel.grid.minor = element_blank(),
          legend.position = "bottom", strip.text = element_text(face = "bold", colour = DPURPLE, hjust = 0))
}
plotly_clean <- function(p) plotly::config(p, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))

# A small "about this figure" note under a chart
note <- function(...) tags$p(class = "sis-note", ...)
