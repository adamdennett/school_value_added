# 01_v21.R - Shared pieces for v2.1's two tabs: Pupils and Places, and School Finances
# ---------------------------------------------------------------------------
# Data: data/v21.rds, written by R/46_app_v21_data.R in the school_value_added project
# from the demographics page (R/42) and the finance pages (R/43, R/44). Sourced after
# 00_setup.R (Shiny sources R/ in alphabetical order).
#
# These figures are for England, regions and local authorities (and, for finance, the
# Sussex districts so far). The shared place is turned into one of those by v21_unit():
# a district gives its authority, and three reorganised councils are shown combined, as
# on the site, so their series run unbroken.
# ---------------------------------------------------------------------------

V21 <- readRDS("data/v21.rds")
COMBINE <- V21$combine
MIN_SCHOOLS <- 5                       # no share, median or line from fewer than 5 schools
T0 <- V21$t0                           # 2018/19, the base year for pupil trends
SUSSEX_LADS <- with(V21$fin$units, unit[level == "District"])
SUSSEX_LAS <- c("Brighton and Hove", "East Sussex", "West Sussex")
LA_REGION <- with(distinct(SCH, la, region), setNames(region, la))
V21_LEVELS <- LEVELS[c("England", "Region", "Local authority", "District")]

# The shared place as one of the areas these tabs have: list(level, area, parent, region, note)
v21_unit <- function(level, area, districts = FALSE) {
  if (is.null(level) || level == "eng") return(list(level = "England", area = "England", parent = NULL, region = NULL, note = NULL))
  if (level == "region") return(list(level = "Region", area = area, parent = NULL, region = NULL, note = NULL))
  if (level == "lad" && districts && area %in% SUSSEX_LADS) {
    p <- with(V21$fin$units, parent[level == "District" & unit == area])
    return(list(level = "District", area = area, parent = p, region = LA_REGION[[p]], note = NULL))
  }
  la <- if (level == "la") area else if (level == "lad") place_la("lad", area) else NULL
  if (is.null(la)) return(NULL)
  u <- if (la %in% names(COMBINE)) unname(COMBINE[[la]]) else la
  note <- c(if (level == "lad") paste0("Figures by district are ", if (districts) "only for the Sussex districts so far" else "not published",
                                        "; showing ", area, "'s authority, ", u, "."),
            if (u != la) paste0(la, " is shown with its neighbours as ", u, ", so the series runs unbroken across the reorganisation."))
  list(level = "Authority", area = u, parent = NULL, region = LA_REGION[[la]], note = if (length(note)) paste(note, collapse = " "))
}
# Each app authority's area in these data (combined where reorganised)
v21_area_of_la <- function(la) ifelse(la %in% names(COMBINE), unname(COMBINE[la]), la)

# The area and what it is compared with, in drawing order, with a fixed colour per role
ROLE_COL <- c(area = DPURPLE, parent = BPURPLE, region = MPURPLE, england = GREY)
ROLE_LW <- c(area = 1.4, parent = 0.8, region = 0.8, england = 0.8)
v21_comp <- function(u) {
  r <- data.frame(level = u$level, area = u$area, role = "area")
  if (!is.null(u$parent) && u$parent != u$area) r <- rbind(r, data.frame(level = "Authority", area = u$parent, role = "parent"))
  if (!is.null(u$region)) r <- rbind(r, data.frame(level = "Region", area = u$region, role = "region"))
  if (u$level != "England") r <- rbind(r, data.frame(level = "England", area = "England", role = "england"))
  r
}
yr0 <- function(y) as.integer(substr(y, 1, 4))                 # "2018-19" -> 2018
yr_lab <- function(x) paste0(x, "/", substr(x + 1, 3, 4))      # 2018 -> "2018/19"
yr_short <- function(x) paste0(substr(x, 3, 4), "/", substr(x + 1, 3, 4))   # 2018 -> "18/19", for axes

# Lines over time for the area and its comparisons; d has area, year and the column y
v21_lines <- function(d, y, ylab, r, hline = NULL, linetype = NULL, by = 2) {
  d <- d[!is.na(d[[y]]), , drop = FALSE]
  d$area <- factor(d$area, r$area); d$x <- yr0(d$year)
  d$grp <- paste(d$area, if (is.null(linetype)) "" else d[[linetype]])
  cols <- setNames(ROLE_COL[r$role], r$area); lws <- setNames(ROLE_LW[r$role], r$area)
  p <- ggplot(d, aes(x, .data[[y]], colour = area, group = grp))
  if (!is.null(hline)) p <- p + geom_hline(yintercept = hline, colour = LIGHT)
  p <- p + (if (is.null(linetype)) geom_line(aes(linewidth = area)) else geom_line(aes(linewidth = area, linetype = .data[[linetype]]))) +
    geom_point(size = 1.5) + scale_colour_manual(values = cols, name = NULL) + scale_linewidth_manual(values = lws, guide = "none") +
    scale_x_continuous(labels = yr_short, breaks = function(l) seq(ceiling(l[1]), floor(l[2]), by = by)) +
    labs(x = NULL, y = ylab) + theme_sis(12) + theme(legend.position = "top", legend.box = "vertical", legend.margin = margin(0, 0, 0, 0))
  p
}

# A choropleth of England's authorities (the app's boundaries), coloured by value, with
# the chosen ones outlined. vals: a named vector by area in these data (combined names);
# dom: the colour domain (values beyond it take the end colour).
v21_map <- function(proxy, vals, pal, dom, chosen_las, title, unit = "", fmt = function(v) f1(v)) {
  b <- D$bounds; v <- unname(vals[v21_area_of_la(b$la)])
  vc <- pmin(pmax(v, dom[1]), dom[2])                         # colours stop at the domain; labels keep the value
  lab <- paste0(b$la, ": ", ifelse(is.na(v), "too few schools or no data", paste0(fmt(v), unit)))
  m <- proxy |> clearShapes() |> clearControls() |>
    addPolygons(data = b, layerId = ~la, fillColor = pal(vc), fillOpacity = 0.85, color = "#ffffff", weight = 0.6,
                label = lab, highlightOptions = highlightOptions(weight = 2, color = DPURPLE, bringToFront = TRUE))
  ch <- b[b$la %in% chosen_las, ]
  if (nrow(ch)) m <- m |> addPolylines(data = ch, color = DPURPLE, weight = 2.6, opacity = 1, group = "chosen")
  m |> addLegend("bottomright", pal = pal, values = dom, title = title, opacity = 0.9, labFormat = labelFormat(suffix = unit))
}
v21_fit <- function(proxy, las) {
  if (!length(las)) return(setView(proxy, -1.5, 52.8, 6))
  bb <- sf::st_bbox(D$bounds[D$bounds$la %in% las, ])
  fitBounds(proxy, bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
}
# The app authorities a v21 unit covers (for outlining and zooming)
v21_las <- function(u) {
  if (is.null(u) || u$level == "England") return(character(0))
  if (u$level == "Region") return(names(LA_REGION)[LA_REGION == u$area])
  a <- if (u$level == "District") u$parent else u$area
  D$bounds$la[v21_area_of_la(D$bounds$la) == a]
}

# Colours with no better or worse: falls brown, rises teal (ColorBrewer BrBG), and a
# fixed cap so one extreme area does not wash out the rest
BRBG <- c("#8c510a", "#d8b365", "#f6e8c3", "#f5f5f5", "#c7eae5", "#5ab4ac", "#01665e")
div_dom <- function(values) { cap <- max(1, unname(quantile(abs(values), 0.975, na.rm = TRUE))); c(-cap, cap) }
pct <- function(x, d = 0) ifelse(is.na(x), "–", paste0(f1(x, d), "%"))
spct <- function(x, d = 1) ifelse(is.na(x), "–", paste0(sg(x, d), "%"))
kpi <- function(label, value, sub = NULL, cls = "") div(class = paste("sis-kpi", cls), div(class = "sis-kpi-l", label), div(class = "sis-kpi-v", value), if (!is.null(sub)) div(class = "sis-kpi-s", sub))
