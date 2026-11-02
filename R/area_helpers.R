# area_helpers.R - The original area documents' charts and tables, on v2.0
# ---------------------------------------------------------------------------
# Ported from school_attainment_tool's R/area_report_helpers.R. Each now
# reads Value Added v2.0 and the v2 absence-against-intake measure, in the
# site's palette. Sourced by the area pages after site_helpers.R.
# ---------------------------------------------------------------------------

#' The original project's area file for a place: absence series, reasons,
#' suspensions and profiles. These are data, not model output, so v2 reuses them.
load_area_v1 <- function(slug) readRDS(sat("data", paste0("area_", slug, ".rds")))
load_place <- function() readRDS(here::here("data", "place_absence.rds"))

#' Suspensions counted both ways for a set of schools: episodes per 100
#' pupils and pupils suspended at least once per 100 (the DfE file counts
#' episodes, and one pupil can be suspended several times)
susp_both <- function(es) {
  es <- es[!is.na(es$headcount) & es$headcount > 0, ]
  hc <- sum(es$headcount); su <- sum(es$suspension, na.rm = TRUE); pu <- sum(es$one_plus, na.rm = TRUE)
  list(schools = nrow(es), susp_rate = 100 * su / hc, pupil_rate = 100 * pu / hc,
       per_pupil = if (pu > 0) su / pu else NA_real_)
}

#' A district's rank among England's districts with five or more schools
#' (1 = highest); a smaller district is placed among them
district_rank <- function(lad_profiles, year, lad, col, dir = -1) {
  lp <- lad_profiles[lad_profiles$year_label == year, ]
  v <- lp[[col]][lp$lad == lad]
  others <- lp[lp$schools >= 5 & lp$lad != lad, ]
  list(r = 1 + sum(dir * others[[col]] < dir * v, na.rm = TRUE), n = nrow(others) + 1)
}

decomp_table <- function(dec, a) {
  dec |>
    filter(area == a) |>
    mutate(component = case_when(component %in% c("Region", "Local authority") ~ "Place (region and local authority)",
                                 component %in% c("Year", "Admissions (selection)") ~ "Other model terms",
                                 TRUE ~ as.character(component))) |>
    group_by(group, component) |> summarise(points = sum(points), .groups = "drop") |>
    pivot_wider(names_from = group, values_from = points) |>
    arrange(`All pupils`) |>
    mutate(across(where(is.numeric), ~sprintf("%+.2f", .x))) |>
    rename(Factor = component) |>
    kt(align = "lrr")
}

area_cols <- function(areas) {
  extra <- setdiff(areas, names(AREA_COL))
  c(AREA_COL, setNames(rep(MPURPLE, length(extra)), extra))[c(areas, "England")]
}

fig_abs_series <- function(ser, areas, title) {
  ser |>
    filter(area %in% c(areas, "England")) |>
    mutate(year = yr(time_period), area = factor(area, c(areas, "England"))) |>
    ggplot(aes(year, abs_rate, colour = area, group = area)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    scale_colour_manual(values = area_cols(areas), name = NULL) +
    labs(x = NULL, y = "Overall absence, % of sessions", title = title) +
    theme_note() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

fig_reasons <- function(rs, areas, title) {
  w <- rs |> select(area, reason, pp) |> pivot_wider(names_from = area, values_from = pp)
  top <- w |> arrange(desc(England)) |> slice_head(n = 10) |> pull(reason)
  rs |> filter(reason %in% top, area %in% c(areas, "England")) |>
    mutate(reason = factor(reason, rev(top)), area = factor(area, c(areas, "England"))) |>
    ggplot(aes(pp, reason, fill = area)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    scale_fill_manual(values = area_cols(areas), name = NULL) +
    labs(x = "% of possible sessions", y = NULL, title = title) +
    theme_note()
}

#' The joint-signal plane as an interactive chart: every school in England
#' in grey, the area's schools coloured by authority, each with its figures
#' on hover; drag to zoom, double-click to reset
fig_plane_live <- function(js, focus) {
  xl <- quantile(js$abs_excess, c(0.005, 0.995), na.rm = TRUE); yl <- quantile(js$va_m2, c(0.005, 0.995), na.rm = TRUE)
  xl <- c(min(xl[1], -4), max(xl[2], 4.5)); yl <- c(min(yl[1], -8), max(yl[2], 9))
  bg <- js |> filter(!URN %in% focus$URN, !is.na(abs_excess), !is.na(va_m2))
  fg <- focus |> filter(!is.na(abs_excess), !is.na(va_m2)) |>
    mutate(tip = sprintf("<b>%s</b><br>%s<br>Value added, attendance counted in: %s<br>Absence against intake: %s pp<br>Value added v2.0: %s",
                         SCHNAME, coalesce(lad, LANAME), sgn(va_m2), sgn(abs_excess),
                         ifelse(is.na(va_shrunk_all), "n/a", sgn(va_shrunk_all))))
  cols <- area_cols(unique(fg$LANAME))
  corner <- function(x, y, xa, ya, txt, col) list(x = x, y = y, xref = "paper", yref = "paper", xanchor = xa, yanchor = ya, text = txt,
    showarrow = FALSE, font = list(size = 11, color = col))
  p <- plotly::plot_ly() |>
    plotly::add_markers(data = bg, x = ~abs_excess, y = ~va_m2, name = "Every school in England",
                        marker = list(color = LIGHT, size = 4, opacity = 0.6),
                        text = ~SCHNAME, hovertemplate = "%{text}<extra></extra>")
  for (la in unique(fg$LANAME)) p <- p |>
    plotly::add_markers(data = fg |> filter(LANAME == la), x = ~abs_excess, y = ~va_m2, name = la,
                        marker = list(color = unname(cols[la]), size = 11, line = list(color = "white", width = 1)),
                        text = ~tip, hovertemplate = "%{text}<extra></extra>")
  p |> plotly::layout(
    xaxis = list(title = "Absence above what the intake predicts (percentage points)", range = xl,
                 zeroline = TRUE, zerolinecolor = GREY, gridcolor = "#ece8f3"),
    yaxis = list(title = "Value added v2.0, attendance counted in (points)", range = yl,
                 zeroline = TRUE, zerolinecolor = GREY, gridcolor = "#ece8f3"),
    legend = list(orientation = "h", y = -0.15), font = list(family = "inherit", color = INK),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    annotations = list(
      corner(0, 1, "left", "top", "Adds value, attendance<br>better than intake predicts", DPURPLE),
      corner(1, 1, "right", "top", "Adds value despite<br>worse attendance", BPURPLE),
      corner(1, 0, "right", "bottom", "Below expectation,<br>attendance worse", BLUE),
      corner(0, 0, "left", "bottom", "Below expectation despite<br>better attendance", GREY))) |>
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

#' How many of the area's schools sit in each quadrant of the plane
plane_quadrants <- function(focus) {
  f <- focus |> filter(!is.na(abs_excess), !is.na(va_m2))
  c(good_both = sum(f$va_m2 > 0 & f$abs_excess <= 0), despite = sum(f$va_m2 > 0 & f$abs_excess > 0),
    below_worse = sum(f$va_m2 <= 0 & f$abs_excess > 0), below_better = sum(f$va_m2 <= 0 & f$abs_excess <= 0),
    n = nrow(f))
}

fig_governance <- function(g, title = "Schools by who ran them in 2024/25") {
  ord_g <- g |> mutate(grp = case_when(grepl("^Maintained", run_by) ~ 1,
                                       grepl("^Single|not recorded", run_by) ~ 3, TRUE ~ 2)) |>
    group_by(run_by, grp) |> summarise(n = n(), .groups = "drop") |> arrange(grp, desc(n), run_by) |> pull(run_by)
  wrap_lab <- function(x) sapply(x, function(s) paste(strwrap(s, 24), collapse = "\n"))
  g |>
    mutate(run_by = factor(wrap_lab(run_by), wrap_lab(ord_g)), SCHNAME = reorder(SCHNAME, va),
           route = factor(route, c("Maintained", "Converter academy", "Sponsor-led academy",
                                   "Free school, UTC or studio"))) |>
    ggplot(aes(va, SCHNAME)) +
    geom_vline(xintercept = 0, colour = GREY) +
    geom_segment(aes(x = 0, xend = va, yend = SCHNAME), colour = LIGHT) +
    geom_point(aes(shape = route), fill = MPURPLE, size = 3.2, colour = DPURPLE, stroke = 0.4) +
    facet_grid(run_by ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_shape_manual(values = c(21, 22, 24, 23), name = NULL, drop = FALSE) +
    scale_y_discrete(position = "right") +
    labs(x = "Value added v2.0, all pupils (Attainment 8 points, against England)", y = NULL, title = title) +
    theme_note(10) +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", colour = DPURPLE, size = 8),
          panel.spacing.y = unit(2, "pt"), panel.grid.major.y = element_blank())
}

fig_effects <- function(eff, title) {
  eff |>
    mutate(spec = factor(spec, c("Published specification", "With a school term"),
                         c("Without a school term", "With a school term"))) |>
    ggplot(aes(pct(effect), lad, colour = spec)) +
    geom_vline(xintercept = 0, colour = GREY) +
    geom_errorbarh(aes(xmin = pct(ci_lo), xmax = pct(ci_hi)), height = 0,
                   position = position_dodge(width = 0.6), linewidth = 0.8) +
    geom_point(position = position_dodge(width = 0.6), size = 2.4) +
    facet_wrap(~outcome, scales = "free_x") +
    scale_colour_manual(values = c(BPURPLE, BLUE), name = NULL) +
    labs(x = "District effect, %", y = NULL, title = title) +
    theme_note(10)
}

fig_trusts <- function(te, title) {
  te |>
    mutate(label = sprintf("%s (%d)", name, schools_in_data),
           sigc = ifelse(sig, "Distinguishable from average", "Not distinguishable")) |>
    group_by(label) |> mutate(o = effect[outcome == "All pupils"][1]) |> ungroup() |>
    ggplot(aes(pct(effect), reorder(label, o), colour = sigc)) +
    geom_vline(xintercept = 0, colour = GREY) +
    geom_errorbarh(aes(xmin = pct(ci_lo), xmax = pct(ci_hi)), height = 0) +
    geom_point(size = 2.4) +
    facet_wrap(~outcome, scales = "free_x") +
    scale_colour_manual(values = c("Distinguishable from average" = BPURPLE, "Not distinguishable" = GREY),
                        name = NULL) +
    labs(x = "Trust effect, %", y = NULL, title = title) +
    theme_note(10)
}

tidy_trust <- function(x) ifelse(is.na(x), NA_character_, sub(" Limited$", "", gsub("\\bOf\\b", "of",
  gsub("\\bAnd\\b", "and", tools::toTitleCase(tolower(x))))))

#' League table: v2 value added by year for one group, then the shrunk
#' four-year figure with its interval
va_by_year <- function(V, urns, g, names) {
  pv <- load_prov()
  prov <- if (is.null(pv)) tibble(URN = integer(), Prov = numeric()) else
    pv$va |> filter(group == g) |> select(URN, Prov = va_2526)
  V$school_years |> filter(URN %in% urns, group == g) |>
    select(URN, year_label, va_nat) |>
    mutate(year_label = yl(year_label)) |>
    pivot_wider(names_from = year_label, values_from = va_nat) |>
    left_join(V$va |> filter(group == g) |> select(URN, Shrunk = va_shrunk, lo = va_lo, hi = va_hi),
              by = "URN") |>
    left_join(prov, by = "URN") |>
    left_join(names, by = "URN") |>
    arrange(desc(Shrunk)) |>
    mutate(`95% interval` = sprintf("%+.1f to %+.1f", lo, hi),
           across(where(is.double), ~ifelse(is.na(.x), "", sprintf("%+.1f", .x)))) |>
    select(School = SCHNAME, starts_with("20"), `v2.0, four years (shrunk)` = Shrunk, `95% interval`,
           `2025/26 (provisional)` = Prov)
}

#' Each school's value added year by year (points), its three-year centred
#' moving average (line), and the provisional year as a hollow point
fig_trajectories <- function(yrs, w, g = "all", n = 3) {
  y <- yrs |> filter(URN %in% w$URN, group == g) |> left_join(w |> select(URN, SCHNAME), by = "URN")
  years <- sort(unique(y$year_label))
  ma <- bind_rows(lapply(years, function(c) centred_ma(y, c, n) |> filter(group == g))) |>
    left_join(w |> select(URN, SCHNAME), by = "URN") |> rename(year_label = centre)
  ord_s <- w |> arrange(desc(.data[[paste0("va_shrunk_", g)]])) |> pull(SCHNAME)
  y <- y |> mutate(SCHNAME = factor(SCHNAME, ord_s), yr = yl(year_label))
  ma <- ma |> mutate(SCHNAME = factor(SCHNAME, ord_s), yr = yl(year_label))
  ggplot(y, aes(yr, va)) +
    geom_hline(yintercept = 0, colour = GREY, linewidth = 0.3) +
    geom_line(data = ma, aes(y = ma, group = SCHNAME), colour = MPURPLE, linewidth = 1) +
    geom_point(aes(shape = provisional), colour = DPURPLE, size = 1.8) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = c("Final", "Provisional (2025/26)"), name = NULL) +
    facet_wrap(~SCHNAME, ncol = 4, labeller = label_wrap_gen(28)) +
    labs(x = NULL, y = "Value added (Attainment 8 points)",
         title = paste0("Year by year (points) and ", n, "-year centred average (line), ", tolower(GROUP_LABELS[[g]]))) +
    theme_note(9) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
                          strip.text = element_text(size = 7.5))
}

#' The area's mean value added by year, for every group
fig_area_years <- function(yrs, w, name) {
  yrs |> filter(URN %in% w$URN) |>
    group_by(group, year_label, provisional) |> summarise(va = mean(va, na.rm = TRUE), .groups = "drop") |>
    mutate(group = factor(GROUP_LABELS[group], GROUP_LABELS), yr = yl(year_label)) |>
    ggplot(aes(yr, va, colour = group, group = group)) +
    geom_hline(yintercept = 0, colour = GREY) +
    geom_line(linewidth = 1) +
    geom_point(aes(shape = provisional), size = 2.6, fill = "white") +
    scale_colour_manual(values = GROUP_COL, name = NULL) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 21), labels = c("Final", "Provisional"), name = NULL) +
    labs(x = NULL, y = "Mean value added (points)", title = paste0(name, ": the schools' average, year by year")) +
    theme_note()
}
