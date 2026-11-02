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

#' Every school in England on absence against intake and value added with
#' attendance counted in, the area's schools picked out and named
fig_plane <- function(js, focus_urns, title = "Attendance and value added go together school by school") {
  sel <- js |> filter(URN %in% focus_urns)
  xl <- quantile(js$abs_excess, c(0.005, 0.995)); yl <- quantile(js$va_m2, c(0.005, 0.995))
  xl <- c(min(xl[1], -4), max(xl[2], 4.5)); yl <- c(min(yl[1], -8), max(yl[2], 9))
  ggplot(js, aes(abs_excess, va_m2)) +
    geom_point(colour = LIGHT, size = 0.8, alpha = 0.6) +
    geom_hline(yintercept = 0, colour = GREY) + geom_vline(xintercept = 0, colour = GREY) +
    geom_point(data = sel, fill = DPURPLE, shape = 21, colour = "white", size = 3.4) +
    ggrepel::geom_text_repel(data = sel, aes(label = SCHNAME), size = 2.6, max.overlaps = 50,
                             segment.colour = GREY, colour = INK) +
    annotate("text", x = xl[1], y = yl[2], hjust = 0, vjust = 1, size = 3, colour = DPURPLE,
             label = "Adds value, attendance\nbetter than intake predicts") +
    annotate("text", x = xl[2], y = yl[2], hjust = 1, vjust = 1, size = 3, colour = BPURPLE,
             label = "Adds value despite\nworse attendance") +
    annotate("text", x = xl[2], y = yl[1], hjust = 1, vjust = 0, size = 3, colour = BLUE,
             label = "Below expectation,\nattendance worse") +
    annotate("text", x = xl[1], y = yl[1], hjust = 0, vjust = 0, size = 3, colour = GREY,
             label = "Below expectation despite\nbetter attendance") +
    coord_cartesian(xlim = xl, ylim = yl) +
    labs(x = "Absence above what the intake predicts (percentage points)",
         y = "Value added v2.0, attendance counted in (points)", title = title) +
    theme_note()
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
