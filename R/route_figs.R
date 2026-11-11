# route_figs.R - Figures for example journeys: distance.qmd and method.qmd
# ---------------------------------------------------------------------------
# Draws from data/route_examples.rds (R/34). Basemap tiles are Esri's light grey canvas, as the
# site's other maps, cached in data/cache/tiles; attribution goes in each figure's caption.
# ---------------------------------------------------------------------------

proj_db <- system.file("proj", package = "terra"); if (nzchar(proj_db)) Sys.setenv(PROJ_LIB = proj_db, PROJ_DATA = proj_db)
suppressPackageStartupMessages({ library(sf); library(ggspatial); library(patchwork) })
RX <- readRDS(here::here("data", "route_examples.rds"))
ESRI <- "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg"
TILE_DIR <- here::here("data", "cache", "tiles"); dir.create(TILE_DIR, showWarnings = FALSE, recursive = TRUE)
BASEMAP_CREDIT <- "Basemap: Esri, HERE, Garmin, © OpenStreetMap contributors."
ROUTE_CREDIT <- paste("Routes: r5r on OpenStreetMap and Bus Open Data Service timetables,", format(RX$date, "%e %B %Y."))
tiles <- function(z = 14) annotation_map_tile(type = ESRI, zoom = z, cachedir = TILE_DIR, progress = "none")
to_bng <- function(x) st_transform(x, 27700)
map_theme <- function() theme_void(base_size = 9) + theme(plot.title = element_text(face = "bold", size = 9, colour = INK),
  plot.subtitle = element_text(size = 7.5, colour = GREY), panel.border = element_rect(fill = NA, colour = LIGHT))
ends_sf <- function(i) { e <- RX$ends[RX$ends$id == i, ]
  list(home = to_bng(st_as_sf(e, coords = c("lon", "lat"), crs = 4326)), school = to_bng(st_as_sf(e, coords = c("s_lon", "s_lat"), crs = 4326))) }
bus_stops <- function(bus) if (!nrow(bus)) NULL else do.call(rbind, lapply(seq_len(nrow(bus)), function(k) {
  cc <- st_coordinates(bus[k, ]); st_as_sf(data.frame(x = cc[c(1, nrow(cc)), 1], y = cc[c(1, nrow(cc)), 2]), coords = c("x", "y"), crs = 27700) }))
route_layers <- function(l, bus_lab = TRUE) {
  bus <- l[l$mode != "WALK", ]; walk <- l[l$mode == "WALK", ]; st <- bus_stops(bus)
  lab <- if (nrow(bus) && bus_lab) st_sf(route = bus$route_no, geometry = st_cast(st_line_sample(st_geometry(bus), sample = 0.5), "POINT")) else NULL
  list(geom_sf(data = walk, colour = INK, linewidth = 0.7, linetype = "22"),
       if (nrow(bus)) geom_sf(data = bus, colour = BPURPLE, linewidth = 1.3),
       if (!is.null(st)) geom_sf(data = st, shape = 21, fill = "white", colour = BPURPLE, size = 1.8, stroke = 0.9),
       if (!is.null(lab)) geom_sf_label(data = lab, aes(label = route), size = 2.6, colour = "white", fill = BPURPLE, linewidth = 0, label.padding = unit(0.12, "lines")))
}
end_layers <- function(i) { e <- ends_sf(i)
  list(geom_sf(data = e$home, shape = 21, fill = INK, colour = "white", size = 3), geom_sf(data = e$school, shape = 22, fill = HERITAGE, colour = INK, size = 3.2)) }

# ---- Distance page: the example journeys at one scale ------------------------------------------
ROUTE_ORDER <- c("wh_quick", "wh_ds", "typ_sussex", "typ_bristol", "typ_solent", "typ_devon")
ROUTE_TITLE <- c(wh_quick = "Whitehawk to Varndean, its quickest school", wh_ds = "Whitehawk to Dorothy Stringer",
                 typ_sussex = "Brighton and Hove, typical for a deprived area", typ_bristol = "Bristol, typical for a deprived area",
                 typ_solent = "Southampton, typical for a deprived area", typ_devon = "Plymouth, typical for a deprived area")
fig_route_examples <- function(ids = ROUTE_ORDER) {
  ext <- sapply(ids, function(i) { b <- st_bbox(to_bng(RX$legs[RX$legs$id == i, ])); max(b[3] - b[1], b[4] - b[2]) }); half <- max(ext) / 2 + 400
  one <- function(i) {
    s <- RX$summary[RX$summary$id == i, ]; l <- to_bng(RX$legs[RX$legs$id == i, ]); bb <- st_bbox(l); cx <- mean(bb[c(1, 3)]); cy <- mean(bb[c(2, 4)])
    how <- if (s$buses == 0) "all of it on foot" else sprintf("%.0f waiting, %.0f on %s %s", s$wait, s$bus, if (s$buses == 1) "bus" else "buses", s$routes)
    sub <- sprintf("%s%.0f min from leaving home: %.0f walking, %s. %.0f m climbed on foot.", if (startsWith(i, "wh_")) "" else sprintf("%s to %s. ", s$area, sub(" School$", "", s$school)), s$total, s$walk, how, s$climb)
    ggplot() + tiles() + route_layers(l) + end_layers(i) + annotation_scale(location = "br", width_hint = 0.25, text_cex = 0.6, height = unit(0.12, "cm")) +
      coord_sf(xlim = cx + c(-1, 1) * half, ylim = cy + c(-1, 1) * half, expand = FALSE, crs = 27700, datum = NA) +
      labs(title = ROUTE_TITLE[[i]], subtitle = paste(strwrap(sub, 62), collapse = "\n")) + map_theme() }
  wrap_plots(lapply(ids, one), ncol = 2)
}
# The parts of each journey, in minutes, beside the measure the analysis uses
fig_route_anatomy <- function(ids = ROUTE_ORDER) {
  s <- RX$summary[match(ids, RX$summary$id), ]; s$lab <- ROUTE_TITLE[s$id]
  d <- rbind(data.frame(lab = s$lab, part = "Walking", min = s$walk), data.frame(lab = s$lab, part = "Waiting", min = s$wait), data.frame(lab = s$lab, part = "On the bus", min = s$bus))
  d$part <- factor(d$part, c("Walking", "Waiting", "On the bus")); d$lab <- factor(d$lab, rev(s$lab))
  tot <- data.frame(lab = factor(s$lab, rev(s$lab)), total = s$total, climb = s$climb, mean = s$t_mean)
  ggplot(d, aes(min, lab, fill = part)) + geom_col(width = 0.62, colour = "white", linewidth = 0.4) +
    geom_point(data = tot[!is.na(tot$mean), ], aes(mean, lab), inherit.aes = FALSE, shape = 124, size = 6, colour = INK) +
    geom_text(data = tot, aes(pmax(total, mean, na.rm = TRUE), lab, label = sprintf("%.0f min, %.0f m up", total, climb)), inherit.aes = FALSE, hjust = -0.12, size = 3, colour = INK) +
    scale_fill_manual(values = c(Walking = BLUE, Waiting = LIGHT, `On the bus` = BPURPLE), name = NULL) +
    scale_x_continuous(limits = c(0, max(tot$total, tot$mean, na.rm = TRUE) * 1.32), expand = c(0, 0)) +
    labs(x = "Minutes from leaving home", y = NULL) + theme_note() + theme(legend.position = "top", panel.grid.major.y = element_blank())
}

# ---- Methodology: one journey, step by step ----------------------------------------------------
WXD <- RX$worked
POI_COL <- c(food_drink = "#993bff", shops = "#1f7fa0", leisure = "#c2701c", open_space = "#3a8f4f", civic = "#6b6680")
POI_LAB <- c(food_drink = "Food and drink", shops = "Shops", leisure = "Leisure", open_space = "Open space", civic = "Civic")
# 1. The itinerary, its stops with their 200 m circles, and a close-up of the change of bus
fig_worked_route <- function() {
  i <- WXD$id; l <- to_bng(RX$legs[RX$legs$id == i, ]); st <- to_bng(WXD$stops); p <- to_bng(WXD$pois)
  bb <- st_bbox(st_buffer(l, 300))
  a <- ggplot() + tiles() + geom_sf(data = st_buffer(st, 200), fill = NA, colour = BPURPLE, linewidth = 0.4, linetype = "31") + route_layers(l) + end_layers(i) +
    annotation_scale(location = "br", width_hint = 0.25, text_cex = 0.6, height = unit(0.12, "cm")) + coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = FALSE, crs = 27700, datum = NA) +
    labs(title = "1. The journey at its median departure", subtitle = "Walking (dashed) and bus (purple) legs; circles are 200 m around\nwhere the child boards, changes and gets off") + map_theme()
  ch <- st[st$role == "change", ]; cb <- st_bbox(st_buffer(st_union(ch), 320)); pz <- p[p$at_stop | p$walk_legs, ]
  b <- ggplot() + tiles(z = 16) + geom_sf(data = st_buffer(st_union(ch), 200), fill = PPURPLE, alpha = 0.35, colour = BPURPLE, linewidth = 0.4) +
    geom_sf(data = st_buffer(st_union(l[l$mode == "WALK", ]), 50), fill = LIGHT, alpha = 0.5, colour = NA) + route_layers(l, bus_lab = FALSE) +
    geom_sf(data = pz, aes(colour = cat), size = 1.3) + scale_colour_manual(values = POI_COL, labels = POI_LAB, name = NULL) +
    coord_sf(xlim = cb[c(1, 3)], ylim = cb[c(2, 4)], expand = FALSE, crs = 27700, datum = NA) +
    labs(title = "2. The change of bus, close up", subtitle = sprintf("%d points of interest within 200 m of its stops\n(shaded), and along the 50 m band of the walk between them",
                                                                       sum(lengths(st_is_within_distance(pz, st_union(ch), 200)) > 0))) +
    map_theme() + theme(legend.position = "bottom", legend.text = element_text(size = 7)) + guides(colour = guide_legend(nrow = 2, override.aes = list(size = 2.5)))
  a + b + plot_layout(widths = c(1.15, 1))
}
# 2. The walking route on foot, and the climb along it
fig_worked_climb <- function() {
  w <- to_bng(WXD$walk_route); e <- ends_sf(WXD$id); bb <- st_bbox(st_buffer(w, 250)); s <- WXD$walk_stats; pr <- WXD$walk_profile
  a <- ggplot() + tiles() + geom_sf(data = w, colour = BLUE, linewidth = 1) + geom_sf(data = e$home, shape = 21, fill = INK, colour = "white", size = 3) +
    geom_sf(data = st_sf(geometry = st_cast(st_geometry(w), "POINT")[length(st_cast(st_geometry(w), "POINT"))]), shape = 22, fill = HERITAGE, colour = INK, size = 3.2) +
    annotation_scale(location = "br", width_hint = 0.25, text_cex = 0.6, height = unit(0.12, "cm")) + coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = FALSE, crs = 27700, datum = NA) +
    labs(title = "1. The route on foot to the quickest school on foot", subtitle = sprintf("%.1f km; %.0f min at 3.6 km/h on the flat, %.0f with the hills", s$metres / 1000, s$t_flat, s$t_out)) + map_theme()
  b <- ggplot(pr, aes(d / 1000, z)) + geom_area(fill = PPURPLE, colour = NA) + geom_line(colour = DPURPLE, linewidth = 0.8) +
    annotate("text", x = 0, y = max(pr$z, na.rm = TRUE), hjust = 0, vjust = 1, size = 3, colour = INK,
             label = sprintf("Metres climbed: %.0f on the way to school,\n%.0f on the way home (the same route reversed)", s$up_out, s$up_back)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) + labs(x = "Kilometres from home", y = "Height above sea level (m)", title = "2. Heights every 25 m, OS Terrain 50") +
    theme_note() + theme(plot.title = element_text(face = "bold", size = 9))
  a + b + plot_layout(widths = c(1, 1.1))
}
# 3. Missing a journey: arrival time for every departure minute
fig_worked_miss <- function() {
  bm <- WXD$by_minute; ms <- WXD$miss[WXD$miss$bell == "bell_0845", ]; bell <- 8 * 60 + 45; hm <- function(x) sprintf("%02d:%02d", as.integer(x) %/% 60, as.integer(round(x)) %% 60)
  nxt <- min(bm$arrive[bm$leave > ms$last_leave])
  ggplot(bm, aes(leave, arrive)) + geom_hline(yintercept = bell, colour = GREY, linetype = "22") + geom_step(colour = BPURPLE, linewidth = 0.9) +
    annotate("point", x = ms$last_leave, y = bm$arrive[bm$leave == ms$last_leave], size = 2.6, colour = INK) +
    annotate("segment", x = ms$last_leave + 1, xend = ms$last_leave + 1, y = bell, yend = nxt, colour = INK, arrow = arrow(length = unit(0.12, "cm"))) +
    annotate("text", x = ms$last_leave - 2, y = bm$arrive[bm$leave == ms$last_leave], hjust = 1, size = 3, colour = INK, label = sprintf("Last departure that arrives by %s:\nleave %s", hm(bell), hm(ms$last_leave))) +
    annotate("text", x = ms$last_leave + 3, y = (bell + nxt) / 2 + 3, hjust = 0, size = 3, colour = INK, label = sprintf("Miss it and the next best\narrives %.1f minutes late", ms$late)) +
    annotate("text", x = min(bm$leave), y = bell, vjust = -0.5, hjust = 0, size = 3, colour = GREY, label = "Assumed bell, 08:45") +
    scale_x_continuous(labels = hm, breaks = seq(420, 530, 15)) + scale_y_continuous(labels = hm) +
    labs(x = "Leaving home at", y = "Arriving at school at") + theme_note()
}
# 4. From routes to a measure: every Output Area in Brighton and Hove, and the most deprived third
fig_worked_oas <- function() {
  o <- RX$worked$oa_bh; o <- st_transform(st_as_sf(o, coords = c("lon", "lat"), crs = 4326), 27700); bb <- st_bbox(o)
  dep <- weighted.mean(o$t_bus[o$third], o$kids[o$third]); rest <- weighted.mean(o$t_bus[!o$third], o$kids[!o$third])
  ggplot() + tiles(13) + geom_sf(data = o[!o$third, ], aes(fill = pmin(t_bus, 60)), shape = 21, colour = "white", stroke = 0.15, size = 1.6) +
    geom_sf(data = o[o$third, ], aes(fill = pmin(t_bus, 60)), shape = 21, colour = INK, stroke = 0.55, size = 1.9) +
    scale_fill_gradient(low = PPURPLE, high = DPURPLE, limits = c(0, 60), name = "Minutes to the\nquickest school") +
    coord_sf(xlim = bb[c(1, 3)] + c(-300, 300), ylim = bb[c(2, 4)] + c(-300, 300), expand = FALSE, crs = 27700, datum = NA) +
    labs(subtitle = sprintf("Ringed: the most deprived third of neighbourhoods. Child-weighted means: %.1f minutes for them,\n%.1f for the rest, a gap of %+.1f.", dep, rest, dep - rest)) +
    map_theme() + theme(legend.position = "right")
}
