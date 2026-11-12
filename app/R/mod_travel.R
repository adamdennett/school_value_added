# mod_travel.R - Travel and Distance: how far children live from school, and how long
# the journey takes, for deprived neighbourhoods against the rest. What is here now:
#   - straight-line distance from every neighbourhood in England to its nearest state
#     secondary, by authority and district (R/18_distance.R, distance.qmd)
#   - journeys on real networks (walking, and walking and bus at 08:00) in 109 mostly
#     urban authorities (R/26-R/29, distance.qmd#sec-la-journeys)
#   - climb on the walk to school (R/25_climb.R)
# What is coming: national walking and cycling routes from every Output Area to every
# school within 120 minutes' walk (the school_active_travel project), summarised by
# school in data/travel_national.rds by R/41_app_travel_national.R as it completes.

TRAVEL_MEASURES <- tibble::tribble(
  ~key, ~src, ~label, ~unit, ~dp, ~good,
  "near_dep", "dist", "Straight-line km to the nearest state secondary, deprived third", " km", 2, "low",
  "near_rest", "dist", "Straight-line km to the nearest state secondary, other neighbourhoods", " km", 2, "low",
  "gap", "dist", "Extra straight-line km for the deprived third", " km", 2, "div",
  "t_bus_dep", "jny", "Minutes by walking and bus, deprived third (109 authorities)", " min", 1, "low",
  "t_bus_gap", "jny", "Extra minutes by walking and bus for the deprived third", " min", 1, "div",
  "t_walk_dep", "jny", "Minutes walking, deprived third", " min", 1, "low",
  "t_walk_gap", "jny", "Extra minutes walking for the deprived third", " min", 1, "div",
  "p_two_dep", "jny", "Share needing two or more buses, deprived third", "%", 0, "low",
  "late_dep", "jny", "Minutes late if the last workable journey is missed, deprived third", " min", 1, "low",
  "up_out", "climb", "Metres climbed walking to school (typical journey)", " m", 0, "low")

travel_la <- local({
  t <- D$travel
  j <- t$jny_la |> transmute(la, t_bus_dep, t_bus_rest, t_bus_gap = t_bus_dep - t_bus_rest, t_walk_dep, t_walk_rest,
                             t_walk_gap = t_walk_dep - t_walk_rest, p_two_dep = 100 * p_two_dep, p_two_rest = 100 * p_two_rest,
                             late_dep, late_rest, km1_dep, km1_rest, up_back_dep, up_back_rest, n30_dep, n30_rest)
  t$dist_la |> full_join(j, by = "la") |> full_join(t$climb_la |> select(la, up_out, up_back), by = "la")
})
lad_key <- function(x) tolower(trimws(sub(",.*$", "", x)))
LAD_LA <- SCH |> distinct(lad, la) |> mutate(k = lad_key(lad))

national_travel_card <- function(tn) {
  div(class = "sis-national",
    h6(icon("person-walking"), " National walking routes (early results)"),
    div(class = "sis-kpis",
      div(class = "sis-kpi", div(class = "sis-kpi-l", "Children 10-14 for whom it is the quickest walk"), div(class = "sis-kpi-v", format(round(tn$kids), big.mark = ","))),
      div(class = "sis-kpi", div(class = "sis-kpi-l", "Their median walk"), div(class = "sis-kpi-v", paste(f1(tn$walk_med), "min"))),
      div(class = "sis-kpi", div(class = "sis-kpi-l", "Within a 20-minute walk"), div(class = "sis-kpi-v", paste0(f1(tn$within20, 0), "%"))),
      div(class = "sis-kpi", div(class = "sis-kpi-l", "Median climb to school"), div(class = "sis-kpi-v", paste(f1(tn$climb_med, 0), "m"))),
      div(class = "sis-kpi", div(class = "sis-kpi-l", "Quiet-street cycle route under 15 min"), div(class = "sis-kpi-v", paste0(f1(tn$lts2_15, 0), "%")))),
    note("From the national routing of every Output Area to every school within 120 minutes' walk (r5r on OpenStreetMap, OS Terrain 50 heights), so far for ",
         TN$coverage$las, " of ", TN$coverage$las_total, " districts. The children are those in Output Areas whose quickest walk to a state secondary is this school; ",
         "a stand-in for the real intake."))
}

mod_travel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(width = 330, title = "Journeys to school",
        selectInput(ns("measure"), "Map", setNames(TRAVEL_MEASURES$key, TRAVEL_MEASURES$label), selected = "gap"),
        selectizeInput(ns("la"), "Local authority", sort(unique(SCH$la)), selected = "Brighton and Hove"),
        note("The deprived third: each authority's most deprived third of neighbourhoods by Census 2021 household deprivation, weighted by children aged 10 to 14. Click the map to choose an authority.")),
      layout_columns(col_widths = c(7, 5),
        card(full_screen = TRUE, card_header(textOutput(ns("map_head"), inline = TRUE), class = "sis-ch"), leafletOutput(ns("map"), height = 500),
             card_footer(class = "sis-note", "Grey: not yet measured. Journey times are for 109 mostly urban authorities so far.")),
        card(card_header(textOutput(ns("la_head"), inline = TRUE), class = "sis-ch"), uiOutput(ns("facts")))),
      layout_columns(col_widths = c(5, 7),
        card(card_header("Its districts: straight-line distance", class = "sis-ch"), tableOutput(ns("districts")),
             card_footer(class = "sis-note", "Kilometres to the nearest state secondary school, weighted by children aged 10 to 14; deprived third within each district.")),
        card(card_header("Its schools: journeys from the neighbourhoods they are quickest for", class = "sis-ch"), DTOutput(ns("schools")),
             card_footer(class = "sis-note", "Where measured: non-selective schools that are the quickest school (walking and bus, 08:00) for at least 100 children aged 10 to 14.")))),
    div(class = "container-fluid sis-travel-text",
      layout_columns(col_widths = c(6, 6),
        card(card_body(h5(icon("magnifying-glass-chart"), " What we have found so far"),
          tags$ul(
            tags$li("Across England, children in deprived neighbourhoods usually live nearer a state secondary than other children. In Brighton and Hove they live about a third further away."),
            tags$li("On real networks, Brighton and Hove's deprived third has the longest extra journey of the 109 authorities measured: about 4.7 minutes more by walking and bus, and 18% need two or more buses."),
            tags$li("Between authorities, absence goes with distance, more for disadvantaged pupils: with every control, about 2 percentage points more absence among disadvantaged pupils for each extra km the deprived third live from school (109 authorities). Journey time, changes of bus and waiting add nothing beyond distance; climb may add a little. No direct link from distance to Attainment 8 is seen."),
            tags$li("Much of the raw link between distance and absence is the ethnic mix of authorities, so the numbers here describe places; they are not the effect of moving a school or a child.")),
          p(tags$a(href = paste0(SITE, "/distance.html"), target = "_blank", "The full analysis, with its models and caveats"), "."))),
        card(card_body(h5(icon("route"), " Coming: walking and cycling routes for all of England"),
          p("Every Census 2021 Output Area is being routed to every school within 120 minutes' walk, with walking times, slope-adjusted times, the height every 50 m, and cycling times on quiet streets and on any street. ",
            "It will give each school a walking catchment, and each place the share of children who could walk or cycle to school, for an active travel model."),
          uiOutput(ns("national")))))))
}

mod_travel_server <- function(id, ctx, sel, place) {
  moduleServer(id, function(input, output, session) {
    # The shared place: an authority, or a district's own authority, is shown here. Choosing an
    # authority here makes it the shared place, unless the shared place is already in it (a district)
    observeEvent(list(place$level, place$area), { la <- place_la(place$level, place$area)
      if (!is.null(la) && !identical(la, input$la)) updateSelectizeInput(session, "la", selected = la) }, ignoreInit = TRUE)
    observeEvent(input$la, { req(input$la %in% SCH$la)
      if (!identical(place_la(place$level, place$area), input$la)) { place$level <- "la"; place$area <- input$la } }, ignoreInit = TRUE)
    md <- reactive(as.list(TRAVEL_MEASURES[TRAVEL_MEASURES$key == input$measure, ]))
    output$map_head <- renderText(md()$label)
    output$map <- renderLeaflet(base_map(-1.6, 52.9, 6))
    ready <- reactive(!is.null(input$map_zoom))
    observe({ req(ready())
      m <- md(); b <- D$bounds |> left_join(travel_la |> select(la, v = all_of(m$key)), by = "la")
      v <- b$v; cap <- unname(quantile(abs(v), 0.975, na.rm = TRUE))   # one extreme authority does not wash out the rest
      b$vc <- if (m$good == "div") pmax(pmin(v, cap), -cap) else v
      pal <- if (m$good == "div") colorNumeric(rev(PRGN), c(-cap, cap), na.color = "#e3e1ea") else colorNumeric(rev(PRGN_SEQ), range(v, na.rm = TRUE), na.color = "#e3e1ea")
      leafletProxy(session$ns("map"), data = b) |> clearShapes() |> clearControls() |>
        addPolygons(layerId = ~la, fillColor = ~pal(vc), fillOpacity = 0.85, color = "white", weight = 0.6,
                    label = ~paste0(la, ": ", ifelse(is.na(v), "not measured", paste0(f1(v, m$dp), m$unit))),
                    highlightOptions = highlightOptions(color = DPURPLE, weight = 2, bringToFront = TRUE)) |>
        addLegend("bottomright", pal = pal, values = if (m$good == "div") c(-cap, cap) else range(v, na.rm = TRUE), title = NULL,
                  labFormat = labelFormat(suffix = m$unit), opacity = 0.9, na.label = "not measured")
    })
    observe({ req(ready()); md(); b <- D$bounds[D$bounds$la == input$la, ]; req(nrow(b))   # redrawn after the polygons are
      leafletProxy(session$ns("map")) |> clearGroup("sel") |> addPolylines(data = b, color = INK, weight = 3, group = "sel") })
    observeEvent(input$map_shape_click, { la <- input$map_shape_click$id; if (!is.null(la) && la %in% SCH$la) updateSelectizeInput(session, "la", selected = la) })

    output$la_head <- renderText(input$la)
    output$facts <- renderUI({
      r <- travel_la[travel_la$la == input$la, ]; if (!nrow(r)) return(p("Not measured."))
      rk <- function(col, v) { x <- travel_la[[col]]; paste0(sum(x > v, na.rm = TRUE) + 1, " of ", sum(!is.na(x))) }
      row <- function(lab, dep, rest, unit, dp = 1) tags$tr(tags$th(lab), tags$td(paste0(f1(dep, dp), unit)), tags$td(paste0(f1(rest, dp), unit)))
      tagList(
        tags$table(class = "table table-sm sis-facts",
          tags$thead(tags$tr(tags$th(""), tags$th("Deprived third"), tags$th("Other neighbourhoods"))),
          tags$tbody(
            row("Straight-line km to nearest state secondary", r$near_dep, r$near_rest, " km", 2),
            if (!is.na(r$t_bus_dep)) tagList(
              row("Minutes, walking and bus", r$t_bus_dep, r$t_bus_rest, " min"),
              row("Minutes, walking", r$t_walk_dep, r$t_walk_rest, " min"),
              row("Share needing two or more buses", r$p_two_dep, r$p_two_rest, "%", 0),
              row("Minutes late if the last journey is missed", r$late_dep, r$late_rest, " min"),
              row("Metres climbed walking home", r$up_back_dep, r$up_back_rest, " m", 0)))),
        p(HTML(paste0("Extra distance for the deprived third: <b>", sg(r$gap, 2), " km</b> (", rk("gap", r$gap), " authorities, largest first).",
          if (!is.na(r$t_bus_gap)) paste0(" Extra time by walking and bus: <b>", sg(r$t_bus_gap), " min</b> (", rk("t_bus_gap", r$t_bus_gap), ").") else "",
          if (!is.na(r$up_out)) paste0(" A typical walk to school climbs ", f1(r$up_out, 0), " m (", rk("up_out", r$up_out), ").") else ""))),
        if (!is.na(r$abs_dis)) p(HTML(paste0("Secondary absence: disadvantaged pupils <b>", f1(r$abs_dis), "%</b>, others ", f1(r$abs_non), "%."))),
        if (is.na(r$t_bus_dep)) note("Journey times on real networks are not yet measured here (109 mostly urban authorities so far)."))
    })

    output$districts <- renderTable({
      k <- LAD_LA$k[LAD_LA$la == input$la]; d <- D$travel$dist_lad |> mutate(k = lad_key(lad)) |> filter(k %in% !!k)
      validate(need(nrow(d) > 0, "No district figures."))
      d |> transmute(District = lad, Children = format(round(kids), big.mark = ","), `Deprived third` = f1(near_dep, 2),
                     Others = f1(near_rest, 2), `Extra km` = sg(gap, 2)) |> arrange(District)
    }, striped = TRUE, spacing = "s", align = "lrrrr")

    output$schools <- renderDT({
      j <- D$travel$jny_school |> inner_join(SCH |> select(URN, School = name, la, lad), by = "URN") |> filter(la == input$la)
      validate(need(nrow(j) > 0, "No schools measured here yet."))
      t <- j |> transmute(School, District = lad, Children = round(children), `Straight km` = round(km_s, 2), `Walk min` = round(t_walk, 1),
                          `Walk+bus min` = round(t_bus, 1), `Need a bus %` = round(bus_share), `Two+ buses %` = round(two_share),
                          `Late if missed, min` = round(late, 1), `Climb to school m` = round(up_out)) |> arrange(desc(`Walk+bus min`))
      datatable(t, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "tip"), class = "compact stripe hover")
    })

    output$national <- renderUI({
      if (is.null(TN)) return(note("Not yet in the app. Routing started in November 2026 and the first authorities (London north of the Thames, Brighton and Hove) are done; school summaries will appear here as authorities finish."))
      cv <- TN$coverage; s <- TN$schools |> inner_join(SCH |> select(URN, name, la), by = "URN") |> filter(la == input$la)
      tagList(
        p(HTML(paste0("<b>", cv$las, "</b> of ", cv$las_total, " local authority districts fully routed so far (", format(cv$pairs, big.mark = ","), " walking routes to state secondary schools from the Output Areas routed so far), as of ", format(cv$built, "%d %B %Y"), "."))),
        if (nrow(s)) tagList(h6("Schools in ", input$la),
          tableOutput(session$ns("national_tab"))) else note("Not yet routed for ", input$la, "."))
    })
    output$national_tab <- renderTable({
      req(TN); TN$schools |> inner_join(SCH |> select(URN, School = name, la), by = "URN") |> filter(la == input$la) |>
        transmute(School, Children = format(round(kids), big.mark = ","), `Median walk, min` = f1(walk_med), `Within 20 min %` = f1(within20, 0),
                  `Median climb, m` = f1(climb_med, 0), `Quiet cycle < 15 min %` = f1(lts2_15, 0)) |> arrange(School)
    }, striped = TRUE, spacing = "s")
  })
}
