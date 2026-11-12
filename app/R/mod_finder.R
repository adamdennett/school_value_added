# mod_finder.R - School Finder: a school by name, area or map, with its results, value
# added and absence against expected, year by year, for all, disadvantaged and
# non-disadvantaged pupils (with girls and boys)

mod_finder_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(fillable = FALSE,
    sidebar = sidebar(width = 330, title = "Find a school",
      area_picker_ui(ns, level = "la"),
      selectizeInput(ns("school"), "School", choices = NULL, options = list(placeholder = "Type a name...")),
      selectInput(ns("colour"), "Colour the map by", MEASURE_CHOICES, selected = "va"),
      checkboxInput(ns("all_eng"), "Show every school in England on the map", FALSE),
      hr(),
      actionButton(ns("to_twin"), "Find its twins", icon = icon("users"), class = "btn-outline-primary w-100 mb-2"),
      actionButton(ns("to_sim"), "Open in the simulator", icon = icon("sliders"), class = "btn-primary w-100")),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE, card_header("Schools on the map", class = "sis-ch"), leafletOutput(ns("map"), height = 460),
           card_footer(class = "sis-note", "Click a school to choose it. Value added and absence against expected use a fixed national colour scale; other measures run across the schools shown.")),
      card(card_header(textOutput(ns("title"), inline = TRUE), class = "sis-ch"), uiOutput(ns("card")))),
    card(card_header("This school's groups of pupils", class = "sis-ch"), tableOutput(ns("groups")),
         card_footer(class = "sis-note", textOutput(ns("groups_note")))),
    card(full_screen = TRUE, card_header("Year by year", class = "sis-ch"), plotOutput(ns("trend"), height = 330),
         card_footer(class = "sis-note", "Yearly value added is not shrunk, so it moves more than the four-year figure. Open circles: provisional 2025/26. The dashed line in the right panel is the Attainment 8 the intake predicts.")),
    uiOutput(ns("travel")))
}

mod_finder_server <- function(id, ctx, sel, go) {
  moduleServer(id, function(input, output, session) {
    area_picker_server(input, session)
    pool <- reactive({ req(area_ok(input$level, input$area)); in_area(SCH, input$level, input$area) })
    observeEvent(pool(), {
      p <- pool(); ch <- setNames(p$URN, school_label(p))[order(p$name)]
      cur <- if (sel() %in% p$URN) sel() else p$URN[order(p$name)][1]
      updateSelectizeInput(session, "school", choices = ch, selected = cur, server = TRUE)
    })
    observeEvent(input$school, { u <- as.integer(input$school); if (!is.na(u) && !identical(u, sel())) sel(u) }, ignoreInit = TRUE)
    observeEvent(sel(), { if (!identical(as.integer(input$school), sel()) && sel() %in% pool()$URN)
      updateSelectizeInput(session, "school", selected = sel()) }, ignoreInit = TRUE)
    observeEvent(input$to_twin, go("twin")); observeEvent(input$to_sim, go("sim"))

    s <- reactive(SCH[SCH$URN == sel(), ][1, ])
    m <- reactive(ctx()$M)

    output$map <- renderLeaflet(base_map())
    ready <- reactive(!is.null(input$map_zoom))   # a map on a hidden tab exists only once shown
    observe({ req(ready())
      M <- m(); key <- input$colour; req(key)
      d <- if (isTRUE(input$all_eng)) M else { req(area_ok(input$level, input$area)); in_area(M, input$level, input$area) }
      d <- d[!is.na(d$lat), ]; req(nrow(d) > 0)
      pl <- measure_pal(key, d[[key]], cap = if (mdef(key)$scale == "div") cap_for(M, key))
      v <- pl$clamp(d[[key]]); md <- mdef(key)
      leafletProxy(session$ns("map"), data = d) |> clearMarkers() |> clearControls() |>
        addCircleMarkers(lng = ~lon, lat = ~lat, layerId = ~URN, radius = ~pmin(pmax(sqrt(pupils) / 5, 3), 10),
                         fillColor = pl$pal(v), fillOpacity = 0.9, color = "#3a3a4e", weight = 0.6,
                         popup = popup_school(d, ctx()$glab, ctx()$txt), label = ~name) |>
        addLegend("bottomright", pal = pl$pal, values = pl$domain, title = md$short, opacity = 0.9,
                  labFormat = labelFormat(suffix = md$unit))
    })
    observeEvent(list(input$area, input$level, input$all_eng, ready()), { req(ready())
      req(area_ok(input$level, input$area)); d <- in_area(SCH, input$level, input$area); if (!isTRUE(input$all_eng)) leafletProxy(session$ns("map")) |> fit_to(d)
    })
    observeEvent(list(sel(), ready()), { req(ready())
      x <- s(); leafletProxy(session$ns("map")) |> clearGroup("focus") |>
        addCircleMarkers(lng = x$lon, lat = x$lat, radius = 13, fill = FALSE, color = INK, weight = 2.5, group = "focus")
    })
    observeEvent(input$map_marker_click, { u <- input$map_marker_click$id; if (!is.null(u)) sel(as.integer(u)) })

    output$title <- renderText(s()$name)
    output$card <- renderUI({
      x <- s(); M <- m(); r <- M[M$URN == x$URN, ]
      kpi <- function(lab, val, sub, cls = "") div(class = paste("sis-kpi", cls), div(class = "sis-kpi-l", lab), div(class = "sis-kpi-v", val), div(class = "sis-kpi-s", sub))
      va_cls <- if (is.na(r$va)) "" else if (!is.na(r$lo) && r$lo > 0) "up" else if (!is.na(r$hi) && r$hi < 0) "down" else ""
      ab_cls <- if (is.na(r$abs_excess)) "" else if (r$abs_excess > 0.5) "down" else if (r$abs_excess < -0.5) "up" else ""
      tagList(
        p(class = "sis-where", where_txt(x)),
        div(class = "sis-kpis",
            kpi("Attainment 8", f1(r$att8), paste("Expected", f1(r$exp))),
            kpi(paste("Value added", VA_VERSION), sg(r$va), if (!is.na(r$lo)) paste0(sg(r$lo), " to ", sg(r$hi)) else paste(r$n_years %||% 0, "year(s)"), va_cls),
            kpi("Absence", paste0(f1(r$abs_obs), "%"), paste0(sg(r$abs_excess), " pp against expected"), ab_cls)),
        tags$table(class = "table table-sm sis-facts",
          tags$tbody(
            tags$tr(tags$th("Run by"), tags$td(x$run_by %||% "–", if (!is.na(x$gov) && !is.na(x$trust)) tags$span(class = "sis-sub", paste0(" (", x$gov, ")")))),
            tags$tr(tags$th("Type"), tags$td(paste(na.omit(c(x$type, x$gender, if (isTRUE(x$age16)) "11-16" else "with sixth form")), collapse = ", "))),
            tags$tr(tags$th("Admissions"), tags$td(c("SEL" = "Selective", "OTHER NON SEL" = "Non-selective", "NON SEL IN HIGHLY SEL AREA" = "Non-selective, in a selective area")[x$admissions] %||% "–")),
            tags$tr(tags$th("Pupils"), tags$td(format(x$pupils, big.mark = ","), tags$span(class = "sis-sub", paste0(" · FSM6 ", f1(x$fsm), "% · EAL ", f1(x$eal), "% · SEN support ", f1(x$sen_support), "%")))),
            tags$tr(tags$th("Ofsted"), tags$td(x$ofsted_latest %||% x$ofsted %||% "–")))),
        p(class = "sis-note", "Green: value added's interval is above zero, or absence over half a point below expected; purple: the reverse."))
    })

    output$groups <- renderTable({
      x <- s(); c <- ctx()
      rows <- lapply(GROUPS, function(g) { r <- c$for_group(g); r <- r[r$URN == x$URN, ]
        data.frame(Pupils = GLAB[[g]], `Attainment 8` = f1(r$att8), Expected = f1(r$exp),
                   `Value added` = sg(r$va), `95% interval` = if (is.na(r$lo)) "–" else paste(sg(r$lo), "to", sg(r$hi)),
                   check.names = FALSE) })
      do.call(rbind, rows)
    }, striped = TRUE, hover = TRUE, spacing = "s", align = "lrrrr")
    output$groups_note <- renderText(paste0(ctx()$txt, ". Expected: the Attainment 8 schools with this intake achieve across England. ",
                                            "Girls and boys are shown where a school has enough of each; a single-sex school has one."))

    output$trend <- renderPlot({
      x <- s()
      y <- D$years[D$years$URN == x$URN & D$years$group %in% MAIN3, ] |> mutate(Pupils = factor(GLAB[group], GLAB[MAIN3]))
      validate(need(nrow(y) > 0, "No yearly results for this school."))
      long <- bind_rows(y |> transmute(year, Pupils, prov, panel = "Value added (points against England)", v = va),
                        y |> transmute(year, Pupils, prov, panel = "Attainment 8", v = att8))
      ex <- y |> transmute(year, Pupils, panel = "Attainment 8", v = exp)
      ggplot(long, aes(yl(year), v, colour = Pupils, group = Pupils)) +
        geom_hline(data = data.frame(panel = "Value added (points against England)", z = 0), aes(yintercept = z), colour = GREY, linewidth = 0.4) +
        geom_line(data = ex, linetype = "dashed", linewidth = 0.6, alpha = 0.7) +
        geom_line(linewidth = 1) + geom_point(aes(shape = prov), size = 2.8, stroke = 1.1, fill = "white") +
        scale_shape_manual(values = c(`FALSE` = 19, `TRUE` = 21), guide = "none") +
        scale_colour_manual(values = GROUP_COL) + facet_wrap(~panel, scales = "free_y") +
        labs(x = NULL, y = NULL, colour = NULL) + theme_sis()
    }, res = 96)

    output$travel <- renderUI({
      x <- s(); j <- D$travel$jny_school[D$travel$jny_school$URN == x$URN, ]
      tn <- if (!is.null(TN)) TN$schools[TN$schools$URN == x$URN, ] else NULL
      if (!nrow(j) && (is.null(tn) || !nrow(tn))) return(card(card_header("Journeys to this school", class = "sis-ch"),
        p(class = "sis-note", "Journey times on real networks are so far for 109 mostly urban authorities, for non-selective schools that are the quickest school for at least 100 children. ",
          "National walking and cycling routes are being added; see Travel and Distance.")))
      card(card_header("Journeys to this school", class = "sis-ch"),
        if (nrow(j)) tagList(
          div(class = "sis-kpis",
            div(class = "sis-kpi", div(class = "sis-kpi-l", "Children for whom it is the quickest school"), div(class = "sis-kpi-v", format(round(j$children), big.mark = ","))),
            div(class = "sis-kpi", div(class = "sis-kpi-l", "Straight-line distance"), div(class = "sis-kpi-v", paste(f1(j$km_s, 2), "km"))),
            div(class = "sis-kpi", div(class = "sis-kpi-l", "Walking"), div(class = "sis-kpi-v", paste(f1(j$t_walk), "min"))),
            div(class = "sis-kpi", div(class = "sis-kpi-l", "Walking and bus"), div(class = "sis-kpi-v", paste(f1(j$t_bus), "min")),
                div(class = "sis-kpi-s", paste0(f1(j$bus_share, 0), "% need a bus, ", f1(j$two_share, 0), "% two or more"))),
            div(class = "sis-kpi", div(class = "sis-kpi-l", "Climb to school"), div(class = "sis-kpi-v", paste(f1(j$up_out, 0), "m")),
                div(class = "sis-kpi-s", paste(f1(j$up_back, 0), "m on the way home")))),
          note("Averages over the children aged 10 to 14 in the Output Areas for which this is the quickest school by walking and bus, at 08:00 on a school day (r5r on OpenStreetMap and bus timetables). A stand-in for real journeys: children do not all go to their quickest school.")),
        if (!is.null(tn) && nrow(tn)) national_travel_card(tn))
    })
  })
}
