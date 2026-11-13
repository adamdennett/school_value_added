# mod_pupils.R - Pupils and Places (v2.1): how many children are coming through an
# area's schools, and where schools have opened and closed. From the demographics page
# (demographics.qmd, R/42) via data/v21.rds:
#   - pupils by year group in state-funded schools, January 2016 to January 2026 (DfE)
#   - the trend since 2018/19 and the Year 7 roll-forward to 2032/33 (each cohort in
#     school now moved up a year at a time by the area's recent progression ratios)
#   - state mainstream schools opened (new places) and closed or merged since 2015/16 (GIAS)

PUPIL_MAP <- c("Primary-age pupils, 2018/19 to 2025/26 (%)" = "primary_chg",
               "Reception, 2018/19 to 2025/26 (%)" = "reception_chg",
               "Secondary-age pupils, 2018/19 to 2025/26 (%)" = "secondary_chg",
               "Year 7 intake, 2025/26 to 2032/33, rolled forward (%)" = "y7_chg_2032",
               "Secondary-age pupils, 2025/26 to 2030/31, rolled forward (%)" = "sec_chg_2030")
EVENT_COL <- c(Opened = DPURPLE, Closed = BPURPLE, Merged = LPURPLE)

mod_pupils_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(fillable = FALSE,
    sidebar = sidebar(width = 310, title = "Choose an area",
      area_picker_ui(ns, level = "la", levels = V21_LEVELS),
      selectInput(ns("measure"), "Colour the map by", PUPIL_MAP, selected = "primary_chg"),
      note("Pupils in state-funded schools in the January census, by year group: primary-age is Reception to Year 6, secondary-age Years 7 to 11 (nursery and sixth forms are left out). Click the map to choose an authority."),
      note(tags$a(href = paste0(SITE, "/demographics.html"), target = "_blank", "Pupil numbers and school places"), " on the site has the full method.")),
    uiOutput(ns("unit_note")),
    uiOutput(ns("kpis")),
    layout_columns(col_widths = c(6, 6), fill = FALSE,
      card(full_screen = TRUE, card_header(paste0("Pupils since 2015/16, indexed to ", yl(T0), " = 100"), class = "sis-ch"),
           plotOutput(ns("trend"), height = 360),
           card_footer(class = "sis-note", paste0("Pupils in each January census, against the same area in ", yl(T0), ", when primary-age numbers in England were near their peak."))),
      card(full_screen = TRUE, card_header("Year 7 intakes to 2032/33, indexed to 2025/26 = 100", class = "sis-ch"),
           plotOutput(ns("y7"), height = 360),
           card_footer(class = "sis-note", "Solid: counted. Dashed: rolled forward from the children already in primary school, at each area's progression ratios over the last three years, so moves in and out (and to independent schools) carry on as recently. No births assumption is needed."))),
    layout_columns(col_widths = c(6, 6), fill = FALSE,
      card(full_screen = TRUE, card_header(textOutput(ns("map_head"), inline = TRUE), class = "sis-ch"),
           leafletOutput(ns("map"), height = 470),
           card_footer(class = "sis-note", "Brown: fewer pupils; teal: more. Neither is better or worse. Three reorganised councils are shown combined, as on the site. Grey: no figures (Isles of Scilly).")),
      card(full_screen = TRUE, card_header(textOutput(ns("yg_head"), inline = TRUE), class = "sis-ch"),
           plotOutput(ns("yg"), height = 470),
           card_footer(class = "sis-note", paste0("Bars: pupils in each year group now. Open circles: the same year group in ", yl(T0), ". Each smaller cohort in primary school now reaches Year 7 in the years to come.")))),
    card(full_screen = TRUE, fill = FALSE, card_header(textOutput(ns("ev_head"), inline = TRUE), class = "sis-ch"),
         card_body(fillable = FALSE, plotOutput(ns("events"), height = 250), DTOutput(ns("events_tbl"))),
         card_footer(class = "sis-note", "State-funded mainstream schools (primary, middle, secondary and all-through), from Get Information about Schools. Opened: new schools with no predecessor (new places, not academy conversions or fresh starts). Closed: closed with no successor; merged: closed into another school. Years run September to August.")))
}

mod_pupils_server <- function(id, ctx, place) {
  moduleServer(id, function(input, output, session) {
    area_picker_server(input, session, place, levels = V21_LEVELS)
    U <- reactive({ req(area_ok(input$level, input$area)); v21_unit(input$level, input$area) })
    R <- reactive({ u <- U(); validate(need(!is.null(u), "Choose an authority, a district within one authority, a region or England.")); v21_comp(u) })
    has <- function(u) u$area %in% V21$trend$area

    output$unit_note <- renderUI({
      u <- U()
      msg <- c(if (is.null(u)) "Pupil numbers are by authority: choose an authority, a district within one authority, a region or England." else u$note,
               if (!is.null(u) && !has(u)) paste0("No census figures are published for ", u$area, " on their own."))
      if (length(msg)) div(class = "alert alert-light sis-note py-2", icon("circle-info"), " ", paste(msg, collapse = " "))
    })

    output$kpis <- renderUI({
      u <- U(); req(u, has(u)); t <- V21$trend; a <- t[t$area == u$area, ]; e <- t[t$area == "England", ]
      ev <- ev_area()
      n <- function(x) format(round(x), big.mark = ",")
      cmp <- function(x, ex) if (u$level == "England") NULL else paste0("England ", spct(ex))
      div(class = "sis-kpis",
        kpi("Primary-age pupils, 2025/26", n(a$primary_now), paste0(spct(a$primary_chg), " since ", yl(T0), if (u$level != "England") paste0(" (England ", spct(e$primary_chg), ")"))),
        kpi("Primary-age peak", yl(a$primary_peak_year), paste0("now ", spct(a$primary_from_peak), " from the peak of ", n(a$primary_peak))),
        kpi("Year 7 intake by 2032/33", spct(a$y7_chg_2032), paste0(n(a$Y7_now), " in 2025/26 to about ", n(a$y7_2032), if (u$level != "England") paste0("; England ", spct(e$y7_chg_2032)))),
        kpi("Secondary-age pupils by 2030/31", spct(a$sec_chg_2030), paste0(n(a$secondary_now), " now to about ", n(a$sec_2030), if (u$level != "England") paste0("; England ", spct(e$sec_chg_2030)))),
        kpi("Schools opened since 2015/16", sum(ev$event == "Opened"),
            paste0(sum(ev$event == "Closed"), " closed and ", sum(ev$event == "Merged"), " merged; ", a$primary_open + a$secondary_open, " open now")))
    })

    output$trend <- renderPlot({
      r <- R(); req(has(U()))
      p <- V21$pupils[V21$pupils$area %in% r$area, c("area", "year", "primary", "secondary")]
      p <- p |> tidyr::pivot_longer(c(primary, secondary), names_to = "stage", values_to = "n") |> group_by(area, stage) |>
        mutate(idx = 100 * n / n[year == T0]) |> ungroup() |>
        mutate(stage = factor(ifelse(stage == "primary", "Primary-age", "Secondary-age")))
      v21_lines(p, "idx", paste0("Pupils (", yl(T0), " = 100)"), r, hline = 100, by = 3) + facet_wrap(~stage, nrow = 1) +
        geom_vline(xintercept = yr0(T0), colour = LIGHT, linetype = "dotted")
    }, res = 96)

    output$y7 <- renderPlot({
      r <- R(); req(has(U()))
      now <- tail(V21$years, 1)
      a <- V21$pupils[V21$pupils$area %in% r$area, c("area", "year", "Y7")] |> mutate(kind = "Counted")
      f <- V21$proj[V21$proj$area %in% r$area, c("area", "year", "Y7")] |> mutate(kind = "Rolled forward")
      f <- bind_rows(a[a$year == now, ] |> mutate(kind = "Rolled forward"), f)
      d <- bind_rows(a, f) |> group_by(area) |> mutate(idx = 100 * Y7 / Y7[kind == "Counted" & year == now]) |> ungroup()
      v21_lines(d, "idx", "Year 7 pupils (2025/26 = 100)", r, hline = 100, linetype = "kind") +
        scale_linetype_manual(values = c(Counted = "solid", `Rolled forward` = "22"), name = NULL)
    }, res = 96)

    # ---- map ----
    output$map_head <- renderText(names(PUPIL_MAP)[PUPIL_MAP == input$measure])
    output$map <- renderLeaflet(base_map())
    observe({
      req(!is.null(input$map_zoom), input$measure)
      t <- V21$trend[V21$trend$level == "Authority", ]; vals <- setNames(t[[input$measure]], t$area)
      dom <- div_dom(vals)
      v21_map(leafletProxy(session$ns("map")), vals, colorNumeric(BRBG, dom, na.color = "#c9c9d6"), dom, v21_las(U()), "Change", unit = "%", fmt = function(v) sg(v, 1)) |>
        v21_fit(v21_las(U()))
    })
    observeEvent(input$map_shape_click, {
      la <- input$map_shape_click$id
      if (!is.null(la) && la %in% AREA_CHOICES[["la"]]) { place$level <- "la"; place$area <- la }
    })

    # ---- year groups ----
    output$yg_head <- renderText(paste0("Pupils by year group, ", yl(tail(V21$years, 1)), ", ", U()$area))
    output$yg <- renderPlot({
      u <- U(); req(u, has(u))
      g <- c("R", paste0("Y", 1:11)); now <- tail(V21$years, 1)
      d <- V21$pupils[V21$pupils$area == u$area & V21$pupils$year %in% c(now, T0), c("year", g)] |>
        tidyr::pivot_longer(-year, names_to = "yg", values_to = "n") |>
        mutate(yg = factor(yg, g, c("Reception", paste("Year", 1:11))), stage = ifelse(yg %in% c("Reception", paste("Year", 1:6)), "Primary", "Secondary"))
      ggplot(d[d$year == now, ], aes(yg, n)) + geom_col(aes(fill = stage), width = 0.72) +
        geom_point(data = d[d$year == T0, ], shape = 21, fill = "white", colour = DPURPLE, size = 2.6, stroke = 1) +
        scale_fill_manual(values = c(Primary = MPURPLE, Secondary = DPURPLE), name = NULL) +
        scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
        labs(x = NULL, y = "Pupils") + theme_sis(12) + theme(legend.position = "top", axis.text.x = element_text(angle = 40, hjust = 1))
    }, res = 96)

    # ---- openings and closures ----
    ev_area <- reactive({
      u <- U(); req(u); e <- V21$events
      switch(u$level, England = e, Region = e[e$region == u$area, ], e[e$area == u$area, ])
    })
    output$ev_head <- renderText(paste0("State schools opened and closed since 2015/16, ", U()$area))
    output$events <- renderPlot({
      e <- ev_area(); validate(need(nrow(e) > 0, "No state mainstream schools opened or closed here since 2015/16."))
      d <- e |> count(year, stage, event) |> mutate(x = yr0(year), event = factor(event, names(EVENT_COL)))
      ggplot(d, aes(x, n, fill = event)) + geom_col(width = 0.75, colour = "white", linewidth = 0.3) + facet_wrap(~stage, nrow = 1) +
        scale_fill_manual(values = EVENT_COL, name = NULL, drop = FALSE) + scale_x_continuous(labels = yr_short, breaks = function(l) seq(ceiling(l[1]), floor(l[2]), by = 3)) +
        scale_y_continuous(breaks = function(l) unique(floor(pretty(seq(0, max(1, l[2]))))), expand = expansion(mult = c(0, 0.05))) +
        labs(x = NULL, y = "Schools") + theme_sis(12) + theme(legend.position = "top")
    }, res = 96)
    output$events_tbl <- renderDT({
      e <- ev_area()
      t <- e |> arrange(desc(date)) |> transmute(School = name, Authority = la, Stage = stage, Event = event, Year = yl(year),
                                                 Date = format(as.Date(date), "%d %b %Y"), Reason = reason, Type = type, Pupils = pupils, Capacity = capacity)
      datatable(t, rownames = FALSE, filter = "top", extensions = "Buttons",
                options = list(pageLength = 8, scrollX = TRUE, dom = "Bfrtip", buttons = c("csv")), class = "compact stripe hover")
    })
  })
}
