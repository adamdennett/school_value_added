# mod_area.R - Area Overview: a region, combined or mayoral authority, local authority,
# district or academy trust. Its schools on a map and in a table, pupil-weighted area
# figures for every group beside England, its schools' value added, and the area against
# the others of its kind. For local authorities, the model's own authority effect too.

mod_area_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(fillable = FALSE,
    sidebar = sidebar(width = 310, title = "Choose an area",
      area_picker_ui(ns, level = "la", levels = LEVELS[-1]),
      selectInput(ns("colour"), "Colour the map by", MEASURE_CHOICES, selected = "va"),
      note("Area figures are pupil-weighted means of their schools. A trust's figures cover its secondary schools in this data; trusts with fewer than two are not listed.")),
    uiOutput(ns("kpis")),
    layout_columns(col_widths = c(6, 6),
      card(full_screen = TRUE, card_header("Schools in the area", class = "sis-ch"), leafletOutput(ns("map"), height = 470)),
      card(card_header("Every group of pupils, against England", class = "sis-ch"), tableOutput(ns("groups")),
           card_footer(class = "sis-note", textOutput(ns("groups_note"))))),
    card(full_screen = TRUE, card_header("Each school's value added", class = "sis-ch"), uiOutput(ns("schools_ui")),
         card_footer(class = "sis-note", "All, disadvantaged and non-disadvantaged pupils. Lines are 95% intervals (four-year period only); a solid point's interval is clear of zero.")),
    card(full_screen = TRUE, card_header(textOutput(ns("rank_head"), inline = TRUE), class = "sis-ch"), plotOutput(ns("rank"), height = 440),
         card_footer(class = "sis-note", textOutput(ns("rank_note")))),
    card(card_header("All the schools", class = "sis-ch"), DTOutput(ns("table"))))
}

mod_area_server <- function(id, ctx, sel, go) {
  moduleServer(id, function(input, output, session) {
    area_picker_server(input, session)
    A <- reactive({ req(area_ok(input$level, input$area)); in_area(ctx()$M, input$level, input$area) })
    nm <- reactive(paste0(input$area, " (", tolower(LEVEL_LAB[[input$level]]), ")"))

    output$kpis <- renderUI({
      a <- A(); M <- ctx()$M; w <- a$pupils
      box <- function(t, v, s, th = "primary") value_box(title = t, value = v, p(s), theme = th, class = "sis-vb")
      layout_columns(col_widths = c(3, 3, 3, 3), fill = FALSE,
        box("Schools", nrow(a), paste(format(sum(w, na.rm = TRUE), big.mark = ","), "pupils"), "light"),
        box(paste("Attainment 8,", tolower(ctx()$glab)), f1(wmean(a$att8, w)), paste0("Expected ", f1(wmean(a$exp, w)), "; England ", f1(wmean(M$att8, M$pupils))), "light"),
        box("Value added", sg(wmean(a$va, w)), paste0(sum(a$va > 0, na.rm = TRUE), " schools above expected, ", sum(a$va < 0, na.rm = TRUE), " below"), "light"),
        box("Absence against expected", paste(sg(wmean(a$abs_excess, w)), "pp"), paste0("Absence ", f1(wmean(a$abs_obs, w)), "%, all pupils"), "light"))
    })

    output$groups <- renderTable({
      c <- ctx(); a0 <- A()
      do.call(rbind, lapply(GROUPS, function(g) {
        M <- c$for_group(g); a <- M[M$URN %in% a0$URN, ]
        data.frame(Pupils = GLAB[[g]], Schools = sum(!is.na(a$att8)), `Attainment 8` = f1(wmean(a$att8, a$pupils)),
                   Expected = f1(wmean(a$exp, a$pupils)), `Value added` = sg(wmean(a$va, a$pupils)),
                   `England Att8` = f1(wmean(M$att8, M$pupils)), check.names = FALSE) }))
    }, striped = TRUE, spacing = "s", align = "lrrrrr")
    output$groups_note <- renderText(paste0(ctx()$txt, ". England's value added is zero by construction."))

    output$map <- renderLeaflet(base_map())
    ready <- reactive(!is.null(input$map_zoom))
    observe({ req(ready())
      a <- A(); M <- ctx()$M; key <- input$colour; req(key); a <- a[!is.na(a$lat), ]; req(nrow(a) > 0)
      pl <- measure_pal(key, a[[key]], cap = if (mdef(key)$scale == "div") cap_for(M, key)); md <- mdef(key)
      las <- unique(a$la); b <- D$bounds[D$bounds$la %in% las, ]
      mp <- leafletProxy(session$ns("map")) |> clearMarkers() |> clearShapes() |> clearControls()
      if (input$level %in% c("la", "region", "ca") && nrow(b))
        mp <- mp |> addPolygons(data = b, fill = TRUE, fillColor = PPURPLE, fillOpacity = 0.25, color = DPURPLE, weight = 1.2, label = ~la)
      mp |> addCircleMarkers(data = a, lng = ~lon, lat = ~lat, layerId = ~URN, radius = ~pmin(pmax(sqrt(pupils) / 4.5, 4), 11),
                             fillColor = pl$pal(pl$clamp(a[[key]])), fillOpacity = 0.92, color = "#3a3a4e", weight = 0.7,
                             popup = popup_school(a, ctx()$glab, ctx()$txt), label = ~name) |>
        addLegend("bottomright", pal = pl$pal, values = pl$domain, title = md$short, opacity = 0.9, labFormat = labelFormat(suffix = md$unit)) |>
        fit_to(a)
    })
    observeEvent(input$map_marker_click, { u <- input$map_marker_click$id; if (!is.null(u)) { sel(as.integer(u)); go("finder") } })

    output$schools_ui <- renderUI({ n <- nrow(A()); plotOutput(session$ns("schools"), height = if (n > 45) 520 else max(260, 26 * n + 110)) })
    output$schools <- renderPlot({
      c <- ctx(); a0 <- A()
      d <- bind_rows(lapply(MAIN3, function(g) { m <- c$for_group(g); m[m$URN %in% a0$URN, c("URN", "name", "va", "lo", "hi")] |> mutate(Pupils = GLAB[[g]]) })) |>
        filter(!is.na(va)) |> mutate(Pupils = factor(Pupils, GLAB[MAIN3]), clear = !is.na(lo) & (lo > 0 | hi < 0))
      validate(need(nrow(d) > 0, "No value added for these schools."))
      ord <- d |> filter(Pupils == GLAB[["all"]]) |> arrange(va) |> pull(name)
      if (length(unique(d$name)) > 45) {
        return(ggplot(d, aes(va, fill = Pupils)) + geom_vline(xintercept = 0, colour = GREY) + geom_histogram(bins = 30, alpha = 0.8, position = "identity") +
          facet_wrap(~Pupils, ncol = 1) + scale_fill_manual(values = GROUP_COL, guide = "none") +
          labs(x = "Value added (points against England)", y = "Schools", subtitle = paste(length(unique(d$name)), "schools: too many to list, so their spread")) + theme_sis())
      }
      d$name <- factor(d$name, unique(c(ord, setdiff(d$name, ord))))
      ggplot(d, aes(va, name, colour = Pupils)) + geom_vline(xintercept = 0, colour = GREY) +
        geom_linerange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.65), linewidth = 0.5, na.rm = TRUE) +
        geom_point(aes(shape = clear), position = position_dodge(width = 0.65), size = 2.2, fill = "white", stroke = 1) +
        scale_shape_manual(values = c(`TRUE` = 19, `FALSE` = 21), guide = "none") + scale_colour_manual(values = GROUP_COL) +
        labs(x = "Value added (points against England)", y = NULL, colour = NULL) + theme_sis(12) + theme(legend.position = "top")
    }, res = 96)

    output$rank_head <- renderText(if (input$level == "la") "The authority's effect on results, among England's authorities" else
      paste("The area among", c(region = "England's regions", ca = "the combined and mayoral authorities", lad = "England's districts", trust = "academy trusts")[[input$level]]))
    output$rank <- renderPlot({
      lev <- input$level; c <- ctx()
      if (lev == "la") {
        e <- D$la_effects |> filter(group %in% MAIN3) |> group_by(group) |> arrange(pct, .by_group = TRUE) |>
          mutate(r = row_number(), top = n() - r + 1, n = n()) |> ungroup() |> mutate(me = la == input$area)
        me <- e[e$me, ]
        strip <- setNames(paste0(GLAB[MAIN3], ": ", sapply(MAIN3, function(g) { x <- me[me$group == g, ]
          if (nrow(x)) paste0(sg(x$pct), "%, ", x$top, " of ", x$n, " from the top") else "not estimated" })), MAIN3)
        e$Pupils <- factor(strip[e$group], strip); me$Pupils <- factor(strip[me$group], strip)
        return(ggplot(e, aes(pct, r)) + geom_vline(xintercept = 0, colour = GREY) +
          geom_linerange(aes(xmin = lo, xmax = hi), colour = LIGHT, linewidth = 0.4) + geom_point(colour = MPURPLE, size = 0.9) +
          geom_linerange(data = me, aes(xmin = lo, xmax = hi), colour = DPURPLE, linewidth = 1.4) + geom_point(data = me, colour = DPURPLE, size = 3.2) +
          facet_wrap(~Pupils, nrow = 1) + labs(x = "Authority effect on Attainment 8 (%), with 95% interval", y = "Authorities, lowest to highest") +
          theme_sis() + theme(axis.text.y = element_blank()))
      }
      M <- c$M; k <- M[!is.na(M[[lev]]) & !is.na(M$va), ]
      ag <- k |> group_by(area = .data[[lev]]) |> summarise(va = wmean(va, pupils), n = n(), .groups = "drop")
      if (lev == "trust") ag <- ag |> filter(n >= 3 | area == input$area)
      ag <- ag |> arrange(va) |> mutate(r = row_number(), me = area == input$area)
      many <- nrow(ag) > 40
      p <- ggplot(ag, aes(va, if (many) r else factor(area, area))) + geom_vline(xintercept = 0, colour = GREY) +
        geom_segment(aes(x = 0, xend = va, yend = if (many) r else factor(area, area)), colour = LIGHT) +
        geom_point(aes(colour = me, size = me)) + scale_colour_manual(values = c(`TRUE` = DPURPLE, `FALSE` = MPURPLE), guide = "none") +
        scale_size_manual(values = c(`TRUE` = 3.5, `FALSE` = 1.6), guide = "none") +
        labs(x = paste("Pupil-weighted value added,", tolower(c$glab)), y = NULL) + theme_sis(11)
      if (many) p <- p + geom_text(data = ag[ag$me, ], aes(label = paste0(area, " (", r, " of ", nrow(ag), ")")), hjust = -0.1, colour = DPURPLE, size = 3.6) +
        theme(axis.text.y = element_blank()) + labs(y = paste(nrow(ag), "areas, lowest to highest"))
      p
    }, res = 96)
    output$rank_note <- renderText(if (input$level == "la")
      "From the value-added model: how much higher or lower results are in schools in the authority than in similar schools elsewhere, over four years. Its interval shows the uncertainty." else
      paste0(ctx()$txt, ". Pupil-weighted means of schools' value added; areas with few schools move more.",
             if (input$level == "trust") " Trusts with three or more secondary schools here." else ""))

    output$table <- renderDT({
      a <- A()
      t <- a |> transmute(School = name, District = lad, Authority = la, `Run by` = run_by, Pupils = pupils,
                          `FSM6 %` = round(fsm, 1), `Attainment 8` = round(att8, 1), Expected = round(exp, 1),
                          `Value added` = round(va, 1), Low = round(lo, 1), High = round(hi, 1),
                          `Absence %` = round(abs_obs, 1), `Absence against expected` = round(abs_excess, 1), Ofsted = ofsted) |>
        arrange(desc(`Value added`))
      if (all(is.na(t$Low))) t <- t |> select(-Low, -High)
      datatable(t, rownames = FALSE, filter = "top", extensions = "Buttons",
                options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip", buttons = c("csv")), class = "compact stripe hover")
    })
  })
}
