# mod_obs.R - Observed vs Predicted: every school's Attainment 8 against what its intake
# predicts (the vertical gap is value added before shrinkage), and its absence against
# what its intake predicts, with an area and the chosen school picked out

mod_obs_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(fillable = FALSE,
    sidebar = sidebar(width = 300, title = "Pick out",
      area_picker_ui(ns, level = "la", label = "Area to highlight"),
      checkboxInput(ns("school_on"), "Mark the chosen school", TRUE),
      uiOutput(ns("stats"))),
    layout_columns(col_widths = c(6, 6),
      card(full_screen = TRUE, card_header("Attainment 8: observed against expected", class = "sis-ch"), plotlyOutput(ns("att8"), height = 470),
           card_footer(class = "sis-note", "Above the line: better results than schools with the same intake across England. The vertical distance is value added (unshrunk); hover for each school.")),
      card(full_screen = TRUE, card_header("Absence: observed against expected", class = "sis-ch"), plotlyOutput(ns("abs"), height = 470),
           card_footer(class = "sis-note", "All pupils. Below the line: lower absence than the intake predicts. The absence model has years and regions but no local authority level, so an authority's own pattern shows."))),
    card(full_screen = TRUE, card_header("Value added in the area against England", class = "sis-ch"), plotOutput(ns("dist"), height = 260)))
}

mod_obs_server <- function(id, ctx, sel, place) {
  moduleServer(id, function(input, output, session) {
    area_picker_server(input, session, place)
    dd <- reactive({
      M <- ctx()$M |> mutate(hl = if (input$level == "eng") TRUE else (!is.na(.data[[input$level]]) & .data[[input$level]] == input$area))
      M
    })
    scatter <- function(d, x, y, xl, yl, hover) {
      d <- d[!is.na(d[[x]]) & !is.na(d[[y]]), ]
      r <- range(c(d[[x]], d[[y]])); bg <- d[!d$hl, ]; fg <- d[d$hl, ]; me <- d[d$URN == sel(), ]
      p <- plot_ly(type = "scattergl", mode = "markers") |>
        add_trace(data = bg, x = bg[[x]], y = bg[[y]], text = hover(bg), hoverinfo = "text", name = "England",
                  marker = list(color = LIGHT, size = 5, opacity = 0.7)) |>
        add_trace(data = fg, x = fg[[x]], y = fg[[y]], text = hover(fg), hoverinfo = "text",
                  name = if (input$level == "eng") "England" else input$area, marker = list(color = BPURPLE, size = 7, opacity = 0.85, line = list(color = "white", width = 0.5))) |>
        add_lines(x = r, y = r, line = list(color = GREY, dash = "dash", width = 1), hoverinfo = "none", showlegend = FALSE, inherit = FALSE)
      if (isTRUE(input$school_on) && nrow(me)) p <- p |> add_trace(data = me, x = me[[x]], y = me[[y]], text = hover(me), hoverinfo = "text",
        name = me$name, marker = list(color = INK, size = 14, symbol = "diamond", line = list(color = "white", width = 1.5)))
      p |> layout(xaxis = list(title = xl), yaxis = list(title = yl), legend = list(orientation = "h", y = -0.18)) |> plotly_clean()
    }
    output$att8 <- renderPlotly({
      c <- ctx()
      scatter(dd(), "exp", "att8", "Expected Attainment 8, given intake", paste("Attainment 8,", tolower(c$glab)),
              function(d) paste0("<b>", d$name, "</b><br>", d$lad, ", ", d$la, "<br>Attainment 8 ", f1(d$att8), "; expected ", f1(d$exp), "<br>Value added ", sg(d$va)))
    })
    output$abs <- renderPlotly(
      scatter(dd(), "abs_exp", "abs_obs", "Expected absence, given intake (%)", "Overall absence (%)",
              function(d) paste0("<b>", d$name, "</b><br>", d$lad, ", ", d$la, "<br>Absence ", f1(d$abs_obs), "%; expected ", f1(d$abs_exp), "%<br>", sg(d$abs_excess), " pp")))

    output$stats <- renderUI({
      d <- dd(); a <- d[d$hl, ]; c <- ctx()
      tagList(hr(), h6(if (input$level == "eng") "England" else input$area),
        p(nrow(a), " schools. Pupil-weighted:"),
        tags$ul(class = "sis-list",
          tags$li("Attainment 8 ", tags$strong(f1(wmean(a$att8, a$pupils))), " against ", f1(wmean(a$exp, a$pupils)), " expected"),
          tags$li("Value added ", tags$strong(sg(wmean(a$va, a$pupils)))),
          tags$li(sum(a$va > 0, na.rm = TRUE), " above expected, ", sum(a$va < 0, na.rm = TRUE), " below"),
          tags$li("Absence ", tags$strong(sg(wmean(a$abs_excess, a$pupils))), " pp against expected")),
        note(c$glab, ", ", c$txt, "."))
    })

    output$dist <- renderPlot({
      d <- dd(); d <- d[!is.na(d$va), ]
      nm <- if (input$level == "eng") "England" else input$area
      g <- bind_rows(d |> mutate(set = "England"), d[d$hl, ] |> mutate(set = nm)) |> mutate(set = factor(set, unique(c("England", nm))))
      ggplot(g, aes(va, fill = set, colour = set)) + geom_vline(xintercept = 0, colour = GREY) +
        geom_density(alpha = 0.35, adjust = 0.9) + scale_fill_manual(values = c(LIGHT, BPURPLE)) + scale_colour_manual(values = c(GREY, DPURPLE)) +
        labs(x = "Value added (Attainment 8 points against England)", y = NULL, fill = NULL, colour = NULL) + theme_sis() +
        theme(axis.text.y = element_blank())
    }, res = 96)
  })
}
