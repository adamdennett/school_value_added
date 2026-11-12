# mod_twin.R - Find My Twin: the schools in England most like a school in intake and
# setting, or the look-alikes that do better. Matching as on the site's twins page
# (twins.qmd): each characteristic's gap in standard deviations, capped at three
# (0 or 1 for a category), times its weight, averaged over the weights; similarity is
# 100 x (1 - that). Weights are how much each characteristic moves Attainment 8 or
# absence across schools; results and attendance are never matched on.

mod_twin_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(fillable = FALSE,
    sidebar = sidebar(width = 330, title = "Find a twin",
      selectizeInput(ns("school"), "School", choices = NULL, options = list(placeholder = "Type a name...")),
      radioButtons(ns("mode"), "Show", c("Closest twins" = "closest", "Look-alikes doing better" = "better"), selected = "closest"),
      conditionalPanel(sprintf("input['%s'] == 'better'", ns("mode")),
        selectInput(ns("on"), "Doing better on", c("Value added" = "va", "Attainment 8" = "att8", "Absence against expected (lower)" = "abs_excess"))),
      selectInput(ns("wt"), "Match weighted for", c("What drives Attainment 8" = "att8", "What drives absence" = "absence", "Every characteristic equally" = "eq")),
      selectInput(ns("scope"), "Look in", c("All of England" = "eng", "The same region" = "region", "Outside its own authority" = "notla")),
      sliderInput(ns("pool"), "Look-alikes to consider", 10, 100, 30, step = 5),
      sliderInput(ns("n"), "Schools to show", 3, 15, 8, step = 1)),
    card(card_header(textOutput(ns("head"), inline = TRUE), class = "sis-ch"), uiOutput(ns("summary"))),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE, card_header("Value added: the school and its twins", class = "sis-ch"), plotOutput(ns("chart"), height = 420),
           card_footer(class = "sis-note", "All, disadvantaged and non-disadvantaged pupils. With the four-year period, the lines are 95% intervals.")),
      card(full_screen = TRUE, card_header("Where they are", class = "sis-ch"), leafletOutput(ns("map"), height = 420))),
    card(card_header("The twins side by side", class = "sis-ch"), DTOutput(ns("table"))),
    accordion(open = FALSE, accordion_panel("How the matching is weighted", plotOutput(ns("weights"), height = 330),
      note("Each characteristic's weight is the standard deviation of its fitted contribution in a regression of the measure on all fifteen (log absence for absence), scaled to average one; no weight is below 0.05. ",
           "The characteristics explain ", round(100 * D$twins$r2[["att8"]]), "% of Attainment 8 and ", round(100 * D$twins$r2[["absence"]]),
           "% of log absence between schools. A twin that does better is a reason to ask what it does differently, not proof that copying it would work. ",
           tags$a(href = paste0(SITE, "/method.html#sec-twins-method"), target = "_blank", "The full method, with a worked example"), "."))))
}

mod_twin_server <- function(id, ctx, sel, go) {
  moduleServer(id, function(input, output, session) {
    TW <- D$twins; TS <- TW$schools |> inner_join(SCH |> select(URN, name, la, lad, region, lat, lon, pupils), by = "URN")
    SD <- sapply(TW$num, function(v) sd(TS[[v]], na.rm = TRUE))
    updateSelectizeInput(session, "school", choices = setNames(TS$URN, school_label(TS))[order(TS$name)],
                         selected = isolate(sel()), server = TRUE)
    observeEvent(input$school, { u <- as.integer(input$school); if (!is.na(u) && !identical(u, sel())) sel(u) }, ignoreInit = TRUE)
    observeEvent(sel(), if (!identical(as.integer(input$school), sel()) && sel() %in% TS$URN)
      updateSelectizeInput(session, "school", choices = setNames(TS$URN, school_label(TS))[order(TS$name)], selected = sel(), server = TRUE),
      ignoreInit = TRUE)

    weights <- reactive({ v <- c(TW$num, TW$cat); if (input$wt == "eq") setNames(rep(1, length(v)), v) else TW$weights[[input$wt]][v] })
    found <- reactive({
      f <- TS[TS$URN == sel(), ]
      validate(need(nrow(f) == 1, "This school is not in the matching set (it needs 2025/26 absence and a full intake profile)."))
      wt <- weights(); cand <- TS[TS$URN != f$URN, ]
      if (input$scope == "region") cand <- cand[cand$region == f$region, ]
      if (input$scope == "notla") cand <- cand[cand$la != f$la, ]
      tot <- 0; den <- 0
      for (v in TW$num) { g <- pmin(abs(cand[[v]] - f[[v]]) / SD[[v]] / 3, 1); ok <- !is.na(g)
        tot <- tot + ifelse(ok, wt[[v]] * g, 0); den <- den + ifelse(ok, wt[[v]], 0) }
      for (v in TW$cat) { g <- as.numeric(cand[[v]] != f[[v]]); ok <- !is.na(g)
        tot <- tot + ifelse(ok, wt[[v]] * g, 0); den <- den + ifelse(ok, wt[[v]], 0) }
      cand$sim <- 100 * (1 - tot / den)
      cand <- cand[order(-cand$sim), ][seq_len(min(input$pool, nrow(cand))), ]
      M <- ctx()$M; add <- function(d) d |> left_join(M |> select(URN, va_now = va, lo, hi, att8_now = att8, exp_now = exp, abs_excess), by = "URN")
      f <- add(f); cand <- add(cand)
      if (input$mode == "better") {
        k <- input$on; fv <- f[[c(va = "va_now", att8 = "att8_now", abs_excess = "abs_excess")[[k]]]]
        cv <- cand[[c(va = "va_now", att8 = "att8_now", abs_excess = "abs_excess")[[k]]]]
        cand$gap <- if (k == "abs_excess") fv - cv else cv - fv
        list(f = f, pool = cand, list = cand[!is.na(cand$gap) & cand$gap > 0, ] |> arrange(desc(gap)) |> head(input$n))
      } else list(f = f, pool = cand, list = head(cand, input$n))
    })

    output$head <- renderText(paste("Twins of", SCH$name[SCH$URN == sel()]))
    output$summary <- renderUI({
      r <- found(); f <- r$f
      better <- sum(r$pool$va_now > f$va_now, na.rm = TRUE)
      tagList(p(class = "sis-where", where_txt(SCH[SCH$URN == f$URN, ])),
        p(HTML(paste0("Among its ", nrow(r$pool), " closest look-alikes (similarity ", f1(min(r$pool$sim), 0), " to ", f1(max(r$pool$sim), 0),
          " out of 100), <b>", better, "</b> have higher value added for ", tolower(ctx()$glab), " (", ctx()$txt, ").",
          if (input$mode == "better") paste0(" The ", nrow(r$list), " furthest ahead are shown.") else ""))),
        note("Intake in the table is from the 2025/26 matching set (as on the site's twins page), so it can differ a little from the School Finder's latest model year."))
    })

    output$chart <- renderPlot({
      r <- found(); urns <- c(r$f$URN, r$list$URN); c <- ctx()
      lab <- function(u) { s <- SCH[match(u, SCH$URN), ]; paste0(s$name, " (", s$la, ")", ifelse(u == r$f$URN, "  ◀ this school", "")) }
      d <- bind_rows(lapply(MAIN3, function(g) { m <- c$for_group(g); m[m$URN %in% urns, c("URN", "va", "lo", "hi")] |> mutate(Pupils = GLAB[[g]]) })) |>
        mutate(focal = URN == r$f$URN, name = factor(lab(URN), rev(lab(urns))), Pupils = factor(Pupils, GLAB[MAIN3]))
      validate(need(nrow(d) > 0, "No value added to show."))
      ggplot(d, aes(va, name, colour = Pupils)) + geom_vline(xintercept = 0, colour = GREY) +
        geom_linerange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.6), linewidth = 0.7, na.rm = TRUE) +
        geom_point(aes(size = focal), position = position_dodge(width = 0.6)) +
        scale_size_manual(values = c(`FALSE` = 2.2, `TRUE` = 3.6), guide = "none") + scale_colour_manual(values = GROUP_COL) +
        labs(x = "Value added (points against England)", y = NULL, colour = NULL) + theme_sis() + theme(legend.position = "top")
    }, res = 96)

    output$map <- renderLeaflet(base_map())
    ready <- reactive(!is.null(input$map_zoom))   # drawn once the map has its size
    observe({ req(ready())
      r <- found(); d <- bind_rows(r$f |> mutate(role = "This school"), r$list |> mutate(role = "Twin"))
      leafletProxy(session$ns("map")) |> clearMarkers() |> addCircleMarkers(data = d, lng = ~lon, lat = ~lat, radius = ~ifelse(role == "This school", 10, 7),
        fillColor = ~ifelse(role == "This school", DPURPLE, MPURPLE), fillOpacity = 0.9, color = "white", weight = 1.5,
        label = ~paste0(name, " (", la, ")"), popup = ~paste0("<b>", htmlEscape(name), "</b><br>", htmlEscape(la), "<br>",
          ifelse(role == "Twin", paste0("Similarity ", f1(sim, 0), "<br>"), ""), "Value added ", sg(va_now), "<br>Attainment 8 ", f1(att8_now))) |>
        fit_to(d)
    })

    output$table <- renderDT({
      r <- found(); d <- bind_rows(r$f |> mutate(sim = NA), r$list)
      t <- d |> transmute(School = name, Authority = la, Similarity = round(sim), `FSM %` = round(fsm, 1), KS2 = round(ks2, 1),
                          `EAL %` = round(eal, 1), `SEN support %` = round(sen, 1), Pupils = pupils,
                          `Attainment 8` = round(att8_now, 1), Expected = round(exp_now, 1), `Value added` = round(va_now, 1),
                          `Absence against expected` = round(abs_excess, 1))
      datatable(t, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = TRUE, scrollX = TRUE), class = "compact stripe hover") |>
        formatStyle("School", target = "row", fontWeight = styleEqual(r$f$name, "bold"))
    })

    output$weights <- renderPlot({
      w <- weights(); d <- data.frame(v = names(w), w = unname(w), lab = TW$labels[names(w)]) |> arrange(w)
      ggplot(d, aes(w, factor(lab, lab))) + geom_col(fill = MPURPLE, width = 0.7) + geom_vline(xintercept = 1, colour = GREY, linetype = "dashed") +
        labs(x = "Weight (average 1)", y = NULL) + theme_sis(12)
    }, res = 96)
  })
}
