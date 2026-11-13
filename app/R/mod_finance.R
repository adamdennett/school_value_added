# mod_finance.R - School Finances (v2.1): how state schools' budgets stand in an area,
# against its region and England, and how many schools show the warning signs that came
# before closure. From the finance pages (finance.qmd, finance_areas/*, R/43, R/44) via
# data/v21.rds:
#   - DfE consistent financial reporting (CFR) returns for maintained schools, financial
#     years 2014-15 to 2025-26, and academies' accounts returns (AAR), 2016-17 to 2024-25
#   - aggregates only: no share, median or line from fewer than 5 schools, and no school
#     named, as agreed for the finance work
#   - warning signs: counts of open schools with three or more of the five signs, and in
#     the closure model's riskiest 2% (R/43), by authority and Sussex district

FIN_STAGES <- c("Primary" = "Primary", "Secondary" = "Secondary", "Both" = "All")
FIN_SECTORS <- c("Maintained schools" = "Maintained", "Academies" = "Academy", "Both" = "All")
FIN_MAP <- c("Share of schools in deficit (%)" = "deficit_pct",
             "Share with an in-year deficit (%)" = "iy_deficit_pct",
             "Median reserve, % of income" = "med_reserve",
             "Income per pupil, % of England" = "income_rel")
SECTOR_TXT <- c(Maintained = "maintained schools", Academy = "academies", All = "state schools")

mod_finance_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(fillable = FALSE,
    sidebar = sidebar(width = 310, title = "Choose an area",
      area_picker_ui(ns, level = "la", levels = V21_LEVELS),
      radioButtons(ns("stage"), "Schools", FIN_STAGES, selected = "Primary", inline = TRUE),
      radioButtons(ns("sector"), "Run by", FIN_SECTORS, selected = "Maintained"),
      selectInput(ns("measure"), "Colour the map by", FIN_MAP, selected = "deficit_pct"),
      note("Maintained schools' returns run to 2025-26, academies' to 2024-25. An academy's reserve leaves out its trust's central reserve, and some trusts report none, so maintained schools give the clearest reserve series."),
      note("In deficit: a negative revenue reserve at the year end. Districts: the Sussex districts so far. Click the map to choose an authority."),
      note(tags$a(href = paste0(SITE, "/finance.html"), target = "_blank", "School finances before closure"), " and ",
           tags$a(href = paste0(SITE, "/method.html#sec-finance-method"), target = "_blank", "the early-warning model"), " on the site have the method.")),
    uiOutput(ns("unit_note")),
    uiOutput(ns("kpis")),
    layout_columns(col_widths = c(6, 6), fill = FALSE,
      card(full_screen = TRUE, card_header(textOutput(ns("def_head"), inline = TRUE), class = "sis-ch"), plotOutput(ns("deficit"), height = 340),
           card_footer(class = "sis-note", "Share of schools with a return whose revenue reserve was negative at the year end. Years are financial years (maintained schools, April to March) or academic years (academies, September to August).")),
      card(full_screen = TRUE, card_header("Median revenue reserve, % of income", class = "sis-ch"), plotOutput(ns("reserve"), height = 340),
           card_footer(class = "sis-note", "The middle school's reserve as a share of its income. Below zero, more than half the schools are in deficit."))),
    layout_columns(col_widths = c(6, 6), fill = FALSE,
      card(full_screen = TRUE, card_header("Income per pupil (£, cash)", class = "sis-ch"), plotOutput(ns("income"), height = 340),
           card_footer(class = "sis-note", "Total income over total pupils (FTE), in cash terms (not adjusted for inflation). Income follows need through the funding formula, so a lower figure is not in itself a problem.")),
      card(full_screen = TRUE, card_header(textOutput(ns("band_head"), inline = TRUE), class = "sis-ch"), plotOutput(ns("bands"), height = 340),
           card_footer(class = "sis-note", "Share of schools in each band of revenue reserve as a % of income, in the latest year."))),
    layout_columns(col_widths = c(7, 5), fill = FALSE,
      card(full_screen = TRUE, card_header(textOutput(ns("map_head"), inline = TRUE), class = "sis-ch"), leafletOutput(ns("map"), height = 480),
           card_footer(class = "sis-note", "Latest year. Grey: fewer than 5 schools with a return. Three reorganised councils are shown combined.")),
      card(card_header("Warning signs among open schools", class = "sis-ch"), uiOutput(ns("warn")))),
    uiOutput(ns("sussex_ui")))
}

mod_finance_server <- function(id, ctx, place) {
  moduleServer(id, function(input, output, session) {
    area_picker_server(input, session, place, levels = V21_LEVELS)
    U <- reactive({ req(area_ok(input$level, input$area)); v21_unit(input$level, input$area, districts = TRUE) })
    R <- reactive({ u <- U(); validate(need(!is.null(u), "Choose an authority, a district within one authority, a region or England.")); v21_comp(u) })
    LAT <- reactive(V21$fin$latest[[input$sector]])
    S <- reactive({                                      # every year for the area and its comparisons, chosen stage and sector
      r <- R(); s <- V21$fin$stats
      s <- s[s$stage == input$stage & s$sector == input$sector & paste(s$level, s$unit) %in% paste(r$level, r$area), ]
      few <- s$schools < MIN_SCHOOLS
      s[few, c("deficit_pct", "iy_deficit_pct", "med_reserve", "med_in_year", "income_pp")] <- NA
      s$area <- s$unit; s
    })

    output$unit_note <- renderUI({
      u <- U(); s <- V21$fin$stats
      msg <- c(if (is.null(u)) "Finance figures are by authority (and the Sussex districts): choose an authority, a district within one authority, a region or England." else u$note,
               if (!is.null(u) && !any(s$level == u$level & s$unit == u$area)) paste0("No finance returns are summarised for ", u$area, "."))
      if (length(msg)) div(class = "alert alert-light sis-note py-2", icon("circle-info"), " ", paste(msg, collapse = " "))
    })

    output$kpis <- renderUI({
      u <- U(); req(u); s <- S(); lat <- LAT()
      a <- s[s$level == u$level & s$unit == u$area & s$year == lat, ]; e <- s[s$level == "England" & s$year == lat, ]
      req(nrow(a) == 1)
      eng <- function(x, f) if (u$level == "England") NULL else paste0("England ", f(e[[x]]))
      few <- a$schools < MIN_SCHOOLS
      first <- s[s$level == u$level & s$unit == u$area & s$year == "2021-22", ]
      res_m <- function(x) paste0(ifelse(x < 0, "−£", "£"), f1(abs(x) / 1e6, 1), "m")
      w <- warn_of(u, input$stage)
      div(class = "sis-kpis",
        kpi(paste0("In deficit, ", yl(lat)), if (few) "–" else pct(a$deficit_pct),
            if (few) paste0("Fewer than ", MIN_SCHOOLS, " ", SECTOR_TXT[[input$sector]], " with a return") else
              paste0(round(a$deficit_pct * a$schools / 100), " of ", a$schools, " ", SECTOR_TXT[[input$sector]], if (!is.null(eng("deficit_pct", pct))) paste0("; ", eng("deficit_pct", pct))),
            cls = if (!few && u$level != "England" && a$deficit_pct > e$deficit_pct + 5) "down" else ""),
        kpi("Median reserve, % of income", if (few) "–" else spct(a$med_reserve), eng("med_reserve", spct)),
        kpi(paste0("Net revenue reserves, ", yl(lat)), if (few) "–" else res_m(a$reserve),
            if (few) NULL else if (nrow(first) && first$schools >= MIN_SCHOOLS && input$sector == "Maintained") paste0(res_m(first$reserve), " in 2021/22") else paste(a$schools, "schools")),
        kpi("Income per pupil", if (few) "–" else paste0("£", format(round(a$income_pp), big.mark = ",")),
            if (u$level != "England" && !few) paste0(round(100 * a$income_pp / e$income_pp), "% of England's")),
        if (!is.null(w)) kpi("Three or more warning signs", w$three_plus, paste0("of ", format(w$schools, big.mark = ","), " open schools; ", w$flagged, " in the model's riskiest 2%")))
    })

    output$def_head <- renderText(paste0("Share of ", SECTOR_TXT[[input$sector]], " in deficit"))
    output$deficit <- renderPlot({
      d <- S(); validate(need(any(!is.na(d$deficit_pct)), "Too few schools with a return here."))
      v21_lines(d, "deficit_pct", "% in deficit", R(), hline = 0) + scale_y_continuous(labels = function(x) paste0(x, "%"))
    }, res = 96)
    output$reserve <- renderPlot({
      d <- S(); validate(need(any(!is.na(d$med_reserve)), "Too few schools with a return here."))
      v21_lines(d, "med_reserve", "Median reserve, % of income", R(), hline = 0) + scale_y_continuous(labels = function(x) paste0(x, "%"))
    }, res = 96)
    output$income <- renderPlot({
      d <- S(); validate(need(any(!is.na(d$income_pp)), "Too few schools with a return here."))
      v21_lines(d, "income_pp", "Income per pupil", R()) + scale_y_continuous(labels = function(x) paste0("£", format(x, big.mark = ",")))
    }, res = 96)

    output$band_head <- renderText(paste0("Reserves in ", yl(LAT()), ", against England"))
    output$bands <- renderPlot({
      u <- U(); r <- R(); b <- V21$fin$bands
      b <- b[b$stage == input$stage & b$sector == input$sector & paste(b$level, b$unit) %in% paste(r$level, r$area), ]
      b <- b |> group_by(unit) |> mutate(total = sum(schools), share = 100 * schools / total) |> ungroup() |> filter(total >= MIN_SCHOOLS)
      validate(need(any(b$unit == u$area), "Too few schools with a return here."))
      b <- b |> filter(unit %in% c(u$area, "England"))
      b$band <- factor(b$band, V21$fin$band_labels); b$unit <- factor(b$unit, unique(c(u$area, "England")))
      b$neg <- b$band %in% V21$fin$band_labels[1:3]
      ggplot(b, aes(band, share, fill = unit)) + geom_col(position = position_dodge(width = 0.8), width = 0.75) +
        annotate("rect", xmin = 0.5, xmax = 3.5, ymin = -Inf, ymax = Inf, fill = DOWN, alpha = 0.06) +
        scale_fill_manual(values = setNames(c(DPURPLE, LIGHT)[seq_len(nlevels(b$unit))], levels(b$unit)), name = NULL) +
        scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
        labs(x = "Revenue reserve, % of income (shaded: in deficit)", y = "Schools") + theme_sis(12) +
        theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))
    }, res = 96)

    # ---- map ----
    output$map_head <- renderText(paste0(names(FIN_MAP)[FIN_MAP == input$measure], ", ", SECTOR_TXT[[input$sector]], ", ", yl(LAT())))
    output$map <- renderLeaflet(base_map())
    observe({
      req(!is.null(input$map_zoom), input$measure)
      s <- V21$fin$stats; s <- s[s$stage == input$stage & s$sector == input$sector & s$year == LAT(), ]
      e <- s$income_pp[s$level == "England"]; s <- s[s$level == "Authority", ]
      s$income_rel <- 100 * s$income_pp / e
      v <- s[[input$measure]]; v[s$schools < MIN_SCHOOLS] <- NA; vals <- setNames(v, s$unit)
      key <- input$measure
      # worse is purple and better green for deficits and reserves; income has no better or worse
      dom <- switch(key, deficit_pct = , iy_deficit_pct = c(0, max(5, unname(quantile(v, 0.975, na.rm = TRUE)))),
                    med_reserve = { cap <- max(2, unname(quantile(abs(v), 0.975, na.rm = TRUE))); c(-cap, cap) },
                    income_rel = range(v, na.rm = TRUE))
      cols <- switch(key, deficit_pct = , iy_deficit_pct = rev(PRGN_SEQ), med_reserve = PRGN, income_rel = NEUTRAL)
      pal <- colorNumeric(cols, dom, na.color = "#c9c9d6")
      v21_map(leafletProxy(session$ns("map")), vals, pal, dom, v21_las(U()),
              c(deficit_pct = "In deficit", iy_deficit_pct = "In-year deficit", med_reserve = "Median reserve", income_rel = "Of England")[[key]],
              unit = "%", fmt = function(x) f1(x, 0)) |>
        v21_fit(v21_las(U()))
    })
    observeEvent(input$map_shape_click, {
      la <- input$map_shape_click$id
      if (!is.null(la) && la %in% AREA_CHOICES[["la"]]) { place$level <- "la"; place$area <- la }
    })

    # ---- warning signs ----
    output$warn <- renderUI({
      u <- U(); req(u); m <- V21$model
      rows <- lapply(c("Primary", "Secondary"), function(st) { w <- warn_of(u, st); if (!is.null(w)) tags$tr(tags$td(st), tags$td(format(w$schools, big.mark = ",")), tags$td(w$three_plus), tags$td(w$flagged)) })
      tagList(
        if (length(Filter(Negate(is.null), rows)))
          tags$table(class = "table table-sm mb-2", tags$thead(tags$tr(tags$th(""), tags$th("Open schools"), tags$th("Three or more signs"), tags$th("Riskiest 2%"))), tags$tbody(rows))
        else p(class = "sis-note", "No warning-sign counts for this area."),
        p(class = "sis-note", "The five signs, each seen before closures since 2017/18: a small school (under 100 pupils, primary; under 600, secondary); rolls down 15% or more over three years; under 70% full; an in-year deficit worse than 3% of income; a negative reserve."),
        p(class = "sis-note", paste0("The riskiest 2%: schools above a ", f1(100 * m$cut, 1), "% predicted chance of closing within two years in the early-warning model (rolls, finance and the area's primary-age trend). Tested on ",
          yl("2021-22"), " to ", yl(m$years[2]), ", it ranked schools well (AUC ", f1(m$auc, 2), ") and its riskiest 2% held ", round(100 * m$top2[["caught"]] / m$top2[["closures"]]), "% of the closures.")),
        p(class = "sis-note", tags$strong("No school is named."), " A warning sign is not a forecast for any one school: most schools showing them do not close, and naming them could make it more likely."))
    })

    # ---- Sussex districts ----
    output$sussex_ui <- renderUI({
      u <- U(); req(u)
      if (!(u$area %in% c(SUSSEX_LAS, SUSSEX_LADS) || identical(u$parent, "East Sussex") || identical(u$parent, "West Sussex"))) return(NULL)
      card(card_header(paste0("Across Sussex: the districts, ", SECTOR_TXT[[input$sector]], ", ", yl(LAT())), class = "sis-ch"), tableOutput(session$ns("sussex")),
           card_footer(class = "sis-note", "Each school is placed in its district by its location. Shares and medians need 5 schools; warning signs count all open state schools. The site has a page for each authority and district."))
    })
    output$sussex <- renderTable({
      s <- V21$fin$stats; s <- s[s$level == "District" & s$stage == input$stage & s$sector == input$sector & s$year == LAT(), ]
      un <- V21$fin$units; un <- un[un$level == "District", ]; par <- setNames(un$parent, un$unit)
      t <- data.frame(District = s$unit, Authority = par[s$unit], Schools = s$schools,
                      `In deficit` = ifelse(s$schools < MIN_SCHOOLS, "–", pct(s$deficit_pct)),
                      `Median reserve` = ifelse(s$schools < MIN_SCHOOLS, "–", spct(s$med_reserve)),
                      `Income per pupil` = ifelse(s$schools < MIN_SCHOOLS, "–", paste0("£", format(round(s$income_pp), big.mark = ","))), check.names = FALSE)
      w <- lapply(t$District, function(d) warn_of(list(level = "District", area = d), input$stage))
      t$`Three or more signs` <- vapply(w, function(x) if (is.null(x)) "–" else as.character(x$three_plus), "")
      t$`Riskiest 2%` <- vapply(w, function(x) if (is.null(x)) "–" else as.character(x$flagged), "")
      t[order(t$Authority, t$District), ]
    }, striped = TRUE, spacing = "s", align = "llrrrrrr")
  })
}

# Warning-sign counts for an area and stage ("All" adds primary and secondary)
warn_of <- function(u, stage) {
  w <- V21$warn; w <- w[w$level == u$level & w$area == u$area & (stage == "All" | w$stage == stage), ]
  if (!nrow(w)) return(NULL)
  list(schools = sum(w$schools), three_plus = sum(w$three_plus), flagged = sum(w$flagged))
}
