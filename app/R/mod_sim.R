# mod_sim.R - Policy Simulator: what the Value Added v2.1 models say a school's (or every
# school in an area's) Attainment 8 would be if absence or intake were different.
# A port of the site's browser simulator (simulator.js). Each model is on log Attainment 8,
# so a lever's change multiplies the school's latest observed Attainment 8 by
# exp(coefficient x change), on the log scale for absence, FSM6 and EAL. Area results are
# pupil-weighted means over the area's schools.

SIM_LEVERS <- tibble::tribble(
  ~k, ~term, ~log, ~label, ~unit, ~min, ~max, ~step, ~dp, ~kind,
  "abs", "log(PERCTOT)", TRUE, "Overall absence", "%", -5, 5, 0.1, 1, "Attendance",
  "fsm", "log(PTFSM6CLA1A)", TRUE, "Disadvantaged pupils (FSM6)", "%", -15, 15, 0.5, 1, "Intake",
  "eal", "log(PNUMEAL)", TRUE, "English as an additional language", "%", -15, 15, 0.5, 1, "Intake",
  "ks2", "ks2_c", FALSE, "KS2 prior attainment (scaled score)", "", -3, 3, 0.1, 1, "Intake",
  "l4", "z_l4", FALSE, "Neighbourhood education (SD)", "", -1, 1, 0.05, 2, "Intake",
  "sen", "sen_support", FALSE, "SEN support", "%", -10, 10, 0.5, 1, "Intake",
  "ehcp", "sen_ehcp", FALSE, "EHC plans", "%", -3, 3, 0.1, 1, "Intake",
  "mob", "mobile", FALSE, "Joined mid-phase (mobility)", "%", -5, 5, 0.5, 1, "Intake")
SIM_ADM <- c("As now" = "", "Non-selective" = "OTHER NON SEL", "Selective" = "SEL", "Non-selective, in a selective area" = "NON SEL IN HIGHLY SEL AREA")
SIM_ADM_TERM <- c("OTHER NON SEL" = "ADMPOL_PTOTHER NON SEL", "SEL" = "ADMPOL_PTSEL")
SIM_S <- D$sim$schools |> inner_join(SCH |> select(URN, name, la, lad, region, ca, trust, pupils), by = "URN")
SIM_ENG_ABS <- wmean(SIM_S$abs, SIM_S$pupils)

# New lever values for every school in S
sim_values <- function(S, val, area, abs_mode, abs_pct) {
  lapply(setNames(SIM_LEVERS$k, SIM_LEVERS$k), function(k) {
    L <- SIM_LEVERS[SIM_LEVERS$k == k, ]; x <- S[[k]]
    y <- if (k == "abs" && area && abs_mode == "pct") x * (1 - abs_pct / 100)
         else if (k == "abs" && area && abs_mode == "toEng") pmin(x, SIM_ENG_ABS) else x + val[[k]]
    if (L$log) y <- pmax(y, 0.1)
    if (L$unit == "%") y <- pmin(pmax(y, 0), 100)
    y
  })
}
# Each school's Attainment 8 now and in the scenario, and each lever's part alone, by group
sim_calc <- function(S, val, area = FALSE, abs_mode = "shift", abs_pct = 0, adm = "", six = "") {
  nv <- sim_values(S, val, area, abs_mode, abs_pct)
  res <- lapply(setNames(GROUPS, GROUPS), function(g) {
    b <- D$sim$coefs[[g]]; a <- S[[paste0("a_", g)]]
    parts <- sapply(SIM_LEVERS$k, function(k) {
      L <- SIM_LEVERS[SIM_LEVERS$k == k, ]; beta <- unname(b[L$term]); if (is.na(beta)) beta <- 0; x <- S[[k]]; y <- nv[[k]]
      p <- if (L$log) beta * (log(pmax(y, 0.1)) - log(pmax(x, 0.1))) else beta * (y - x)
      ifelse(is.na(p), 0, p) }, simplify = "matrix")
    if (!is.matrix(parts)) parts <- matrix(parts, nrow = nrow(S), dimnames = list(NULL, SIM_LEVERS$k))
    if (!area) {
      admB <- function(x) { v <- unname(b[SIM_ADM_TERM[x]]); v[is.na(v)] <- 0; v }   # reference: non-selective in a selective area
      b16 <- if (is.na(b["age16TRUE"])) 0 else unname(b["age16TRUE"])
      parts <- cbind(parts, adm = if (nzchar(adm)) admB(adm) - admB(S$adm) else 0,
                     six = if (nzchar(six)) b16 * (as.numeric(six) - S$age16) else 0)
    }
    tot <- rowSums(parts)
    list(now = a, new = a * exp(tot), alone = a * (exp(parts) - 1))
  })
  list(nv = nv, g = res)
}

mod_sim_ui <- function(id) {
  ns <- NS(id)
  lever <- function(L) div(class = "sis-lever",
    if (L$k == "abs") conditionalPanel(sprintf("!(input['%s'] == 'area' && input['%s'] != 'shift')", ns("mode"), ns("abs_mode")),
      sliderInput(ns(L$k), paste0(L$label, if (L$unit == "%") " (change in points)" else " (change)"), L$min, L$max, 0, step = L$step, width = "100%"))
    else sliderInput(ns(L$k), paste0(L$label, if (L$unit == "%") " (change in points)" else " (change)"), L$min, L$max, 0, step = L$step, width = "100%"))
  layout_sidebar(fillable = FALSE,
    sidebar = sidebar(width = 360, title = "Pull the right lever",
      radioButtons(ns("mode"), "Simulate", c("One school" = "school", "Every school in an area" = "area"), inline = TRUE),
      conditionalPanel(sprintf("input['%s'] == 'school'", ns("mode")),
        selectizeInput(ns("school"), "School", choices = NULL, options = list(placeholder = "Type a name..."))),
      conditionalPanel(sprintf("input['%s'] == 'area'", ns("mode")), area_picker_ui(ns, level = "lad"),
        selectInput(ns("abs_mode"), "Absence scenario", c("Change every school's absence by" = "shift", "Cut every school's absence by a share" = "pct",
                                                           "Bring schools above England's average down to it" = "toEng")),
        conditionalPanel(sprintf("input['%s'] == 'pct'", ns("abs_mode")), sliderInput(ns("abs_pct"), "Cut absence by (%)", 0, 50, 0, step = 1)),
        conditionalPanel(sprintf("input['%s'] == 'toEng'", ns("abs_mode")),
          note("England's average absence is ", f1(SIM_ENG_ABS), "% (pupil-weighted). Schools already below it stay as they are."))),
      h6(class = "sis-h6", "Attendance"), lever(SIM_LEVERS[1, ]),
      h6(class = "sis-h6", "Intake: what if the pupils were different"),
      lapply(seq_len(nrow(SIM_LEVERS))[-1], function(i) lever(SIM_LEVERS[i, ])),
      conditionalPanel(sprintf("input['%s'] == 'school'", ns("mode")), h6(class = "sis-h6", "The school"),
        selectInput(ns("adm"), "Admissions", SIM_ADM),
        selectInput(ns("six"), "Sixth form", c("As now" = "", "Stops at 16" = "1", "Has a sixth form" = "0"))),
      actionButton(ns("reset"), "Reset every lever", icon = icon("rotate-left"), class = "btn-outline-secondary w-100")),
    card(card_header(textOutput(ns("head"), inline = TRUE), class = "sis-ch"), uiOutput(ns("summary"))),
    layout_columns(col_widths = c(6, 6),
      card(full_screen = TRUE, card_header("Attainment 8 now and in the scenario", class = "sis-ch"), plotOutput(ns("dumbbell"), height = 300),
           card_footer(class = "sis-note", "Grey: now (the latest model year); dark purple: the scenario. Green if it rises, purple if it falls.")),
      card(full_screen = TRUE, card_header(textOutput(ns("contrib_head"), inline = TRUE), class = "sis-ch"), plotOutput(ns("contrib"), height = 300),
           card_footer(class = "sis-note", "Each lever moved on its own. Together they combine on the log scale, so the parts need not add exactly to the total."))),
    card(card_header("The levers: now and in the scenario", class = "sis-ch"), tableOutput(ns("levers"))),
    conditionalPanel(sprintf("input['%s'] == 'area'", ns("mode")),
      card(card_header("Schools in the area", class = "sis-ch"), DTOutput(ns("area_table")))),
    card(class = "sis-caution", card_body(
      p(tags$strong("What this is and is not. "), "The simulator applies the model's coefficients to a school's latest results. ",
        "It shows what schools with that intake or absence achieve on average across England, not what a policy would certainly do. ",
        "Absence is the lever schools and councils can most affect; the intake levers ask what the model expects if a school's pupils were different, as an admissions change might make them."))))
}

mod_sim_server <- function(id, ctx, sel) {
  moduleServer(id, function(input, output, session) {
    area_picker_server(input, session)
    ch <- setNames(SIM_S$URN, school_label(SIM_S))[order(SIM_S$name)]
    updateSelectizeInput(session, "school", choices = ch, selected = isolate(sel()), server = TRUE)
    observeEvent(input$school, { u <- as.integer(input$school); if (!is.na(u) && !identical(u, sel())) sel(u) }, ignoreInit = TRUE)
    observeEvent(sel(), if (!identical(as.integer(input$school), sel()) && sel() %in% SIM_S$URN)
      updateSelectizeInput(session, "school", choices = ch, selected = sel(), server = TRUE), ignoreInit = TRUE)
    observeEvent(input$reset, { for (k in SIM_LEVERS$k) updateSliderInput(session, k, value = 0)
      updateSliderInput(session, "abs_pct", value = 0); updateSelectInput(session, "adm", selected = ""); updateSelectInput(session, "six", selected = "") })

    area <- reactive(identical(input$mode, "area"))
    S <- reactive({
      if (area()) { req(area_ok(input$level, input$area)); in_area(SIM_S, input$level, input$area) }
      else { u <- as.integer(input$school %||% sel()); SIM_S[SIM_S$URN == u, ] }
    })
    val <- reactive(setNames(lapply(SIM_LEVERS$k, function(k) input[[k]] %||% 0), SIM_LEVERS$k))
    R <- reactive({ s <- S(); validate(need(nrow(s) > 0, "No school with model results here."))
      list(s = s, r = sim_calc(s, val(), area(), input$abs_mode %||% "shift", input$abs_pct %||% 0, input$adm %||% "", input$six %||% "")) })
    agg <- reactive({
      x <- R(); s <- x$s
      lapply(x$r$g, function(z) { ok <- !is.na(z$now)
        list(now = wmean(z$now[ok], s$pupils[ok]), new = wmean(z$new[ok], s$pupils[ok]),
             alone = apply(z$alone[ok, , drop = FALSE], 2, function(v) wmean(v, s$pupils[ok]))) })
    })

    output$head <- renderText(if (area()) paste0(LEVEL_LAB[[input$level]], ": ", input$area, " (", nrow(S()), " schools)") else S()$name[1])
    output$summary <- renderUI({
      a <- agg(); d <- sapply(a, function(z) z$new - z$now); gap_now <- a$non_disadvantaged$now - a$disadvantaged$now
      gap_new <- a$non_disadvantaged$new - a$disadvantaged$new
      tagList(
        p(HTML(paste0("In the scenario, Attainment 8 for all pupils goes from <b>", f1(a$all$now), "</b> to <b>", f1(a$all$new),
          "</b> (<b>", sg(d[["all"]], 2), "</b>); disadvantaged pupils ", sg(d[["disadvantaged"]], 2), ", non-disadvantaged ",
          sg(d[["non_disadvantaged"]], 2), ", girls ", sg(d[["girls"]], 2), ", boys ", sg(d[["boys"]], 2), "."))),
        p(HTML(paste0("The gap between non-disadvantaged and disadvantaged pupils goes from ", f1(gap_now), " to ", f1(gap_new),
          " points (", sg(gap_new - gap_now, 2), ")."))),
        note(if (area()) "Pupil-weighted means over the area's schools, each in its latest model year." else
          paste0("Starting from ", yl(S()$sim_year[1]), ", the school's latest year in the model.")))
    })
    output$dumbbell <- renderPlot({
      a <- agg(); d <- tibble(Pupils = factor(GLAB[names(a)], rev(GLAB)), now = sapply(a, `[[`, "now"), new = sapply(a, `[[`, "new")) |>
        filter(!is.na(now)) |> mutate(dir = ifelse(new - now > 0.005, "up", ifelse(new - now < -0.005, "down", "same")))
      ggplot(d, aes(y = Pupils)) + geom_segment(aes(x = now, xend = new, colour = dir), linewidth = 2.2, alpha = 0.55) +
        geom_point(aes(x = now), colour = "#8a8aa3", fill = LIGHT, shape = 21, size = 4) + geom_point(aes(x = new), colour = DPURPLE, size = 4) +
        geom_text(aes(x = pmax(now, new), label = sg(new - now, 2), colour = dir), hjust = -0.35, size = 3.8, fontface = "bold") +
        scale_colour_manual(values = c(up = UP, down = DOWN, same = GREY), guide = "none") +
        scale_x_continuous(expand = expansion(mult = c(0.05, 0.18))) + labs(x = "Attainment 8", y = NULL) + theme_sis()
    }, res = 96)
    output$contrib_head <- renderText(paste("What each lever contributes,", tolower(ctx()$glab)))
    output$contrib <- renderPlot({
      a <- agg()[[ctx()$group]]; lab <- c(setNames(SIM_LEVERS$label, SIM_LEVERS$k), adm = "Admissions", six = "Sixth form")
      d <- tibble(k = names(a$alone), v = unname(a$alone)) |> filter(abs(v) >= 0.005) |> mutate(l = lab[k])
      validate(need(nrow(d) > 0, "Move a lever to see what each one contributes."))
      ggplot(d, aes(v, reorder(l, v), fill = v > 0)) + geom_col(width = 0.6) + geom_vline(xintercept = 0, colour = GREY) +
        geom_text(aes(label = sg(v, 2), hjust = ifelse(v > 0, -0.2, 1.2)), size = 3.8) +
        scale_fill_manual(values = c(`TRUE` = UP, `FALSE` = DOWN), guide = "none") +
        scale_x_continuous(expand = expansion(mult = 0.25)) + labs(x = "Change in Attainment 8 (points)", y = NULL) + theme_sis()
    }, res = 96)
    output$levers <- renderTable({
      x <- R(); s <- x$s; nv <- x$r$nv
      tibble(Lever = SIM_LEVERS$label,
             Now = mapply(function(k, dp) f1(wmean(s[[k]], s$pupils), dp), SIM_LEVERS$k, SIM_LEVERS$dp),
             Scenario = mapply(function(k, dp) f1(wmean(nv[[k]], s$pupils), dp), SIM_LEVERS$k, SIM_LEVERS$dp),
             England = mapply(function(k, dp) f1(wmean(SIM_S[[k]], SIM_S$pupils), dp), SIM_LEVERS$k, SIM_LEVERS$dp))
    }, striped = TRUE, spacing = "s", align = "lrrr")
    output$area_table <- renderDT({
      req(area()); x <- R(); s <- x$s; g <- x$r$g
      t <- tibble(School = s$name, District = s$lad, `Absence now` = round(s$abs, 1), `Absence then` = round(x$r$nv$abs, 1),
                  `All now` = round(g$all$now, 1), `All change` = round(g$all$new - g$all$now, 2),
                  `Disadv. change` = round(g$disadvantaged$new - g$disadvantaged$now, 2),
                  `Non-disadv. change` = round(g$non_disadvantaged$new - g$non_disadvantaged$now, 2)) |> arrange(desc(`All change`))
      datatable(t, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE), class = "compact stripe hover")
    })
  })
}
