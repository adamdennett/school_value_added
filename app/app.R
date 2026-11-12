# app.R - School Information System v2.0
# ---------------------------------------------------------------------------
# A Shiny version of the v1.0 School Attainment Policy Simulator, rebuilt on Value
# Added v2.1 (https://adamdennett.github.io/school_value_added). Deployed on Posit
# Connect Cloud from the app/ folder of the school_value_added repository.
#
# The bar under the title sets the pupil group and the period for every tab. Each tab
# is a module in R/ (mod_*.R); R/00_setup.R loads the data and shared helpers.
# Rebuild the data with Rscript R/40_app_data.R from the project root.
# ---------------------------------------------------------------------------

theme <- bs_theme(
  version = 5, primary = DPURPLE, secondary = MPURPLE, info = LPURPLE, light = PPURPLE, dark = DPURPLE,
  success = UP, danger = DOWN, bg = "#fdfcff", fg = INK,
  base_font = font_collection("UCL Sans", "DM Sans", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("UCL Sans", "DM Sans", "Segoe UI", "Arial", "sans-serif"),
  "headings-color" = DPURPLE, "link-color" = "#6b2bb8", "link-hover-color" = DPURPLE,
  "navbar-light-color" = DPURPLE, "navbar-light-active-color" = "#6b2bb8", "border-radius" = "0.5rem")

controls <- div(
  class = "sis-bar",
  div(class = "sis-ctl", tags$label("Pupils", `for` = "g_group"),
      selectInput("g_group", NULL, GROUPS, selected = "all", width = "190px")),
  div(class = "sis-ctl", tags$label("Period", `for` = "g_period"),
      selectInput("g_period", NULL, c("Four years, shrunk" = "four", "One year" = "year", "Moving average" = "ma"),
                  selected = "four", width = "180px")),
  conditionalPanel("input.g_period != 'four'", class = "sis-ctl",
                   tags$label(textOutput("year_label", inline = TRUE), `for` = "g_year"),
                   selectInput("g_year", NULL, YEAR_CHOICES, selected = tail(MODEL_YEARS, 1), width = "190px")),
  conditionalPanel("input.g_period == 'ma'", class = "sis-ctl",
                   tags$label("Window", `for` = "g_window"),
                   selectInput("g_window", NULL, c("3 years" = 3, "5 years" = 5), selected = 3, width = "110px")),
  div(class = "sis-bar-note", textOutput("period_note", inline = TRUE)))

ui <- page_navbar(
  id = "nav", title = tags$span(class = "sis-title", "School Information System", tags$small(VERSION)),
  window_title = "School Information System v2.0", theme = theme, fillable = FALSE,
  navbar_options = navbar_options(bg = PPURPLE, theme = "light", collapsible = TRUE),
  header = tagList(tags$head(tags$link(rel = "stylesheet", href = "custom.css")), controls),
  nav_panel("About", value = "about", icon = icon("house"), about_ui()),
  nav_panel("School Finder", value = "finder", icon = icon("magnifying-glass"), mod_finder_ui("finder")),
  nav_panel("Find My Twin", value = "twin", icon = icon("users"), mod_twin_ui("twin")),
  nav_panel("Observed vs Predicted", value = "obs", icon = icon("braille"), mod_obs_ui("obs")),
  nav_panel("Policy Simulator", value = "sim", icon = icon("sliders"), mod_sim_ui("sim")),
  nav_panel("Area Overview", value = "area", icon = icon("map"), mod_area_ui("area")),
  nav_panel("Travel and Distance", value = "travel", icon = icon("person-walking"), mod_travel_ui("travel")),
  nav_menu("Coming next", icon = icon("compass"),
           nav_panel("v2.1: Places and cohorts", value = "v21", future_cohorts_ui()),
           nav_panel("v2.2: Primary schools", value = "v22p", future_primary_ui()),
           nav_panel("v2.2: Post-16", value = "v22s", future_post16_ui())),
  nav_spacer(),
  nav_item(tags$a(href = SITE, target = "_blank", icon("book-open"), "Methods and findings")),
  footer = div(class = "sis-footer",
               "Adam Dennett · Bartlett Centre for Advanced Spatial Analysis, UCL · ",
               "Built from published DfE, ONS, GIAS and Ofsted data. Nothing uses pupil-level records.")
)

server <- function(input, output, session) {
  output$year_label <- renderText(if (identical(input$g_period, "ma")) "Centred on" else "Year")
  ctx <- reactive({
    p <- input$g_period %||% "four"; y <- input$g_year %||% tail(MODEL_YEARS, 1); w <- as.integer(input$g_window %||% 3)
    g <- input$g_group %||% "all"
    list(group = g, glab = GLAB[[g]], period = p, year = y, window = w, txt = period_label(p, y, w),
         M = metrics(g, p, y, w), for_group = function(gg) metrics(gg, p, y, w))
  })
  output$period_note <- renderText({
    c <- ctx(); win <- if (c$period == "four") character(0) else window_years(c$year, if (c$period == "year") 1 else c$window)
    paste0(c$glab, ", ", c$txt, if (!is.na(PROV_YEAR) && PROV_YEAR %in% win && c$period == "ma") ". Includes provisional 2025/26." else ".")
  })

  # One school chosen anywhere is the school everywhere
  sel <- reactiveVal(SCH$URN[SCH$name == "Dorothy Stringer School"][1] %||% SCH$URN[1])
  go <- function(tab) nav_select("nav", tab, session = session)

  # One place chosen anywhere (an area type and an area) is the place in every tab that has one
  place <- reactiveValues(level = "la", area = DEFAULT_AREA[["la"]])
  mod_finder_server("finder", ctx, sel, go, place)
  mod_twin_server("twin", ctx, sel, go)
  mod_obs_server("obs", ctx, sel, place)
  mod_sim_server("sim", ctx, sel, place)
  mod_area_server("area", ctx, sel, go, place)
  mod_travel_server("travel", ctx, sel, place)
}

shinyApp(ui, server)
