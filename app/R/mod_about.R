# mod_about.R - The About tab: what the system is, what is new in v2.0, and what comes next

about_ui <- function() {
  s <- D$model$summary; r2 <- setNames(round(100 * s$r2_conditional), s$group)
  tab_row <- function(ic, name, what) tags$tr(tags$td(icon(ic), " ", tags$strong(name)), tags$td(what))
  # Text is pasted into one string per paragraph: HTML puts a space between separate pieces
  P <- function(..., class = NULL) p(class = class, HTML(paste0(...)))
  L <- function(...) tags$li(HTML(paste0(...)))
  B <- function(x) paste0("<strong>", x, "</strong>")
  A <- function(href, x) paste0("<a href='", href, "' target='_blank'>", x, "</a>")
  tagList(
    div(class = "sis-hero",
        h1("School Information System"),
        P(class = "sis-hero-sub", "Version 2.0, on Value Added ", VA_VERSION,
          ": how every state secondary school in England is doing, against what its intake would lead us to expect."),
        P(class = "sis-hero-credit", "Designed by ", B("Prof Adam Dennett"), " · Bartlett Centre for Advanced Spatial Analysis, UCL · ",
          A("https://ai4ci.ac.uk/", "UKRI AI for Collective Intelligence Research Hub"), " · with assistance from Claude"),
        div(class = "sis-hero-logos",
            tags$a(href = "https://ai4ci.ac.uk/", target = "_blank", tags$img(src = "ai4ci_logo.svg", alt = "AI4CI", height = 44)),
            tags$a(href = "https://www.ukri.org/", target = "_blank", tags$img(src = "ukri_logo.png", alt = "UKRI", height = 40)))),
    div(class = "container sis-about",
      card(card_body(
        h4(icon("bullseye"), " What is this?"),
        P("A tool for school leaders, governors, councils and trusts who want to look past raw league tables.",
          " For every state-funded mainstream secondary school in England it sets ", B("Attainment 8"),
          " beside ", B(paste("Value Added", VA_VERSION)), ": how far a school's results sit above or below",
          " what schools with the same intake achieve across England."),
        P("It also shows how each school's ", B("absence"), " compares with what its intake would predict,",
          " finds the schools most like any school, simulates what the model expects if absence or intake changed,",
          " and starts to bring in what we are learning about ", B("journeys to school"), "."))),
      card(card_body(
        h4(icon("star"), " New in version 2.0"),
        tags$ul(
          L(B("Two headline outcomes: "), "Attainment 8 and Value Added ", VA_VERSION, ", side by side everywhere."),
          L(B("Five groups of pupils: "), "all, disadvantaged and non-disadvantaged pupils, with girls and boys."),
          L(B("Periods: "), "the four-year figure (shrunk, with a 95% interval), any single year, or a moving average",
            " over three or five years centred on a year you choose. 2025/26 is included, provisional."),
          L(B("More ways to cut England: "), "regions, combined and mayoral authorities, local authorities,",
            " districts and academy trusts."),
          L(B("Absence against expected: "), "each school's absence above or below what its intake predicts."),
          L(B("Travel and distance: "), "how far children live from school, and how long the journey takes,",
            " for deprived neighbourhoods against the rest. More will follow as national routing is finished."),
          L(B("Maps "), "on Esri's light grey canvas, with colour scales that read for colour-blind users.")))),
      card(card_body(
        h4(icon("chart-line"), " The model behind the numbers"),
        P("Value Added ", VA_VERSION, " is a multilevel model of each school's Attainment 8 over ", length(MODEL_YEARS), " years (",
          yl(MODEL_YEARS[1]), " to ", yl(tail(MODEL_YEARS, 1)), "), with years, regions and local authorities as levels.",
          " The expectation uses only intake and setting, nothing a school controls once its pupils arrive: disadvantage,",
          " prior attainment at KS2, absence, English as an additional language, SEN support and EHC plans, pupil mobility,",
          " neighbourhood education, admissions, single-sex and 11-16 schools."),
        P("The models explain ", r2[["all"]], "% of the variation in results for all pupils, ", r2[["disadvantaged"]],
          "% for disadvantaged pupils and ", r2[["non_disadvantaged"]], "% for non-disadvantaged pupils.",
          " Value added is what is left: Attainment 8 points above or below England for that intake.",
          " Over four years it is shrunk towards zero by its reliability, so small or volatile schools are not overstated."),
        P("Absence against expected comes from a model of each school's absence on the same intake terms, with years and",
          " regions but no local authority level, so an authority's high or low absence stays visible."),
        P("Full methods, diagnostics and findings are on the ", A(SITE, "School Value Added site"), " (",
          A(paste0(SITE, "/method.html"), "methodology"), "). Version 1.0 of this tool, on the original model, is ",
          A("https://adamdennett.github.io/school_attainment_tool/", "documented here"), "."))),
      card(card_body(
        h4(icon("compass"), " Guide to the tabs"),
        tags$table(class = "table table-sm align-middle mb-0",
          tags$tbody(
            tab_row("magnifying-glass", "School Finder", "Find a school by name, by area or on the map; see its results, value added and absence against expected, year by year."),
            tab_row("users", "Find My Twin", "The schools in England most like a school in intake and setting, and the look-alikes that do better."),
            tab_row("braille", "Observed vs Predicted", "Every school's Attainment 8 against what its intake predicts, and its absence against expected; an area or a school highlighted."),
            tab_row("sliders", "Policy Simulator", "What the model expects if a school's (or every school in an area's) absence or intake were different, for every group of pupils."),
            tab_row("map", "Area Overview", "A region, combined authority, local authority, district or trust: its schools on a map and in a table, and the area against others."),
            tab_row("person-walking", "Travel and Distance", "Distance and journey times to school, deprived neighbourhoods against the rest; the national walking and cycling routes as they arrive."),
            tab_row("compass", "Coming next", "Plans for v2.1 (places and pupil cohorts) and v2.2 (primary and post-16)."))))),
      card(card_body(
        h4(icon("rocket"), " Getting started"),
        tags$ol(
          L("Choose the ", B("pupils"), " and ", B("period"), " in the bar at the top; every tab follows them."),
          L("Find your school in the <em>School Finder</em>, then open its twins or the simulator from there."),
          L("Use the <em>Area Overview</em> to see an authority, district or trust as a whole.")))),
      card(class = "sis-caution", card_body(
        h4(icon("triangle-exclamation"), " Reading the numbers"),
        p("Value added is a measure of how a school's results compare with similar intakes, not a verdict on its teaching.",
          " It carries uncertainty (the four-year interval shows how much), single years move more than four-year figures,",
          " and what the model calls the school includes whatever it cannot see. The simulator shows associations in the",
          " model, not guaranteed effects of a policy. Use it alongside professional judgement."),
        p(D$model$prov_note))),
      div(class = "sis-small text-center",
          "Data: DfE school performance tables and absence statistics, GIAS, ONS Census 2021, Ofsted (to ",
          format(D$model$ofsted_as_at, "%d %B %Y"), "). App data built ", format(D$built, "%d %B %Y"), ".")
    )
  )
}
