# mod_future.R - Placeholders for the sections planned for v2.2 of the system.
# Each says what the section will show and what it needs; none shows data yet.

future_page <- function(version, title, lead, will, needs, status) {
  div(class = "container sis-about",
    div(class = "sis-future-head", span(class = "sis-badge", version), h2(title), p(class = "lead", lead)),
    layout_columns(col_widths = c(6, 6),
      card(card_body(h5(icon("list-check"), " What it will show"), tags$ul(lapply(will, tags$li)))),
      card(card_body(h5(icon("database"), " What it needs"), tags$ul(lapply(needs, tags$li)),
                     h6(class = "mt-3", "Status"), p(status)))),
    note("A placeholder: the analysis behind this section has not been done yet. ",
         tags$a(href = paste0(SITE, "/roadmap.html"), target = "_blank", "The project roadmap"), " has the wider plan."))
}

future_primary_ui <- function() future_page("v2.2", "Primary schools",
  "The same view for primary schools: KS2 results against what intake predicts, and absence against expected.",
  will = list("KS2 attainment (reading, writing and maths at the expected standard, and scaled scores) beside a primary value-added measure from KS1 or the reception baseline where published",
              "All, disadvantaged and non-disadvantaged pupils, as for secondary schools",
              "Absence against expected for primary schools",
              "Walking distance and time to primary schools from the national routing, which already covers every primary school",
              "School finder, twins, area overview and simulator for primary schools"),
  needs = list("KS2 school-level performance data for the same years (DfE, published)",
               "A primary value-added specification, tested as v2.1 was",
               "Primary absence and census data by school (DfE, published)"),
  status = "Planned. The routing for travel includes primary schools; the attainment model is not yet built.")

future_post16_ui <- function() future_page("v2.2", "Post-16",
  "Sixth forms and colleges: results at 16 to 18, what happens to young people after Year 11, and travel to post-16 places.",
  will = list("16-18 results (A level, applied general and technical) by school sixth form and college",
              "Destinations after KS4 and after 16-18: staying in education, apprenticeships, employment, NEET",
              "How a school's Year 11 value added carries through to its post-16 outcomes",
              "Journey times to the nearest post-16 options for every neighbourhood"),
  needs = list("DfE 16-18 performance tables and destination measures (published)",
               "A comparable progress or value-added measure for 16-18",
               "Post-16 institutions added to the national routing"),
  status = "Planned, after primary. 11-16 schools are already flagged in Value Added v2.1, which this will build on.")
