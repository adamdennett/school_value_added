# 45_finance_pages.R - Write the area finance pages from templates/finance_area.qmd
# ---------------------------------------------------------------------------
# One page per area in AREAS, as finance_areas/<slug>.qmd, with the area's name and level
# filled in. The pages read data/area_finance.rds (R/44_area_finance.R). Brighton and Hove
# first; East Sussex, West Sussex and the twelve districts are a line each below when wanted.
#   Rscript R/45_finance_pages.R && quarto render finance_areas/
# ---------------------------------------------------------------------------

AREAS <- data.frame(area = c("Brighton and Hove"), level = c("Authority"))
# AREAS <- rbind(AREAS, data.frame(area = c("East Sussex", "West Sussex"), level = "Authority"),
#                data.frame(area = c("Eastbourne", "Hastings", "Lewes", "Rother", "Wealden", "Adur", "Arun",
#                                    "Chichester", "Crawley", "Horsham", "Mid Sussex", "Worthing"), level = "District"))

tpl <- readLines(here::here("templates", "finance_area.qmd"), warn = FALSE)
dir.create(here::here("finance_areas"), showWarnings = FALSE)
for (i in seq_len(nrow(AREAS))) {
  slug <- gsub("[^a-z]+", "_", tolower(AREAS$area[i]))
  out <- gsub("{{AREA}}", AREAS$area[i], gsub("{{LEVEL}}", AREAS$level[i], tpl, fixed = TRUE), fixed = TRUE)
  writeLines(out, here::here("finance_areas", paste0(slug, ".qmd")))
  message("wrote finance_areas/", slug, ".qmd")
}
