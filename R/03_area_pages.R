# 03_area_pages.R - Write one page per Sussex area from templates/area.qmd
# ---------------------------------------------------------------------------
# Three tiers, as in the original project's AREA_REPORTS.md:
#   region     Sussex                                   sussex.qmd
#   la         Brighton and Hove, East and West Sussex  areas/<slug>.qmd
#   district   the twelve districts of the two counties areas/<slug>.qmd
# Run before `quarto render`. The generated .qmd files are git-ignored: the
# template is the source.
#   Rscript R/03_area_pages.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))

DISTRICTS <- list(
  "East Sussex" = c("Eastbourne", "Hastings", "Lewes", "Rother", "Wealden"),
  "West Sussex" = c("Adur", "Arun", "Chichester", "Crawley", "Horsham", "Mid Sussex", "Worthing"))
slug <- function(x) gsub("[^a-z]+", "_", tolower(x))

# Each area's document in the original project
v1_doc <- function(name, tier) {
  if (tier == "region") return("sussex_review.html")
  if (name == "Brighton and Hove") return("pulling_the_right_lever_v2.html")
  if (tier == "la") return(paste0(slug(name), ".html"))
  if (name == "Mid Sussex") return("mid_sussex.html")
  paste0("district_", slug(name), ".html")
}

AREAS <- c(
  list(list(name = "Sussex", tier = "region", las = SUSSEX_LAS, parent = "", file = "sussex.qmd", root = "",
            title = "Sussex", subtitle = "Value Added v2.0 across East Sussex, West Sussex and Brighton and Hove.")),
  lapply(SUSSEX_LAS, function(la) list(name = la, tier = "la", las = la, parent = "Sussex",
         file = file.path("areas", paste0(slug(la), ".qmd")), root = "../", title = la,
         subtitle = paste0("Value Added v2.0 for ", la, "'s secondary schools."))),
  unlist(lapply(names(DISTRICTS), function(la) lapply(DISTRICTS[[la]], function(d) list(
    name = d, tier = "district", las = la, parent = la, file = file.path("areas", paste0(slug(d), ".qmd")),
    root = "../", title = d, subtitle = paste0("Value Added v2.0 for the secondary schools of ", d, ", ", la, ".")))),
    recursive = FALSE))

write_area <- function(a) {
  t <- readLines(here::here("templates", "area.qmd"), encoding = "UTF-8")
  rep <- c("@@TITLE@@" = a$title, "@@SUBTITLE@@" = a$subtitle, "@@NAME@@" = a$name, "@@TIER@@" = a$tier,
           "@@LAS@@" = paste0("c(", paste0('"', a$las, '"', collapse = ", "), ")"),
           "@@PARENT@@" = a$parent, "@@V1DOC@@" = v1_doc(a$name, a$tier), "@@ROOT@@" = a$root)
  for (k in names(rep)) t <- gsub(k, rep[[k]], t, fixed = TRUE)
  dir.create(dirname(here::here(a$file)), showWarnings = FALSE, recursive = TRUE)
  writeLines(t, here::here(a$file), useBytes = TRUE)
  message("  ", a$file)
}

if (sys.nframe() == 0) invisible(lapply(AREAS, write_area))
