# refresh_ofsted.R - Bring in Ofsted's newest monthly release, without refitting
# ---------------------------------------------------------------------------
# Ofsted publishes its management information around the middle of each
# month. This fetches the newest release (pipeline/10_ofsted_latest.R
# --force) and, if it is newer than the one in data/va_v2.rds, replaces
# every school's Ofsted fields there. Nothing in the model uses Ofsted, so
# nothing is refitted: the schools are matched to their records through
# the URN families saved by R/01_inputs.R (data/lookups/urn_family.csv),
# so this needs neither GIAS nor the original project.
#
# Run monthly by .github/workflows/refresh-ofsted.yml, which then
# regenerates the area pages, renders the site and commits. By hand:
#
#   Rscript R/refresh_ofsted.R && Rscript R/03_area_pages.R && quarto render
#
# On GitHub Actions it writes changed=true|false to $GITHUB_OUTPUT.
# ---------------------------------------------------------------------------

source(here::here("R", "01_inputs.R"))   # ofsted_fields(); its build does not run when sourced

say <- function(changed, as_at = NULL) {
  out <- Sys.getenv("GITHUB_OUTPUT")
  if (!nzchar(out)) return(invisible())
  cat(sprintf("changed=%s\n", tolower(changed)), file = out, append = TRUE)
  if (!is.null(as_at)) cat(sprintf("as_at=%s\n", format(as_at, "%d %B %Y")), file = out, append = TRUE)
}

status <- system2(file.path(R.home("bin"), "Rscript"),
                  c(shQuote(here::here("pipeline", "10_ofsted_latest.R")), "--force"))
if (status != 0) stop("pipeline/10_ofsted_latest.R failed")

V <- readRDS(here::here("data", "va_v2.rds"))
family <- read.csv(here::here("data", "lookups", "urn_family.csv"))
new <- ofsted_fields(family)
as_at <- attr(new, "as_at")
if (!is.null(V$ofsted_as_at) && as_at <= V$ofsted_as_at) {
  message(sprintf("Ofsted's newest release is as at %s; the site already has it. Nothing to do.",
                  format(as_at, "%d %B %Y")))
  say(FALSE)
  quit(status = 0)
}

cols <- setdiff(names(new), "URN")
before <- V$schools
for (k in cols) V$schools[[k]] <- new[[k]][match(V$schools$URN, new$URN)]
moved <- which(coalesce(before$ofsted_latest, "") != coalesce(V$schools$ofsted_latest, ""))
message(sprintf("Ofsted as at %s (was %s): %d schools' latest judgement changed",
                format(as_at, "%d %B %Y"), format(V$ofsted_as_at, "%d %B %Y"), length(moved)))
sx <- moved[V$schools$LANAME[moved] %in% SUSSEX_LAS]
for (i in sx) message(sprintf("  %s: %s -> %s", V$schools$SCHNAME[i],
                              coalesce(before$ofsted_latest[i], "none"), V$schools$ofsted_latest[i]))
V$ofsted_as_at <- as_at
saveRDS(V, here::here("data", "va_v2.rds"))
say(TRUE, as_at)
