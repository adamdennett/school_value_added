# snapshot_area.R - Copy the original project's Sussex area files into data/area/
# ---------------------------------------------------------------------------
# The Sussex pages reuse the original project's area analyses (absence
# series, reasons, suspensions, profiles: its R/17 onward). They are kept
# here as a committed snapshot, so the site renders without that project,
# on GitHub Actions too. Rerun this after the original project rebuilds
# its area files, then rerender.
#
#   Rscript R/snapshot_area.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))

from <- list.files(sat("data"), "^area_.*\\.rds$", full.names = TRUE)
if (!length(from)) stop("No area files in ", sat("data"))
to <- here::here("data", "area")
dir.create(to, showWarnings = FALSE)
ok <- file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
message(sprintf("Copied %d of %d area files to %s", sum(ok), length(from), to))
