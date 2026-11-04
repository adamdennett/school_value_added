# School Value Added v2.1

**How much does a school add? And how much does absence (or anything else) take away?**

Value added for every state-funded mainstream secondary school in England:
Attainment 8 above or below what a multilevel model expects from a school's
intake and context, for all pupils, disadvantaged and non-disadvantaged
pupils, girls and boys.

**Site:** https://adamdennett.github.io/school_value_added/

v2.1 follows on from
[How to Pull the Right Lever](https://github.com/adamdennett/school_attainment_tool)
and is built on the recommendations of its value-added review
(`review.qmd`). Built from published DfE and Ofsted data and the 2021
Census; nothing uses pupil-level records.

## Build

Everything the model needs is built from public sources by this
repository: DfE's performance tables (Compare School and College
Performance), Explore Education Statistics, GIAS, the 2021 Census (nomis,
ONS), postcodes.io and Ofsted. The scripts are in `pipeline/`, taken from
the original project and cut to what v2.1 uses; downloads go to `data-raw/`
and `data/`, both git-ignored.

```
Rscript R/00_fetch_data.R    # download and build the inputs (first run: about an hour)
Rscript R/01_inputs.R        # assemble data/inputs.rds
Rscript R/02_fit_va.R        # fit v2.1 for every group -> data/va_v2.rds, data/va_v2_schools.csv
Rscript R/04_place_and_absence.R  # absence against intake, district test, trusts, scenarios on v2 (~15 min)
Rscript R/05_provisional_2526.R   # provisional 2025/26, predicted from the fitted model
Rscript R/06_bh_effect.R          # why Brighton and Hove's authority effect is large (~5 min)
Rscript R/07_v21_candidates.R     # v2.1 candidates, and what matters most in the model (~10 min)
Rscript R/03_area_pages.R    # write the Sussex area pages from templates/area.qmd
quarto render                # build the site into docs/
```

`R/00_fetch_data.R --dry-run` says what it would do; a step named on its
command line is fetched afresh. A rebuild reuses what is already
downloaded, so it reproduces the last build.

Three things still come from the original project
(`../school_attainment_tool`), and only for Sussex or for comparison:

- **the Sussex area analyses** (absence series, reasons, suspensions,
  profiles), kept as a committed snapshot in `data/area/` by
  `R/snapshot_area.R`;
- **who ran each Sussex school**, read by `R/01_inputs.R` if it is there;
- **the absence reasons** behind `R/06_bh_effect.R`.

v1 value added, the published measure the site compares against, is a
fixed snapshot in `data/lookups/v1_published.csv`.

`R/00_config.R` holds the specification, the pupil groups and the paths.
`R/site_helpers.R` holds the palette, charts, decomposition and map used by
every page.

## Ofsted, monthly

`.github/workflows/refresh-ofsted.yml` runs on the 20th of each month (and
by hand from the Actions tab). `R/refresh_ofsted.R` fetches Ofsted's newest
management information; if it is newer than the site's, it updates every
school's latest judgement in `data/va_v2.rds` without refitting (nothing in
the model uses Ofsted), and the workflow regenerates the area pages,
renders and commits. Locally:

```
Rscript R/refresh_ofsted.R && Rscript R/03_area_pages.R && quarto render
```

## Adding a year

1. Add the year to `YEAR_CONFIG` in `pipeline/helpers.R` (and its KS4 file:
   final or provisional), then `Rscript R/00_fetch_data.R`.
2. Rerun the build steps above. v2.1 takes its years from the data;
   nothing in the model is tied to a year.

## Files

| | |
|---|---|
| `R/00_fetch_data.R`, `pipeline/` | download and build the model inputs from public sources |
| `R/00_config.R` | specification, groups, paths |
| `R/01_inputs.R` | assemble the modelling data |
| `R/refresh_ofsted.R`, `.github/workflows/refresh-ofsted.yml` | the monthly Ofsted refresh |
| `R/snapshot_area.R`, `data/area/` | the original project's Sussex area analyses, snapshotted |
| `data/lookups/` | fixed lookups: v1 as published, URN families, combined authorities, DfE region codes |
| `R/02_fit_va.R` | the v2.1 models and value added |
| `R/03_area_pages.R` | area pages from `templates/area.qmd` (format: `AREA_REPORTS.md`) |
| `R/04_place_and_absence.R` | the original area analyses refitted on v2.1 |
| `R/05_provisional_2526.R` | provisional 2025/26 value added |
| `R/06_bh_effect.R` | Brighton and Hove's authority effect, term by term (`bh_effect.qmd`) |
| `R/07_v21_candidates.R` | the v2.1 candidates (kind of absence, 11–16) and the importance charts |
| `R/area_helpers.R` | the original area documents' charts, ported |
| `R/site_helpers.R`, `R/map_js.R` | palette, charts, decomposition, map |
| `data/va_v2.rds`, `data/va_v2_schools.csv` | the results (committed) |
| `data/value_added_review.rds` | the review's results, from the original project |
| `docs/` | the built site (GitHub Pages) |
