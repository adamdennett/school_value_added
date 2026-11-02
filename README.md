# School Value Added v2.0

Value added for every state-funded mainstream secondary school in England:
Attainment 8 above or below what a multilevel model expects from a school's
intake and context, for all pupils, disadvantaged and non-disadvantaged
pupils, girls and boys.

**Site:** https://adamdennett.github.io/school_value_added/

v2.0 follows on from
[How to Pull the Right Lever](https://github.com/adamdennett/school_attainment_tool)
and is built on the recommendations of its value-added review
(`review.qmd`). Built from published DfE and Ofsted data and the 2021
Census; nothing uses pupil-level records.

## Build

The inputs come from the original project, expected as a sibling folder
(`../school_attainment_tool`), which holds the DfE extraction pipeline and
its git-ignored data.

```
Rscript R/01_inputs.R        # assemble data/inputs.rds from the original project
Rscript R/02_fit_va.R        # fit v2.0 for every group -> data/va_v2.rds, data/va_v2_schools.csv
Rscript R/03_area_pages.R    # write the Sussex area pages from templates/area.qmd
quarto render                # build the site into docs/
```

`R/00_config.R` holds the specification, the pupil groups and the paths.
`R/site_helpers.R` holds the palette, charts, decomposition and map used by
every page.

## Adding a year

1. In `school_attainment_tool`, extend the panel with the new year's
   performance tables, absence, census and SEN data (its
   `R/01_extract_data.R` to `R/04_compute_derived.R`), then rebuild its
   neighbourhood, district and Ofsted inputs if they have changed.
2. Here, rerun the four build steps above. v2.0 takes its years from the
   data; nothing in the model is tied to a year.

To refresh Ofsted only: `Rscript R/15b_ofsted_latest.R` in the original
project, then the four steps here.

## Files

| | |
|---|---|
| `R/00_config.R` | specification, groups, paths |
| `R/01_inputs.R` | modelling data from the original project |
| `R/02_fit_va.R` | the v2.0 models and value added |
| `R/03_area_pages.R` | area pages from `templates/area.qmd` |
| `R/site_helpers.R`, `R/map_js.R` | palette, charts, decomposition, map |
| `data/va_v2.rds`, `data/va_v2_schools.csv` | the results (committed) |
| `data/value_added_review.rds` | the review's results, from the original project |
| `docs/` | the built site (GitHub Pages) |
