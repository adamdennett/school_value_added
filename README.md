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
Rscript R/02b_school_context.R    # attendance, governance and other map measures for every school
Rscript R/04_place_and_absence.R  # absence against intake, district test, trusts, scenarios on v2 (~15 min)
Rscript R/05_provisional_2526.R   # provisional 2025/26, predicted from the fitted model
Rscript R/06_bh_effect.R          # geographical effects: why Brighton and Hove's authority effect is large (~5 min)
Rscript R/07_v21_candidates.R     # v2.1 candidates, and what matters most in the model (~10 min)
Rscript R/08_absence_causes.R     # what goes with school absence, school by school (~3 min)
Rscript R/09_absence_change.R     # how school absence changes, 2016/17 to 2025/26 (~3 min; needs fixest)
Rscript R/10_independent.R        # independent schools: the data and how far a comparison can go (~1 min)
Rscript R/11_oac_neighbourhoods.R # neighbourhood types (OAC) against results and value added (~2 min)
Rscript R/12_geography.R          # geographical effects across England (~1 min)
Rscript R/13_neighbourhood_covariates.R  # the neighbourhood covariates experiment (~6 min)
Rscript R/14_v22_candidates.R     # v2.2 candidates across every group and year (~25 min)
Rscript R/15_graduate_deprivation.R  # two kinds of deprived neighbourhood (~6 min)
Rscript R/16_parents_not_adults.R # parents, not adults (first run downloads from ONS; ~30 min)
Rscript R/18_distance.R           # distance to school, in straight lines (first run fetches ONS lookups; ~3 min)
Rscript R/19_distance_absence.R   # how firm the link with absence is: years, measures, controls, primary, within authorities (first run fetches IoD 2019, ONS, nomis, DfE; ~5 min)
Rscript R/20_journey_times.R      # r5r journey times for nine places (needs Java 21, ~40 GB heap; fetch OSM, BODS and Terrain 50 to data-raw/r5/src)
Rscript R/22_journey_structure.R  # buses, changes, missing a journey, climb on routes (R5R_THREADS=6 R5R_HEAP=52G for London)
Rscript R/23_school_buses.R       # school buses from the timetables, for all of England, against absence
Rscript R/24_school_bus_penalty.R # what it costs to miss a school bus, in the routed places
Rscript R/25_climb.R              # climb to and from school across England, against absence
Rscript R/21_journey_analysis.R   # the journey time analysis
STEP=frame Rscript R/26_la_networks.R  # the 109 mostly urban authorities: networks (then STEP=osm, STEP=gtfs, STEP=freq)
bash pipeline/la_resume.sh        # R/27 routing and R/28 POI counts for every network but London, then R/29 (hours; pipeline/la_pause.sh stops it)
bash pipeline/la_resume.sh london # London alone (10 to 14 hours, 52 GB heap), then R/29
Rscript R/30_distance_gcse.R      # from distance to absence to Attainment 8, and the mixing estimate on that scale (~1 min)
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
| `R/02b_school_context.R` | attendance, suspensions and governance for every school, for the explorer |
| `R/03_area_pages.R` | area pages from `templates/area.qmd` (format: `AREA_REPORTS.md`) |
| `R/04_place_and_absence.R` | the original area analyses refitted on v2.1 |
| `R/05_provisional_2526.R` | provisional 2025/26 value added |
| `R/06_bh_effect.R` | geographical effects: Brighton and Hove's authority effect, term by term (`bh_effect.qmd`) |
| `R/07_v21_candidates.R` | the v2.1 candidates (kind of absence, 11–16) and the importance charts |
| `R/08_absence_causes.R` | what goes with school-level absence: trend by reason, seven outcomes on intake, needs and place (`absence_causes.qmd`) |
| `R/09_absence_change.R` | how school absence changes: a 2016/17 to 2025/26 panel joined through URN changes, variance shares, stability, the pre/post-pandemic shift, within- vs between-school intake effects, event studies of academy conversion and trust changes (`absence_change.qmd`); fetches the 2018/19 census file from the performance tables service if it is missing |
| `R/18_distance.R` | distance to school: straight-line distance from every Census 2021 LSOA to the nearest open state secondary in GIAS, most deprived third against the rest in every district and authority (ONS RUC 2021 for rural and urban), against absence by authority for all three groups and by school, what if a school moved in Brighton and Hove (`distance.qmd`) |
| `R/19_distance_absence.R` | how firm the link between distance and absence is, for all three groups: six years with each September's school network from GIAS dates, eleven distance measures, a specification curve, a falsification test on primary absence, absence by pupil residence and IDACI tenth within authorities, and school and authority effects; writes `data/distance_absence.rds`. Sources and formulas: `method.qmd#sec-distance-method` |
| `R/20_journey_times.R` | routes every Output Area in Brighton and Hove and eight comparator places to nearby state secondaries with r5r, walking and by bus, with and without hills; writes `data/cache/journey`. Sources and settings: `method.qmd#sec-journey-method` |
| `R/21_journey_analysis.R` | journey times by deprivation and IDACI tenth, against straight lines, absence and the city's model; writes `data/journey_times.rds` |
| `R/22_journey_structure.R` | what each routed journey is made of: buses, changes, waits, the penalty for missing the last workable journey, and the climb each way from OS Terrain 50; writes `data/cache/journey` |
| `R/23_school_buses.R` | finds dedicated school buses in the Bus Open Data Service timetables for all of England and relates them to absence; writes `data/school_buses.rds` |
| `R/24_school_bus_penalty.R` | how late a child is if they miss a school bus, by the best real alternative; writes `data/cache/journey` |
| `R/25_climb.R` | climb from every Output Area to its nearest school across England, against absence, for schools, IDACI tenths and all three pupil groups by authority; writes `data/climb.rds` |
| `R/26_la_networks.R` | networks for the 109 mostly urban authorities: Output Areas, 28 shared networks, OpenStreetMap and Bus Open Data Service clips, frequency trips made timetabled; writes `data/cache/journey_la` and `data-raw/r5/net_la` |
| `R/27_la_routing.R` | routes every Output Area in those networks: journey times, journey structure, walking routes with climb, and missing a journey, saved step by step |
| `R/28_la_pois.R` | points of interest from OpenStreetMap around stops and along walking routes of each journey, with a control around home |
| `R/29_la_analysis.R` | is Brighton and Hove unusual among the 109; journeys and points of interest against absence for authorities, IDACI tenths and schools; writes `data/journey_la.rds` |
| `R/30_distance_gcse.R` | carries the distance link through absence to Attainment 8, checks it directly, and puts Gorard and Siddiqui's mixing estimate on the same scale; writes `data/distance_gcse.rds` |
| `R/16_parents_not_adults.R` | parents, not adults: neighbourhood education from ONS custom Census tables (qualification by dependent children and age by MSOA; by age by LSOA), against v2.1 for every group, with held-out years, the split between parents and other adults, and London tested separately (`neighbourhood_covariates.qmd`); caches about 1.5 GB of API responses in `data-raw/census2021/ons_custom/` |
| `R/15_graduate_deprivation.R` | two kinds of deprived neighbourhood: tests whether the deprivation effect is a recent-graduate effect (interaction, by kind of place, early-career measures, deprivation in parts) and value added by kind of neighbourhood (`neighbourhood_covariates.qmd`) |
| `R/14_v22_candidates.R` | v2.2 candidates: pupils' ethnicity (school White British share each January, from the SPC releases downloaded into `data-raw/ees/spc_all/`) and neighbourhood household deprivation (Census 2021 TS011), against v2.1 for every group, with leave-one-year-out prediction, year-by-year fits and who gains (`v22.qmd`) |
| `R/12_geography.R` | geographical effects across England: variance components by group, every local authority's effect, region averages, authority absence effects, and simplified county/unitary boundaries (`geography.qmd`) |
| `R/13_neighbourhood_covariates.R` | the neighbourhood covariates experiment: ten Census 2021 measures over each school's 150 nearest OAs (nomis bulk tables) and the school's own White British share, added to v2.1 one at a time and in sets; effects, AIC, and the change in authority and region variance (`neighbourhood_covariates.qmd`) |
| `R/11_oac_neighbourhoods.R` | neighbourhood types: the 2021/2 Output Area Classification (download from data.geods.ac.uk into `data-raw/oac2021/`) profiled over each school's 150 nearest OAs at three tiers, tested against results and value added out of sample, and against local authority effects (`neighbourhood_types.qmd`) |
| `R/10_independent.R` | independent schools: GIAS, census and SEN coverage, KS4 Attainment 8 against KS5 A level results, a reduced specification tested on state schools, and DfE's A level value added by sector (`independent.qmd`); fetches the 2024/25 KS5 and vaqual files if missing |
| `simulator.qmd`, `simulator.js` | Policy simulator v2.0: the v2.1 coefficients applied in the browser to a school's latest result, or every school in an area, with absence and intake as levers |
| `twins.qmd`, `twins.js` | Find a twin: a school's closest look-alikes in intake and setting (Gower-type distance, in the browser) and the anti-twins that do better on absence, attainment or value added; data from `R/08` |
| `R/area_helpers.R` | the original area documents' charts, ported |
| `R/site_helpers.R`, `R/map_js.R` | palette, charts, decomposition, map |
| `data/va_v2.rds`, `data/va_v2_schools.csv` | the results (committed) |
| `data/value_added_review.rds` | the review's results, from the original project |
| `docs/` | the built site (GitHub Pages) |
