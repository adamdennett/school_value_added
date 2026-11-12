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
Rscript R/31_admissions_journeys.R # what Brighton and Hove's admissions changes did, from the school system model's published runs
Rscript R/32_admissions_proposals.R # the proposals that did not go ahead, run on the published school system model (~1 min)
Rscript R/33_admissions_neighbourhoods.R # the same runs neighbourhood by neighbourhood: Whitehawk and the others the map moved
R5R_HEAP=18G Rscript R/34_route_examples.R # example routes for the maps and the methodology's worked example (needs the networks of R/26 and R/27)
Rscript R/36_workforce.R          # teacher retention, sickness and staffing from School Workforce in England, and their link with value added
TRUST="ALDRIDGE EDUCATION" Rscript R/35_trust_summary.R  # a trust's schools: value added, absence, look-alikes, levers, targets (after R/36)
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

## Pupil numbers and school places

`demographics.qmd` reads `data/demographics.rds`, built by `R/42_demographics.R` from
DfE's school census (pupils by year group, every local authority, 2015/16 to 2025/26)
and GIAS. The census data set is large (900 MB unpacked), so it is fetched once and cut
to the all-schools rows:

```
mkdir -p data-raw/ees/spc_2526 && cd data-raw/ees/spc_2526
curl -L -o yeargroup.csv.gz "https://api.education.gov.uk/statistics/v1/data-sets/019e7403-0e46-7122-9bc2-3252d775517c/csv"
zcat yeargroup.csv.gz | awk -F'","' 'NR==1 || ($3 != "Parliamentary constituency" && $13=="Total" && $15=="Total" && $16=="Total" && $17=="Total" && $18=="Total" && $19=="Total" && $20=="Total" && $21=="Total")' > yeargroup_totals.csv
cd ../../.. && Rscript R/42_demographics.R && quarto render demographics.qmd
```

When DfE publishes the next census (each June), update the data set id from the EES
API (`/publications/a91d9e05-be82-474c-85ae-4913158406d0/data-sets`) and rerun.

## School finances before closure

`finance.qmd` reads `data/finance_closures.rds`, built by `R/43_finance_closures.R`. It needs
the finance workbooks from DfE's Financial Benchmarking and Insights Tool:

```
mkdir -p data-raw/finance && cd data-raw/finance
B=https://financial-benchmarking-and-insights-tool.education.gov.uk/files
for y in 2014-15 2015-16 2016-17 2017-18 2018-19 2019-20 2020-21 2021-22 2022-23 2023-24 2024-25 2025-26; do curl -sL -o cfr_$y.xlsx "$B/CFR_${y}_Full_Data_Workbook.xlsx"; done
for y in 2016-17 2017-18 2018-19 2019-20 2020-21 2021-22 2022-23 2023-24 2024-25; do curl -sL -o aar_$y.xlsx "$B/AAR_${y}_download.xlsx"; done
cd ../.. && Rscript R/43_finance_closures.R && quarto render finance.qmd && quarto render method.qmd
```

No open school is named on the site. The model's school-level scores are written to
`data-raw/private/closure_risk_open_schools.csv`, which is not committed; share it only
privately.

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
3. Rebuild the app's data (below) and push.

## The School Information System (Shiny app)

`app/` is the School Information System v2.0, a Shiny version of the
original v1.0 policy simulator app (`school_attainment_tool/app`) rebuilt
on Value Added v2.1. It is deployed on Posit Connect Cloud
(<https://adamdennett-school-information-system.share.connect.posit.cloud/>)
from this repository's `app/app.R`, republishing on every push to `main`
(the app can be unavailable for a minute while it swaps over), and is
embedded in the site's `simulator.qmd`. It reads one bundle, `app/data/app_data.rds`, so it never
needs `data/`:

```
Rscript R/40_app_data.R                  # the app's data, from the files the site builds
Rscript R/41_app_travel_national.R       # optional: national walking routes by school, from ../school_active_travel
Rscript -e 'rsconnect::writeManifest("app")'   # only if packages or app files were added
Rscript -e 'shiny::runApp("app")'        # run it locally
```

`app/README.md` describes the tabs and the code.

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
| `R/31_admissions_journeys.R` | what Brighton and Hove's 2024 map and priority 6 did to journeys and mixing, from the published runs of the city's school system model, on the scales of `R/30`; writes `data/admissions_journeys.rds` |
| `R/32_admissions_proposals.R` | runs the published school system model (code and inputs at a fixed commit) for the council's 2024 proposal and Class Divide's (priority 6 at 30%, with the same cuts), after reproducing its published run of today; writes `data/admissions_proposals.rds` |
| `R/33_admissions_neighbourhoods.R` | the same model's runs zone by zone: whose journeys the 2024 map and the 2024 proposal changed, grouped by how the map moved each zone and named by House of Commons Library MSOA names; writes `data/admissions_neighbourhoods.rds` |
| `R/34_route_examples.R` | example journeys chosen by rule (Whitehawk; a typical deprived journey in Brighton and Hove, Bristol, Southampton and Plymouth), routed again with every leg kept, and the methodology's worked example (walking route and heights, stops, points of interest, every departure minute); writes `data/route_examples.rds`, drawn by `R/route_figs.R` |
| `R/35_trust_summary.R` | one trust's secondary schools (by GIAS trust name): value added and expected Attainment 8 by year, absence against intake, place effects, ten national look-alikes, levers (attendance, subject areas, slots, staffing) with schools to talk to, and intake-aware targets; writes `data/trust_<slug>.rds` for `trust_aldridge.qmd` |
| `R/36_workforce.R` | School Workforce in England, school level, 2021/22 to 2024/25: teacher retention, sickness days, pupils per qualified teacher and others, and their joint link with Value Added v2.1; writes `data/workforce_levers.rds` |
| `R/42_demographics.R` | pupil numbers by year group, every local authority, 2015/16 to 2025/26 (school census), trend classes, the cohorts in school rolled forward to Year 7 in 2032/33, and state mainstream schools opened, closed and merged from GIAS; writes `data/demographics.rds` for `demographics.qmd` |
| `R/43_finance_closures.R` | school finances (DfE CFR 2014-15 to 2025-26 and AAR 2016-17 to 2024-25, downloaded from the Financial Benchmarking and Insights Tool into `data-raw/finance/`) joined to every state mainstream school through its predecessor URNs; the years before closure, a logistic model of closure within two years tested on later years, warning signs, and counts by authority; writes `data/finance_closures.rds` for `finance.qmd` and the methodology section `_method_finance.qmd` (included in `method.qmd`: coefficients, sample flow, bootstrap AUC interval, calibration, refits). School-level scores for open schools go only to `data-raw/private/` (not committed) |
| `R/44_area_finance.R`, `R/45_finance_pages.R`, `templates/finance_area.qmd` | school finances by area (England, regions, authorities, Sussex districts) from the finance panel R/43 leaves in `data/cache/` (not committed); aggregates only, in `data/area_finance.rds`, with the Sussex districts' warning-sign counts (counted from R/43's private open-school list; no school written out). R/45 writes a page per area into `finance_areas/` from the template: Brighton and Hove, East Sussex, West Sussex and the twelve districts. Shares, medians and lines need five or more schools; districts with fewer maintained schools (Hastings, Eastbourne) get no reserve charts. Rebuild: `Rscript R/44_area_finance.R; Rscript R/45_finance_pages.R; quarto render finance_areas/` |
| `R/16_parents_not_adults.R` | parents, not adults: neighbourhood education from ONS custom Census tables (qualification by dependent children and age by MSOA; by age by LSOA), against v2.1 for every group, with held-out years, the split between parents and other adults, and London tested separately (`neighbourhood_covariates.qmd`); caches about 1.5 GB of API responses in `data-raw/census2021/ons_custom/` |
| `R/15_graduate_deprivation.R` | two kinds of deprived neighbourhood: tests whether the deprivation effect is a recent-graduate effect (interaction, by kind of place, early-career measures, deprivation in parts) and value added by kind of neighbourhood (`neighbourhood_covariates.qmd`) |
| `R/14_v22_candidates.R` | v2.2 candidates: pupils' ethnicity (school White British share each January, from the SPC releases downloaded into `data-raw/ees/spc_all/`) and neighbourhood household deprivation (Census 2021 TS011), against v2.1 for every group, with leave-one-year-out prediction, year-by-year fits and who gains (`v22.qmd`) |
| `R/12_geography.R` | geographical effects across England: variance components by group, every local authority's effect, region averages, authority absence effects, and simplified county/unitary boundaries (`geography.qmd`) |
| `R/13_neighbourhood_covariates.R` | the neighbourhood covariates experiment: ten Census 2021 measures over each school's 150 nearest OAs (nomis bulk tables) and the school's own White British share, added to v2.1 one at a time and in sets; effects, AIC, and the change in authority and region variance (`neighbourhood_covariates.qmd`) |
| `R/11_oac_neighbourhoods.R` | neighbourhood types: the 2021/2 Output Area Classification (download from data.geods.ac.uk into `data-raw/oac2021/`) profiled over each school's 150 nearest OAs at three tiers, tested against results and value added out of sample, and against local authority effects (`neighbourhood_types.qmd`) |
| `R/10_independent.R` | independent schools: GIAS, census and SEN coverage, KS4 Attainment 8 against KS5 A level results, a reduced specification tested on state schools, and DfE's A level value added by sector (`independent.qmd`); fetches the 2024/25 KS5 and vaqual files if missing |
| `simulator.qmd` | the School Information System page: the Shiny app embedded from Posit Connect Cloud, and how its simulator works (it replaced the in-browser `simulator.js`) |
| `app/`, `R/40_app_data.R`, `R/41_app_travel_national.R` | the School Information System v2.0 (Shiny, Posit Connect Cloud) and the scripts that build its data: `app_data.rds` (schools, value added and Attainment 8 by year and over four years, absence against intake, authority effects, the simulator's and twins' data, distance and journeys) and `travel_national.rds` (walking catchments from the national routing, partial until it finishes) |
| `twins.qmd`, `twins.js` | Find a twin: a school's closest look-alikes in intake and setting (Gower-type distance, in the browser) and the anti-twins that do better on absence, attainment or value added; data from `R/08` |
| `R/area_helpers.R` | the original area documents' charts, ported |
| `R/site_helpers.R`, `R/map_js.R` | palette, charts, decomposition, map |
| `R/explore_js.R`, `R/explore_data.R` | the area explorer (map with measure, period and smoothing controls, trajectory chart, sortable table) and the school data it carries; used by `explore.qmd` and the trust map in `trust_aldridge.qmd` |
| `data/va_v2.rds`, `data/va_v2_schools.csv` | the results (committed) |
| `data/value_added_review.rds` | the review's results, from the original project |
| `docs/` | the built site (GitHub Pages) |
