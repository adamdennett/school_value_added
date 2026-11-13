# School Information System v2.1

A Shiny app for every state-funded mainstream secondary school in England. It sets
Attainment 8 beside Value Added v2.1, for all, disadvantaged and non-disadvantaged
pupils (with girls and boys), over four years (shrunk), a single year or a moving
average. It replaces the v1.0 School Attainment Policy Simulator
(`school_attainment_tool/app`), which ran on the original model.

Deployed on Posit Connect Cloud from this folder (`manifest.json`). The methods and
findings are on the [School Value Added site](https://adamdennett.github.io/school_value_added).

## Tabs

| Tab | Module | What it does |
|---|---|---|
| About | `R/mod_about.R` | what the system is, what is new, the model, a guide to the tabs |
| School Finder | `R/mod_finder.R` | a school by area, name or map: Attainment 8, expected, value added with its interval, absence against expected, every group, year by year, journeys to it |
| Find My Twin | `R/mod_twin.R` | the closest look-alikes in intake and setting, or those doing better; matching as on the site's twins page |
| Observed vs Predicted | `R/mod_obs.R` | Attainment 8 and absence against what intake predicts, with an area and a school picked out |
| Policy Simulator | `R/mod_sim.R` | the v2.1 coefficients applied to one school, or every school in an area (a port of the site's `simulator.js`) |
| Area Overview | `R/mod_area.R` | a region, combined or mayoral authority, local authority, district or academy trust: its schools, every group against England, the area among others of its kind, the authority's effect |
| Travel and Distance | `R/mod_travel.R` | distance and journey times, deprived third against the rest; walking catchments for every state secondary from the national routing |
| Pupils and Places (v2.1) | `R/mod_pupils.R` | pupils by year group since 2015/16 for England, a region or an authority, against its region and England; the Year 7 roll-forward to 2032/33; a map of change by authority; schools opened, closed and merged since 2015/16 |
| School Finances (v2.1) | `R/mod_finance.R` | share in deficit, median reserve, income per pupil and reserve bands for maintained schools, academies or both, by England, region, authority or Sussex district; a map by authority; warning-sign counts and the closure model; the Sussex districts side by side. Aggregates only: nothing from fewer than 5 schools, no school named |
| Coming next | `R/mod_future.R` | placeholders for v2.2 (primary, post-16) |

The bar under the title (pupils, period, year, window) applies to every tab, and so do the
chosen school and the chosen place: an area type and area picked in the Finder, Observed vs
Predicted, Simulator or Area Overview is picked in all of them, and Travel and Distance follows
its local authority (`place` in app.R, `area_picker_server()` in R/00_setup.R).
`R/00_setup.R` loads the data and holds the shared helpers (Shiny sources `R/` in
alphabetical order, so it runs first): the period logic (`metrics()`), area filters,
colour scales and the base map (Esri's light grey canvas).
`R/01_v21.R` holds what the two v2.1 tabs share: `v21_unit()` turns the shared place into
an area they have (a district gives its authority, except the Sussex districts in School
Finances; three reorganised councils are shown combined), the comparison lines, and the
authority map. Both tabs follow the shared place and set it when the map is clicked.

## Data

`data/app_data.rds` is written by `R/40_app_data.R` in the project root, from files the
site already builds; its header lists each table and its source. `data/travel_national.rds`
is optional, written by `R/41_app_travel_national.R` from the sibling school_active_travel
project (complete since 13 November 2026). `data/v21.rds` is written by
`R/46_app_v21_data.R` from the demographics and finance pages' data (R/42, R/43, R/44).
Nothing is pupil-level, and the finance figures are aggregates: no school is named.

## Colours

The current UCL palette, led by its lighter purples. Better and worse use ColorBrewer
PRGn (purple worse, green better), readable for colour-blind users; value added and
absence against expected sit on fixed national scales (the 97.5th percentile of the
absolute value across England), so a colour means the same in every area.
