# Area pages: the format (Value Added v2.0)

The record of how the v2 area pages are laid out, so the format can be
changed in one place and carried to every page. It follows the original
project's `AREA_REPORTS.md` (school_attainment_tool), with every
model-based analysis refitted on Value Added v2.0. To change the format,
edit `templates/area.qmd` (or `districts.qmd`) and this file together,
then rebuild.

Built from published DfE and Ofsted data and the 2021 Census; nothing
uses or hints at pupil-level records.

## The tiers

| Tier | Area | Page | Source |
|---|---|---|---|
| Region | Sussex | `sussex.html` | `templates/area.qmd` |
| Authority | Brighton and Hove, East Sussex, West Sussex | `areas/<slug>.html` | `templates/area.qmd` |
| District | the twelve districts of East and West Sussex | `areas/<slug>.html` | `templates/area.qmd` |
| Districts together | all twelve | `districts.html` | `districts.qmd` |

One template serves all three tiers; sections adapt to the tier (`TIER`
is `region`, `la` or `district`). `R/03_area_pages.R` lists the areas and
writes one `.qmd` per area (git-ignored; the template is the source).

## Building

```
Rscript R/01_inputs.R            # modelling data from the original project
Rscript R/02_fit_va.R            # Value Added v2.0, five groups
Rscript R/04_place_and_absence.R # absence against intake, district test, trusts, M2, scenarios (~15 min)
Rscript R/03_area_pages.R        # area pages from the template
quarto render                    # the site, into docs/
```

The original project's area files (`../school_attainment_tool/data/area_<slug>.rds`)
supply what is data rather than model output: the absence series and
reasons, suspensions, school-level reasons, and the comparator profiles.

## What each analysis uses

| Analysis | v1 (original) | v2 (here) |
|---|---|---|
| Value added | published model, within the LA | v2.0, against England, shrunk with intervals |
| Absence against intake | intake-only stage 1 on v1 terms | intake-only stage 1 on the **v2 intake terms** (`R/04`) |
| Decomposition | published model | v2.0 model (`decompose()`) |
| District test | `(1 | gor_name/LANAME/lad)`, with and without `(1 | URN)` | the same on v2.0 (`R/04`) |
| Trusts | trust level with route and school term | the same on v2.0 (`R/04`) |
| Joint-signal plane | M2 (published model without absence) | v2.0 without its absence term (`R/04`) |
| Scenarios | published absence coefficients | v2.0 absence coefficients (`R/04`) |
| Absence series, reasons, suspensions | DfE data | the same data, reused |

## Conventions

- **Value added** is v2.0 against England, shrunk by reliability, with a
  95% interval; "significantly above / below" means the interval excludes
  zero. Five groups: all, disadvantaged, non-disadvantaged, girls, boys.
- **Absence against intake** is observed absence minus what the v2 intake
  terms predict (with year and region, without the authority), mean over
  2021/22 to 2024/25, in percentage points. It is on the map, in the
  school table, in the profile table and in the summary for every area.
- **Comparators.** A district against its county and Brighton and Hove;
  an authority against the other two Sussex authorities; Sussex against
  its three authorities. England always.
- **Ranks.** Authorities among all authorities; districts among the
  districts in England with five or more schools (`district_rank()`).
- **Unitary authorities.** A unitary authority is its own single district,
  so the district test does not apply to it (the term would duplicate the
  authority's); the page says so.
- **Suspensions** are given both ways: episodes per 100 pupils and pupils
  suspended at least once per 100 (`susp_both()`), with England on both.
- **Ofsted.** The latest judgement from Ofsted's monthly management
  information, dated, beside every school. The map's single Ofsted measure
  takes the report card where there is one; for an inspection between
  September 2024 and November 2025 with no overall grade, the most common
  of its key judgements (ties to quality of education); otherwise the last
  graded inspection.
- **Exclusions.** A school-year is left out of every model when its pupils
  averaged fewer than three GCSE entries or fewer than half achieved any
  qualification (`R/01_inputs.R`, `excluded`). The method page lists them.
- **2025/26 is provisional.** Predicted from the fitted v2.0 model on
  DfE's provisional KS4 results (`R/05_provisional_2526.R`): absence
  bridged from autumn and spring, the unpublished intake data carried
  forward, the year centred on England. It appears as its own column
  (school table, league tables) and map colouring, as the last point in
  the trajectories, and in any explorer moving average that includes it, always
  labelled provisional, never averaged into the four-year figure and never
  shrunk, and every appearance carries `PROV_NOTE`: it will be updated
  with the revised KS4 release in February 2027.
- **Over time.** Yearly value added is the school-year's `va_nat` (not
  shrunk), averaged where two merged schools share a year (`va_years()`).
  Moving averages are centred on a chosen year over 1, 3 or 5 years
  (`centred_ma()`); a window past either end uses the years there are, and
  one that includes 2025/26 is flagged provisional. The four-year shrunk
  figure stays the headline.
- **Lists of schools.** Running text names schools only up to `FEW` (4);
  beyond that it gives the count and the names go in a table (the gap and
  girls/boys tables, the absence table), so no paragraph becomes a list.
- **Absence periods are always labelled.** Three measures appear and each
  says which it is: absence against intake is a mean over the four model
  years, 2021/22 to 2024/25; the add-and-take figure uses each school's
  latest full year (2024/25), a mean of schools; the absence series and
  reasons use autumn and spring of the latest year, sessions-weighted.
- **Every number in the text is computed**, never typed.

## Section order (`templates/area.qmd`)

The thrust of every page is value added in the face of attendance: what
schools achieve with their intake, set beside how well they hold
attendance against what that intake predicts.

1. **Summary.** Schools and mean value added, significant above/below;
   groups; strongest and weakest; absence against intake (with the
   authority's rank, and schools more than a point above); value added
   and attendance quadrant counts (Value Added v2.0 against absence
   against intake); the
   authority's own effect (authority tier); the district test (district
   and authority tiers, not unitary); suspensions both ways and reduced
   timetables; trusts that stand out; latest Ofsted.
2. **The schools.** Map (value added by default; the menu has every value
   added measure including no allowance for absence, every attendance measure
   of the original documents (overall, persistent, against intake,
   illness, unauthorised, reduced timetables, suspensions), Attainment 8 for
   all, disadvantaged and other pupils, the intake, Ofsted old and new,
   governance and religious character); school table; who runs them
   (governance chart).
3. **Value added and attendance** (`#school-by-school`). The joint-signal
   plane, interactive (`fig_plane_live()`, plotly): absence against intake
   (across) by Value Added v2.0 (up), every school in England
   in grey; quadrant counts (`plane_quadrants()`); the schools furthest
   above and below; value added with no allowance for absence explained
   and kept off the chart, since it contains absence against intake.
4. **How the area compares.** Profile table against comparators and
   England, with ranks, plus v2 value added and absence against intake.
5. **What explains attainment.** v2 decomposition chart for the area and
   its comparators; table; every piece of at least 0.25 points listed.
6. **Place.** Authority effects (caterpillar); the v2 district test chart
   and sentence; district table (region and authority tiers).
7. **Trusts.** v2 trust effects for the trusts running the area's schools.
8. **The disadvantaged gap, and girls and boys.** Value added by school
   for the groups; counts, then a table of gap schools and schools above
   for both, and a table of girl/boy differences of more than two points.
9. **Absence.** Series; absence against intake by school with reasons;
   reasons chart; suspensions both ways, by school; scenarios.
10. **Over time.** The area's mean value added by year for each group
    against England (`fig_area_years()`), with which way it moved; every
    school's yearly value added with its three-year centred moving average
    (`fig_trajectories()`, provisional points hollow); a table of three-year
    centred averages by school.
11. **Contextual league tables.** Value added by year for each group.
12. **Questions for discussion**, by rule: disadvantaged pupils (always);
    attendance "a lever" if absence costs more than 1 point or the area
    runs more than 1 point above intake, "modest" above 0.3 / 0.5,
    otherwise "not the main lever"; schools not the place (district test);
    practice if reduced timetables exceed 1.5 times or suspensions 1.3
    times the national rate; the authority effect for disadvantaged
    pupils.
13. **Notes on method.**

## District summary (`districts.qmd`)

Summary; profile table with ranks, v2 value added by group and absence
against intake; decomposition heat map; district effects; absence and
practice table; a paragraph and link per district.
