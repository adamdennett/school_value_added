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
  information, dated, beside every school.
- **Every number in the text is computed**, never typed.

## Section order (`templates/area.qmd`)

1. **Summary.** Schools and mean value added, significant above/below;
   groups; strongest and weakest; absence against intake (with the
   authority's rank, and schools more than a point above); the authority's
   own effect (authority tier); the district test (district and authority
   tiers, not unitary); suspensions both ways and reduced timetables;
   trusts that stand out; latest Ofsted.
2. **The schools.** Map (value added by default; absence against intake,
   v1, intake, Ofsted and governance in the menu); school table; who runs
   them (governance chart).
3. **How the area compares.** Profile table against comparators and
   England, with ranks, plus v2 value added and absence against intake.
4. **What explains attainment.** v2 decomposition chart for the area and
   its comparators; table; every piece of at least 0.25 points listed.
5. **Place.** Authority effects (caterpillar); the v2 district test chart
   and sentence; district table (region and authority tiers).
6. **Trusts.** v2 trust effects for the trusts running the area's schools.
7. **The disadvantaged gap, and girls and boys.** Value added by school
   for the groups; gap schools; girl/boy differences of more than two
   points.
8. **Absence.** Series; absence against intake by school with reasons;
   reasons chart; suspensions both ways, by school; scenarios.
9. **School by school.** The joint-signal plane: absence against intake
   by v2 value added with attendance counted in.
10. **Contextual league tables.** Value added by year for each group.
11. **Questions for discussion**, by rule: disadvantaged pupils (always);
    attendance "a lever" if absence costs more than 1 point or the area
    runs more than 1 point above intake, "modest" above 0.3 / 0.5,
    otherwise "not the main lever"; schools not the place (district test);
    practice if reduced timetables exceed 1.5 times or suspensions 1.3
    times the national rate; the authority effect for disadvantaged
    pupils.
12. **Notes on method.**

## District summary (`districts.qmd`)

Summary; profile table with ranks, v2 value added by group and absence
against intake; decomposition heat map; district effects; absence and
practice table; a paragraph and link per district.
