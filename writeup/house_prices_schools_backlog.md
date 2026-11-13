# Do schools push up house prices? A national follow-up to Tan and Dennett (backlog)

Logged 13 November 2026. Not started; Adam wants to do this.

## Why now

- **The claim.** Cllr Tobias Sheard, youth voice adviser to Brighton and Hove's cabinet, said that nationally house prices had gone up by 20 to 25 per cent close to the most popular and successful schools.
  - Source: [Brighton and Hove News, 13 November 2026](https://www.brightonandhovenews.org/2026/11/13/pupil-intakes-to-be-cut-at-two-more-schools/).
  - The figure's own source is not given. Trace it; it may be an unadjusted premium.
- **Our earlier finding.** Tan, J. and Dennett, A. *Do good schools influence property prices? Disentangling school and neighbourhood effects with empirical evidence from Brighton* (CUPUM book chapter; MSc work 2023).
  - **Data:** 81,652 Brighton sales, 2000 to 2019; Price Paid linked to EPCs (Chi et al. 2022); r5r walking and bus times to schools, stations and the centre; school quality at the time of sale; IMD; wards.
  - **What it found:** once dwelling attributes and neighbourhood are accounted for, nearness to good schools has only a small effect:
    - good secondary within 5–10 minutes' walk: about +3–4%;
    - good primary within 5 minutes: +5.5%;
    - against IMD decile 10 against 1: +24–28%;
    - against a 5-minute bus ride to the centre: +27–37%.
  - **Catchments:** the effect vanishes once wards are included.
  - **The 2008 lottery reform** did not dampen prices near popular schools.
  - **Limits it named:** perceived quality, not value added; one city.

## The data we now have

- **Prices and dwellings, nationally.** Chris Beech's DuckDB refactor of Bin Chi's Price Paid to EPC linkage: <https://github.com/chris-beech-housing/Land-Registry-PPD-EPC-price-per-square-metre>.
  - It produces `pp-psqm.csv`: price, floor area, rooms, type, age, energy rating and postcode for every matched sale in England and Wales.
  - Under 5 minutes on a laptop, plus an extra "unit signature" rule (+66,000 matches).
  - **Inputs:**
    - Land Registry `pp-complete.csv`;
    - all domestic EPC certificates, from get-energy-performance-data.communities.gov.uk (Adam downloads these; the service needs a sign-in);
    - Bin Chi's `rulechi.csv` (in the repo).
  - The code's own licence line says academic use only.
- **Walking routes to every school.** The national routing (`school_active_travel`, Zenodo 10.5281/zenodo.24188820): walking times, distance and climb from every Output Area to every school within 120 minutes' walk.
- **School quality over time.**
  - Ofsted grades as at 31 August each year (2018–2023 downloaded; earlier years on gov.uk).
  - Value Added v2.1 and Attainment 8 (secondaries, 2021/22 to 2025/26).
  - KS2 results (primary, for v2.2); free school meals and SEN (school census).
- **Popularity.** DfE *Secondary and primary school applications and offers*, school level: first preferences against places, which is closest to "most popular".
- **Neighbourhood.** IMD, Output Area Classification, LSOA/MSOA, and distance to stations and centres (routing extensions).

## Plan

1. **Build the sales panel.**
   - Run Beech's pipeline; geocode each sale to its 2021 Output Area by postcode (ONSPD).
   - Attach the nearest primary and secondary by walking time, and the schools within 10, 15 and 20 minutes, with each school's quality at the date of sale.
2. **Reproduce the councillor's number first.** The raw premium near the most popular and successful schools (top decile of preference ratio or Ofsted Outstanding), unadjusted. It may well be 20–25%.
3. **Then adjust, as in Tan and Dennett:**
   - log price on dwelling attributes and year by area;
   - then neighbourhood fixed effects (LSOA or MSOA), so the school term is identified within neighbourhoods;
   - report how much of the raw premium survives.
4. **The stronger national designs:**
   - (a) Changes in a school's Ofsted grade or value added against price changes nearby, against matched postcodes further away (event study or difference-in-differences).
   - (b) Admissions rules: areas with distance criteria against lottery or banded areas.
   - (c) Do buyers pay for value added or for intake (raw attainment)? Value Added v2.1 makes that testable for the first time at this scale.
5. **Primary against secondary**, and heterogeneity by region, London, urban–rural and deprivation. Always split by disadvantage where pupils come in ([[three pupil groups]] applies to school measures, not to sales).

## Outputs

- A paper extending Tan and Dennett nationally.
- A site page (School Value Added) and possibly a tab in the School Information System.
- The linked sales and school file, where licences allow.
