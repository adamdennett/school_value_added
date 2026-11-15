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

## Progress (15 November 2026)

Code is in the private repo `school_house_prices` (R/01 to R/08).

- **Panel built.**
  - 24,618,450 sales matched to an EPC (83% of transactions).
  - 6,401,832 sales from September 2018 to September 2026, with Ofsted at the date of sale, walking minutes to Outstanding schools, value added, Attainment 8, and popularity (first preferences per place).
- **Steps 2 and 3 done** (R/06; 5.18m sales). The raw premiums are the councillor's order of magnitude: an Outstanding secondary within 10 minutes' walk +37%; nearest secondary Outstanding +32%; top 10% Attainment 8 +22%. They shrink like for like:
  - for the same kind of home (district effects): +2.7%, +5.2% and +4.1%;
  - for the same home within the same LSOA: +0.6%, +1.0% and +0.9%.
  - A top 10% value-added school: −11% raw, −4% like for like, 0 within LSOA.
  - **Buyers pay for intake (raw results), not value added**: step 4(c) is answered at first pass.
- **The source of the 20 to 25% figure is not found.** Industry studies give 12–18% (Lloyds, Knight Frank, HouseSimple, Rightmove primary 18%), all unadjusted area comparisons.
- **New key reference.** Greaves, E. and Venturin, A. (2026) *Re-examining the property price premium around 'better' schools*. IZA DP 18750, June 2026.
  - England, secondary schools, boundary discontinuity design.
  - Average premium about 2.5%; about 6.5% where the quality gap across the boundary is in the top quartile; weak or absent elsewhere.
  - "Escaping the bottom" ≈ "reaching the top".
  - Value added has no independent pricing power.
  - The premium needs attainment and an affluent intake together.
  - Our within-LSOA results agree. Next is to add their "gap" heterogeneity with our walking-time nearest pairs.

## Progress (15 November 2026, afternoon)

Steps 1 to 4 are done (R/09 to R/12). Findings are in `school_house_prices/report/findings.qmd`: private working notes, not on the public site yet.

- **Quality gaps (R/09; walking-time analogue of Greaves and Venturin):**
  - the better side of a split LSOA sells for +0.6%;
  - by gap quartile: +0.1%, +0.6%, +0.8%, +1.1%;
  - a premium only where higher results and fewer FSM pupils go together;
  - value added −0.3%.
- **Ofsted regrading event study (R/10):**
  - upgrades +0.1–0.2% on average over three years;
  - secondary upgrades build to about +0.8% by three and a half years;
  - downgrades show no clear fall;
  - some pre-trends.
- **Admissions (R/11):**
  - distance or catchment schools +1.5% per SD of Attainment 8; faith +0.95%; grammar +0.15%;
  - in the split-LSOA design, no premium when the better school is a faith school.
  - Brighton and Hove alone is too imprecise (23,600 sales).
- **Region (R/11):** highest in Yorkshire and The Humber and Inner London; lowest in the South East. No deprivation gradient.
- **Three pupil groups (R/12):**
  - each group's Attainment 8 is priced about equally (+0.5–0.7% per SD);
  - value added is zero or negative for every group;
  - with the FSM share in the model, the FSM share carries the premium (−1.0% per 10 points).

**Next:**

- drop nearest-school lines that follow major roads, rivers and railways (OS Open Roads and Open Rivers; rail from OSM);
- actual catchment boundaries where councils publish them (start with Brighton and Hove);
- school measures as at the date of sale (KS4 by year);
- decide whether a public page goes on the School Value Added site.
