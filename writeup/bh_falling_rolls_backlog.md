# Brighton and Hove: why are numbers falling? (backlog)

Logged 13 November 2026. Not started.

## The trigger

Brighton and Hove News, 13 November 2026, Sarah Booker-Lewis (local democracy reporter), "Pupil intakes to be cut at two more schools":
<https://www.brightonandhovenews.org/2026/11/13/pupil-intakes-to-be-cut-at-two-more-schools/>

- **What is proposed:** cabinet approved consultation on the published admission number for September 2028.
  - **Brunswick Primary:** 120, then 90 (2023), then a temporary 60 (2025); the 60 to be made permanent.
  - **Coldean Primary:** 30 in some years; kept 60 this year but offered only 36 places.
  - **Consultation:** opens 23 November 2026 for seven weeks.
- **Causes given:**
  - Cllr Tobias Sheard (Labour): house prices; young adults stuck in shared housing, HMOs or the family home.
  - Cllr Bella Sankey (Labour, leader): will "use every lever" against the falling birth rate; affordable and social housing.
- **Figure quoted:** under-fives fell by a quarter between the 2011 and 2021 Censuses.

What we already know from published data (R/47):
- **Births:** 2,952 (2015) to 2,118 (2025), −28%, against −16% in England.
- **Births ratio:** 0.92, so about 8% of each birth cohort is gone by Reception (moves, independent and special schools).
- **Year 7:** −20% by 2032/33, −24% by 2036/37.

## The question

Is affordability behind Brighton and Hove's very low fertility and falling rolls, as councillors say, or is something else at work over and above it?

## Steps 1 and 2: done 13 November 2026 (`R/49_bh_fertility_affordability.R`)

Figures: `writeup/figures/bh_tfr_affordability_over_time.png`, `writeup/figures/district_change_tfr_vs_affordability.png`.

- **Fertility, not births.**
  - **Brighton and Hove:** total fertility rate 1.36 in 2013 to 0.98 in 2025 (0.96 at its lowest, 2023 and 2024).
  - **England:** 1.83 to 1.39.
  - Brighton and Hove has been about 25% below England throughout. Its rate in 2023–25 is the 2nd lowest of 292 districts in England, after Cambridge.
- **Births fell because of rates, not women.**
  - **Brighton and Hove, 2013 to 2025:** births −29%, women aged 15–44 +4%, general fertility rate −31%.
  - **England:** births −16%, women +8%, rate −22%.
- **The deficit is under 30**, against England in 2025, births per 1,000 women:
  - aged 20–24: 8.6 against 34.8 (a quarter);
  - aged 25–29: 33.9 against 72.0 (under half);
  - aged 30–34: 71.5 against 94.3;
  - aged 35–39: 64.4 against 56.6, higher than England.
  - Standardised mean age of mother: 33.4 against 31.2.
  - This is consistent with the city's young adults (students, renters) not having children while they live there, and with people leaving before or as they do.
- **The trend is national.** The absolute fall was −0.41 children per woman, about the typical district's −0.39. From a low base that is a large proportional fall: −30%, the 13th largest of 292, against a median of −21%.
- **Affordability does not explain the trend.**
  - **Change in affordability against change in fertility** (2013–15 to 2023–25, 292 districts): no relationship (R² = 0.001).
  - **Within each district over time** (district and year effects, 2013–25): same-year affordability has no effect (−0.002, p = 0.91). Affordability three years earlier has, if anything, the opposite sign: about +0.05 children per woman for a doubling of the ratio (0.077 per unit of log ratio), p < 0.001. That reflects sorting rather than a causal effect.
  - **Brighton and Hove's own series:** the ratio rose from 8.9 (2013) to 12.4 (2021), then eased to 10.6 (2025). Fertility fell in step with England's and has levelled since 2022.
- **Affordability explains little of the level either.** Across districts in 2023–25, R² = 0.07. Brighton and Hove is 0.44 children per woman below what its affordability predicts, the 2nd largest gap.
- **The decisive comparison is the neighbours.** Lewes, Mid Sussex and Wealden face almost the same ratio (10.1 to 10.7) but have fertility of 1.45 to 1.58.
  - The lowest-fertility places are university cities, whatever their prices: Cambridge 0.92, Brighton and Hove 0.97, Norwich 1.04 (ratio only 6.8), York 1.05, Exeter 1.05, Oxford 1.06.
  - So the pattern is about who lives in the city at family-forming ages (students and young adults in, families out to the commuter districts), more than local prices.
- **Next:** step 3 (Census drivers) and step 4 (migration of 0–4s and 30–39s by age) test that directly. For step 5, use ONS house prices by MSOA (HPSSA, median price by MSOA) to map prices within the city.

## Plan

1. **Fertility, not just births.** ONS total fertility rate and age-specific fertility rates by authority (Nomis NM_207_1), 2013 to 2025.
   - Where does Brighton and Hove rank, and has the gap with England widened?
   - Split the fall in births into fewer women aged 20 to 39 against lower rates per woman.
2. **Affordability.** ONS house price to residence-based earnings ratio by authority, annual.
   - Across authorities and over time: does affordability explain fertility?
   - Where does Brighton and Hove sit against its prediction (the residual)?
3. **What else, and does it explain the residual?** Census 2021 by authority (and Output Area within the city):
   - students and the student-age share of women, which deflates rates;
   - graduates;
   - private renting and HMOs;
   - one-person households;
   - adults living with parents;
   - lesbian, gay and bisexual share (Brighton and Hove is among the highest in England; check the exact rank);
   - partnership and marriage;
   - mothers born outside the UK.
   - Compare with similar places: Bristol, Oxford, Cambridge, Exeter, York, Norwich, Bath, Reading.
4. **Families moving out.** ONS internal migration by single year of age, authority to authority.
   - Net loss of 0 to 4s and adults aged 30 to 39 from Brighton and Hove, and where they go (Lewes, Mid Sussex, Adur, Wealden?).
   - The births ratio of 0.92 already hints at it.
5. **Within the city.** Children aged 0 to 4 per woman aged 15 to 44 by Output Area (Census 2021, as in the active travel data).
   - Map it beside house prices or rents and the area types, to show where the fall is concentrated.
   - That bears directly on which schools lose pupils: Brunswick (central Hove) against Coldean (edge of the city, near the universities).

All from published ONS, Land Registry or ONS price statistics, and Census data; no pupil-level records.

## Step 4, first pass: births against migration (13 November 2026, `R/50_pupils_births_migration.R`)

Change in pupils from 2018/19 to 2025/26, split exactly into log points (×100): births + net migration of those children since birth + state-school share.
- **Brighton and Hove, primary:** −14.7 = births −18.6, migration +2.5, state-school share +1.4. Almost all of the fall is births.
  - About 8–10% of each Brighton cohort is gone by primary age: 0.90 to 0.92 of births are still resident, against England's 1.01 to 1.05.
  - That net loss is long-standing, not new. It explains the low level, not the fall.
- **Brighton and Hove, Reception:** −22.8 = births −28.5, migration +13.3, state-school share −7.6. The last two partly offset and may reflect ONS's revised migration estimates from mid-2022, so read them together (+5.7).
- **Brighton and Hove, Year 7:** −7.1 = births −4.4, migration −5.1, state-school share +2.5.
- **England, primary:** −5.5 = births −6.9, migration +3.5, state-school share −2.1.
- **London, median borough, primary:** −12.4 = births −6.0, migration −6.0. Half the fall is families leaving; in Hackney (−15.0) and Tower Hamlets (−14.1), migration is most of it.
- **Across authorities:**
  - for Reception, births go with 87% of the differences in change;
  - for primary as a whole, births and migration go with about half each;
  - for Year 7, migration goes with 49%.

## To do next (logged 13 November 2026)

1. **Write up R/50 on the demographics page.**
   - A short section after "Births, fertility and the cost of housing", with the two charts in `writeup/figures/pupil_change_*.png`.
   - England, London, Brighton and Hove, and the shares across authorities.
   - Caveats: modelled migration in the ONS mid-year estimates (method change from mid-2022); migration and state-school share trade off.
2. **Test education against fertility.** Lower fertility has long gone with more highly educated women.
   - **Measure:** % of women aged about 25–44 with Level 4+ qualifications, from Census 2021. Use highest qualification by sex and age (an RM table) if available, else TS067 for all adults. The Level 4 measure used before in this project is TS067 by Output Area.
   - **Models:**
     - total fertility rate (2021, or 2023–25) on female Level 4+;
     - add the house price to earnings ratio, the student share, the private-renting share and the share of mothers born outside the UK;
     - see whether education absorbs Brighton and Hove's −0.44 gap and the university-city pattern.
   - **Change over time:** Census 2011 against 2021 change in female Level 4+ against the change in fertility.
   - **Also:** age-specific rates under 30 against graduate share, as a postponement test.
