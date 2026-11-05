# 12_geography.R - Geographical effects across England
# ---------------------------------------------------------------------------
# Assembles, for the England-wide geography page (geography.qmd):
#   * how much of Attainment 8 sits with region, local authority and school
#     once the v2.1 terms are allowed for (the fitted variance components);
#   * every local authority's effect, for every group, with intervals;
#   * each region's average value added against England (schools' average
#     log residual, weighted by years);
#   * local authorities' absence effects (R/08) beside their value added
#     effects;
#   * county and unitary authority boundaries (ONS, December 2023),
#     simplified for the web, from the original project's copy.
#
# Output: data/geography.rds (committed).   Rscript R/12_geography.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(sf))

V  <- readRDS(here::here("data", "va_v2.rds"))
AC <- readRDS(here::here("data", "absence_causes.rds"))

# how big each level is, as a standard deviation in % of Attainment 8
sd_pct <- function(s) 100 * (exp(s) - 1)
levels <- V$vc |> filter(grp %in% c("gor_name", "LANAME:gor_name")) |>
  transmute(group, level = ifelse(grp == "gor_name", "Region", "Local authority"), sd = sdcor) |>
  bind_rows(V$summary |> transmute(group, level = "School (lasting)", sd = tau_school)) |>
  mutate(pct = sd_pct(sd))

# local authority effects, every group
la <- V$la |> mutate(pct = sd_pct(effect), lo_pct = sd_pct(lo), hi_pct = sd_pct(hi)) |>
  left_join(V$schools |> distinct(LANAME, gor_name), by = "LANAME", relationship = "many-to-one")

# region averages of value added against England
reg <- V$va |> inner_join(V$schools |> select(URN, gor_name), by = "URN") |>
  group_by(group, gor_name) |>
  summarise(schools = n(), rbar = weighted.mean(rbar_nat, years), .groups = "drop") |>
  mutate(pct = sd_pct(rbar))

# absence: each authority's effect on each kind of absence, beside value added
abs_la <- AC$la_effects |> select(outcome, LANAME = la, abs_pct = la_pct)
la_abs <- la |> filter(group %in% c("all", "disadvantaged")) |> select(LANAME, group, pct) |>
  inner_join(abs_la, by = "LANAME", relationship = "many-to-many")
abs_cor <- la_abs |> group_by(group, outcome) |>
  summarise(las = n(), r = cor(pct, abs_pct), .groups = "drop")

# boundaries, simplified
b <- readRDS(file.path(SAT, "app", "data", "la_boundaries_utla.rds")) |>
  st_transform(27700) |> st_simplify(dTolerance = 250, preserveTopology = TRUE) |>
  st_transform(4326) |> transmute(LANAME = CTYUA23NM, code = CTYUA23CD) |>
  filter(substr(code, 1, 1) == "E")

saveRDS(list(built = Sys.Date(), levels = levels, la = la, reg = reg, abs_cor = abs_cor, la_abs = la_abs,
             bounds = b), here::here("data", "geography.rds"))
message("Saved data/geography.rds (", format(object.size(b), units = "MB"), " of boundaries)")
print(levels |> select(group, level, pct) |> tidyr::pivot_wider(names_from = level, values_from = pct))
print(reg |> filter(group %in% c("all", "disadvantaged")) |> select(group, gor_name, pct) |>
        tidyr::pivot_wider(names_from = group, values_from = pct) |> arrange(desc(disadvantaged)))
print(abs_cor)
