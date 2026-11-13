# 50_pupils_births_migration.R - How much of the fall in pupils is births, and how much is migration?
# ---------------------------------------------------------------------------
# For each local authority (and England), the change in pupils from 2018/19 to 2025/26 is
# split exactly into three parts, since
#
#   pupils = births of those cohorts x (children living there now / births) x (pupils / children)
#
#   births      the cohorts' size when born in the area (ONS mid-year estimate at age 0)
#   migration   net migration of those children since birth, in and out of the area, within the
#               UK and from abroad (children of the cohorts living there now / at age 0; deaths at
#               these ages are negligible)
#   schooling   the share of the area's children in its state-funded mainstream schools: moves to
#               and from independent and special schools, home education, and children crossing
#               authority borders to school
#
# In logs the three add up to the change in pupils exactly. They are reported as percentage
# points of log change (x100), and as shares of the change.
#
# Groups and years:
#   Reception      age 4 at mid-year, 2018/19 to 2025/26
#   Primary        Reception to Year 6, ages 4 to 10, 2018/19 to 2025/26
#   Year 7         age 11, 2018/19 to 2025/26
#   Secondary      Years 7 to 11, ages 11 to 15, 2019/20 to 2025/26 (cohorts aged 15 in
#                  2019 were 0 in 2004, the first year downloaded)
# A child in Reception in academic year t/t+1 was born September t-5 to August t-4, so at 30
# June t most (10 of 12 months of births) are aged 4: school year group g is taken as age g+4
# at mid-year t, and the cohort's births as the mid-year estimate at age 0 in year t-age.
#
# Sources (all published):
#   ONS mid-year population estimates by single year of age, county and unitary authorities
#     (April 2023), ages 0 to 15, mid-2004 to mid-2025, Nomis NM_2002_1, downloaded one year at
#     a time to data-raw/ons/mye_age0_15_utla_2004_2025_nomis_NM_2002_1.csv (README has the
#     command). Mid-2022 onwards use ONS's revised (admin-based) migration methods.
#   Pupils by year group: as R/42 (data/demographics.rds, DfE school census, state-funded
#     primary and secondary schools, January 2016 to January 2026).
# Areas: as R/42, with three reorganised councils combined.
#
# Output: data/pupils_births_migration.rds, writeup/figures/pupil_change_decomposition*.png
#   Rscript R/50_pupils_births_migration.R   (under a minute)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
DEM <- readRDS(here::here("data", "demographics.rds"))
area_of <- function(la) ifelse(la %in% names(DEM$combine), DEM$combine[la], la)
canon <- function(x) gsub("[^a-z]", "", tolower(gsub("&", "and", x)))

# ---- population by single year of age --------------------------------------------------
M <- fread(here::here("data-raw", "ons", "mye_age0_15_utla_2004_2025_nomis_NM_2002_1.csv"))
setnames(M, c("year", "name", "code", "age", "pop"))
M <- M[substr(code, 1, 1) == "E"]                                   # England and English authorities
M[, age := as.integer(sub("Age ", "", age))]
M[, area := ifelse(name == "England", "England", area_of(name))]
M <- M[, .(pop = sum(pop)), by = .(area, year, age)]
pop <- function(a, y) M[age == a & year == y, .(area, pop)]

# ---- pupils by year group ----------------------------------------------------------------
YG <- as.data.table(DEM$yeargroups)[level %in% c("Local authority", "National")]
YG[, y0 := as.integer(substr(year, 1, 4))]                          # 2018 for 2018-19
steps <- c("R", paste0("Y", 1:11))

# one group of year groups (by index 0 = Reception ... 11 = Year 11), one year
parts <- function(g, y) {
  ages <- g + 4L
  pu <- YG[y0 == y, .(area, pupils = rowSums(.SD)), .SDcols = steps[g + 1]]
  pp <- M[year == y & age %in% ages, .(children = sum(pop)), by = area]
  bb <- rbindlist(lapply(ages, function(a) M[year == y - a & age == 0, .(area, b = pop)]))[, .(births = sum(b)), by = area]
  Reduce(function(x, z) merge(x, z, by = "k"), list(pu[, k := canon(area)], pp[, k := canon(area)][, area := NULL], bb[, k := canon(area)][, area := NULL]))
}
decompose <- function(label, g, y1, y2) {
  a <- parts(g, y1); b <- parts(g, y2)
  d <- merge(a, b, by = c("k", "area"), suffixes = c("_1", "_2"))
  d[, `:=`(group = label, from = y1, to = y2,
           total = 100 * log(pupils_2 / pupils_1),
           births = 100 * log(births_2 / births_1),
           migration = 100 * log((children_2 / births_2) / (children_1 / births_1)),
           schooling = 100 * log((pupils_2 / children_2) / (pupils_1 / children_1)),
           pct_pupils = 100 * (pupils_2 / pupils_1 - 1),
           retain_1 = children_1 / births_1, retain_2 = children_2 / births_2,
           share_1 = pupils_1 / children_1, share_2 = pupils_2 / children_2)]
  d[]
}
X <- rbind(decompose("Reception", 0L, 2018L, 2025L),
           decompose("Primary (Reception to Year 6)", 0:6, 2018L, 2025L),
           decompose("Year 7", 7L, 2018L, 2025L),
           decompose("Secondary (Years 7 to 11)", 7:11, 2019L, 2025L))
bad <- X[!is.finite(total + births + migration + schooling)]; if (nrow(bad)) { message("Dropped (missing or zero): ", paste(unique(bad$area), collapse = ", ")); X <- X[is.finite(total + births + migration + schooling)] }
stopifnot(all(abs(X$total - (X$births + X$migration + X$schooling)) < 1e-6))
X <- X[area != "City of London"]                                  # a few hundred children: ratios swing wildly
unmatched <- setdiff(unique(YG[level == "Local authority", area]), c(X$area, "City of London"))
message("Census areas not matched to population estimates: ", paste(unmatched, collapse = ", "))

# Across authorities: how much of the variation in pupil change goes with each part
# (covariance share: cov(part, total) / var(total); the three add up to 1)
var_share <- X[area != "England", .(areas = .N,
  births = cov(births, total) / var(total), migration = cov(migration, total) / var(total),
  schooling = cov(schooling, total) / var(total)), by = group]
# England, London and Brighton and Hove, and the regions' authorities summarised
reg <- unique(as.data.table(DEM$trend)[level == "Local authority", .(area, region)])
X <- merge(X, reg, by = "area", all.x = TRUE)
X[area == "England", region := "England"]
by_region <- X[area != "England", lapply(.SD, median), by = .(group, region), .SDcols = c("total", "births", "migration", "schooling")]
key_areas <- X[area %in% c("England", "Brighton and Hove", "Camden", "Westminster", "Southwark", "Hackney", "Lambeth", "Cambridgeshire",
                           "Oxfordshire", "Bristol, City of", "East Sussex", "West Sussex", "Kent", "Surrey")]

res <- list(X = X, var_share = var_share, by_region = by_region, unmatched = unmatched, built = Sys.Date())
saveRDS(res, here::here("data", "pupils_births_migration.rds"))

# ---- figures --------------------------------------------------------------------------------
dir.create(here::here("writeup", "figures"), showWarnings = FALSE, recursive = TRUE)
th <- theme_minimal(base_size = 12) + theme(legend.position = "top", panel.grid.minor = element_blank(), plot.title.position = "plot")
PCOL <- c(Births = "#361a54", Migration = "#993bff", `State-school share` = "#c9c9d6")
pick <- c("England", "Brighton and Hove", "Westminster", "Camden", "Southwark", "Lambeth", "Bristol, City of", "Cambridgeshire", "East Sussex", "West Sussex", "Kent", "Surrey")
f <- melt(X[group == "Primary (Reception to Year 6)" & area %in% pick, .(area, total, Births = births, Migration = migration, `State-school share` = schooling)],
          id.vars = c("area", "total"), variable.name = "part", value.name = "pp")
f[, area := factor(area, X[group == "Primary (Reception to Year 6)" & area %in% pick][order(-total), area])]
tot <- unique(f[, .(area, total)])
g1 <- ggplot(f, aes(pp, area, fill = part)) + geom_col(width = 0.7) + geom_vline(xintercept = 0, colour = "#6b6680") +
  geom_point(data = tot, aes(total, area), inherit.aes = FALSE, shape = 23, size = 2.6, fill = "white", colour = "#1a1a2e") +
  scale_fill_manual(values = PCOL, name = NULL) +
  labs(x = "Change in primary pupils, 2018/19 to 2025/26 (log points x 100; diamond = total)", y = NULL,
       title = "What lies behind the change in primary pupils",
       caption = "ONS mid-year population estimates (Nomis NM_2002_1); DfE school census") + th
ggsave(here::here("writeup", "figures", "pupil_change_decomposition_primary.png"), g1, width = 8.5, height = 5.5, dpi = 150)
g2 <- ggplot(X[group == "Primary (Reception to Year 6)" & area != "England"], aes(births, migration, colour = region == "London")) +
  geom_hline(yintercept = 0, colour = "#d3cde0") + geom_vline(xintercept = 0, colour = "#d3cde0") +
  geom_point(alpha = 0.8, size = 2) + scale_colour_manual(values = c(`TRUE` = "#993bff", `FALSE` = "#9e9eb0"), labels = c(`TRUE` = "London", `FALSE` = "Elsewhere"), name = NULL) +
  ggrepel::geom_text_repel(data = X[group == "Primary (Reception to Year 6)" & area %in% c("Brighton and Hove", "Westminster", "Camden", "Southwark", "Lambeth", "Hackney")],
                           aes(label = area), colour = "#361a54", size = 3.2, seed = 50, box.padding = 0.5, min.segment.length = 0) +
  labs(x = "Births part (log points x 100)", y = "Migration part (log points x 100)",
       title = "Births and migration behind the change in primary pupils, by local authority",
       subtitle = "2018/19 to 2025/26", caption = "ONS mid-year population estimates (Nomis NM_2002_1); DfE school census") + th
ggsave(here::here("writeup", "figures", "pupil_change_births_vs_migration.png"), g2, width = 8, height = 6, dpi = 150)

options(width = 160)
cat("\n== England and selected areas (log points x 100; total = births + migration + schooling)\n")
print(key_areas[order(group, -total), .(group, area, pct_pupils = round(pct_pupils, 1), total = round(total, 1), births = round(births, 1),
                                         migration = round(migration, 1), schooling = round(schooling, 1),
                                         retain_1 = round(retain_1, 3), retain_2 = round(retain_2, 3), share_1 = round(share_1, 3), share_2 = round(share_2, 3))])
cat("\n== Across authorities: share of the variation in pupil change going with each part\n"); print(var_share, digits = 3)
cat("\n== Median by region, primary\n"); print(by_region[group == "Primary (Reception to Year 6)"][order(total)], digits = 3)
