# 16_parents_not_adults.R - Parents, not adults: is neighbourhood education
# measuring the wrong people?
# ---------------------------------------------------------------------------
# v2.1's neighbourhood term is the share of ALL adults with level 4+
# qualifications in the 150 Output Areas nearest a school. R/15 suggested that
# young, often childless graduates may inflate it around inner-city schools.
# This replaces it with measures of the adults most likely to be pupils'
# parents, from Census 2021 custom tables (ONS API):
#
#   parents   level 4+ among adults aged 25-64 in households with dependent
#             children (qualification x dependent children x age, by MSOA, the
#             finest level ONS releases it; the age limit drops 16-18 year olds
#             who are themselves dependent children)
#   nonpar    the same for adults aged 25-64 without dependent children
#   a3549     level 4+ among those aged 35-49 (by LSOA)
#   a2534     level 4+ among those aged 25-34 (by LSOA)
#   all_msoa, all_lsoa   all adults at those geographies, so that a difference
#             is about who is counted, not the coarser geography
#
# Each is the mean over the school's same 150 nearest Output Areas (each OA
# taking its MSOA's or LSOA's value). For every group, against v2.1, on
# identical rows: fit (AIC, maximum likelihood), each year held out in turn,
# and the three things the idea should explain: the young-adult coefficient,
# the deprivation coefficient and value added by kind of neighbourhood.
#
# Output: data/parents_not_adults.rds (committed).
# Rscript R/16_parents_not_adults.R   (first run downloads ~1.5 GB, cached)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
source(here::here("pipeline", "08_census.R"))
suppressPackageStartupMessages(library(lme4))
num <- function(x) suppressWarnings(as.numeric(x))
K <- 150; BH <- "Brighton and Hove"
API <- "https://api.beta.ons.gov.uk/v1"
cache <- here::here("data-raw", "census2021", "ons_custom"); dir.create(cache, showWarnings = FALSE, recursive = TRUE)

# ---- 1. the lookup and the custom tables -------------------------------------------
lookup_file <- file.path(cache, "oa_lsoa_msoa_2021.csv")
if (!file.exists(lookup_file)) {
  svc <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/OA_LSOA_MSOA_EW_DEC_2021_LU_v3/FeatureServer/0/query"
  # the service stops paging by offset at 100,000 rows, so page by code
  out <- list(); last <- ""
  repeat {
    w <- utils::URLencode(sprintf("OA21CD>'%s'", last), reserved = TRUE)
    r <- jsonlite::fromJSON(paste0(svc, "?where=", w, "&outFields=OA21CD,LSOA21CD,MSOA21CD&orderByFields=OA21CD",
                                   "&returnGeometry=false&f=json&resultRecordCount=2000"))
    a <- r$features$attributes; if (is.null(a) || !nrow(a)) break
    out[[length(out) + 1]] <- a; last <- a$OA21CD[nrow(a)]
    if (length(out) %% 20 == 0) message("  lookup ", 2000 * length(out))
  }
  readr::write_csv(bind_rows(out), lookup_file)
}
lk <- readr::read_csv(lookup_file, show_col_types = FALSE) |> rename(oa21cd = OA21CD, lsoa = LSOA21CD, msoa = MSOA21CD)
message(nrow(lk), " Output Areas in the lookup")

#' One batch of areas: returns area, then each dimension's label, and the count
fetch_batch <- function(pt, level, codes, dims) {
  url <- sprintf("%s/population-types/%s/census-observations?area-type=%s,%s&dimensions=%s", API, pt, level, paste(codes, collapse = ","), dims)
  for (try in 1:5) {
    # fetched explicitly: jsonlite treats a string over ~2,084 characters as
    # JSON text, not a URL, and these URLs are longer than that
    r <- tryCatch({ h <- curl::curl_fetch_memory(url)
                    if (h$status_code == 200) jsonlite::fromJSON(rawToChar(h$content), simplifyVector = FALSE) else NULL },
                  error = function(e) NULL)
    if (!is.null(r$observations)) break
    Sys.sleep(2 * try)
  }
  if (is.null(r$observations)) stop("ONS API failed for a batch at ", level)
  o <- r$observations
  nd <- length(o[[1]]$dimensions)
  cols <- lapply(seq_len(nd), function(j) vapply(o, function(x) x$dimensions[[j]]$option, ""))
  a <- vapply(o, function(x) x$dimensions[[1]]$option_id, "")
  tb <- tibble(area = a, n = vapply(o, function(x) as.numeric(x$observation), 0))
  for (j in 2:nd) tb[[paste0("d", j - 1)]] <- cols[[j]]
  tb
}
fetch_all <- function(file, pt, level, codes, dims, size) {
  f <- file.path(cache, file)
  if (file.exists(f)) return(readRDS(f))
  batches <- split(codes, ceiling(seq_along(codes) / size)); res <- vector("list", length(batches))
  for (i in seq_along(batches)) {
    res[[i]] <- fetch_batch(pt, level, batches[[i]], dims)
    if (i %% 20 == 0) message(sprintf("  %s %d of %d", file, i, length(batches)))
  }
  x <- bind_rows(res); saveRDS(x, f); x
}
is_l4 <- function(q) startsWith(q, "Level 4")
valid_q <- function(q) q != "Does not apply"

# all adults by age, LSOA
la <- fetch_all("lsoa_qual_age.rds", "UR", "lsoa", sort(unique(lk$lsoa)), "highest_qualification_7a,resident_age_6a", 200)
ls <- la |> rename(q = d1, age = d2) |> filter(valid_q(q)) |> group_by(area) |>
  summarise(all_lsoa = 100 * sum(n[is_l4(q) & age != "Aged 15 years and under"]) / sum(n[age != "Aged 15 years and under"]),
            a3549 = 100 * sum(n[is_l4(q) & age == "Aged 35 to 49 years"]) / sum(n[age == "Aged 35 to 49 years"]),
            a2534 = 100 * sum(n[is_l4(q) & age == "Aged 25 to 34 years"]) / sum(n[age == "Aged 25 to 34 years"]), .groups = "drop") |>
  rename(lsoa = area)
# parents, MSOA
mp <- fetch_all("msoa_qual_dep_age.rds", "UR_HH", "msoa", sort(unique(lk$msoa)),
                "highest_qualification_7a,hh_dependent_children_3a,resident_age_6a", 100)
AGES <- c("Aged 25 to 34 years", "Aged 35 to 49 years", "Aged 50 to 64 years")
ms <- mp |> rename(q = d1, dep = d2, age = d3) |> filter(valid_q(q), age != "Aged 15 years and under") |> group_by(area) |>
  summarise(all_msoa = 100 * sum(n[is_l4(q)]) / sum(n),
            parents = 100 * sum(n[is_l4(q) & dep == "Household with dependent children" & age %in% AGES]) /
              sum(n[dep == "Household with dependent children" & age %in% AGES]),
            nonpar = 100 * sum(n[is_l4(q) & dep == "No dependent children in household" & age %in% AGES]) /
              sum(n[dep == "No dependent children in household" & age %in% AGES]), .groups = "drop") |>
  rename(msoa = area)

# ---- 2. neighbourhoods -----------------------------------------------------------------
t11 <- fetch_census_table("ts011"); t07 <- fetch_census_table("ts007a")
oa <- tibble(oa21cd = t11[["geography code"]],
  dep = 100 * (t11[["Household deprivation: Household is deprived in two dimensions; measures: Value"]] +
               t11[["Household deprivation: Household is deprived in three dimensions; measures: Value"]] +
               t11[["Household deprivation: Household is deprived in four dimensions; measures: Value"]]) /
    t11[["Household deprivation: Total: All households; measures: Value"]]) |>
  left_join(tibble(oa21cd = t07[["geography code"]],
                   young = 100 * (t07[["Age: Aged 25 to 29 years"]] + t07[["Age: Aged 30 to 34 years"]]) / t07[["Age: Total"]]), by = "oa21cd") |>
  left_join(lk, by = "oa21cd") |> left_join(ls, by = "lsoa") |> left_join(ms, by = "msoa")
MEAS <- c(all_lsoa = "All adults (LSOA)", a3549 = "Aged 35 to 49 (LSOA)", a2534 = "Aged 25 to 34 (LSOA)",
          all_msoa = "All adults (MSOA)", parents = "Parents (MSOA)", nonpar = "Adults without dependent children (MSOA)")
cent <- readRDS(here::here("data", "cache", "oa_pwc_2021.rds")) |> inner_join(oa, by = "oa21cd")
d <- readRDS(here::here("data", "inputs.rds"))
gias <- readr::read_csv(here::here("data-raw", "gias", "edubasealldata20260610.csv"), col_types = readr::cols(.default = "c"), progress = FALSE) |>
  transmute(URN = as.integer(URN), easting = num(Easting), northing = num(Northing))
sch <- d |> distinct(URN) |> inner_join(gias, by = "URN") |> filter(!is.na(easting), easting > 0)
nn <- nabor::knn(as.matrix(cent[, c("easting", "northing")]), as.matrix(sch[, c("easting", "northing")]), k = K)
VARS <- c(names(MEAS), "dep", "young"); Mx <- as.matrix(cent[, VARS])
for (v in VARS) sch[[v]] <- rowMeans(matrix(Mx[as.vector(nn$nn.idx), v], nrow = nrow(sch)), na.rm = TRUE)

read_wbri <- function(yr) {
  z <- here::here("data-raw", "ees", "spc_all", paste0("spc_", yr, ".zip"))
  f <- grep("supporting-files/spc_school_level_underlying_data", unzip(z, list = TRUE)$Name, value = TRUE)
  x <- readr::read_csv(unz(z, f[1]), col_types = readr::cols(.default = "c"), progress = FALSE, name_repair = "unique_quiet")
  tibble(URN = as.integer(x[[grep("^urn$", names(x), ignore.case = TRUE, value = TRUE)[1]]]), year_label = yr,
         wbri_sch = num(x[[grep("^% of pupils classified as white British", names(x), ignore.case = TRUE, value = TRUE)[1]]]))
}
eth <- bind_rows(lapply(c("2021-22", "2022-23", "2023-24", "2024-25"), read_wbri)) |>
  filter(!is.na(URN), !is.na(wbri_sch)) |> distinct(URN, year_label, .keep_all = TRUE)
zsc <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
d <- d |> left_join(eth, by = c("URN", "year_label")) |> left_join(sch |> select(-easting, -northing), by = "URN")
if ("urn_published" %in% names(d)) d <- d |>
  left_join(eth |> rename(urn_published = URN, wbri_pub = wbri_sch), by = c("urn_published", "year_label")) |>
  mutate(wbri_sch = coalesce(wbri_sch, wbri_pub)) |> select(-wbri_pub)
d <- d |> mutate(z_eth = zsc(wbri_sch), across(all_of(VARS), zsc, .names = "z_{.col}"))
ZV <- paste0("z_", VARS)

# ---- 3. the comparison -----------------------------------------------------------------
# Each spec: the neighbourhood term(s) in place of z_l4, and anything added
SPECS <- list(
  `v2.1: all adults (Output Areas)` = list(n = "z_l4"),
  `All adults (LSOA)` = list(n = "z_all_lsoa"),
  `Aged 35 to 49 (LSOA)` = list(n = "z_a3549"),
  `All adults (MSOA)` = list(n = "z_all_msoa"),
  `Parents (MSOA)` = list(n = "z_parents"),
  `Adults without dependent children (MSOA)` = list(n = "z_nonpar"),
  `Parents and non-parents together` = list(n = c("z_parents", "z_nonpar")),
  `Aged 35 to 49 and 25 to 34 together` = list(n = c("z_a3549", "z_a2534")))
CHECKS <- list(`All adults + ethnicity, deprivation, young adults` = list(n = "z_l4", x = c("z_eth", "z_dep", "z_young")),
               `Parents + ethnicity, deprivation, young adults` = list(n = "z_parents", x = c("z_eth", "z_dep", "z_young")),
               `Parents + ethnicity` = list(n = "z_parents", x = "z_eth"),
               `All adults + ethnicity` = list(n = "z_l4", x = "z_eth"),
               `Parents and non-parents + ethnicity` = list(n = c("z_parents", "z_nonpar"), x = "z_eth"))
f_of <- function(g, s) {
  fx <- fixed_for(g); fx <- c(setdiff(fx, "z_l4"), s$n, s$x)
  as.formula(paste0("log(", GROUPS[[g]], ") ~ ", paste(c(fx, RANDOM_V2), collapse = " + ")))
}
la_eff <- function(m) { r <- ranef(m)$`LANAME:gor_name`; setNames(r[, 1], sub(":.*", "", rownames(r))) }
oos_year <- function(g, s, dd, hold) {
  tr <- dd |> filter(year_label != hold) |> droplevels(); te <- dd |> filter(year_label == hold, LANAME %in% tr$LANAME)
  f <- f_of(g, s); m <- lmer(f, data = tr, REML = TRUE, control = CTRL)
  X <- model.matrix(delete.response(terms(lme4::nobars(f))), te)[, colnames(getME(m, "X")), drop = FALSE]
  la <- la_eff(m); rg <- ranef(m)$gor_name
  p <- as.vector(X %*% fixef(m)) + mean(ranef(m)$year_label[, 1]) + coalesce(la[as.character(te$LANAME)], 0) + coalesce(rg[as.character(te$gor_name), 1], 0)
  sqrt(mean((log(te[[GROUPS[[g]]]]) - p)^2))
}
school_r <- function(m, dd, g) {
  lp <- as.vector(getME(m, "X") %*% fixef(m)) + ranef(m)$year_label[as.character(dd$year_label), 1]
  tibble(URN = dd$URN, r = log(dd[[GROUPS[[g]]]]) - lp) |> group_by(URN) |> summarise(r = mean(r), .groups = "drop")
}
OOS <- c("v2.1: all adults (Output Areas)", "All adults (LSOA)", "Aged 35 to 49 (LSOA)", "All adults (MSOA)", "Parents (MSOA)",
         "Parents and non-parents together")
YEARS <- sort(unique(d$year_label))
kind_of <- d |> filter(year_label == "2024-25", !excluded) |> distinct(URN, .keep_all = TRUE) |>
  mutate(tdep = ntile(dep, 3), tl4 = ntile(z_l4, 3),
         kind = case_when(tdep == 3 & tl4 == 3 ~ "Deprived, many graduates", tdep == 3 & tl4 == 1 ~ "Deprived, few graduates",
                          tdep == 1 & tl4 == 3 ~ "Not deprived, many graduates", tdep == 1 & tl4 == 1 ~ "Not deprived, few graduates")) |>
  select(URN, kind)

run_group <- function(g) {
  dd <- model_rows(d, g, extra = c("z_eth", ZV)); message(sprintf("%s: %d school-years", g, nrow(dd)))
  fit <- function(s, reml = FALSE) lmer(f_of(g, s), data = dd, REML = reml, control = CTRL)
  all <- c(SPECS, CHECKS); fits <- lapply(all, fit)
  a0 <- AIC(fits[[1]]); r0 <- school_r(fits[[1]], dd, g)
  cmp <- bind_rows(lapply(names(fits), function(k) {
    m <- fits[[k]]; ct <- as.data.frame(summary(m)$coefficients); ct$term <- rownames(ct)
    rr <- school_r(m, dd, g) |> inner_join(r0, by = "URN", suffix = c("", "0"))
    vla <- as.data.frame(VarCorr(m)) |> filter(grp == "LANAME:gor_name") |> pull(vcov)
    message("  ", k)
    tibble(group = g, spec = k, rows = nrow(dd), aic_change = AIC(m) - a0,
           rmse_oos = if (k %in% OOS) mean(sapply(YEARS, function(h) oos_year(g, all[[k]], dd, h))) else NA_real_,
           r_school = cor(rr$r, rr$r0), moved = mean(ntile(rr$r, 5) != ntile(rr$r0, 5)), la_var = vla,
           bh = 100 * (exp(la_eff(m)[BH]) - 1),
           coefs = list(ct |> filter(grepl("^z_", term)) |> transmute(term, est = Estimate, se = `Std. Error`)))
  }))
  kv <- bind_rows(lapply(c(`All adults` = "v2.1: all adults (Output Areas)", `Parents` = "Parents (MSOA)",
                           `All adults + ethnicity` = "All adults + ethnicity", `Parents + ethnicity` = "Parents + ethnicity"), function(k) {
    school_r(fits[[k]], dd, g) |> inner_join(kind_of, by = "URN") |> filter(!is.na(kind)) |> group_by(kind) |>
      summarise(schools = n(), va = 100 * (exp(mean(r)) - 1), se = 100 * sd(r) / sqrt(n()), .groups = "drop")
  }), .id = "model") |> mutate(group = g)
  list(cmp = cmp, kv = kv)
}
RES <- lapply(names(GROUPS), run_group)
cmp <- bind_rows(lapply(RES, `[[`, "cmp")); kv <- bind_rows(lapply(RES, `[[`, "kv"))

# Is the parents and non-parents split London? The same model with ethnicity,
# on London and on everywhere else, separately, and at school level with
# the two measures' difference rather than the two collinear levels.
split_test <- bind_rows(lapply(c("all", "disadvantaged", "non_disadvantaged"), function(g) {
  dd <- model_rows(d, g, extra = c("z_eth", ZV)) |> mutate(z_diff = zsc(nonpar - parents))
  one <- function(rows, lab, s) {
    x <- dd |> filter(rows) |> droplevels()
    re <- if (n_distinct(x$gor_name) > 1) RANDOM_V2 else c("(1 | year_label)", "(1 | LANAME)")
    fx <- c(setdiff(fixed_for(g), "z_l4"), s)
    m <- lmer(as.formula(paste0("log(", GROUPS[[g]], ") ~ ", paste(c(fx, re), collapse = " + "))), data = x, REML = FALSE, control = CTRL)
    ct <- as.data.frame(summary(m)$coefficients); ct$term <- rownames(ct)
    ct |> filter(term %in% s) |> transmute(group = g, sample = lab, rows = nrow(x), term, est = Estimate, se = `Std. Error`)
  }
  ldn <- as.character(dd$gor_name) == "London"
  bind_rows(one(rep(TRUE, nrow(dd)), "England", c("z_parents", "z_nonpar", "z_eth")),
            one(!ldn, "Outside London", c("z_parents", "z_nonpar", "z_eth")),
            one(ldn, "London", c("z_parents", "z_nonpar", "z_eth")),
            one(rep(TRUE, nrow(dd)), "England, level and gap", c("z_all_msoa", "z_diff", "z_eth")),
            one(!ldn, "Outside London, level and gap", c("z_all_msoa", "z_diff", "z_eth")))
})) |> mutate(pct = 100 * (exp(est) - 1), t = est / se)

# how the measures differ, by kind of neighbourhood
sl <- d |> distinct(URN, .keep_all = TRUE) |> inner_join(kind_of, by = "URN") |> filter(!is.na(kind))
gap <- sl |> group_by(kind) |> summarise(schools = n(), l4 = mean(l4, na.rm = TRUE), all_msoa = mean(all_msoa, na.rm = TRUE), parents = mean(parents, na.rm = TRUE),
                                         nonpar = mean(nonpar, na.rm = TRUE), a3549 = mean(a3549, na.rm = TRUE), a2534 = mean(a2534, na.rm = TRUE), .groups = "drop")
msr <- d |> distinct(URN, .keep_all = TRUE)
cors <- cor(as.matrix(msr |> select(z_l4, all_of(paste0("z_", names(MEAS))))), use = "pairwise.complete.obs")
saveRDS(list(built = Sys.Date(), k = K, meas = MEAS, specs = names(SPECS), checks = names(CHECKS), oos = OOS, cmp = cmp, kv = kv, gap = gap, cors = cors, split_test = split_test),
        here::here("data", "parents_not_adults.rds"))
message("Saved data/parents_not_adults.rds")
print(as.data.frame(cmp |> transmute(group, spec = substr(spec, 1, 44), aic = round(aic_change), rmse = round(rmse_oos, 4), r = round(r_school, 3), moved = round(moved, 2), bh = round(bh, 2))))
for (i in which(cmp$spec %in% c(names(CHECKS), "Parents and non-parents together", "Aged 35 to 49 and 25 to 34 together"))) {
  cat("\n", cmp$group[i], "|", cmp$spec[i], "\n"); print(as.data.frame(cmp$coefs[[i]] |> mutate(pct = round(100 * (exp(est) - 1), 2), t = round(est / se, 1)) |> select(term, pct, t))) }
print(as.data.frame(gap |> mutate(across(where(is.double), ~ round(.x, 1)))))
print(as.data.frame(kv |> filter(group %in% c("disadvantaged", "non_disadvantaged")) |> mutate(s = sprintf("%+.1f", va)) |> select(group, kind, model, s) |> tidyr::pivot_wider(names_from = model, values_from = s)))
print(round(cors, 2))
print(as.data.frame(split_test |> transmute(group, sample, term, pct = round(pct, 2), t = round(t, 1), rows)))
