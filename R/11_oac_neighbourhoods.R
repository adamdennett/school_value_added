# 11_oac_neighbourhoods.R - Is there more to neighbourhood than education?
# ---------------------------------------------------------------------------
# Value Added v2.1's neighbourhood term is neighbourhood education: the
# share of adults with level 4+ qualifications in the 150 Output Areas
# nearest each school. This tests whether the 2021 Output Area
# Classification (OAC; Supergroup 8, Group 21, Subgroup 52 classes) says
# anything more, using the same 150 OAs.
#
#   1. Each school's OAC profile: the share of its 150 nearest OAs in each
#      class, at each tier, and how mixed the neighbourhood is (entropy).
#   2. Does the profile explain value added? v2.1 value added (each
#      school's average log residual, against England and within its
#      authority) on the shares, out of sample (10-fold cross-validation,
#      repeated), against region alone.
#   3. Does it explain results better than neighbourhood education? Log
#      Attainment 8, 2024/25, on the other v2.1 terms with neither, either
#      or both, out of sample.
#   4. Which neighbourhood types carry it: shares with no intercept, so each
#      coefficient is the value added of a school whose 150 OAs were all of
#      that class (an extrapolation, read with its interval).
#
# Output: data/oac_neighbourhoods.rds (committed).
# Rscript R/11_oac_neighbourhoods.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
set.seed(2021)
K <- 150
num <- function(x) suppressWarnings(as.numeric(x))

# ---- 1. OAC profiles of the 150 nearest OAs ----------------------------------
oac <- readr::read_csv(here::here("data-raw", "oac2021", "uk_oac_final.csv"), show_col_types = FALSE) |>
  transmute(oa21cd = GeographyCode, sg = as.character(Supergroup), g = Group, sub = Subgroup)
names_oac <- readr::read_csv(here::here("data-raw", "oac2021", "classification_codes_and_names_ukoac_lsoac_msoac.csv"),
                             show_col_types = FALSE) |>
  transmute(level = Level, code = as.character(`Classification Code`), name = `Classification Name`) |> distinct()
lev_name <- function(code, lev) names_oac$name[match(paste(lev, code), paste(names_oac$level, names_oac$code))]
cent <- readRDS(here::here("data", "cache", "oa_pwc_2021.rds")) |> inner_join(oac, by = "oa21cd")

V <- readRDS(here::here("data", "va_v2.rds"))
I <- readRDS(here::here("data", "inputs.rds"))
gias <- readr::read_csv(here::here("data-raw", "gias", "edubasealldata20260610.csv"),
                        col_types = readr::cols(.default = "c"), progress = FALSE) |>
  transmute(URN = as.integer(URN), easting = num(Easting), northing = num(Northing))
sch <- V$schools |> select(URN, SCHNAME, LANAME, gor_name) |>
  inner_join(gias, by = "URN") |> filter(!is.na(easting), easting > 0)
nn <- nabor::knn(data = as.matrix(cent[, c("easting", "northing")]),
                 query = as.matrix(sch[, c("easting", "northing")]), k = K)
shares <- function(col) {
  lev <- sort(unique(cent[[col]]))
  m <- matrix(0, nrow(sch), length(lev), dimnames = list(NULL, lev))
  idx <- matrix(match(cent[[col]][as.vector(nn$nn.idx)], lev), nrow = nrow(sch))
  for (j in seq_along(lev)) m[, j] <- rowMeans(idx == j)
  m
}
TIERS <- c(Supergroup = "sg", Group = "g", Subgroup = "sub")
S <- lapply(TIERS, shares)
entropy <- function(m) { p <- m; p[p == 0] <- NA; -rowSums(p * log(p), na.rm = TRUE) / log(ncol(m)) }
prof <- sch |> select(URN) |>
  mutate(ent_sg = entropy(S$Supergroup), ent_g = entropy(S$Group), ent_sub = entropy(S$Subgroup),
         top_sub = colnames(S$Subgroup)[max.col(S$Subgroup, "first")], top_sub_share = apply(S$Subgroup, 1, max),
         radius_km = nn$nn.dists[, K] / 1000)

# ---- 2. the outcomes ---------------------------------------------------------
va <- V$va |> filter(group %in% c("all", "disadvantaged"), years >= 2) |>
  select(URN, group, rbar_nat, rbar_la) |>
  tidyr::pivot_wider(names_from = group, values_from = c(rbar_nat, rbar_la))
lat <- I |> filter(!excluded, year_label == "2024-25") |> distinct(URN, .keep_all = TRUE)
D <- sch |> mutate(row = row_number()) |> inner_join(va, by = "URN") |>
  left_join(lat |> select(URN, ATT8SCR, PTFSM6CLA1A, PERCTOT, PNUMEAL, ks2_c, ADMPOL_PT, z_l4, sen_support, sen_ehcp,
                          mobile, girls_school, age16), by = "URN") |>
  left_join(prof, by = "URN")
message(nrow(D), " schools with value added and an OAC profile")
X_of <- function(tier, rows) S[[tier]][rows, , drop = FALSE]

# ---- 3. cross-validated fit --------------------------------------------------
cv_r2 <- function(y, X, folds = 10, reps = 5) {
  ok <- complete.cases(y, X); y <- y[ok]; X <- X[ok, , drop = FALSE]
  r <- replicate(reps, {
    f <- sample(rep(seq_len(folds), length.out = length(y))); pred <- numeric(length(y))
    for (k in seq_len(folds)) {
      tr <- f != k
      m <- lm.fit(cbind(1, X[tr, , drop = FALSE]), y[tr])
      b <- m$coefficients; b[is.na(b)] <- 0
      pred[!tr] <- cbind(1, X[!tr, , drop = FALSE]) %*% b
    }
    1 - sum((y - pred)^2) / sum((y - mean(y))^2)
  })
  mean(r)
}
reg_mm <- model.matrix(~ gor_name, D)[, -1]
drop_ref <- function(m) m[, -which.max(colMeans(m)), drop = FALSE]   # shares sum to one: drop the largest
va_fit <- bind_rows(lapply(c(`All pupils` = "rbar_nat_all", `Disadvantaged` = "rbar_nat_disadvantaged",
                             `All pupils, within authority` = "rbar_la_all", `Disadvantaged, within authority` = "rbar_la_disadvantaged"),
  function(y) {
    yy <- D[[y]]
    out <- tibble(model = c("Region", paste(names(TIERS)), paste("Region +", names(TIERS)),
                            "Neighbourhood education", "Region + neighbourhood education"),
                  cv_r2 = c(cv_r2(yy, reg_mm),
                            sapply(names(TIERS), function(t) cv_r2(yy, drop_ref(X_of(t, D$row)))),
                            sapply(names(TIERS), function(t) cv_r2(yy, cbind(reg_mm, drop_ref(X_of(t, D$row))))),
                            cv_r2(yy, cbind(D$z_l4)), cv_r2(yy, cbind(reg_mm, D$z_l4))))
    out
  }), .id = "outcome")

# OAC's signal is between places. How much of the local authority effects
# does it account for? Against LA fixed effects at school level, and at LA
# level: each LA's v2.1 random effect on its schools' average OAC profile.
la_mm <- model.matrix(~ LANAME, D)[, -1]
la_fit <- bind_rows(lapply(c(`All pupils` = "rbar_nat_all", `Disadvantaged` = "rbar_nat_disadvantaged"), function(y) {
  yy <- D[[y]]
  tibble(model = c("Local authority", "OAC group", "Local authority + OAC group"),
         cv_r2 = c(cv_r2(yy, la_mm), cv_r2(yy, drop_ref(X_of("Group", D$row))),
                   cv_r2(yy, cbind(la_mm, drop_ref(X_of("Group", D$row))))))
}), .id = "outcome")
la_prof <- lapply(names(TIERS), function(t) {
  m <- X_of(t, D$row); agg <- rowsum(m, D$LANAME) / as.vector(table(D$LANAME)[rownames(rowsum(m, D$LANAME))]); agg })
names(la_prof) <- names(TIERS)
la_eff <- V$la |> filter(group %in% c("all", "disadvantaged")) |> select(LANAME, group, effect)
la_level <- bind_rows(lapply(c("all", "disadvantaged"), function(gp) {
  e <- la_eff |> filter(group == gp)
  bind_rows(lapply(c("Supergroup", "Group"), function(t) {
    X <- la_prof[[t]][match(e$LANAME, rownames(la_prof[[t]])), , drop = FALSE]; ok <- complete.cases(X)
    tibble(group = gp, tier = t, las = sum(ok), cv_r2 = cv_r2(e$effect[ok], drop_ref(X[ok, , drop = FALSE]), folds = 10, reps = 20))
  }))
}))

# With 151 authorities, 7 to 21 shares at once is too many to predict out
# of sample. One dimension at a time: the correlation of each authority's
# effect with the share of its schools' neighbourhoods in each supergroup
# (and a few groups), weighted by the number of schools.
la_avg <- function(m) { a <- rowsum(m, D$LANAME); a / as.vector(table(D$LANAME)[rownames(a)]) }
la_dims <- cbind(la_avg(X_of("Supergroup", D$row)),
                 la_avg(X_of("Group", D$row))[, c("2a", "2b", "4c", "6a", "6b", "7a", "1a"), drop = FALSE])
la_n <- as.vector(table(D$LANAME)[rownames(la_dims)])
la_cor <- bind_rows(lapply(c("all", "disadvantaged"), function(gp) {
  e <- la_eff |> filter(group == gp); i <- match(e$LANAME, rownames(la_dims)); ok <- !is.na(i)
  bind_rows(lapply(colnames(la_dims), function(cc) {
    x <- la_dims[i[ok], cc]; y <- e$effect[ok]; w <- la_n[i[ok]]
    tibble(group = gp, code = cc, tier = ifelse(nchar(cc) == 1, "Supergroup", "Group"),
           r = cor(x, y), r_w = cov.wt(cbind(x, y), wt = w, cor = TRUE)$cor[1, 2], las = sum(ok))
  }))
})) |> mutate(name = lev_name(code, tier))

# ---- 4. results: OAC against neighbourhood education ---------------------------
Da <- D |> filter(!is.na(ATT8SCR), PTFSM6CLA1A > 0, PERCTOT > 0, PNUMEAL > 0,
                  if_all(c(ks2_c, ADMPOL_PT, z_l4, sen_support, sen_ehcp, mobile, girls_school, age16), ~ !is.na(.x)))
base <- model.matrix(~ log(PTFSM6CLA1A) + log(PERCTOT) + log(PNUMEAL) + ks2_c + ADMPOL_PT + sen_support + sen_ehcp +
                       mobile + girls_school + age16 + gor_name, Da)[, -1]
ya <- log(Da$ATT8SCR)
att_fit <- bind_rows(
  tibble(model = "Other v2.1 terms and region, no neighbourhood", cv_r2 = cv_r2(ya, base)),
  tibble(model = "+ neighbourhood education", cv_r2 = cv_r2(ya, cbind(base, Da$z_l4))),
  bind_rows(lapply(names(TIERS), function(t) tibble(
    model = c(paste("+ OAC", tolower(t)), paste("+ neighbourhood education + OAC", tolower(t))),
    cv_r2 = c(cv_r2(ya, cbind(base, drop_ref(X_of(t, Da$row)))), cv_r2(ya, cbind(base, Da$z_l4, drop_ref(X_of(t, Da$row)))))))))
# and how much of the neighbourhood education term OAC reproduces
l4_by_oac <- sapply(names(TIERS), function(t) cv_r2(D$z_l4, drop_ref(X_of(t, D$row))))
mix_test <- summary(lm(rbar_nat_all ~ ent_sub + gor_name, D))$coefficients["ent_sub", ]

# ---- 5. which neighbourhood types ----------------------------------------------
pure <- function(y, tier, Dx = D) {
  X <- X_of(tier, Dx$row); ok <- complete.cases(Dx[[y]])
  m <- lm(Dx[[y]][ok] ~ 0 + X[ok, ])
  ct <- summary(m)$coefficients
  tibble(code = colnames(X), est = ct[, 1], se = ct[, 2],
         mean_share = colMeans(X[ok, ]), schools_20 = colSums(X[ok, ] >= 0.2))
}
types <- bind_rows(lapply(names(TIERS), function(t) bind_rows(
  pure("rbar_nat_all", t) |> mutate(outcome = "All pupils"),
  pure("rbar_la_all", t) |> mutate(outcome = "All pupils, within authority"),
  pure("rbar_nat_disadvantaged", t) |> mutate(outcome = "Disadvantaged")) |>
  mutate(tier = t, name = lev_name(code, t)))) |>
  mutate(pct = 100 * (exp(est) - 1), lo = 100 * (exp(est - 1.96 * se) - 1), hi = 100 * (exp(est + 1.96 * se) - 1))

# the profile of the average school's neighbourhood, for the page
avg_profile <- bind_rows(lapply(names(TIERS), function(t)
  tibble(tier = t, code = colnames(S[[t]]), share = colMeans(S[[t]][D$row, ])) |> mutate(name = lev_name(code, t))))

saveRDS(list(built = Sys.Date(), k = K, schools = nrow(D), va_fit = va_fit, att_fit = att_fit, l4_by_oac = l4_by_oac,
             la_fit = la_fit, la_level = la_level, la_cor = la_cor,
             mix_test = mix_test, types = types, avg_profile = avg_profile, names = names_oac,
             profiles = D |> select(URN, SCHNAME, LANAME, gor_name, rbar_nat_all, rbar_la_all, z_l4, ent_sub, top_sub, top_sub_share, radius_km),
             shares_sub = cbind(URN = D$URN, S$Subgroup[D$row, ])),
        here::here("data", "oac_neighbourhoods.rds"))
message("Saved data/oac_neighbourhoods.rds")
print(la_cor |> filter(group == "disadvantaged") |> arrange(r_w), n = 20)
