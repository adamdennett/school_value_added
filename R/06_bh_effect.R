# 06_bh_effect.R - Why Brighton and Hove's authority effect is so large
# ---------------------------------------------------------------------------
# On the v2.0 model Brighton and Hove has one of the largest local authority
# effects in England. This refits the all-pupils model with one term
# removed or added at a time and records where the authority's effect goes:
#
#   1. Terms out: absence, neighbourhood education, prior attainment.
#   2. Terms in: what kind of absence it is (the illness share, or absence
#      split into illness and the rest), whether the school is 11-16, and
#      local segregation.
#   3. Every group, with and without the absence term.
#   4. The composition of absence, the authority against England, and the
#      arithmetic: what the authority's extra absence costs in expectation.
#
# Absence reasons are DfE's school-level absence data, assembled by the
# original project (data/exclusions_absence.rds there). Every model is fitted
# on school-years with reasons, so the rows match across specifications.
#
# Output: data/bh_effect.rds.  Rscript R/06_bh_effect.R (~5 min)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(lme4))

TARGET <- "Brighton and Hove"
d <- readRDS(here::here("data", "inputs.rds"))
reasons <- readRDS(sat("data", "exclusions_absence.rds"))$joined |>
  transmute(urn_published = as.integer(urn),
            year_label = paste0(substr(time_period, 1, 4), "-", substr(time_period, 5, 6)),
            illness = 100 * sess_auth_illness / poss,
            other = 100 * (overall - sess_auth_illness) / poss,
            unauthorised = 100 * unauthorised / poss,
            auth_other = 100 * (authorised - sess_auth_illness) / poss)

with_reasons <- function(dg) {
  dg |> mutate(year_label = as.character(year_label)) |>
    inner_join(reasons, by = c("urn_published", "year_label")) |>
    filter(illness > 0, other > 0) |>
    mutate(year_label = factor(year_label), ill_share = 100 * illness / (illness + other),
           age16 = as.numeric(as.numeric(AGEHIGH) <= 16))
}

fit_la <- function(dg, y, terms) {
  m <- lmer(as.formula(paste0("log(", y, ") ~ ", paste(c(terms, RANDOM_V2), collapse = " + "))),
            data = dg, REML = TRUE, control = CTRL)
  r <- ranef(m, condVar = TRUE)$`LANAME:gor_name`
  e <- setNames(r[, 1], sub(":.*", "", rownames(r)))
  se <- sqrt(attr(r, "postVar")[1, 1, ])[match(TARGET, names(e))]
  list(m = m, effect = unname(e[TARGET]), se = se, rank = unname(rank(-e)[TARGET]), n_la = length(e), all = e)
}

# ---- 1-2. All pupils: terms out and terms in --------------------------------
dg <- with_reasons(model_rows(d, "all"))
base <- fixed_for("all")
no_abs <- setdiff(base, "log(PERCTOT)")
specs <- list(
  v2 = list(label = "Value Added v2.0", terms = base),
  no_absence = list(label = "Without absence", terms = no_abs),
  no_l4 = list(label = "Without neighbourhood education", terms = setdiff(base, "z_l4")),
  no_ks2 = list(label = "Without prior attainment (KS2)", terms = setdiff(base, "ks2_c")),
  ill_share = list(label = "Plus the illness share of absence", terms = c(base, "ill_share")),
  split = list(label = "Absence split: illness and the rest", terms = c(no_abs, "log(illness)", "log(other)")),
  age16 = list(label = "Plus an 11-16 school flag", terms = c(base, "age16")),
  segregation = list(label = "Plus local segregation (Gorard index)", terms = c(base, "gorard_segregation")),
  both = list(label = "Plus the illness share and the 11-16 flag", terms = c(base, "ill_share", "age16")))
fits <- lapply(specs, function(s) fit_la(dg, "ATT8SCR", s$terms))
tests <- tibble(spec = names(specs), label = vapply(specs, `[[`, "", "label"),
                effect = vapply(fits, `[[`, 0, "effect"), se = vapply(fits, `[[`, 0, "se"),
                rank = vapply(fits, `[[`, 0, "rank"), n_la = vapply(fits, `[[`, 0, "n_la"),
                aic = vapply(fits, function(f) AIC(refitML(f$m)), 0),
                r_v2 = vapply(fits, function(f) cor(f$all, fits$v2$all[names(f$all)]), 0))
coef_of <- function(spec, term) unname(fixef(fits[[spec]]$m)[term])
coefs <- c(absence = coef_of("v2", "log(PERCTOT)"), ill_share = coef_of("ill_share", "ill_share"),
           illness = coef_of("split", "log(illness)"), other = coef_of("split", "log(other)"),
           age16 = coef_of("age16", "age16"))

# Does absence cost less inside the authority? An interaction on the v2 rows
dg$in_la <- as.numeric(dg$LANAME == TARGET)
dg$labs <- log(dg$PERCTOT) - mean(log(dg$PERCTOT))
mi <- lmer(as.formula(paste("log(ATT8SCR) ~", paste(c(no_abs, "labs", "labs:in_la", RANDOM_V2), collapse = " + "))),
           data = dg, REML = TRUE, control = CTRL)
slope <- summary(mi)$coefficients[c("labs", "labs:in_la"), 1:2]

# ---- 3. Every group, with and without absence --------------------------------
groups <- bind_rows(lapply(names(GROUPS), function(g) {
  dd <- model_rows(d, g)
  a <- fit_la(dd, GROUPS[[g]], fixed_for(g))
  b <- fit_la(dd, GROUPS[[g]], setdiff(fixed_for(g), "log(PERCTOT)"))
  tibble(group = g, effect = a$effect, rank = a$rank, effect_no_abs = b$effect, rank_no_abs = b$rank, n_la = a$n_la)
}))

# ---- 4. What kind of absence, and what it costs ------------------------------
comp <- dg |> mutate(area = ifelse(in_la == 1, TARGET, "England")) |>
  group_by(area) |>
  summarise(total = mean(PERCTOT), illness = mean(illness), unauthorised = mean(unauthorised),
            auth_other = mean(auth_other), ill_share = mean(ill_share), age16 = mean(age16, na.rm = TRUE),
            log_abs = mean(log(PERCTOT)), schools = n_distinct(URN), .groups = "drop")
la_share <- dg |> group_by(LANAME) |> summarise(s = mean(ill_share), a = mean(PERCTOT), .groups = "drop")
ranks <- c(ill_share = rank(-la_share$s)[la_share$LANAME == TARGET], absence = rank(-la_share$a)[la_share$LANAME == TARGET],
           n_la = nrow(la_share))
# Expected attainment the authority's extra absence takes off, on the v2 coefficient
gap_log <- comp$log_abs[comp$area == TARGET] - comp$log_abs[comp$area == "England"]
cost <- coefs[["absence"]] * gap_log

# ---- 5. Neighbourhood education ----------------------------------------------
# v2.0 measures it as the share of adults with a degree-level (level 4+)
# qualification in the 150 census output areas nearest the school. Refit
# with the neighbourhood drawn at every scale the original project built,
# and with the share of adults with no qualifications in its place or
# beside it, to see whether a different measure of the same thing takes
# the authority effect away.
census <- readRDS(sat("data", "census", "school_census_context.rds")) |> mutate(URN = as.integer(URN))
da <- model_rows(d, "all")
da <- da |> left_join(census |> rename(urn_published = URN), by = "urn_published")
zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
neigh_specs <- list(
  k150 = list(label = "Level 4+, 150 nearest output areas (v2.0)", vars = "cens_l4plus_k150"),
  k100 = list(label = "Level 4+, 100 nearest", vars = "cens_l4plus_k100"),
  k50 = list(label = "Level 4+, 50 nearest", vars = "cens_l4plus_k50"),
  k25 = list(label = "Level 4+, 25 nearest", vars = "cens_l4plus_k25"),
  k10 = list(label = "Level 4+, 10 nearest", vars = "cens_l4plus_k10"),
  k5 = list(label = "Level 4+, 5 nearest", vars = "cens_l4plus_k5"),
  idw50 = list(label = "Level 4+, distance-weighted over 50", vars = "cens_l4plus_idw50"),
  noquals = list(label = "No qualifications, 150 nearest, in its place", vars = "cens_noquals_k150"),
  both = list(label = "Level 4+ and no qualifications, 150 nearest", vars = c("cens_l4plus_k150", "cens_noquals_k150")))
for (v in unique(unlist(lapply(neigh_specs, `[[`, "vars")))) da[[paste0("z_", v)]] <- zs(da[[v]])
da <- da[stats::complete.cases(da[, paste0("z_", unique(unlist(lapply(neigh_specs, `[[`, "vars"))))]), ]
no_l4 <- setdiff(base, "z_l4")
nfits <- lapply(neigh_specs, function(s) fit_la(da, "ATT8SCR", c(no_l4, paste0("z_", s$vars))))
nfits$none <- fit_la(da, "ATT8SCR", no_l4)
neigh <- tibble(spec = c(names(neigh_specs), "none"),
                label = c(vapply(neigh_specs, `[[`, "", "label"), "No neighbourhood term"),
                effect = vapply(nfits, `[[`, 0, "effect"), rank = vapply(nfits, `[[`, 0, "rank"),
                n_la = vapply(nfits, `[[`, 0, "n_la"),
                aic = vapply(nfits, function(f) AIC(refitML(f$m)), 0),
                coef = vapply(names(nfits), function(k) if (k == "none") NA_real_ else
                  unname(fixef(nfits[[k]]$m)[paste0("z_", neigh_specs[[k]]$vars[1])]), 0))

# Where the city's schools sit: their neighbourhoods against England's
# schools, and each authority's mean
sch <- da |> group_by(URN, LANAME) |>
  summarise(l4 = mean(cens_l4plus_k150), l4_10 = mean(cens_l4plus_k10), noq = mean(cens_noquals_k150),
            .groups = "drop")
la_l4 <- sch |> group_by(LANAME) |> summarise(l4 = mean(l4), .groups = "drop") |> mutate(rank = rank(-l4))
neigh_place <- list(
  la_l4 = la_l4$l4[la_l4$LANAME == TARGET], la_rank = la_l4$rank[la_l4$LANAME == TARGET], n_la = nrow(la_l4),
  eng_l4 = mean(sch$l4), eng_l4_10 = mean(sch$l4_10), eng_noq = mean(sch$noq),
  la_l4_10 = mean(sch$l4_10[sch$LANAME == TARGET]), la_noq = mean(sch$noq[sch$LANAME == TARGET]),
  sd_l4 = sd(sch$l4),
  schools = sch |> filter(LANAME == TARGET) |> select(URN, l4, l4_10, noq) |>
    mutate(pct_england = vapply(l4, function(x) mean(sch$l4 <= x), 0)))

saveRDS(list(la = TARGET, tests = tests, coefs = coefs, slope = slope, groups = groups, comp = comp,
             neigh = neigh, neigh_place = neigh_place,
             ranks = ranks, cost = cost, rows = nrow(dg), built_at = Sys.time()),
        here::here("data", "bh_effect.rds"))
message("Wrote data/bh_effect.rds")
print(tests |> mutate(across(c(effect, se), ~round(.x, 4)), aic = round(aic), r_v2 = round(r_v2, 3)))
print(groups); print(comp); print(round(coefs, 4)); print(ranks); cat("cost", round(cost, 4), "\n"); print(slope)
print(neigh |> mutate(across(c(effect, coef), ~round(.x, 4)), aic = round(aic - aic[spec == "k150"])))
str(neigh_place[setdiff(names(neigh_place), "schools")])
