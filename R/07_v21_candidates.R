# 07_v21_candidates.R - Value Added v2.1: does the kind of absence belong?
# ---------------------------------------------------------------------------
# v2.0 counts absence by its amount only. Brighton and Hove's authority
# effect (bh_effect.qmd) suggested its kind matters: illness absence goes
# with a smaller fall in attainment than other absence. This tests two
# candidate terms, for every pupil group, on identical rows:
#
#   ill_share  illness as a percentage of all absence (EES, by reason)
#   age16      the school stops at 16 (no sixth form)
#
# against v2.0, alone and together. For each it records:
#   - fit: AIC (maximum likelihood), and the new terms' coefficients;
#   - out of sample: fitted on 2021/22 to 2023/24, the error predicting
#     2024/25 (with its year effect set to the mean of the others);
#   - what moves: how closely schools' value added and authorities'
#     effects agree with v2.0, and Brighton and Hove's effect.
#
# And, for v2.0 and the fullest candidate, what matters most in each model:
#   - standardised coefficients: the change in Attainment 8 for a one
#     standard deviation change in each term, with its 95% interval
#     (strength and direction);
#   - a Shapley decomposition of the variance the intake terms explain
#     (ordinary least squares with year effects always in): each term's
#     average gain in R-squared over every order it could enter, so
#     variation two terms share is credited fairly between them.
#
# Output: data/v21_candidates.rds.   Rscript R/07_v21_candidates.R (~10 min)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages(library(lme4))

d0 <- readRDS(here::here("data", "inputs.rds"))
CANDIDATES <- list(v2.0 = character(0), ill_share = "ill_share", age16 = "age16",
                   both = c("ill_share", "age16"))
LABELS <- c(v2.0 = "Value Added v2.0", ill_share = "+ illness share of absence",
            age16 = "+ 11-16 school", both = "+ both")
BH <- "Brighton and Hove"

rows_for <- function(g) {
  model_rows(d0, g) |> filter(!is.na(ill_share), !is.na(age16)) |>
    mutate(age16 = as.numeric(age16))
}
f_for <- function(g, extra, re = RANDOM_V2) {
  as.formula(paste0("log(", GROUPS[[g]], ") ~ ", paste(c(fixed_for(g), extra, re), collapse = " + ")))
}

#' Value added against England per school (mean over its years, unshrunk),
#' as R/02_fit_va.R computes it before shrinkage
school_va <- function(m, d, g) {
  X <- getME(m, "X")
  xb <- as.vector(X %*% fixef(m))
  uy <- ranef(m)$year_label[as.character(d$year_label), 1]
  pred <- exp(xb + uy) * exp(sigma(m)^2 / 2)
  tibble(URN = d$URN, va = d[[GROUPS[[g]]]] - pred) |> group_by(URN) |> summarise(va = mean(va), .groups = "drop")
}
la_effects <- function(m) {
  r <- ranef(m)$`LANAME:gor_name`
  setNames(r[, 1], sub(":.*", "", rownames(r)))
}

#' Fit on three years, predict the fourth: root mean squared error of log Attainment 8
oos <- function(g, extra, d) {
  tr <- d |> filter(year_label != "2024-25") |> droplevels()
  te <- d |> filter(year_label == "2024-25")
  m <- lmer(f_for(g, extra), data = tr, REML = TRUE, control = CTRL)
  te <- te |> filter(LANAME %in% tr$LANAME)
  X <- model.matrix(delete.response(terms(lme4::nobars(f_for(g, extra)))), te)
  X <- X[, colnames(getME(m, "X")), drop = FALSE]
  la <- la_effects(m); rg <- ranef(m)$gor_name
  p <- as.vector(X %*% fixef(m)) + mean(ranef(m)$year_label[, 1]) +
    coalesce(la[as.character(te$LANAME)], 0) + coalesce(rg[as.character(te$gor_name), 1], 0)
  sqrt(mean((log(te[[GROUPS[[g]]]]) - p)^2))
}

compare_group <- function(g) {
  d <- rows_for(g)
  message(sprintf("%s: %d school-years", g, nrow(d)))
  fits <- lapply(CANDIDATES, function(x) lmer(f_for(g, x), data = d, REML = TRUE, control = CTRL))
  base_va <- school_va(fits$v2.0, d, g); base_la <- la_effects(fits$v2.0)
  bind_rows(lapply(names(CANDIDATES), function(k) {
    m <- fits[[k]]; x <- CANDIDATES[[k]]
    cf <- summary(m)$coefficients
    va <- school_va(m, d, g) |> inner_join(base_va, by = "URN", suffix = c("", "_v20"))
    la <- la_effects(m)
    tibble(group = g, spec = k, label = LABELS[[k]], rows = nrow(d),
           aic = AIC(refitML(m)),
           b_ill = if ("ill_share" %in% x) cf["ill_share", 1] else NA_real_,
           t_ill = if ("ill_share" %in% x) cf["ill_share", 3] else NA_real_,
           b_age16 = if ("age16" %in% x) cf["age16", 1] else NA_real_,
           t_age16 = if ("age16" %in% x) cf["age16", 3] else NA_real_,
           b_abs = cf["log(PERCTOT)", 1],
           rmse_oos = oos(g, x, d),
           r_school = cor(va$va, va$va_v20),
           moved_quintile = mean(ntile(va$va, 5) != ntile(va$va_v20, 5)),
           r_la = cor(la, base_la[names(la)]),
           bh_effect = unname(la[BH]), bh_rank = unname(rank(-la)[BH]), n_la = length(la))
  }))
}

# ---- what matters most ----------------------------------------------------
TERM_NAMES <- c(TERM_LABELS, ill_share = "Illness share of absence (%)", age16 = "11-16 school")

standardised <- function(g, extra) {
  d <- rows_for(g)
  m <- lmer(f_for(g, extra), data = d, REML = TRUE, control = CTRL)
  X <- getME(m, "X")
  cf <- summary(m)$coefficients
  keep <- setdiff(colnames(X), "(Intercept)")
  sdx <- apply(X[, keep, drop = FALSE], 2, sd)
  tibble(group = g, spec = if (length(extra)) "v2.1" else "v2.0", term = keep,
         label = coalesce(unname(TERM_NAMES[keep]), keep),
         beta = cf[keep, 1], se = cf[keep, 2], sd = sdx) |>
    mutate(std = 100 * (exp(beta * sd) - 1), lo = 100 * (exp((beta - 1.96 * se) * sd) - 1),
           hi = 100 * (exp((beta + 1.96 * se) * sd) - 1))
}

#' Shapley values of R-squared over the intake terms: OLS on the same rows,
#' year effects in every model, so a term is credited only with what it
#' explains beyond the year. A factor (admissions) is one term.
shapley <- function(g, extra) {
  d <- rows_for(g)
  terms <- c(fixed_for(g), extra)
  y <- log(d[[GROUPS[[g]]]])
  r2 <- function(s) {
    f <- as.formula(paste("y ~", paste(c("year_label", s), collapse = " + ")))
    summary(lm(f, data = cbind(d, y = y)))$r.squared
  }
  p <- length(terms)
  subsets <- expand.grid(rep(list(c(FALSE, TRUE)), p))
  val <- apply(subsets, 1, function(z) r2(terms[as.logical(z)]))
  key <- apply(subsets, 1, function(z) paste(as.integer(z), collapse = ""))
  names(val) <- key
  phi <- sapply(seq_len(p), function(j) {
    with_j <- subsets[[j]]
    s <- 0
    for (i in which(!with_j)) {
      z <- as.integer(subsets[i, ]); k <- sum(z)
      z2 <- z; z2[j] <- 1L
      w <- factorial(k) * factorial(p - k - 1) / factorial(p)
      s <- s + w * (val[paste(z2, collapse = "")] - val[paste(z, collapse = "")])
    }
    s
  })
  tibble(group = g, spec = if (length(extra)) "v2.1" else "v2.0", term = terms,
         label = coalesce(unname(TERM_NAMES[terms]), terms), shapley = unname(phi),
         r2_full = unname(val[paste(rep(1L, p), collapse = "")]), r2_year = unname(val[paste(rep(0L, p), collapse = "")]))
}

message("Comparing the candidates ...")
comparison <- bind_rows(lapply(names(GROUPS), compare_group))
message("Importance ...")
importance <- bind_rows(lapply(names(GROUPS), function(g) bind_rows(
  standardised(g, character(0)), standardised(g, CANDIDATES$both))))
message("Shapley (all pupils and disadvantaged) ...")
shap <- bind_rows(lapply(c("all", "disadvantaged"), function(g) bind_rows(
  shapley(g, character(0)), shapley(g, CANDIDATES$both))))

# Is illness share something a school might steer? How it moves within a
# school from year to year, against how it differs between schools
d <- rows_for("all")
within <- d |> group_by(URN) |> filter(n() > 1) |>
  summarise(sd_within = sd(ill_share), .groups = "drop")
between <- d |> group_by(URN) |> summarise(m = mean(ill_share), .groups = "drop")
ill_stats <- list(mean = mean(d$ill_share), sd = sd(d$ill_share),
                  sd_within = mean(within$sd_within), sd_between = sd(between$m),
                  icc = var(between$m) / var(d$ill_share),
                  cor_abs = cor(d$ill_share, d$PERCTOT), cor_fsm = cor(d$ill_share, d$PTFSM6CLA1A))

saveRDS(list(comparison = comparison, importance = importance, shapley = shap, ill_stats = ill_stats,
             built_at = Sys.time()), here::here("data", "v21_candidates.rds"))
message("Wrote data/v21_candidates.rds")
print(comparison |> mutate(across(where(is.double), ~signif(.x, 4))), n = 40, width = 200)
print(shap |> group_by(group, spec) |> arrange(desc(shapley), .by_group = TRUE) |>
        mutate(share = round(100 * shapley / sum(shapley), 1)) |> select(group, spec, label, share), n = 60)
str(ill_stats)
