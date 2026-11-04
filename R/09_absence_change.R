# 09_absence_change.R - How school absence changes over time, school by school
# ---------------------------------------------------------------------------
# The second part of the absence work (R/08 is the cross-section). Every
# input is published school-level data.
#
#   1. A panel of England's state-funded secondary schools, autumn and spring
#      terms, 2016/17 to 2025/26 (no 2019/20: not collected), from the one
#      EES file that covers every year on the same basis. 2020/21 is kept for
#      the trend but left out of the models: in a year of lockdowns, pupils
#      told to stay at home were not counted as absent.
#   2. Schools followed through changes of URN (academy conversion, a change
#      of trust) with GIAS's one-to-one predecessor and successor links;
#      mergers, amalgamations and splits end a chain.
#   3. Intake by year from the January school census: 2018/19 and 2021/22 to
#      2024/25 from the performance tables' census files, 2025/26 from the
#      January 2026 school census and SEN files.
#   4. Analyses: the trend by reason; how much of absence is fixed to a
#      school and how much moves; whether schools keep their place from year
#      to year; who rose most from before the pandemic to now; whether a
#      change in a school's own intake goes with a change in its absence
#      (school fixed effects against a between-school model); and before and
#      after academy conversion and a change of trust (staggered event
#      studies, Sun and Abraham).
#
# Output: data/absence_change.rds (committed).   Rscript R/09_absence_change.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(lme4); library(fixest) })
num <- function(x) suppressWarnings(as.numeric(x))
pn  <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
ees <- function(...) here::here("data-raw", "ees", ...)
yl  <- function(tp) paste0(substr(tp, 1, 4), "/", substr(tp, 5, 6))
OUTCOMES <- c(overall = "Overall absence", illness = "Illness", appointments = "Medical appointments",
              auth_other = "Other authorised", unauth = "Unauthorised", persistent = "Persistent absence (10%+)",
              severe = "Severe absence (50%+)")
ly <- function(v) log(v + 0.1)        # severe absence can be zero

# ---- 1. the absence panel ---------------------------------------------------
raw <- readr::read_csv(unz(ees("absence_2term_school_2025-26.zip"), "data/Absence_2term_school.csv"),
                       col_types = readr::cols(.default = "c"), progress = FALSE) |>
  filter(geographic_level == "School", education_phase == "State-funded secondary")
ab <- raw |>
  transmute(tp = as.integer(time_period), start = tp %/% 100L, URN = as.integer(school_urn),
            name = school_name, la = la_name, region = region_name,
            acad = coalesce(na_if(academy_type, "z"), "Maintained"),
            open = as.Date(academy_open_date, "%d/%m/%Y"),
            possible = num(sess_possible), enrol = num(enrolments),
            s_overall = num(sess_overall), s_auth = num(sess_authorised), s_unauth = num(sess_unauthorised),
            s_ill = num(sess_auth_illness), s_app = num(sess_auth_appointments), s_unhol = num(sess_unauth_holiday),
            pa10 = num(enrolments_pa_10_exact), pa50 = num(enrolments_pa_50_exact),
            overall = num(sess_overall_percent), illness = num(sess_auth_illness_rate),
            appointments = num(sess_auth_appointments_rate),
            auth_other = num(sess_authorised_percent) - illness - appointments,
            unauth = num(sess_unauthorised_percent),
            persistent = num(enrolments_pa_10_exact_percent), severe = num(enrolments_pa_50_exact_percent)) |>
  filter(!is.na(overall), overall > 0, possible > 0)

# the trend by reason, sessions-weighted, on the two-term basis
trend <- ab |>
  filter(if_all(c(s_overall, s_auth, s_unauth, s_ill, s_app, s_unhol, pa10, pa50), ~ !is.na(.x))) |>
  group_by(tp) |>
  summarise(schools = n(),
            overall = 100 * sum(s_overall) / sum(possible),
            Illness = 100 * sum(s_ill) / sum(possible),
            `Medical appointments` = 100 * sum(s_app) / sum(possible),
            `Other authorised` = 100 * sum(s_auth - s_ill - s_app) / sum(possible),
            `Unauthorised holiday` = 100 * sum(s_unhol) / sum(possible),
            `Other unauthorised` = 100 * sum(s_unauth - s_unhol) / sum(possible),
            persistent = 100 * sum(pa10) / sum(enrol), severe = 100 * sum(pa50) / sum(enrol), .groups = "drop") |>
  mutate(year = yl(tp))

# ---- 2. following schools through changes of URN ---------------------------
lk <- readr::read_csv(here::here("data-raw", "gias", "links_edubasealldata20260610.csv"),
                      col_types = readr::cols(.default = "c"), progress = FALSE)
succ <- lk |> filter(LinkType == "Successor") |>
  transmute(from = as.integer(URN), to = as.integer(LinkURN)) |> distinct() |>
  add_count(from, name = "n_out") |> add_count(to, name = "n_in") |>
  filter(n_out == 1, n_in == 1, from != to)
urns <- unique(ab$URN); cid <- urns
for (i in 1:20) {
  m <- match(cid, succ$from); hit <- !is.na(m)
  if (!any(hit)) break
  cid[hit] <- succ$to[m[hit]]
}
ab$cid <- cid[match(ab$URN, urns)]
n_dup <- ab |> count(cid, tp) |> filter(n > 1) |> nrow()
ab <- ab |> arrange(cid, tp, desc(possible)) |> distinct(cid, tp, .keep_all = TRUE)
message(sprintf("%d school-years; %d URNs in %d schools; %d school-years seen under two URNs (larger kept)",
                nrow(ab), length(urns), n_distinct(ab$cid), n_dup))

# where each school is now (its latest row)
now <- ab |> arrange(cid, desc(tp)) |> distinct(cid, .keep_all = TRUE) |>
  select(cid, URN_now = URN, name_now = name, la_now = la, region_now = region, acad_now = acad, last_tp = tp)

# ---- 3. events: academy conversion and a change of trust -------------------
# Each change of URN within a chain. The academic year it happened in comes
# from the new URN's opening date, bounded by when the change shows in the data.
ev <- ab |> arrange(cid, tp) |> group_by(cid) |>
  mutate(prev_urn = lag(URN), prev_acad = lag(acad), prev_start = lag(start)) |>
  filter(!is.na(prev_urn), URN != prev_urn) |> ungroup() |>
  transmute(cid, URN, prev_urn, from = prev_acad, to = acad, open, seen = start, prev_start,
            kind = case_when(from == "Maintained" & grepl("converter", to) ~ "Converter academy",
                             from == "Maintained" & grepl("sponsor", to) ~ "Sponsored academy",
                             from == "Maintained" & to != "Maintained" ~ "Other academy",
                             from != "Maintained" & to != "Maintained" ~ "Trust change",
                             TRUE ~ "Other"),
            open_ay = ifelse(is.na(open), seen,
                             as.integer(format(open, "%Y")) - (as.integer(format(open, "%m")) < 9)),
            ev_start = pmax(pmin(open_ay, seen), prev_start + 1L))
ev_n <- ev |> count(cid, name = "n_events")
message("Changes of URN by kind:"); print(table(ev$kind))

# Event time counts only the years with data (2016/17, 2017/18, 2018/19,
# 2021/22 ... 2025/26): an event in 2019/20, 2020/21 or 2021/22 first shows
# in 2021/22, so the pandemic gap is one step.
PERIODS <- c(2016L, 2017L, 2018L, 2021L, 2022L, 2023L, 2024L, 2025L)
pidx <- function(start) match(start, PERIODS)
first_period <- function(s) sapply(s, function(x) { k <- which(PERIODS >= x); if (length(k)) k[1] else NA_integer_ })

first_acad <- ab |> arrange(cid, tp) |> distinct(cid, .keep_all = TRUE) |> select(cid, acad0 = acad)
es_panel <- function(kinds, base) {
  one <- ev |> semi_join(ev_n |> filter(n_events == 1), by = "cid") |> filter(kind %in% kinds) |>
    transmute(cid, cohort = first_period(ev_start), kind)
  pool <- first_acad |> filter(base(acad0)) |> left_join(ev_n, by = "cid") |>
    filter(is.na(n_events) | cid %in% one$cid)
  ab |> filter(start != 2020L, cid %in% pool$cid) |>
    left_join(one, by = "cid") |>
    mutate(period = pidx(start), cohort = coalesce(cohort, 10000L), kind = coalesce(kind, "Never")) |>
    filter(cohort > 1L)        # a change in 2016/17 has no year before it
}
EVENT_OUT <- c("overall", "illness", "auth_other", "unauth", "persistent", "severe")
event_study <- function(d, label) {
  fits <- lapply(EVENT_OUT, function(y) {
    dd <- d |> mutate(.y = ly(.data[[y]]))
    m <- feols(.y ~ sunab(cohort, period, ref.p = -1) | cid + period, data = dd, cluster = ~cid)
    ct <- as.data.frame(coeftable(m))      # aggregated over cohorts, by event time
    ct$term <- rownames(ct)
    ev_t <- ct |> filter(grepl("^period::", term)) |>
      transmute(rel = as.integer(sub("^period::", "", term)), est = Estimate, se = `Std. Error`)
    att <- as.data.frame(coeftable(summary(m, agg = "ATT")))
    att <- att[rownames(att) == "ATT", ]
    list(event = bind_rows(ev_t, tibble(rel = -1L, est = 0, se = 0)) |> arrange(rel) |>
           mutate(outcome = y, study = label),
         att = tibble(outcome = y, study = label, est = att$Estimate, se = att$`Std. Error`, n = nobs(m)))
  })
  # how many schools sit at each event time (for which coefficients to show)
  support <- d |> filter(cohort < 10000L) |> mutate(rel = period - cohort) |> count(rel, name = "schools") |>
    mutate(study = label)
  list(event = bind_rows(lapply(fits, `[[`, "event")) |> left_join(support |> select(-study), by = "rel"),
       att = bind_rows(lapply(fits, `[[`, "att")),
       n = d |> distinct(cid, kind) |> count(kind) |> mutate(study = label))
}
conv_d  <- es_panel(c("Converter academy", "Sponsored academy"), function(a) a == "Maintained")
trust_d <- es_panel("Trust change", function(a) a != "Maintained")
ES <- list(conversion = event_study(conv_d, "Becoming an academy"),
           conv_converter = event_study(conv_d |> filter(kind != "Sponsored academy"), "Converter academies only"),
           trust = event_study(trust_d, "A change of trust"))

# ---- 4. intake by year -------------------------------------------------------
read_cscp <- function(ay) {
  f <- here::here("data", "extracted", ay, "england_census.csv")
  if (!file.exists(f)) {     # 2018/19 is not one of the model's years, so fetch it here
    source(here::here("pipeline", "01_download_cscp.R"))
    cscp_download(ay, c("Census", "gias"))
  }
  d <- readr::read_csv(here::here("data", "extracted", ay, "england_census.csv"),
                       col_types = readr::cols(.default = "c"), progress = FALSE)
  nor <- pn(d$NOR)
  tibble(URN = as.integer(d$URN), tp = as.integer(paste0(substr(ay, 1, 4), substr(ay, 8, 9))), nor = nor,
         fsm = 100 * pn(d$NUMFSM) / nor, sen_sup = 100 * pn(d$TSENELK) / nor, ehcp = 100 * pn(d$TSENELSE) / nor,
         eal = 100 * pn(d$NUMEAL) / (pn(d$NUMEAL) + pn(d$NUMENGFL) + pn(d$NUMUNCFL)),
         girls = 100 * pn(d$NORG) / nor)
}
cen <- bind_rows(lapply(c("2018-2019", "2021-2022", "2022-2023", "2023-2024", "2024-2025"), read_cscp))
sp <- readr::read_csv(ees("spc_school_level_underlying_data_2026.csv"), col_types = readr::cols(.default = "c"),
                      name_repair = "unique_quiet", progress = FALSE) |>
  filter(phase_type_grouping == "State-funded secondary")
sen <- readr::read_csv(unz(ees("sen_school_level_2025-26.zip"), "supporting-files/School level underlying data 2026.csv"),
                       col_types = readr::cols(.default = "c"), progress = FALSE) |>
  filter(phase_type_grouping == "State-funded secondary")
c26 <- tibble(URN = as.integer(sp$urn), tp = 202526L, nor = num(sp$`headcount of pupils`),
              fsm = 100 * num(sp$`number of pupils known to be eligible for free school meals`) / num(sp$`headcount of pupils`),
              eal = num(sp$`% of pupils whose first language is known or believed to be other than English`),
              girls = 100 * num(sp$`headcount total female`) / num(sp$`headcount of pupils`)) |>
  left_join(tibble(URN = as.integer(sen$URN),
                   sen_sup = 100 * num(sen$SEN_support) / num(sen$total_pupils),
                   ehcp = 100 * num(sen$EHC_plan) / num(sen$total_pupils)), by = "URN")
cen <- bind_rows(cen, c26) |> filter(nor > 0) |> distinct(URN, tp, .keep_all = TRUE)
X <- c(fsm = "Free school meals (%)", sen_sup = "SEN support (%)", ehcp = "EHC plans (%)",
       eal = "English as an additional language (%)", girls = "Girls (%)", lsize = "Size (log pupils)")
P <- ab |> filter(start != 2020L) |> inner_join(cen, by = c("URN", "tp")) |>
  mutate(lsize = log(nor)) |>
  filter(if_all(all_of(names(X)), ~ is.finite(.x)))
intake_trend <- P |> group_by(tp) |>
  summarise(schools = n(), across(c(fsm, sen_sup, ehcp, eal, girls, nor), ~ mean(.x)), .groups = "drop") |>
  mutate(year = yl(tp))

# Did the pupils change too? The same event studies on size and FSM, in the
# years with a census (2018/19 and 2021/22 on).
comp_study <- function(d, label) {
  dd <- d |> inner_join(cen, by = c("URN", "tp")) |> mutate(lsize = log(nor)) |> filter(is.finite(lsize), !is.na(fsm))
  M <- c(lsize = "Pupils on roll (log)", fsm = "Free school meals (points)", sen_sup = "SEN support (points)")
  bind_rows(lapply(names(M), function(y) {
    lab <- M[[y]]
    m <- feols(as.formula(paste(y, "~ sunab(cohort, period, ref.p = -1) | cid + period")), data = dd, cluster = ~cid)
    att <- as.data.frame(coeftable(summary(m, agg = "ATT"))); att <- att[rownames(att) == "ATT", ]
    tibble(study = label, measure = lab, est = att$Estimate, se = att$`Std. Error`)
  }))
}
COMP <- bind_rows(comp_study(conv_d, "Becoming an academy"), comp_study(trust_d, "A change of trust"))

# ---- 5. what is fixed to a school and what moves ---------------------------
V <- ab |> filter(start != 2020L) |> mutate(year = factor(tp), region = factor(region), la = factor(la), cid = factor(cid))
var_dec <- bind_rows(lapply(names(OUTCOMES), function(y) {
  m <- lmer(.y ~ year + (1 | region) + (1 | la) + (1 | cid), data = V |> mutate(.y = ly(.data[[y]])),
            REML = TRUE, control = CTRL)
  v <- as.data.frame(VarCorr(m)); s <- setNames(v$vcov, v$grp)
  tibble(outcome = y, level = c("Region", "Local authority", "School (lasting)", "Year to year"),
         var = c(s[["region"]], s[["la"]], s[["cid"]], s[["Residual"]])) |>
    mutate(share = var / sum(var))
}))
# how much the national year-to-year swing explains, beside the school terms
year_share <- sapply(names(OUTCOMES), function(y) {
  v <- ly(V[[y]]); 1 - sum((v - ave(v, V$year))^2) / sum((v - mean(v))^2) })

# ---- 6. do schools keep their place? ----------------------------------------
cors <- bind_rows(lapply(names(OUTCOMES), function(y) {
  w <- ab |> mutate(z = ly(.data[[y]])) |> group_by(tp) |> mutate(z = z - mean(z)) |> ungroup() |>
    select(cid, tp, z) |> tidyr::pivot_wider(names_from = tp, values_from = z)
  cm <- cor(as.matrix(w[, -1]), use = "pairwise.complete.obs")
  as.data.frame(as.table(cm)) |> setNames(c("a", "b", "r")) |>
    mutate(outcome = y, a = as.integer(as.character(a)), b = as.integer(as.character(b)))
}))

# ---- 7. before the pandemic and now ------------------------------------------
PRE <- 2016:2018; POST <- 2023:2025
per <- ab |> mutate(when = case_when(start %in% PRE ~ "pre", start %in% POST ~ "post")) |>
  filter(!is.na(when)) |> group_by(cid, when) |>
  summarise(n = n(), across(all_of(names(OUTCOMES)), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  filter(n >= 2) |> select(-n) |>
  tidyr::pivot_longer(all_of(names(OUTCOMES)), names_to = "outcome") |>
  tidyr::pivot_wider(names_from = when, values_from = value) |>
  filter(!is.na(pre), !is.na(post))
base <- P |> filter(tp == 201819L) |> select(cid, all_of(names(X)), nor) |>
  mutate(fsm_q = ntile(fsm, 5))
shift <- per |> inner_join(base, by = "cid") |> left_join(now, by = "cid")
shift_q <- shift |> group_by(outcome, fsm_q) |>
  summarise(schools = n(), fsm = mean(fsm), pre = mean(pre), post = mean(post), .groups = "drop") |>
  mutate(pp = post - pre, pct = 100 * (post / pre - 1))
shift_all <- per |> group_by(outcome) |>
  summarise(schools = n(), pre = mean(pre), post = mean(post), .groups = "drop") |>
  mutate(pp = post - pre, pct = 100 * (post / pre - 1))
# the relative rise on the 2018/19 intake (per SD), with region
shift_reg <- bind_rows(lapply(names(OUTCOMES), function(y) {
  d <- shift |> filter(outcome == y) |>
    mutate(across(all_of(names(X)), ~ (.x - mean(.x)) / sd(.x)), dy = ly(post) - ly(pre))
  m <- feols(as.formula(paste("dy ~", paste(names(X), collapse = " + "), "| region_now")), data = d, vcov = "hetero")
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  ct |> transmute(outcome = y, term, est = Estimate, se = `Std. Error`, n = nobs(m))
}))
# staying in the top fifth
keep_top <- per |> group_by(outcome) |>
  mutate(top_pre = ntile(pre, 5) == 5, top_post = ntile(post, 5) == 5) |>
  summarise(stay = mean(top_post[top_pre]), r = cor(ly(pre), ly(post)), .groups = "drop")

# ---- 8. a school's own intake changing: within against between --------------
UNIT <- c(fsm = 5, sen_sup = 5, ehcp = 1, eal = 5, girls = 10, lsize = log(1.1))
UNIT_LAB <- c(fsm = "5 points more FSM", sen_sup = "5 points more SEN support", ehcp = "1 point more EHC plans",
              eal = "5 points more EAL", girls = "10 points more girls", lsize = "10% more pupils")
RHS <- paste(names(X), collapse = " + ")
wb <- bind_rows(lapply(names(OUTCOMES), function(y) {
  dd <- P |> mutate(.y = ly(.data[[y]]))
  mods <- list(Within = feols(as.formula(paste(".y ~", RHS, "| cid + tp")), data = dd, cluster = ~cid),
               Between = feols(as.formula(paste(".y ~", RHS, "| tp")), data = dd, cluster = ~cid))
  bind_rows(lapply(names(mods), function(k) {
    ct <- as.data.frame(coeftable(mods[[k]])); ct$term <- rownames(ct)
    ct |> transmute(outcome = y, model = k, term, est = Estimate, se = `Std. Error`,
                    n = nobs(mods[[k]]), r2_within = if (k == "Within") fitstat(mods[[k]], "wr2")[[1]] else NA_real_)
  }))
})) |>
  mutate(u = UNIT[term], pct = 100 * (exp(est * u) - 1), lo = 100 * (exp((est - 1.96 * se) * u) - 1),
         hi = 100 * (exp((est + 1.96 * se) * u) - 1), label = UNIT_LAB[term])
# the same without 2025/26, whose intake comes from a different file
wb_check <- bind_rows(lapply(c("overall", "unauth", "persistent", "severe"), function(y) {
  m <- feols(as.formula(paste(".y ~", RHS, "| cid + tp")), cluster = ~cid,
             data = P |> filter(tp < 202526L) |> mutate(.y = ly(.data[[y]])))
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  ct |> transmute(outcome = y, term, est = Estimate, se = `Std. Error`)
})) |> mutate(pct = 100 * (exp(est * UNIT[term]) - 1))
# how much each school's intake actually moves
x_move <- bind_rows(lapply(names(X), function(x) {
  v <- P[[x]]; w <- v - ave(v, P$cid)
  tibble(term = x, between_sd = sd(ave(v, P$cid)), within_sd = sd(w))
}))

# ---- 9. each school's series, for later pages -------------------------------
series <- ab |> select(cid, URN, tp, all_of(names(OUTCOMES)))
rel_pos <- ab |> select(cid, tp, overall, unauth, persistent, severe) |>
  tidyr::pivot_longer(-c(cid, tp), names_to = "outcome") |>
  group_by(outcome, tp) |> mutate(rel = 100 * (exp(ly(value) - mean(ly(value))) - 1)) |> ungroup()

out <- list(
  built = Sys.Date(), outcomes = OUTCOMES, x = X, periods = PERIODS,
  panel = list(school_years = nrow(ab), urns = length(urns), schools = n_distinct(ab$cid), dup = n_dup,
               by_year = ab |> count(tp) |> mutate(year = yl(tp)),
               model_years = nrow(P), model_schools = n_distinct(P$cid)),
  trend = trend, intake_trend = intake_trend,
  events = ev |> left_join(now, by = "cid"), es = ES, es_comp = COMP,
  var_dec = var_dec, year_share = year_share,
  cors = cors,
  shift = list(all = shift_all, by_fsm = shift_q, reg = shift_reg, keep_top = keep_top,
               base_q = base |> group_by(fsm_q) |> summarise(lo = min(fsm), hi = max(fsm), .groups = "drop")),
  wb = wb, wb_check = wb_check, x_move = x_move,
  series = series, rel_pos = rel_pos, now = now
)
saveRDS(out, here::here("data", "absence_change.rds"))
message("Saved data/absence_change.rds")
