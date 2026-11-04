# 00_config.R - Everything the Value Added v2.1 build is configured by
# ---------------------------------------------------------------------------
# Value Added is the school-level residual of a multilevel model of
# Attainment 8. v2.0 re-specified it on the recommendations of the
# value-added review (review.qmd): no Ofsted grade and no workforce terms
# in the expectation; pupil mobility, a single-sex term, SEN rates and
# neighbourhood education added; value added reported against England and
# within the local authority, with shrunken estimates and intervals. v2.1
# adds whether a school stops at 16 (R/07_v21_candidates.R), and publishes
# beside it value added allowing for the kind of absence (KIND_TERM).
#
# The inputs are built from public sources by R/00_fetch_data.R.
# ---------------------------------------------------------------------------

VERSION <- "v2.1"

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

# The original project, as a sibling folder
SAT <- file.path(dirname(here::here()), "school_attainment_tool")
sat <- function(...) file.path(SAT, ...)
ORIGINAL_SITE <- "https://adamdennett.github.io/school_attainment_tool"

# Pupil groups and the Attainment 8 column each is modelled on
GROUPS <- c(all = "ATT8SCR", disadvantaged = "ATT8SCR_FSM6CLA1A",
            non_disadvantaged = "ATT8SCR_NFSM6CLA1A",
            girls = "ATT8SCR_GIRLS", boys = "ATT8SCR_BOYS")
GROUP_LABELS <- c(all = "All pupils", disadvantaged = "Disadvantaged",
                  non_disadvantaged = "Non-disadvantaged", girls = "Girls", boys = "Boys")

# The specification. Every term is intake or context, none is something a
# school controls. The single-sex term is a girls' school flag for every
# group except boys, whose comparison is boys' schools against mixed.
# age16, a school that stops at 16, is the v2.1 addition: its structure,
# like its admissions policy.
FIXED_V2 <- c("log(PTFSM6CLA1A)", "log(PERCTOT)", "log(PNUMEAL)", "ks2_c", "ADMPOL_PT",
              "z_l4", "sen_support", "sen_ehcp", "mobile", "single_sex", "age16")
# The kind of absence: illness as a share of all absence. Partly the
# school's own coding (what it authorises as illness), so it is not in the
# published measure; value added allowing for it is published beside it.
KIND_TERM <- "ill_share"
single_sex_term <- function(group) if (group == "boys") "boys_school" else "girls_school"
fixed_for <- function(group) sub("single_sex", single_sex_term(group), FIXED_V2, fixed = TRUE)
RANDOM_V2 <- c("(1 | year_label)", "(1 | gor_name/LANAME)")

TERM_LABELS <- c(
  "log(PTFSM6CLA1A)" = "Disadvantage (% FSM6, log)", "log(PERCTOT)" = "Absence (log)",
  "log(PNUMEAL)" = "English as an additional language (log)", "ks2_c" = "Prior attainment (KS2, centred)",
  # admissions are relative to non-selective schools in highly selective areas
  "ADMPOL_PTSEL" = "Selective (vs non-selective in a selective area)",
  "ADMPOL_PTOTHER NON SEL" = "Non-selective elsewhere (vs non-selective in a selective area)",
  "z_l4" = "Neighbourhood education (SD)", "sen_support" = "SEN support %", "sen_ehcp" = "SEN with EHCP %",
  "mobile" = "Pupil mobility %", "girls_schoolTRUE" = "Girls' school", "boys_schoolTRUE" = "Boys' school",
  "age16TRUE" = "11-16 school", "ill_share" = "Illness share of absence (%)",
  "(Intercept)" = "Intercept")

CTRL <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000))

# The school-years a group's model is fitted on
model_rows <- function(d, group, extra = character(0)) {
  y <- GROUPS[[group]]
  ss <- single_sex_term(group)
  keep <- !d$excluded & d[[y]] > 0 & d$PTFSM6CLA1A > 0 & d$PERCTOT > 0 & d$PNUMEAL > 0 &
    complete.cases(d[, c(y, "ks2_c", "ADMPOL_PT", "z_l4", "sen_support", "sen_ehcp", "mobile", ss, "age16",
                         "gor_name", "LANAME", "PTFSM6CLA1A", "PERCTOT", "PNUMEAL", extra)])
  d[which(keep), ] |>      # which(): a missing comparison drops the row rather than adding an NA one
    mutate(year_label = factor(year_label), gor_name = factor(gor_name), LANAME = factor(LANAME))
}

SUSSEX_LAS <- c("East Sussex", "West Sussex", "Brighton and Hove")
