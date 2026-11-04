# 02b_school_context.R - What the maps show beside value added, for every school
# ---------------------------------------------------------------------------
# The Sussex pages' maps colour schools by attendance, results, intake and
# governance measures, most of them from the original project's Sussex area
# files. This builds the same kinds of measure for every school in England,
# from the inputs (R/01_inputs.R) and the pipeline's downloads, so the
# explorer can offer them too. Description only: nothing here enters the
# model, so nothing is refitted.
#
# One row a school (its current URN), its latest model year:
#   persistent      persistent absence, % of pupils missing 10%+ of sessions
#   illness, unauth, reduced   illness, unauthorised absence and reduced
#                   timetables, % of possible sessions (EES, full year)
#   susp_rate       suspensions per 100 pupils (EES exclusions, latest year)
#   gov_nat, run_by_nat   who runs it: maintained, or the academy's trust and
#                   route, from GIAS (a trust running two or more open
#                   establishments counts as a multi-academy trust)
#
# Output: data/school_context.rds (committed).   Rscript R/02b_school_context.R
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))

d <- readRDS(here::here("data", "inputs.rds"))
latest <- d |> arrange(URN, desc(year_label)) |> distinct(URN, .keep_all = TRUE)

# Suspensions, every school-year, matched on the URN each row was published under
zip <- tail(sort(list.files(here::here("data-raw", "ees"), "^exclusions_school_.*\\.zip$", full.names = TRUE)), 1)
susp <- readr::read_csv(unz(zip, "data/exc_school.csv"), col_types = readr::cols(.default = "c"), progress = FALSE) |>
  transmute(urn_published = as.integer(school_urn),
            year_label = paste0(substr(time_period, 1, 4), "-", substr(time_period, 5, 6)),
            susp_rate = suppressWarnings(as.numeric(susp_rate))) |>
  filter(!is.na(susp_rate)) |> distinct(urn_published, year_label, .keep_all = TRUE)
susp_latest <- d |> select(URN, urn_published, year_label) |>
  inner_join(susp, by = c("urn_published", "year_label")) |>
  arrange(URN, desc(year_label)) |> distinct(URN, .keep_all = TRUE) |>
  transmute(URN, susp_rate, susp_year = year_label)

# Who runs it, from GIAS
g <- readRDS(here::here("data", "gias", "gias_establishments.rds")) |> mutate(urn = as.integer(urn))
trust_size <- g |> filter(status == "Open", !is.na(trust_name)) |> count(trust_name, name = "trust_n")
tidy <- function(x) ifelse(is.na(x), NA_character_, sub(" Limited$", "", gsub("\\bOf\\b", "of",
  gsub("\\bAnd\\b", "and", tools::toTitleCase(tolower(x))))))
gov <- g |> select(urn, establishment_type, type_group, trust_name) |>
  left_join(trust_size, by = "trust_name") |>
  transmute(URN = urn,
            gov_nat = case_when(
              type_group %in% "Local authority maintained schools" ~ "Maintained",
              !is.na(trust_name) & coalesce(trust_n, 1L) >= 2 & grepl("sponsor", establishment_type, ignore.case = TRUE) ~
                "Multi-academy trust, sponsored",
              !is.na(trust_name) & coalesce(trust_n, 1L) >= 2 ~ "Multi-academy trust, converter or free school",
              TRUE ~ "Single-academy trust or not recorded"),
            run_by_nat = case_when(gov_nat == "Maintained" ~ "Maintained by the council",
                                   startsWith(gov_nat, "Multi") ~ tidy(trust_name),
                                   TRUE ~ "Single-academy trust or not recorded"))

ctx <- latest |>
  transmute(URN, persistent = PPERSABS10, illness = abs_illness, unauth = abs_unauth,
            reduced = abs_reduced, abs_year = year_label) |>
  left_join(susp_latest, by = "URN") |>
  left_join(gov, by = "URN")
# the date of the GIAS register used, for the map's label
stamp <- tail(sort(sub("^edubasealldata([0-9]{8})[.]csv$", "\\1",
                       list.files(here::here("data-raw", "gias"), "^edubasealldata[0-9]{8}[.]csv$"))), 1)
attr(ctx, "gias_date") <- if (length(stamp)) as.Date(stamp, "%Y%m%d") else NA
saveRDS(ctx, here::here("data", "school_context.rds"))
message(sprintf("Saved school_context.rds: %d schools; suspensions for %d, governance for %d",
                nrow(ctx), sum(!is.na(ctx$susp_rate)), sum(!is.na(ctx$gov_nat))))
print(table(ctx$gov_nat, useNA = "ifany"))
