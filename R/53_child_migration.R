# 53_child_migration.R - Children moving in and out: within the UK against from abroad
# ---------------------------------------------------------------------------
# R/50 split the change in pupils into births, net migration of the cohorts since birth, and
# the state-school share, but its migration part could not tell moves within the UK from
# moves to and from abroad. ONS's components of population change can: for every district,
# year and single year of age, the flows in and out within the UK (internal) and across its
# borders (international).
#
# For children aged 0 to 10 (primary age and below), each district and year from mid-2012 to
# mid-2025: net internal and net international migration per 1,000 children of those ages at
# the start of the year, and the two periods behind R/50's comparison (mid-2013 to mid-2018,
# mid-2019 to mid-2025). Also adults aged 30 to 39, the ages of most parents of young
# children, for the same split.
#
# Source: ONS, Population estimates for the UK, England, Wales, Scotland and Northern
#   Ireland, mid-2011 to mid-2025 detailed time series, table MYEB2 (components of change for
#   England and Wales local authorities, April 2023 boundaries, by sex and single year of
#   age): data-raw/ons/myeb/myebtablesuk20112025.xlsx, from
#   https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2011tomid2025/myebtablesuk20112025.xlsx
#   Ages are at the end of each mid-year to mid-year period. From mid-2022 ONS estimates
#   international migration with revised (admin-based) methods, so the later years are not
#   fully comparable with the earlier.
#
# Output: data/child_migration.rds, writeup/figures/child_migration_*.png
#   Rscript R/53_child_migration.R   (a minute or two: the sheet is large)
# ---------------------------------------------------------------------------

source(here::here("R", "00_config.R"))
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
f <- here::here("data-raw", "ons", "myeb", "myebtablesuk20112025.xlsx")
X <- as.data.table(suppressMessages(readxl::read_excel(f, sheet = "MYEB2", skip = 1)))
X <- X[country == "E"]
X[, age := as.integer(age)]
long <- melt(X, id.vars = c("ladcode23", "laname23", "country", "sex", "age"), variable.name = "var", value.name = "n")
long[, `:=`(year = as.integer(sub(".*_", "", var)), comp = sub("_[0-9]{4}$", "", var), n = as.numeric(n))]
grp <- function(a) fcase(a <= 10, "Children 0-10", a >= 30 & a <= 39, "Adults 30-39", default = NA_character_)
long[, group := grp(age)]
L <- long[!is.na(group), .(n = sum(n, na.rm = TRUE)), by = .(code = ladcode23, name = laname23, group, comp, year)]
eng <- L[, .(n = sum(n)), by = .(group, comp, year)][, `:=`(code = "E92000001", name = "England")]
L <- rbind(L, eng)
W <- dcast(L, code + name + group + year ~ comp, value.var = "n")
setorder(W, code, group, year)
W[, pop_start := shift(population), by = .(code, group)]          # population at the previous mid-year
W <- W[year >= 2012]
W[, `:=`(internal_rate = 1000 * internal_net / pop_start, international_rate = 1000 * international_net / pop_start)]

per <- function(y) fcase(y %between% c(2013, 2018), "2013-2018", y %between% c(2019, 2025), "2019-2025", default = NA_character_)
W[, period := per(year)]
P <- W[!is.na(period), .(internal = mean(internal_rate), international = mean(international_rate)), by = .(code, name, group, period)]
Pw <- dcast(P, code + name + group ~ period, value.var = c("internal", "international"))
setnames(Pw, gsub("-", "_", names(Pw)))
Pw[, `:=`(d_internal = internal_2019_2025 - internal_2013_2018, d_international = international_2019_2025 - international_2013_2018)]

key <- c("England", "Brighton and Hove", "Lewes", "Mid Sussex", "Adur", "Worthing", "Wealden", "Hackney", "Tower Hamlets",
         "Southwark", "Lambeth", "Westminster", "Camden", "Bristol, City of", "Cambridge", "Oxford")
res <- list(W = W, P = Pw, key = Pw[name %in% key], built = Sys.Date())
saveRDS(res, here::here("data", "child_migration.rds"))

# ---- figures ------------------------------------------------------------------------------
th <- theme_minimal(base_size = 12) + theme(legend.position = "top", panel.grid.minor = element_blank(), plot.title.position = "plot")
fg <- melt(W[name %in% c("England", "Brighton and Hove", "Hackney") & group == "Children 0-10", .(name, year, `Within the UK` = internal_rate, `From abroad` = international_rate)],
           id.vars = c("name", "year"), variable.name = "flow", value.name = "rate")
fg[, name := factor(name, c("Brighton and Hove", "Hackney", "England"))]
g <- ggplot(fg, aes(year, rate, colour = flow)) + geom_hline(yintercept = 0, colour = "#6b6680") + geom_line(linewidth = 1.1) + geom_point(size = 1.4) +
  geom_vline(xintercept = 2021.5, colour = "#d3cde0", linetype = "dashed") + facet_wrap(~name) +
  scale_colour_manual(values = c(`Within the UK` = "#361a54", `From abroad` = "#993bff"), name = NULL) +
  labs(x = "Year to mid-year", y = "Net migration per 1,000 children aged 0-10",
       title = "Children aged 0 to 10 moving in (+) and out (-)",
       caption = "ONS mid-year estimates, components of change (MYEB2). Dashed: ONS revised international migration methods from mid-2022.") + th
ggsave(here::here("writeup", "figures", "child_migration_rates.png"), g, width = 9, height = 4.6, dpi = 150)

options(width = 160)
cat("\n== Net migration per 1,000, average a year (children 0-10 and adults 30-39)\n")
print(res$key[order(group, internal_2019_2025), .(group, name, int_13_18 = round(internal_2013_2018, 1), int_19_25 = round(internal_2019_2025, 1),
                                                 intl_13_18 = round(international_2013_2018, 1), intl_19_25 = round(international_2019_2025, 1))])
cat("\n== Brighton and Hove, children 0-10, by year\n"); print(W[name == "Brighton and Hove" & group == "Children 0-10", .(year, pop_start, internal_net, international_net, internal_rate = round(internal_rate, 1), international_rate = round(international_rate, 1))])
