# 08_census.R - Census 2021 neighbourhood context for each school
# -------------------------------------------------------------------------------
# Builds small-area socio-economic context for every school in the KS2 panel,
# as a proxy for the prior-attainment baseline that does not exist at KS2:
# parental human capital (highest qualification) and labour-market position
# (economic activity) of the neighbourhoods each school serves.
#
# Method:
#   1. Census 2021 bulk tables (nomis, output-area level):
#        TS067 Highest level of qualification
#        TS066 Economic activity status
#   2. OA (Dec 2021) population-weighted centroids from the ONS Open Geography
#      ArcGIS service (188,880 OAs, paged REST queries, British National Grid)
#   3. For each school (GIAS easting/northing), find the k nearest OA centroids
#      (nabor::knn) and average the OA indicators - unweighted means for
#      k = 5 and k = 10, plus an inverse-distance-weighted k = 10 version and
#      the mean distance to the 10 nearest OAs (a density/rurality measure).
#
# Output: data/census/school_census_context.rds - one row per URN (time-
# invariant; Census day was 21 March 2021, i.e. just before the panel window).
#
#   Rscript pipeline/08_census.R    (from the original project's R/13_census_context.R)
# -------------------------------------------------------------------------------

source(here::here("pipeline", "helpers.R"))

NOMIS_BULK_URL <- "https://www.nomisweb.co.uk/output/census/2021/census2021-%s.zip"
OA_PWC_QUERY_URL <- paste0(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
  "OA_December_2021_EW_PWC_V4/FeatureServer/0/query"
)

census_raw_dir <- here::here("data-raw", "census2021")
census_dir <- here::here("data", "census")
cache_dir <- here::here("data", "cache")
dir.create(census_raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(census_dir, showWarnings = FALSE, recursive = TRUE)


# ---- 1. Census bulk tables -> OA indicators ----

fetch_census_table <- function(table_id) {
  zip_path <- file.path(census_raw_dir, paste0("census2021-", table_id, ".zip"))
  if (!file.exists(zip_path)) {
    message("Downloading nomis bulk table ", table_id, " ...")
    download.file(sprintf(NOMIS_BULK_URL, table_id), zip_path, mode = "wb", quiet = TRUE)
  }
  exdir <- file.path(census_raw_dir, table_id)
  oa_csv <- file.path(exdir, paste0("census2021-", table_id, "-oa.csv"))
  if (!file.exists(oa_csv)) unzip(zip_path, exdir = exdir)
  read_csv(oa_csv, show_col_types = FALSE)
}

#' Single-column selector by exact header name (fails loudly if DfE/nomis rename)
col_exact <- function(df, name) {
  stopifnot(name %in% names(df))
  df[[name]]
}

build_oa_indicators <- function() {
  message("Building OA-level indicators ...")

  ts067 <- fetch_census_table("ts067")
  qual <- tibble(
    oa21cd = ts067[["geography code"]],
    qual_total = col_exact(ts067, "Highest level of qualification: Total: All usual residents aged 16 years and over"),
    qual_none = col_exact(ts067, "Highest level of qualification: No qualifications"),
    qual_l4plus = col_exact(ts067, "Highest level of qualification: Level 4 qualifications and above")
  ) %>%
    mutate(
      cens_l4plus = 100 * qual_l4plus / qual_total,
      cens_noquals = 100 * qual_none / qual_total
    ) %>%
    select(oa21cd, cens_l4plus, cens_noquals)

  ts066 <- fetch_census_table("ts066")
  econ <- tibble(
    oa21cd = ts066[["geography code"]],
    total_16plus = col_exact(ts066, "Economic activity status: Total: All usual residents aged 16 years and over"),
    active_exstu = col_exact(ts066, "Economic activity status: Economically active (excluding full-time students)"),
    active_stu = col_exact(ts066, "Economic activity status: Economically active and a full-time student"),
    unemp_exstu = col_exact(ts066, "Economic activity status: Economically active (excluding full-time students): Unemployed"),
    unemp_stu = col_exact(ts066, "Economic activity status: Economically active and a full-time student: Unemployed"),
    inactive = col_exact(ts066, "Economic activity status: Economically inactive"),
    lt_sick = col_exact(ts066, "Economic activity status: Economically inactive: Long-term sick or disabled")
  ) %>%
    mutate(
      cens_unemp = 100 * (unemp_exstu + unemp_stu) / pmax(active_exstu + active_stu, 1),
      cens_inactive = 100 * inactive / total_16plus,
      cens_ltsick = 100 * lt_sick / total_16plus
    ) %>%
    select(oa21cd, cens_unemp, cens_inactive, cens_ltsick)

  out <- inner_join(qual, econ, by = "oa21cd")
  message("  OA indicators: ", nrow(out), " output areas")
  out
}


# ---- 2. OA population-weighted centroids (paged ArcGIS REST) ----

fetch_oa_centroids <- function(force = FALSE) {
  cache_path <- file.path(cache_dir, "oa_pwc_2021.rds")
  if (file.exists(cache_path) && !force) {
    message("Using cached OA centroids")
    return(readRDS(cache_path))
  }

  message("Fetching OA population-weighted centroids (~95 paged requests) ...")
  # Keyset pagination on FID (resultOffset paging caps at 100k on this service)
  pages <- list()
  last_fid <- 0
  repeat {
    url <- paste0(
      OA_PWC_QUERY_URL,
      "?where=", utils::URLencode(paste0("FID>", last_fid), reserved = TRUE),
      "&outFields=OA21CD,FID&orderByFields=FID&returnGeometry=true&outSR=27700",
      "&resultRecordCount=2000&f=json"
    )
    resp <- jsonlite::fromJSON(url)
    feats <- resp$features
    if (is.null(feats) || length(feats) == 0 || nrow(feats) == 0) break
    pages[[length(pages) + 1]] <- tibble(
      oa21cd = feats$attributes$OA21CD,
      easting = feats$geometry$x,
      northing = feats$geometry$y
    )
    last_fid <- max(feats$attributes$FID)
    n_so_far <- sum(vapply(pages, nrow, integer(1)))
    if (n_so_far %% 20000 == 0) message("  ... ", n_so_far, " centroids")
    if (nrow(feats) < 2000) break
    Sys.sleep(0.2)
  }
  centroids <- bind_rows(pages)
  message("  Centroids fetched: ", nrow(centroids))
  saveRDS(centroids, cache_path)
  centroids
}


# ---- 3. k-nearest-OA aggregation per school ----

build_school_census_context <- function(k_set = c(5, 10, 25, 50, 100, 150), save = TRUE) {

  k_max <- max(k_set)

  oa <- build_oa_indicators() %>%
    inner_join(fetch_oa_centroids(), by = "oa21cd") %>%
    filter(!is.na(easting), !is.na(northing))
  message("OAs with indicators + centroids: ", nrow(oa))

  # Schools: every URN in the KS4 panel, coordinates from GIAS. The
  # original project ran this over its KS2 and KS5 panels too; a school's
  # values depend only on its own location, so the KS4 schools' are the same.
  panel_urns <- readRDS(here::here("data", "panel_data.rds")) %>% distinct(URN)

  gias <- readRDS(here::here("data", "gias", "gias_establishments.rds")) %>%
    mutate(easting = suppressWarnings(as.numeric(easting)),
           northing = suppressWarnings(as.numeric(northing)))
  schools <- panel_urns %>%
    left_join(gias %>% select(URN = urn, easting, northing), by = "URN") %>%
    filter(!is.na(easting), !is.na(northing), easting > 0)
  message("Schools/providers with coordinates: ", nrow(schools), " of ",
          nrow(panel_urns), " panel URNs")

  # kNN: nearest k_max OA centroids per school (nabor = fast libnabo kd-tree)
  nn <- nabor::knn(data = as.matrix(oa[, c("easting", "northing")]),
                   query = as.matrix(schools[, c("easting", "northing")]),
                   k = k_max)

  indicators <- c("cens_l4plus", "cens_noquals", "cens_unemp",
                  "cens_inactive", "cens_ltsick")
  ind_mat <- as.matrix(oa[, indicators])

  # Long form: school x neighbour rank
  long <- tibble(
    row = rep(seq_len(nrow(schools)), k_max),
    rank = rep(seq_len(k_max), each = nrow(schools)),
    oa_idx = as.vector(nn$nn.idx),
    dist_m = as.vector(nn$nn.dists)
  ) %>%
    bind_cols(as_tibble(ind_mat[.$oa_idx, ]))

  agg_for_k <- function(k) {
    long %>%
      filter(rank <= k) %>%
      group_by(row) %>%
      summarise(
        across(all_of(indicators), \(x) mean(x, na.rm = TRUE),
               .names = paste0("{.col}_k", k)),
        .groups = "drop"
      )
  }

  idw_for_k <- function(k) {
    long %>%
      filter(rank <= k) %>%
      mutate(w = 1 / pmax(dist_m, 50)) %>%
      group_by(row) %>%
      summarise(
        across(all_of(indicators),
               \(x) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)]),
               .names = paste0("{.col}_idw", k)),
        !!paste0("mean_dist_k", k) := mean(dist_m) / 1000,
        .groups = "drop"
      )
  }

  out <- schools %>% mutate(row = row_number())
  for (k in k_set) out <- out %>% left_join(agg_for_k(k), by = "row")
  for (k in intersect(c(10, 50, 100), k_set)) out <- out %>% left_join(idw_for_k(k), by = "row")
  out <- out %>% select(-row)

  if (save) {
    saveRDS(out, file.path(census_dir, "school_census_context.rds"))
    message("Saved school_census_context.rds (", nrow(out), " schools, ",
            ncol(out), " cols)")
  }
  out
}


# ---- Main execution (Rscript only) ----
if (sys.nframe() == 0) build_school_census_context()
