# 09_school_lad.R - The local authority district each school sits in
# ---------------------------------------------------------------------------
# District comes from each school's postcode, via postcodes.io (ONS
# postcode directory, Open Government Licence). Postcodes that have been
# retired are placed by reverse-geocoding the school's grid reference.
#
# From the original project's R/18_district_effects.R, which built this for
# the rows of its own model only (so Sussex schools outside them had to be
# placed again later). Here every school in the panel is placed.
#
# Output: data/cache/school_lad.rds (URN, POSTCODE, easting, northing, lad,
# lad_cd).   Rscript pipeline/09_school_lad.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr); library(jsonlite); library(httr) })

CACHE <- here::here("data", "cache", "school_lad.rds")
`%||%` <- function(a, b) if (is.null(a)) b else a

pc_bulk <- function(pcs) {
  r <- POST("https://api.postcodes.io/postcodes",
            body = list(postcodes = as.list(pcs)), encode = "json")
  stop_for_status(r)
  res <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)$result
  bind_rows(lapply(res, function(x) tibble(
    postcode = x$query,
    lad = if (is.null(x$result)) NA_character_ else x$result$admin_district %||% NA_character_,
    lad_cd = if (is.null(x$result)) NA_character_ else x$result$codes$admin_district %||% NA_character_)))
}

geo_bulk <- function(lonlat) {
  r <- POST("https://api.postcodes.io/postcodes",
            body = list(geolocations = lapply(seq_len(nrow(lonlat)), function(i)
              list(longitude = lonlat$lon[i], latitude = lonlat$lat[i],
                   radius = 2000, limit = 1))),
            encode = "json")
  stop_for_status(r)
  res <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)$result
  bind_rows(lapply(seq_along(res), function(i) {
    h <- res[[i]]$result
    tibble(i = i,
           lad = if (length(h)) h[[1]]$admin_district %||% NA_character_ else NA_character_,
           lad_cd = if (length(h)) h[[1]]$codes$admin_district %||% NA_character_ else NA_character_)
  }))
}

school_districts <- function(schools) {
  pcs <- unique(na.omit(schools$POSTCODE))
  message("  looking up ", length(pcs), " postcodes ...")
  by_pc <- bind_rows(lapply(split(pcs, ceiling(seq_along(pcs) / 100)), function(b) {
    Sys.sleep(0.2); pc_bulk(b)
  }))
  out <- schools |> left_join(by_pc, by = c("POSTCODE" = "postcode"))

  miss <- out |> filter(is.na(lad), !is.na(easting))
  if (nrow(miss)) {
    message("  reverse-geocoding ", nrow(miss), " schools with retired postcodes ...")
    ll <- sf::st_as_sf(miss, coords = c("easting", "northing"), crs = 27700) |>
      sf::st_transform(4326) |> sf::st_coordinates()
    g <- bind_rows(lapply(split(seq_len(nrow(miss)), ceiling(seq_len(nrow(miss)) / 100)), function(ix) {
      Sys.sleep(0.2)
      geo_bulk(tibble(lon = ll[ix, 1], lat = ll[ix, 2])) |> mutate(i = ix[i])
    }))
    miss$lad <- g$lad[order(g$i)]; miss$lad_cd <- g$lad_cd[order(g$i)]
    out <- out |> rows_update(miss |> select(URN, lad, lad_cd), by = "URN")
  }
  out
}

if (sys.nframe() == 0) {
  panel <- readRDS(here::here("data", "panel_data.rds"))
  sch <- panel |> distinct(URN, POSTCODE, easting, northing) |>
    group_by(URN) |> slice(1) |> ungroup() |>
    mutate(POSTCODE = toupper(trimws(POSTCODE)))
  out <- school_districts(sch)
  dir.create(dirname(CACHE), showWarnings = FALSE, recursive = TRUE)
  saveRDS(out, CACHE)
  message(sprintf("Saved %s: %d schools, %d without a district", basename(CACHE), nrow(out), sum(is.na(out$lad))))
}
