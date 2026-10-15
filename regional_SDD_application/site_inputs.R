# Site characteristics and antecedent weather for NW/CLP sites, as fetched by
# NW_CLP_regional_application.Rmd (adapted from the regional repository's
# 02b_pull_site_characteristics.Rmd and 02c_pull_weather_summaries.Rmd).
# Shared with NW_CLP_regional_application_relaxedQA_scratch.Rmd, which uses
# them to add sites and dates the production notebook's caches don't cover.
#
# Expects tidyverse, sf, elevatr, nhdplusTools, StreamCatTools, parallel,
# climateR, and data.table to be loaded, and a log() function, as in the
# notebooks.

WEATHER_MAX_WINDOW_DAYS <- 30
WEATHER_WINDOWS <- c(1, 3, 7, 30)
GRIDMET_VARS <- c("pr", "tmmx", "tmmn", "srad")


# Elevation, the NHDPlusV2 waterbody match and shore_flag, and LakeCat
# catchment metrics, one row per site. `sites` has columns r_id, lat, lon.
# A site is shore_flag 1 if it falls outside its matched waterbody or within
# shore_dist_m of its shoreline.
fetch_site_characteristics <- function(sites, shore_dist_m) {
  log("fetching site characteristics for %s sites", nrow(sites))
  pts_sf <- st_as_sf(sites, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  t0 <- Sys.time()
  elev <- get_elev_point(pts_sf, src = "aws", z = 9)
  log("elevation fetch took %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
  elev_df <- st_drop_geometry(elev) %>% select(r_id, elevation_m = elevation)
  sites <- sites %>% left_join(elev_df, by = "r_id")

  # fresh NHDPlusV2 waterbody match: one bulk fetch over the full site extent,
  # then a local nearest-feature join per site
  aoi <- st_convex_hull(st_union(pts_sf)) %>% st_buffer(0.01)
  t0 <- Sys.time()
  waterbodies <- get_waterbodies(AOI = aoi)
  log("waterbody fetch took %.1f s, %s polygons over the site extent",
      as.numeric(difftime(Sys.time(), t0, units = "secs")), nrow(waterbodies))

  nearest_idx <- st_nearest_feature(pts_sf, waterbodies)
  matched_wb <- waterbodies[nearest_idx, ]
  dist_to_wb <- as.numeric(st_distance(pts_sf, matched_wb, by_element = TRUE))
  dist_to_shore <- as.numeric(st_distance(pts_sf, st_boundary(matched_wb), by_element = TRUE))

  sites <- sites %>%
    mutate(comid = as.character(matched_wb$comid),
           dist_to_wb = dist_to_wb, dist_to_shore = dist_to_shore,
           shore_flag = as.numeric(dist_to_wb > 0 | dist_to_shore < shore_dist_m))
  log("shore_flag distribution:"); print(table(sites$shore_flag))

  lakecat_metrics <- paste(c(
    "pctimp2006", "pcturbhi2006", "pcturblo2006", "pcturbmd2006", "pcturbop2006",
    "pctconif2006", "pctdecid2006", "pctmxfst2006",
    "pcthbwet2006", "pctwdwet2006", "pctcrop2006"
  ), collapse = ",")

  comids <- unique(na.omit(sites$comid))
  chunk_size <- 200
  chunks <- split(comids, ceiling(seq_along(comids) / chunk_size))
  # a chunk where NONE of its comids are LakeCat-covered throws rather than
  # returning empty - catch that per chunk so one bad chunk doesn't abort the
  # whole fetch
  lakecat_raw <- purrr::map_dfr(chunks, function(ch) {
    tryCatch(lc_get_data(comid = paste(ch, collapse = ","), metric = lakecat_metrics,
                         aoi = "catchment", showAreaSqKm = TRUE),
             error = function(e) { log("LakeCat chunk failed (0 covered): %s", conditionMessage(e)); tibble() })
  })
  log("LakeCat rows returned: %s / %s comids requested", nrow(lakecat_raw), length(comids))

  lakecat <- lakecat_raw %>%
    mutate(comid = as.character(comid),
           pct_forest_2006 = pctconif2006cat + pctdecid2006cat + pctmxfst2006cat,
           pct_wetland_2006 = pcthbwet2006cat + pctwdwet2006cat,
           pct_urban_2006 = pcturbhi2006cat + pcturblo2006cat + pcturbmd2006cat + pcturbop2006cat) %>%
    select(comid, catchment_area_sqkm = catareasqkm,
           pct_impervious_2006 = pctimp2006cat, pct_urban_2006, pct_forest_2006,
           pct_cropland_2006 = pctcrop2006cat, pct_wetland_2006)

  sites <- sites %>% left_join(lakecat, by = "comid")
  log("sites with LakeCat data: %s / %s", sum(!is.na(sites$pct_impervious_2006)), nrow(sites))
  sites
}


# Daily gridMET weather per site and date range, fetched in parallel.
# `site_ranges` has columns r_id, lat, lon, min_date, max_date (one row per
# range; a site may have more than one). Returns list(weather, failed).
fetch_gridmet <- function(site_ranges, n_workers = 8) {
  log("fetching gridMET for %s site date ranges (%s sites)", nrow(site_ranges), n_distinct(site_ranges$r_id))
  vars <- GRIDMET_VARS

  fetch_one <- function(i, site_ranges) {
    row <- site_ranges[i, ]
    fetch <- function() {
      pt <- sf::st_as_sf(data.frame(lon = row$lon, lat = row$lat), coords = c("lon", "lat"), crs = 4326)
      res <- getGridMET(AOI = pt, varname = vars,
                        startDate = as.character(row$min_date), endDate = as.character(row$max_date))
      res$r_id <- row$r_id
      res
    }
    tryCatch(fetch(), error = function(e) {
      Sys.sleep(2)
      tryCatch(fetch(), error = function(e2) data.frame(r_id = row$r_id, error = conditionMessage(e2)))
    })
  }
  # ship only what the workers need, not this function's whole environment
  environment(fetch_one) <- list2env(list(vars = vars), parent = globalenv())

  t0 <- Sys.time()
  cl <- makeCluster(n_workers, type = "PSOCK")
  on.exit(stopCluster(cl))
  clusterEvalQ(cl, { suppressMessages(library(climateR)); suppressMessages(library(sf)) })
  results <- parLapply(cl, seq_len(nrow(site_ranges)), fetch_one, site_ranges = site_ranges)
  log("fetch took %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))

  ok <- Filter(function(x) !"error" %in% names(x), results)
  failed <- Filter(function(x) "error" %in% names(x), results)
  log("succeeded: %s ranges, failed: %s ranges", length(ok), length(failed))
  list(weather = bind_rows(ok), failed = bind_rows(failed))
}


# The rolling antecedent-weather summaries used in training, per site and
# date: each window covers the days strictly before the date. Each site's
# daily record should be contiguous.
summarize_weather <- function(weather_raw) {
  weather <- weather_raw %>%
    mutate(date = as.Date(date),
           precip_mm = pr, tmax_degC = tmmx - 273.15, tmin_degC = tmmn - 273.15,
           tmean_degC = (tmax_degC + tmin_degC) / 2, srad_Wm2 = srad) %>%
    select(r_id, date, precip_mm, tmax_degC, tmin_degC, tmean_degC, srad_Wm2)

  setDT(weather)
  setorder(weather, r_id, date)
  site_splits <- split(weather, by = "r_id", keep.by = TRUE)

  make_summary_one_site <- function(df) {
    site_id <- df$r_id[1]
    full_days <- data.table(date = seq(min(df$date), max(df$date), by = "1 day"))
    df <- merge(full_days, df[, -"r_id"], by = "date", all.x = TRUE)
    df[, r_id := site_id]
    for (w in WEATHER_WINDOWS) {
      df[[paste0("precip_mm_prev", w)]] <- frollsum(df$precip_mm, n = w)
      df[[paste0("tmax_degC_prev", w)]] <- frollapply(df$tmax_degC, N = w, FUN = max)
      df[[paste0("tmean_degC_prev", w)]] <- frollmean(df$tmean_degC, n = w)
      df[[paste0("tmin_degC_prev", w)]] <- frollapply(df$tmin_degC, N = w, FUN = min)
      df[[paste0("srad_Wm2_prev", w)]] <- frollmean(df$srad_Wm2, n = w)
    }
    df[, date := date + 1]  # window on `date` covers the w days strictly BEFORE date
    df
  }

  # frollapply() (used for the rolling max/min above) forks worker processes via
  # parallel::mcparallel() whenever data.table has >1 thread available - Positron
  # disallows forking the R session, so force single-threaded (non-forking) here
  old_dt_threads <- getDTthreads()
  setDTthreads(1)
  on.exit(setDTthreads(old_dt_threads))
  t0 <- Sys.time()
  weather_summaries <- rbindlist(lapply(site_splits, make_summary_one_site), fill = TRUE)
  weather_summaries <- weather_summaries[!is.na(r_id)]
  log("weather summaries took %.1f min: %s rows, %s cols",
      as.numeric(difftime(Sys.time(), t0, units = "mins")), nrow(weather_summaries), ncol(weather_summaries))
  weather_summaries
}


# write a cache file via a temp file in the same folder, so an interrupted
# write can't leave a truncated cache
write_cache <- function(df, path) {
  tmp <- paste0(path, ".tmp")
  write_feather(as.data.frame(df), tmp)
  stopifnot("could not replace the cache file" = file.rename(tmp, path))
}
