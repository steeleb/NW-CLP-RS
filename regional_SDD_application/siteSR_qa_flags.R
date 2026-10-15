# siteSR QA as flags rather than filters, shared by the relaxed-QA notebooks
# (NW_CLP_regional_application_relaxedQA_scratch.Rmd for the production model,
# NW_CLP_regional_application_band_only_relaxedQA_scratch.Rmd for the
# band-only model), so both flag every row with the same thresholds.
#
# Each QA step is flagged at the strict thresholds the production notebook
# and the regional training data use, and at the relaxed thresholds below,
# alongside the values the flags are computed from. The QA doesn't depend on
# the model, so neither do these flags.
#
# Expects tidyverse, arrow, AquaMatchr, and a log() function to be loaded, as
# in the notebooks.

# STRICT, as in the production notebook and the regional repository's
# 01_filter_AquaMatch_Data.Rmd / R/siteSR_qa.R
STRICT_DSWE1_SHARE_MIN <- 0.5 # minimum pCount_dswe1 / pCount_dswe_gt0
STRICT_DSWE1_COUNT_MIN <- 8 # pCount_dswe1 must exceed this
STRICT_PROP_CLOUDS_MAX <- 0 # max share of the site buffer flagged cloud
STRICT_CLOUD_COVER_MAX <- 20 # scene-level cloud cover, % (scenes missing from the metadata fail)

# RELAXED
DSWE1_SHARE_MIN <- 0.3
DSWE1_COUNT_MIN <- 3
PROP_CLOUDS_MAX <- 0.1
CLOUD_COVER_MAX <- 50
KEEP_SCENES_WITHOUT_METADATA <- TRUE # scenes missing from AquaMatch scene metadata pass the relaxed scene check
APPLY_BAND_RANSAC <- TRUE # set FALSE to leave band RANSAC out of pass_qa_relaxed

# both levels
SURFACE_TEMP_RANGE_K <- c(273.15, 313.15) # 0-40 °C

MISSION_CODES <- c(LANDSAT_4 = "LT04", LANDSAT_5 = "LT05", LANDSAT_7 = "LE07",
                   LANDSAT_8 = "LC08", LANDSAT_9 = "LC09")
SCENE_ID_PATTERN <- "L[CTE]0\\d_\\d{6}_\\d{8}"


# Read every row of the RS file and flag the pixel, temperature, and scene
# QA. Flags are independent (not cumulative), and a value that can't be
# computed (e.g. a missing surface temperature) fails its check.
read_and_flag_siteSR <- function(nw_rs_path, aquamatch_dir) {
  # one cloud cover per scene (the mission_pathrow_date core of the scene id)
  scene_cloud <- file.path(aquamatch_dir, c("sceneMetadata_Landsat457.csv", "sceneMetadata_Landsat89.csv")) %>%
    map(~ read_csv(.x, show_col_types = FALSE, col_select = c(sat_id, CLOUD_COVER))) %>%
    bind_rows() %>%
    mutate(scene_id = str_extract(sat_id, SCENE_ID_PATTERN)) %>%
    summarise(scene_cloud_cover = max(CLOUD_COVER), .by = scene_id)

  nw_rs <- read_feather(nw_rs_path) %>%
    mutate(mission = unname(MISSION_CODES[mission])) %>%
    mutate(across(c(pCount_dswe1, pCount_dswe_gt0, prop_clouds,
                    med_Red, med_Blue, med_Green, med_Nir, med_Swir1, med_Swir2, med_SurfaceTemp),
                  as.numeric))

  # date: second-to-last "_"-delimited token of system:index
  index_parts <- str_split(nw_rs$`system:index`, "_")
  nw_rs <- nw_rs %>%
    mutate(date = as.Date(map_chr(index_parts, ~ .x[length(.x) - 1]), format = "%Y%m%d"),
           lat = Latitude, lon = Longitude,
           scene_id = str_extract(`system:index`, SCENE_ID_PATTERN)) %>%
    left_join(scene_cloud, by = "scene_id") %>%
    mutate(dswe1_share = pCount_dswe1 / pCount_dswe_gt0,
           surface_temp_degC = med_SurfaceTemp - 273.15,
           in_scene_metadata = !is.na(scene_cloud_cover),
           qa_pixel_strict = coalesce(dswe1_share > STRICT_DSWE1_SHARE_MIN & pCount_dswe1 > STRICT_DSWE1_COUNT_MIN &
                                        prop_clouds <= STRICT_PROP_CLOUDS_MAX, FALSE),
           qa_pixel_relaxed = coalesce(dswe1_share > DSWE1_SHARE_MIN & pCount_dswe1 > DSWE1_COUNT_MIN &
                                         prop_clouds <= PROP_CLOUDS_MAX, FALSE),
           qa_temp = coalesce(between(med_SurfaceTemp, SURFACE_TEMP_RANGE_K[1], SURFACE_TEMP_RANGE_K[2]), FALSE),
           qa_scene_cloud_strict = coalesce(scene_cloud_cover < STRICT_CLOUD_COVER_MAX, FALSE),
           qa_scene_cloud_relaxed = coalesce(scene_cloud_cover < CLOUD_COVER_MAX, KEEP_SCENES_WITHOUT_METADATA))
  stopifnot("date parsing failed for some rows" = sum(is.na(nw_rs$date)) == 0)
  stopifnot("system:index is not unique per row" = !anyDuplicated(nw_rs$`system:index`))
  log("%s rows, %s sites, %s named waterbodies", nrow(nw_rs), n_distinct(nw_rs$r_id), n_distinct(na.omit(nw_rs$gnis_name)))
  nw_rs
}


# AquaMatch Gardner polynomial handoffs to Landsat 7, as in the production
# notebook, on every row. apply_handoffs() only works file to file, so it
# round-trips through temp files.
harmonize_bands <- function(nw_rs, handoff_path) {
  handoff_in <- tempfile(fileext = ".feather")
  handoff_out <- tempfile(fileext = ".parquet")
  on.exit(unlink(c(handoff_in, handoff_out)))
  write_feather(nw_rs, handoff_in)

  apply_handoffs(input_path = handoff_in,
                 handoff_path = handoff_path,
                 correction_method = "Gardner_poly",
                 sat_target = "LS7",
                 algal_mask = FALSE,
                 output_file = handoff_out)

  corr <- read_parquet(handoff_out) %>%
    rename(red_corr7 = red_corr_7, green_corr7 = green_corr_7, blue_corr7 = blue_corr_7,
           nir_corr7 = nir_corr_7, swir1_corr7 = swir1_corr_7, swir2_corr7 = swir2_corr_7,
           temp_corr7 = surfacetemp_corr_7) %>%
    select(-c(sat_harmonize, correction, dswe, sat_to, method), -matches("^flag_.*_7$")) %>%
    mutate(atm_corr_LaSRC = as.numeric(mission %in% c("LC08", "LC09"))) %>%
    # apply_handoffs() does not preserve row order
    arrange(r_id, date, `system:index`)
  stopifnot("band harmonization changed the row count" = nrow(corr) == nrow(nw_rs))
  corr
}


# identical to the production notebook (and ransac_site_multiband() / ols_fit()
# in 01_filter_AquaMatch_Data.Rmd)
ols_fit <- function(t, y) {
  ok <- is.finite(t) & is.finite(y)
  if (sum(ok) < 2 || length(unique(t[ok])) < 2) {
    return(c(intercept = NA_real_, slope = NA_real_))
  }
  tt <- t[ok]; yy <- y[ok]
  tm <- mean(tt); ym <- mean(yy)
  slope <- sum((tt - tm) * (yy - ym)) / sum((tt - tm)^2)
  c(intercept = ym - slope * tm, slope = slope)
}

ransac_site_multiband <- function(date, band_mat,
                                  n_iter = 150,
                                  n_mad = 2.5,
                                  min_diff = c(red_corr7 = 0.01, green_corr7 = 0.01,
                                               blue_corr7 = 0.01, nir_corr7 = 0.01,
                                               swir1_corr7 = 0.01, swir2_corr7 = 0.01,
                                               temp_corr7 = 1),
                                  min_sample = 10,
                                  vote_frac_cutoff = 0.5) {
  n <- nrow(band_mat)
  nb <- ncol(band_mat)
  if (n < min_sample + 1) return(rep(NA, n))

  min_diff_b <- min_diff[colnames(band_mat)]
  t <- as.numeric(date)

  band_scale <- vapply(seq_len(nb), function(b) {
    y <- band_mat[, b]
    cf <- ols_fit(t, y)
    resid <- y - (cf["intercept"] + cf["slope"] * t)
    s <- mad(resid, na.rm = TRUE)
    if (is.na(s) || s == 0) s <- sd(resid, na.rm = TRUE)
    s
  }, numeric(1))

  inlier_votes <- rep(0L, n)
  n_valid_iter <- 0L

  for (i in 1:n_iter) {
    idx <- sample(n, min_sample)
    if (length(unique(t[idx])) < 2) next

    z <- matrix(NA_real_, nrow = n, ncol = nb)
    for (b in seq_len(nb)) {
      y <- band_mat[, b]
      if (length(unique(y[idx])) < 2 || is.na(band_scale[b]) || band_scale[b] == 0) next
      cf <- ols_fit(t[idx], y[idx])
      if (anyNA(cf)) next
      resid <- y - (cf["intercept"] + cf["slope"] * t)
      z[, b] <- abs(resid) / band_scale[b]
      z[abs(resid) < min_diff_b[b], b] <- 0
    }

    combined_z <- rowMeans(z, na.rm = TRUE)
    inliers <- !is.na(combined_z) & combined_z < n_mad
    inlier_votes <- inlier_votes + as.integer(inliers)
    n_valid_iter <- n_valid_iter + 1L
  }

  if (n_valid_iter == 0) return(rep(NA, n))

  best_inliers <- (inlier_votes / n_valid_iter) >= vote_frac_cutoff

  if (any(best_inliers)) {
    in_range <- vapply(seq_len(nb), function(b) {
      y <- band_mat[, b]
      accepted <- y[best_inliers]
      if (length(accepted) == 0 || all(is.na(accepted))) return(rep(TRUE, n))
      rng <- range(accepted, na.rm = TRUE)
      y >= rng[1] & y <= rng[2]
    }, logical(n))
    best_inliers <- best_inliers | apply(in_range, 1, all)
  }

  best_inliers
}

RANSAC_BAND_COLS <- c("red_corr7", "green_corr7", "blue_corr7",
                      "nir_corr7", "swir1_corr7", "swir2_corr7", "temp_corr7")


# Band RANSAC at both levels, each over the scenes that pass the earlier QA
# steps at that level, then the overall QA flags.
# - strict: the production notebook's cached flags (production_ransac_cache),
#   so a strict-QA row gets exactly the production notebook's result;
# - relaxed: run here (sequentially, as in the production notebook) and
#   cached to relaxed_ransac_cache, shared by both relaxed-QA notebooks.
add_ransac_flags <- function(corr, production_ransac_cache, relaxed_ransac_cache, force_refresh = FALSE) {
  strict_rows <- corr %>% filter(qa_pixel_strict, qa_temp, qa_scene_cloud_strict)
  strict_flags <- read_feather(production_ransac_cache) %>%
    filter(`system:index` %in% strict_rows$`system:index`) %>%
    rename(band_inlier_strict = is_band_inlier)
  stopifnot("strict-QA scenes missing from the production RANSAC cache - re-run NW_CLP_regional_application.Rmd" =
              all(strict_rows$`system:index` %in% strict_flags$`system:index`))

  relaxed_rows <- corr %>% filter(qa_pixel_relaxed, qa_temp, qa_scene_cloud_relaxed)
  if (!force_refresh && file.exists(relaxed_ransac_cache)) {
    relaxed_flags <- read_feather(relaxed_ransac_cache)
    stopifnot("relaxed RANSAC cache doesn't match the relaxed-QA scenes - set FORCE_REFRESH_CACHE <- TRUE" =
                setequal(relaxed_flags$`system:index`, relaxed_rows$`system:index`))
  } else {
    set.seed(47)
    t0 <- Sys.time()
    relaxed_flags <- relaxed_rows %>%
      arrange(r_id, date) %>%
      group_by(r_id) %>%
      group_split() %>%
      map(\(site_df) site_df %>%
            mutate(band_inlier_relaxed = ransac_site_multiband(date, as.matrix(pick(all_of(RANSAC_BAND_COLS))))) %>%
            select(`system:index`, band_inlier_relaxed)) %>%
      bind_rows()
    log("relaxed band RANSAC took %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
    dir.create(dirname(relaxed_ransac_cache), showWarnings = FALSE, recursive = TRUE)
    write_feather(relaxed_flags, relaxed_ransac_cache)
  }

  corr %>%
    left_join(strict_flags, by = "system:index") %>%
    left_join(relaxed_flags, by = "system:index") %>%
    mutate(qa_ransac_strict = qa_pixel_strict & qa_temp & qa_scene_cloud_strict & coalesce(band_inlier_strict, TRUE),
           qa_ransac_relaxed = qa_pixel_relaxed & qa_temp & qa_scene_cloud_relaxed & coalesce(band_inlier_relaxed, TRUE),
           pass_qa_strict = qa_ransac_strict,
           pass_qa_relaxed = if (APPLY_BAND_RANSAC) qa_ransac_relaxed else qa_pixel_relaxed & qa_temp & qa_scene_cloud_relaxed)
}


# rows (scene-site pairs) and sites passing every QA step so far, at each level
qa_cascade_log <- function(corr) {
  cascade <- function(level, pixel, scene, ransac) {
    steps <- list(pixel, pixel & corr$qa_temp, pixel & corr$qa_temp & scene, ransac)
    tibble(level = level,
           step = c("Pixel QA", "Median surface temperature 0-40 °C", "Scene cloud cover", "Band RANSAC"),
           n_rows = map_int(steps, sum),
           n_sites = map_int(steps, \(ok) n_distinct(corr$r_id[ok])))
  }
  bind_rows(
    tibble(level = "all", step = "Raw NW/CLP siteSR rows", n_rows = nrow(corr), n_sites = n_distinct(corr$r_id)),
    with(corr, cascade("strict", qa_pixel_strict, qa_scene_cloud_strict, qa_ransac_strict)),
    with(corr, cascade("relaxed", qa_pixel_relaxed, qa_scene_cloud_relaxed, qa_ransac_relaxed))
  )
}


# R port of the regional repository's add_spectral_indices()
add_spectral_indices <- function(df) {
  df %>%
    mutate(BR = blue_corr7 / red_corr7,
           BG = blue_corr7 / green_corr7,
           NR = nir_corr7 / red_corr7,
           GR = green_corr7 / red_corr7,
           fai = nir_corr7 - (red_corr7 + (swir1_corr7 - red_corr7) * ((830 - 660) / (1650 - 660))),
           NDVI = (nir_corr7 - red_corr7) / (nir_corr7 + red_corr7),
           NDSSI = (blue_corr7 - nir_corr7) / (blue_corr7 + nir_corr7),
           NDWI = (green_corr7 - nir_corr7) / (green_corr7 + nir_corr7),    # McFeeters 1996
           MNDWI = (green_corr7 - swir1_corr7) / (green_corr7 + swir1_corr7)) %>%  # Xu 2006
    mutate(across(c(BR, BG, NR, GR, fai, NDVI, NDSSI, NDWI, MNDWI),
                  ~ if_else(is.finite(.x), .x, NA_real_)))
}


# the export's columns, in order: identifiers, estimate, QA, applicability
EXPORT_ID_COLS <- c("r_id", "system_index", "date", "mission", "gnis_name", "lat", "lon")
EXPORT_QA_COLS <- c("pCount_dswe1", "pCount_dswe_gt0", "dswe1_share", "prop_clouds", "surface_temp_degC",
                    "scene_cloud_cover", "in_scene_metadata",
                    "qa_pixel_strict", "qa_pixel_relaxed", "qa_temp", "qa_scene_cloud_strict", "qa_scene_cloud_relaxed",
                    "band_inlier_strict", "band_inlier_relaxed", "pass_qa_strict", "pass_qa_relaxed")
EXPORT_CHECK_COLS <- c("pass_region", "pass_mission", "pass_season", "pass_complete", "pass_range", "pass_aoa",
                       "pass_plausible", "di", "out_of_range")

# Data dictionary for the export. `aoa_params` is the model's
# aoa_parameters.json; `model_label` names the model in the descriptions.
export_data_dictionary <- function(aoa_params, model_label, typical_error_m) {
  season_window <- aoa_params$season_window_doy
  tribble(
    ~column, ~description,
    "r_id", "Site identifier in this repository's RS pull.",
    "system_index", "Earth Engine image index of the scene the row was summarized from.",
    "date", "Landsat acquisition date.",
    "mission", "Landsat mission (LT04, LT05, LE07, LC08, LC09).",
    "gnis_name", "Waterbody name (NHD GNIS name); NA for unnamed waterbodies.",
    "lat, lon", "Site location (WGS84, decimal degrees).",
    "pred_sdd", sprintf("Ensemble-mean predicted Secchi disc depth (m) from the %s model, from all 40 boosters.", model_label),
    "pred_seed_sd", sprintf("Standard deviation of the 10 seed means (m): how much the ensemble members disagree. Not a prediction interval; typical error against in situ SDD is about %s m.", typical_error_m),
    "pCount_dswe1", "Pixels in the site buffer with DSWE = 1 (confident water).",
    "pCount_dswe_gt0", "Pixels in the site buffer with DSWE > 0 (water at any confidence).",
    "dswe1_share", "pCount_dswe1 / pCount_dswe_gt0.",
    "prop_clouds", "Proportion of the site buffer masked for cloud.",
    "surface_temp_degC", "Median surface temperature of the site buffer (°C).",
    "scene_cloud_cover", "AquaMatch scene-level CLOUD_COVER (%); NA if the scene is not in the AquaMatch scene metadata (mostly recent scenes); -1 if it was not calculated.",
    "in_scene_metadata", "TRUE if the scene is in the AquaMatch scene metadata.",
    "qa_pixel_strict", sprintf("dswe1_share > %s, pCount_dswe1 > %s, and prop_clouds <= %s (production / regional training QA).", STRICT_DSWE1_SHARE_MIN, STRICT_DSWE1_COUNT_MIN, STRICT_PROP_CLOUDS_MAX),
    "qa_pixel_relaxed", sprintf("dswe1_share > %s, pCount_dswe1 > %s, and prop_clouds <= %s.", DSWE1_SHARE_MIN, DSWE1_COUNT_MIN, PROP_CLOUDS_MAX),
    "qa_temp", "Median surface temperature between 0 and 40 °C (both levels).",
    "qa_scene_cloud_strict", sprintf("scene_cloud_cover < %s%%; scenes not in the metadata fail.", STRICT_CLOUD_COVER_MAX),
    "qa_scene_cloud_relaxed", sprintf("scene_cloud_cover < %s%%; scenes not in the metadata %s.", CLOUD_COVER_MAX, if (KEEP_SCENES_WITHOUT_METADATA) "pass" else "fail"),
    "band_inlier_strict", "Per-site multiband RANSAC result over the scenes passing the strict pixel, temperature, and scene checks: TRUE inlier, FALSE outlier, NA not evaluated (failed an earlier strict check, or the site has 10 or fewer such scenes).",
    "band_inlier_relaxed", "As band_inlier_strict, over the scenes passing the relaxed checks.",
    "pass_qa_strict", "Passes every strict QA step: pixel, temperature, scene cloud cover, and band RANSAC (a site RANSAC can't evaluate passes). The QA the model's training data passed.",
    "pass_qa_relaxed", sprintf("Passes every relaxed QA step%s.", if (APPLY_BAND_RANSAC) ", including band RANSAC" else " (band RANSAC not applied)"),
    "pass_region", "Applicability: site inside the model's training region (HUC4 basins of the six-state region). The applicability checks are cumulative: each pass_* is TRUE only if every earlier one is. They are independent of the QA flags.",
    "pass_mission", "Applicability: Landsat mission represented in training.",
    "pass_season", sprintf("Applicability: day of year within the training window (%s-%s).", season_window[1], season_window[2]),
    "pass_complete", "Applicability: every model input available.",
    "pass_range", "Applicability: every model input within its training range.",
    "pass_aoa", sprintf("Applicability: inside the area of applicability (di <= %.3f).", aoa_params$aoa_threshold),
    "pass_plausible", sprintf("Applicability: prediction no shallower than the training minimum (%s m). TRUE means the row passes every applicability check.", aoa_params$min_training_sdd_m),
    "di", sprintf("Dissimilarity index: weighted, standardized distance to the nearest training observation of the %s model (Meyer & Pebesma 2021); NA if an AOA input is missing.", model_label),
    "out_of_range", "Model inputs outside their training range, comma-separated; empty when none."
  )
}

# the QA thresholds, for the provenance file
qa_thresholds <- function() {
  list(strict = list(dswe1_share_min = STRICT_DSWE1_SHARE_MIN, dswe1_count_min = STRICT_DSWE1_COUNT_MIN,
                     prop_clouds_max = STRICT_PROP_CLOUDS_MAX, scene_cloud_cover_max = STRICT_CLOUD_COVER_MAX,
                     keep_scenes_without_metadata = FALSE, band_ransac = TRUE),
       relaxed = list(dswe1_share_min = DSWE1_SHARE_MIN, dswe1_count_min = DSWE1_COUNT_MIN,
                      prop_clouds_max = PROP_CLOUDS_MAX, scene_cloud_cover_max = CLOUD_COVER_MAX,
                      keep_scenes_without_metadata = KEEP_SCENES_WITHOUT_METADATA, band_ransac = APPLY_BAND_RANSAC),
       surface_temp_range_degC = SURFACE_TEMP_RANGE_K - 273.15)
}
