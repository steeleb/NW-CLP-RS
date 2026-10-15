# Where the regional SDD model files come from, shared by every notebook in
# regional_SDD_application/. Each model folder (v3_production, v3_band_only)
# holds the per-seed feature lists and fold models, and the applicability
# files (applicability/aoa_parameters.json, aoa_reference.csv,
# training_region.geojson). Files are downloaded once and cached under
# MODEL_CACHE_ROOT, which every notebook shares. Paths are relative to the
# NW-CLP-RS repository root.
#
#   source("regional_SDD_application/regional_model_source.R")
#   fetch_model_file("seed601/xgboost_fold1.json", model = "production")
#
# Directions for applying the models:
# https://rossyndicate.github.io/regional-clarity-RS-model/applying_the_model.html

# GitHub, pinned to a commit so that a later retrain in the regional
# repository doesn't silently change these results. The production model
# files are unchanged from the previous pin (2c8ea05); this commit adds the
# band-only model and each model's training_region.geojson.
REGIONAL_REPO <- "rossyndicate/regional-clarity-RS-model"
REGIONAL_REPO_REF <- "381fe98a1aea74a098834baeb44ac2688e10df38"

# TODO: the models will be published on the Hugging Face Hub (the model cards
# and the regional repository's "Applying the model" page already name these
# repositories), but they are not public yet. Once they are, set MODEL_SOURCE
# to "huggingface" and pin HF_REVISION to the Hub commit of the release used.
# The Hub repositories hold each model folder's contents at their root.
MODEL_SOURCE <- "github" # "github" or "huggingface"
HF_REVISION <- "main"

MODEL_FOLDERS <- c(production = "v3_production", band_only = "v3_band_only")
HF_REPOS <- c(production = "bgsteele/intermountain-west-regional-clarity-sdd-landsat",
              band_only = "bgsteele/intermountain-west-regional-clarity-sdd-landsat-band-only")

model_source_ref <- function() {
  if (MODEL_SOURCE == "github") REGIONAL_REPO_REF else HF_REVISION
}

model_url <- function(rel_path, model) {
  switch(MODEL_SOURCE,
         github = sprintf("https://raw.githubusercontent.com/%s/%s/regional_clarity/xg_models/%s/%s",
                          REGIONAL_REPO, REGIONAL_REPO_REF, MODEL_FOLDERS[[model]], rel_path),
         huggingface = sprintf("https://huggingface.co/%s/resolve/%s/%s",
                               HF_REPOS[[model]], HF_REVISION, rel_path),
         stop("MODEL_SOURCE must be \"github\" or \"huggingface\""))
}

# downloaded files are cached under MODEL_CACHE_ROOT, keyed on the source and
# its ref (set MODEL_CACHE_ROOT before sourcing to cache elsewhere)
if (!exists("MODEL_CACHE_ROOT")) {
  MODEL_CACHE_ROOT <- "data/remoteSensing/IMW_regional_model_application/models"
}
model_cache_dir <- function(model) {
  file.path(MODEL_CACHE_ROOT, paste(MODEL_SOURCE, substr(model_source_ref(), 1, 7), sep = "_"),
            MODEL_FOLDERS[[model]])
}

# download one model-folder file (e.g. "seed601/xgboost_fold1.json") to the
# cache if it isn't there already, and return its local path
fetch_model_file <- function(rel_path, model = "production") {
  local_path <- file.path(model_cache_dir(model), rel_path)
  if (!file.exists(local_path)) {
    dir.create(dirname(local_path), showWarnings = FALSE, recursive = TRUE)
    download.file(model_url(rel_path, model), local_path, mode = "wb", quiet = TRUE)
  }
  local_path
}

# the cached path only, for notebooks that must not write to disk; stops if
# the production notebook (or band-only notebook) hasn't downloaded it yet
cached_model_file <- function(rel_path, model = "production") {
  local_path <- file.path(model_cache_dir(model), rel_path)
  stopifnot("model file not cached - run NW_CLP_regional_application.Rmd first" = file.exists(local_path))
  local_path
}

# each seed's features, in model order, as the regional repository's
# load_ensemble() reads them
seed_features <- function(seed, model = "production", fetch = fetch_model_file) {
  fromJSON(fetch(file.path(paste0("seed", seed), "backward_elim_summary.json"), model))$final_features
}

# the model source, for the exported outputs
model_provenance <- function(model = "production") {
  list(model = model,
       model_folder = MODEL_FOLDERS[[model]],
       source = MODEL_SOURCE,
       repository = if (MODEL_SOURCE == "github") REGIONAL_REPO else HF_REPOS[[model]],
       ref = model_source_ref(),
       model_card = if (MODEL_SOURCE == "github") {
         sprintf("https://github.com/%s/blob/%s/regional_clarity/xg_models/%s/README.md",
                 REGIONAL_REPO, REGIONAL_REPO_REF, MODEL_FOLDERS[[model]])
       } else {
         sprintf("https://huggingface.co/%s", HF_REPOS[[model]])
       },
       applying_the_model = "https://rossyndicate.github.io/regional-clarity-RS-model/applying_the_model.html")
}

# training-region check: TRUE where the site point (lon, lat; WGS84) falls
# inside the model's training_region.geojson (HUC4 basins of the six-state
# region). Points on the boundary count as inside, as in the regional
# repository's 07_regional_application.Rmd and apply_model.py.
read_training_region <- function(model = "production", fetch = fetch_model_file) {
  sf::st_read(fetch("applicability/training_region.geojson", model), quiet = TRUE) %>%
    sf::st_geometry() %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_union()
}

in_region <- function(lon, lat, training_region) {
  pts <- sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326, na.fail = FALSE)
  lengths(sf::st_intersects(pts, training_region)) > 0
}

# the model inputs outside their training range, comma-separated ("" when
# none) - the same out_of_range column 07 and apply_model.py return
out_of_range <- function(df, feature_ranges) {
  names_out <- rep("", nrow(df))
  for (i in seq_len(nrow(feature_ranges))) {
    x <- df[[feature_ranges$feature[i]]]
    outside <- !is.na(x) & (x < feature_ranges$min[i] | x > feature_ranges$max[i])
    names_out <- paste0(names_out, if_else(outside, paste0(feature_ranges$feature[i], ","), ""))
  }
  sub(",$", "", names_out)
}
