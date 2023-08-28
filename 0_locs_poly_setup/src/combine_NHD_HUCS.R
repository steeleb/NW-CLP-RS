combine_NHD_HUCS <- function() {
  huc4_poly_files <- list.files('0_locs_poly_setup/out/', full.names = TRUE) %>% 
    .[grepl('huc4', .)]
  map_dfr(huc4_poly_files, read_sf)
}