#' @title First-pass QAQC of RS feather file
#' 
#' @description
#' Function to make first-pass QAQC of the RS data to remove any rows where the
#' image quality is below 9 (out of 10), where the dswe1 count is less than 8; 
#' recode surface temp if prop_clouds != 0 or where sd_SurfaceTemp > 1
#'
#' @param filepath filepath of a collated .feather file output from the 
#' function "add_metadata.R"
#' @returns filepath of feather file, silently creates filtered .feather file from 
#' collated files in out folder 
#' 
#' 
baseline_QAQC_RS_data <- function(filepath) {
  collated <- read_feather(filepath)
  # do the actual QAQC pass and save the filtered file
  filtered <- collated %>%
    mutate(across(c(IMAGE_QUALITY, pCount_dswe1),
                  ~ as.numeric(.)))  %>% 
    filter(IMAGE_QUALITY >= 9, pCount_dswe1 >= 8) %>% 
    mutate(across(c(med_SurfaceTemp, mean_SurfaceTemp, sd_SurfaceTemp, min_SurfaceTemp),
                  ~ if_else(as.numeric(prop_clouds) != 0 | 
                              as.numeric(sd_SurfaceTemp) > 1,
                            NA_character_,
                            .)))
  # create file name for output, but only grab the short filename
  out_fn <- str_split(str_replace(filepath, "collated", "filtered"), "/")[[1]][3]
  # save it
  write_feather(filtered, 
                file.path("d_baseline_QAQC/out/",
                          out_fn))
  file.path("d_baseline_QAQC/out/",
            out_fn)
}
