# Source functions for this {targets} list
tar_source("d_baseline_QAQC/src/")

d_baseline_QAQC <- list(

  # check for proper directory structure ------------------------------------
  
  tar_target(
    name = d_check_dir_structure,
    command = {
      directories = c("d_baseline_QAQC/out/")
      walk(directories, function(dir) {
        if(!dir.exists(dir)){
          dir.create(dir)
        }
      })
    }
  ),
  
  # make a list of the collated files (regional and local) to map over for the 
  # baseline QAQC
  tar_target(
    name = d_collated_files,
    command = c(b_make_files_with_metadata, # site data
                c_make_files_with_metadata) # regional data
  ),

  # pass the QAQC filter over each of the listed files, creating filtered files
  tar_target(
    name = d_QAQC_filtered_data,
    command = {
      d_check_dir_structure
      baseline_QAQC_RS_data(filepath = d_collated_files)
      },
    packages = c("tidyverse", "feather"),
    pattern = map(d_collated_files)
  )
)