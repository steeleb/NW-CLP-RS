# Source functions for this {targets} list
tar_source("d_baseline_QAQC/src/")

d_baseline_QAQC_list <- list(
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
  

  # pass the QAQC filter over each of the listed files, creating filtered files
  tar_target(
    name = d_QAQC_filtered_data,
    command = baseline_QAQC_RS_data(filepath = d_collated_files),
    packages = c("tidyverse", "feather"),
    pattern = map(d_collated_files)
  )
)