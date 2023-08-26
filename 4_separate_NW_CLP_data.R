# Source functions for this {targets} list
tar_source("4_separate_NW_CLP_data/src/")

# Separate NW and CLP data and save to Drive -------------

# This set of functions join the location information back with the collated 
# data, filters for NW and CLP reservoirs/lakes, and then saves the files in
# the ROSSyndicate Google Drive

p4_targets_list <- list(
  # join collated, corrected GEE output with spatial information.
  tar_target(
    name = p4_add_spatial_info_NW_CLP_points,
    command = {
      p3_make_DSWE1_correction_figures
      add_spatial_information(p3_DSWE1_corrected_file_list %>% 
                                .[grepl('Historical_point', .)], 
                              p0_collated_pts_file)
    },
    packages = c('tidyverse', 'feather')
  ),
  # track the output file
  tar_file_read(
    name = p4_NW_CLP_dataset_with_info,
    command = p4_add_spatial_info_NW_CLP_points,
    read = read_feather(!!.x),
    packages = 'feather'
  ),
  # subset the files for CLP data
  tar_target(
    name = p4_subset_points_for_CLP,
    command = subset_file_by_data_group(p4_add_spatial_info_NW_CLP_points, 'CLP'),
    packages = c('tidyverse', 'feather'),
  ),
  # track the output file
  tar_file_read(
    name = p4_CLP_dataset_with_info,
    command = p4_subset_points_for_CLP,
    read = read_feather(!!.x),
    packages = 'feather'
  ),
  # track the ROSS CLP list
  tar_file_read(
    name = p4_ROSS_CLP_subset,
    command = '~/OneDrive - Colostate/misc/CPL/upper_poudre_lakes_v2.csv',
    read = read_csv(!!.x),
    packages = 'readr'
  )
  #,
  # # subset the files for ROSS CLP data
  # tar_target(
  #   name = p4_subset_points_for_ROSS_CLP,
  #   command = subset_file_by_(p4_NW_CLP_dataset_with_info, 'CLP'),
  #   packages = c('tidyverse', 'feather'),
  # ),
  # # track the output file
  # tar_file_read(
  #   name = p4_CLP_dataset_with_info,
  #   command = p4_subset_points_for_CLP,
  #   read = read_feather(!!.x),
  #   packages = 'feather'
  # ),
  # # subset the files fr NW data
  # tar_target(
  #   name = p4_subset_for_NW
  # ),
  # # save all files to drive
  # tar_target(
  #   name = p4_save_files_to_drive
  # )
)
  
  