# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", "sf")
)

# source functions
tar_source(files = c(
  "a_locs_poly_setup.R",
  "b_site_RS_data_acquisition.R",
  "c_regional_RS_data_acquisition.R",
  "d_baseline_QAQC.R",
  "e_calculate_handoff_coefficients.R"
  ))
# ,
#   "f_apply_handoff_coefficients.R",
#   "g_separate_NW_CLP_data.R"
# ))

# Full targets list 
c(a_locs_poly_setup,
  b_site_RS_data,
  c_regional_RS_data,
  d_baseline_QAQC,
  e_calculate_handoff_coefficients
)
