add_spatial_information <- function(data_file, spatial_info) {
  # check for out directory
  dir.create('4_separate_NW_CLP_data/out/')
  # get some info for parsing out the data to be joined
  filename <- str_split(data_file, "/")[[1]][4]
  # break out file prefix
  file_prefix <- str_split(filename, "_")[[1]][1]
  # and file type so we can merge files of the same type
  file_type <- str_split(filename, "_")[[1]][2]
  # and DSWE type
  DSWE <- str_split(filename, '_')[[1]][3]
  # left join with spatial info
  data <- read_feather(data_file) %>% 
    mutate(rowid = as.numeric(rowid)) %>% 
    left_join(., spatial_info)
  write_feather(data, 
                file.path('4_separate_NW_CLP_data/out/',
                          paste0(file_prefix,
                                 '_', file_type,
                                 '_', DSWE,
                                 '_for_analysis_v',
                                 Sys.getenv('collate_version'),
                                 '.feather')
                                ))
  file.path('4_separate_NW_CLP_data/out/',
            paste0(file_prefix,
                   '_', file_type,
                   '_', DSWE,
                   '_for_analysis_v',
                   Sys.getenv('collate_version'),
                   '.feather'))
}