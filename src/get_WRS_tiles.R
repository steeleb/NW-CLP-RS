#' @title Make list of WRS tiles to map over
#' 
#' @description
#' Function to use the optimal shapefile from get_WRS_detection() to define
#' the list of WRS2 tiles for branching
#' 
#' @param detection_method optimal shapefile from get_WRS_detection()
#' @param yaml contents of the yaml .csv file
#' @param locs sf object of user-provided locations for Landsat acquisition
#' @param poly sf object of polygon areas for Landsat acquisition
#' @param centers sf object of polygon centers for Landsat acquisition
#' @param parent_path parent filepath where the RS run is occurring
#' 
#' @returns list of WRS2 tiles and silently saves a .csv for the python scripts
#' 
#' 
get_WRS_tiles <- function(detection_method, 
                          yaml, 
                          locs, 
                          poly, 
                          centers,
                          parent_path) {
  WRS <- read_sf(file.path(parent_path, "in/WRS2_descending.shp"))
  if (detection_method == "site") {
    locs <- st_as_sf(locs, 
                     coords = c("Longitude", "Latitude"), 
                     crs = yaml$location_crs)
    if (st_crs(locs) == st_crs(WRS)) {
      WRS_subset <- WRS[locs,]
    } else {
      locs <- st_transform(locs, st_crs(WRS))
      WRS_subset <- WRS[locs,]
    }
    write_csv(st_drop_geometry(WRS_subset), 
              file.path(parent_path, "run/WRS_subset_list.csv"))
    return(WRS_subset$PR)
  } else {
    if (detection_method == "centers") {
      if (st_crs(centers) == st_crs(WRS)) {
        WRS_subset <- WRS[centers,]
      } else {
        centers <- st_transform(centers, st_crs(WRS))
        WRS_subset <- WRS[centers,]
      }
      write_csv(st_drop_geometry(WRS_subset), 
                file.path(parent_path, "run/WRS_subset_list.csv"))
      return(WRS_subset$PR)
    } else {
      if (detection_method == "polygon") {
        if (st_crs(poly) == st_crs(WRS)) {
          WRS_subset <- WRS[poly,]
        } else {
          poly <- st_transform(poly, st_crs(WRS))
          WRS_subset <- WRS[poly,]
        }
        write_csv(st_drop_geometry(WRS_subset), 
                  file.path(parent_path, "run/WRS_subset_list.csv"))
        return(WRS_subset$PR)
      }
    }
  }
}

