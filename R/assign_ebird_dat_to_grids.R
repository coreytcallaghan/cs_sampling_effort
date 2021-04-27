# This script is used to assign data to a grid_id for each spatial extent
# and each analysis will be run at the grid level

# packages
library(sf)
library(dplyr)
library(tidyr)

# read in the grids
grids_5 <- readRDS("Spatial data/study_extent_grids_5km.RDS")
grids_10 <- readRDS("Spatial data/study_extent_grids_10km.RDS")
grids_15 <- readRDS("Spatial data/study_extent_grids_15km.RDS")
grids_20 <- readRDS("Spatial data/study_extent_grids_20km.RDS")
grids_25 <- readRDS("Spatial data/study_extent_grids_25km.RDS")
grids_30 <- readRDS("Spatial data/study_extent_grids_30km.RDS")

# make a function
assign_grids_to_dat <- function(file_name){
  
  # read in data
  dat <- readRDS(paste0("Data/", file_name))
  
  # get unique localities
  localities <- dat %>%
    dplyr::select(LOCALITY_ID, LONGITUDE, LATITUDE) %>%
    distinct()
  
  # create sf object
  localities_sf <- st_as_sf(localities, coords=c("LONGITUDE", "LATITUDE")) %>%
    st_set_crs(4326)
  
  # now assign each locality_id to a grid_id
  grids_5_lookup <- localities_sf %>%
    st_intersects(grids_5) %>%
    as.data.frame() %>%
    right_join(., localities %>%
                 mutate(row.id=1:nrow(.))) %>%
    rename(grid_id=col.id) %>%
    dplyr::select(-row.id)
  
  saveRDS(grids_5_lookup, paste0("Data/grid_lookups/5km_grid_lookup_", gsub("_data.RDS", "", file_name), ".RDS"))
  
  # now assign each locality_id to a grid_id
  grids_10_lookup <- localities_sf %>%
    st_intersects(grids_10) %>%
    as.data.frame() %>%
    right_join(., localities %>%
                 mutate(row.id=1:nrow(.))) %>%
    rename(grid_id=col.id) %>%
    dplyr::select(-row.id)
  
  saveRDS(grids_10_lookup, paste0("Data/grid_lookups/10km_grid_lookup_", gsub("_data.RDS", "", file_name), ".RDS"))
  
  # now assign each locality_id to a grid_id
  grids_15_lookup <- localities_sf %>%
    st_intersects(grids_15) %>%
    as.data.frame() %>%
    right_join(., localities %>%
                 mutate(row.id=1:nrow(.))) %>%
    rename(grid_id=col.id) %>%
    dplyr::select(-row.id)
  
  saveRDS(grids_15_lookup, paste0("Data/grid_lookups/15km_grid_lookup_", gsub("_data.RDS", "", file_name), ".RDS"))
  
  # now assign each locality_id to a grid_id
  grids_20_lookup <- localities_sf %>%
    st_intersects(grids_20) %>%
    as.data.frame() %>%
    right_join(., localities %>%
                 mutate(row.id=1:nrow(.))) %>%
    rename(grid_id=col.id) %>%
    dplyr::select(-row.id)
  
  saveRDS(grids_20_lookup, paste0("Data/grid_lookups/20km_grid_lookup_", gsub("_data.RDS", "", file_name), ".RDS"))
  
  # now assign each locality_id to a grid_id
  grids_25_lookup <- localities_sf %>%
    st_intersects(grids_25) %>%
    as.data.frame() %>%
    right_join(., localities %>%
                 mutate(row.id=1:nrow(.))) %>%
    rename(grid_id=col.id) %>%
    dplyr::select(-row.id)
  
  saveRDS(grids_25_lookup, paste0("Data/grid_lookups/25km_grid_lookup_", gsub("_data.RDS", "", file_name), ".RDS"))
  
  # now assign each locality_id to a grid_id
  grids_30_lookup <- localities_sf %>%
    st_intersects(grids_30) %>%
    as.data.frame() %>%
    right_join(., localities %>%
                 mutate(row.id=1:nrow(.))) %>%
    rename(grid_id=col.id) %>%
    dplyr::select(-row.id)
  
  saveRDS(grids_30_lookup, paste0("Data/grid_lookups/30km_grid_lookup_", gsub("_data.RDS", "", file_name), ".RDS"))
  
}


file_list <- c("bcr31_2014_data.RDS", "bcr31_2015_data.RDS",
               "bcr31_2016_data.RDS", "bcr31_2017_data.RDS",
               "bcr31_2018_data.RDS", "bcr31_2019_data.RDS")

# apply function over file list
lapply(file_list, assign_grids_to_dat)
