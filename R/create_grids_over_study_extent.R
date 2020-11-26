# An R script to make grids over the study region
# of varying resolution

# packages
library(sf)
library(ggplot2)
library(dplyr)

# read in bcr map
bcrs <- st_read("Spatial data/bcr_terrestrial_shape/BCR_Terrestrial_master.shp")

plot(bcrs)

bcr_31 <- bcrs %>%
  dplyr::filter(BCR==31) %>%
  st_transform(26917)

plot(bcr_31)

# write out the study extent maybe for making a map later...
bcr_31 %>%
  st_transform(4326) %>%
  st_write(., "Spatial data/bcr_31_extent.geojson")

# make a grid over the bcr of different sizes
grid_10 <- st_make_grid(bcr_31, cellsize=10000) %>%
  st_as_sf() 

grid_10_inter <- grid_10 %>%
  st_intersects(bcr_31) %>%
  as.data.frame()

grid_10_trimmed <- grid_10 %>%
  mutate(row.id=1:nrow(.)) %>%
  dplyr::filter(row.id %in% grid_10_inter$row.id) %>%
  dplyr::select(-row.id) %>%
  mutate(grid_id=1:nrow(.)) %>%
  dplyr::select(grid_id, x) %>%
  rename(geometry=x) %>%
  st_transform(4326)

# write out 10 km grids
saveRDS(grid_10_trimmed, "Spatial data/study_extent_grids_10km.RDS")
st_write(grid_10_trimmed, "Spatial data/10_km_grids_shape/study_extent_grids_10km.shp")
st_write(grid_10_trimmed, "Spatial data/10_km_grids_shape/study_extent_grids_10km.geojson")

# repeat above, but for 5 km grids
grid_5 <- st_make_grid(bcr_31, cellsize=5000) %>%
  st_as_sf() 

grid_5_inter <- grid_5 %>%
  st_intersects(bcr_31) %>%
  as.data.frame()

grid_5_trimmed <- grid_5 %>%
  mutate(row.id=1:nrow(.)) %>%
  dplyr::filter(row.id %in% grid_5_inter$row.id) %>%
  dplyr::select(-row.id) %>%
  mutate(grid_id=1:nrow(.)) %>%
  dplyr::select(grid_id, x) %>%
  rename(geometry=x) %>%
  st_transform(4326)

# write out 5 km grids
saveRDS(grid_5_trimmed, "Spatial data/study_extent_grids_5km.RDS")
st_write(grid_5_trimmed, "Spatial data/5_km_grids_shape/study_extent_grids_5km.shp")
st_write(grid_5_trimmed, "Spatial data/5_km_grids_shape/study_extent_grids_5km.geojson")

# repeat above, but for 1 km grids
grid_1 <- st_make_grid(bcr_31, cellsize=1000) %>%
  st_as_sf() 

grid_1_inter <- grid_1 %>%
  st_intersects(bcr_31) %>%
  as.data.frame()

grid_1_trimmed <- grid_1 %>%
  mutate(row.id=1:nrow(.)) %>%
  dplyr::filter(row.id %in% grid_1_inter$row.id) %>%
  dplyr::select(-row.id) %>%
  mutate(grid_id=1:nrow(.)) %>%
  dplyr::select(grid_id, x) %>%
  rename(geometry=x) %>%
  st_transform(4326)

# write out 1 km grids
saveRDS(grid_1_trimmed, "Spatial data/study_extent_grids_1km.RDS")
st_write(grid_1_trimmed, "Spatial data/1_km_grids_shape/study_extent_grids_1km.shp")
st_write(grid_1_trimmed, "Spatial data/1_km_grids_shape/study_extent_grids_1km.geojson")

# repeat above, but for 20 km grids
grid_20 <- st_make_grid(bcr_31, cellsize=20000) %>%
  st_as_sf() 

grid_20_inter <- grid_20 %>%
  st_intersects(bcr_31) %>%
  as.data.frame()

grid_20_trimmed <- grid_20 %>%
  mutate(row.id=1:nrow(.)) %>%
  dplyr::filter(row.id %in% grid_20_inter$row.id) %>%
  dplyr::select(-row.id) %>%
  mutate(grid_id=1:nrow(.)) %>%
  dplyr::select(grid_id, x) %>%
  rename(geometry=x) %>%
  st_transform(4326)

# write out 20 km grids
saveRDS(grid_20_trimmed, "Spatial data/study_extent_grids_20km.RDS")
st_write(grid_20_trimmed, "Spatial data/20_km_grids_shape/study_extent_grids_20km.shp")
st_write(grid_20_trimmed, "Spatial data/20_km_grids_shape/study_extent_grids_20km.geojson")
