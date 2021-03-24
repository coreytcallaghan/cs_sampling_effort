# summarizing results
# This script follows the 'analysis_v3' script
# and reads in stuff to make figures etc.

# packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(sf)

# read in grids spatial data
grids_5 <- st_read("Spatial data/5_km_grids_shape/study_extent_grids_5km.geojson")
grids_10 <- st_read("Spatial data/10_km_grids_shape/study_extent_grids_10km.geojson")
grids_20 <- st_read("Spatial data/20_km_grids_shape/study_extent_grids_20km.geojson")

# Just going to pick a year for now

year_name="2017"


# now read in the partial dependence data for 2017
pd_dat <- readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_20_0.RDS")) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_20_2.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_10_0.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_10_2.RDS")))

pd_dat %>%
  dplyr::filter(grid_size==10) %>%
  ggplot(., aes(x=completeness, y=number_checklists, color=variable))+
  geom_line()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1")+
  facet_wrap(Order.q~variable, scales="free")

# observed prediction data
observed_prediction_dat <- readRDS(paste0("Results/observed_prediction/observed_", year_name, "_20_0_0.95.RDS")) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/observed_", year_name, "_20_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/observed_", year_name, "_10_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/observed_", year_name, "_10_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/observed_", year_name, "_5_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/observed_", year_name, "_5_2_0.95.RDS")))

ggplot(observed_prediction_dat, aes(x=as.factor(Order.q), y=predicted_checklists, fill=as.factor(Order.q)))+
  geom_violin()+
  stat_summary(fun='mean', geom='point', size=2, col='black')+
  coord_flip()+
  facet_wrap(~grid_size)+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Order (q)")+
  ylab("Number of checklists")+
  scale_fill_brewer(palette="Set1")

#OR

ggplot(observed_prediction_dat, aes(x=as.factor(grid_size), y=predicted_checklists, fill=as.factor(grid_size)))+
  geom_violin()+
  stat_summary(fun='mean', geom='point', size=2, col='black')+
  coord_flip()+
  facet_wrap(~Order.q)+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Grid size (km2)")+
  ylab("Number of checklists to meet 95% completeness")+
  scale_fill_brewer(palette="Set1")


# full prediction data (mainly for spatial plotting)
full_prediction_dat <- readRDS(paste0("Results/full_prediction/full_", year_name, "_20_0_0.95.RDS")) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/full_", year_name, "_20_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/full_", year_name, "_10_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/full_", year_name, "_10_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/full_", year_name, "_5_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/full_", year_name, "_5_2_0.95.RDS")))

# make 20 km grid map
plot_dat <- grids_20 %>%
  left_join(., full_prediction_dat %>%
              dplyr::filter(grid_size==20))

plot_dat <- grids_20 %>%
  left_join(., results_20km, by="grid_id") %>%
  dplyr::filter(q %in% c("q=0", "q=2")) %>%
  dplyr::filter(coverage=="95%")

ggplot()+
  geom_sf(data=plot_dat, aes(fill=log10(predicted_checklists)))+
  facet_wrap(~model_r2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):")+
  #scale_fill_viridis_c(name="Number of samples (log10):", breaks=c(1.8, 2.1, 2.4), labels=c(63, 126, 250))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))







