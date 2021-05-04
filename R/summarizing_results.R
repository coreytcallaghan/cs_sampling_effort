# summarizing results
# This script follows the 'analysis_v3' script
# and reads in stuff to make figures etc.

# packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(sf)
library(patchwork)

# read in grids spatial data
grids_5 <- st_read("Spatial data/5_km_grids_shape/study_extent_grids_5km.geojson")
grids_10 <- st_read("Spatial data/10_km_grids_shape/study_extent_grids_10km.geojson")
grids_20 <- st_read("Spatial data/20_km_grids_shape/study_extent_grids_20km.geojson")

# Just going to pick a year for now

year_name="2019"


# now read in the partial dependence data for 2017
pd_dat <- readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_20_0.RDS")) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_20_2.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_10_0.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_10_2.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_5_0.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_5_2.RDS")))

grid_10_pd <- pd_dat %>%
  dplyr::filter(grid_size==10) %>%
  ggplot(., aes(x=completeness, y=number_checklists, color=variable))+
  geom_line()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1")+
  facet_wrap(Order.q~variable, scales="free")+
  ggtitle("10km2 grid")

grid_10_pd

grid_20_pd <- pd_dat %>%
  dplyr::filter(grid_size==20) %>%
  ggplot(., aes(x=completeness, y=number_checklists, color=variable))+
  geom_line()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1")+
  facet_wrap(Order.q~variable, scales="free")+
  ggtitle("20km2 grid")

grid_20_pd

grid_5_pd <- pd_dat %>%
  dplyr::filter(grid_size==5) %>%
  ggplot(., aes(x=completeness, y=number_checklists, color=variable))+
  geom_line()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1")+
  facet_wrap(Order.q~variable, scales="free")+
  ggtitle("5km2 grid")

grid_5_pd

grid_20_pd + grid_10_pd + grid_5_pd + plot_layout(ncol=1)

# observed prediction data
observed_prediction_dat <- readRDS(paste0("Results/observed_prediction/observed_", year_name, "_20_0_0.95.RDS")) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/observed_", year_name, "_20_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/observed_", year_name, "_10_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/observed_", year_name, "_10_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/observed_", year_name, "_5_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/observed_", year_name, "_5_2_0.95.RDS"))) %>%
  mutate(type=case_when(Order.q==0 ~ "Rare species sensitive",
                        Order.q==2 ~ "Common species sensitive"))

ggplot(observed_prediction_dat, aes(x=type, y=predicted_checklists, fill=type))+
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
  facet_wrap(~type)+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Grain size (km2)")+
  ylab("Number of checklists to meet 95% completeness")+
  scale_fill_brewer(palette="Set1")+
  guides(fill=FALSE)

observed_prediction_dat %>%
  group_by(grid_size, type) %>%
  summarize(mean=mean(predicted_checklists))

#OR

ggplot(observed_prediction_dat, aes(x=as.factor(grid_size), y=predicted_checklists, fill=as.factor(Order.q)))+
  geom_violin(position=position_dodge())+
  #stat_summary(fun='mean', geom='point', size=2, col='black')+
  coord_flip()+
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
plot_dat_20 <- grids_20 %>%
  left_join(., full_prediction_dat %>%
              dplyr::filter(grid_size==20))

ggplot()+
  geom_sf(data=plot_dat_20, aes(fill=log10(predicted_checklists)))+
  facet_wrap(~Order.q)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):")+
  #scale_fill_viridis_c(name="Number of samples (log10):", breaks=c(1.8, 2.1, 2.4), labels=c(63, 126, 250))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))+
  ggtitle("20 km2 grids")

# make 10 km grid map
plot_dat_10 <- grids_10 %>%
  left_join(., full_prediction_dat %>%
              dplyr::filter(grid_size==10))

ggplot()+
  geom_sf(data=plot_dat_10, aes(fill=log10(predicted_checklists)))+
  facet_wrap(~Order.q)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):")+
  #scale_fill_viridis_c(name="Number of samples (log10):", breaks=c(1.8, 2.1, 2.4), labels=c(63, 126, 250))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))+
  ggtitle("10 km2 grids")

# full prediction data (mainly for spatial plotting)
full_prediction_dat2 <- readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_20_0_0.95.RDS")) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_20_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_10_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_10_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_5_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_5_2_0.95.RDS")))

# make 20 km grid map
plot_dat_20.2 <- grids_20 %>%
  left_join(., full_prediction_dat2 %>%
              dplyr::filter(grid_size==20))

ggplot()+
  geom_sf(data=plot_dat_20.2, aes(fill=log10(predicted_checklists)))+
  facet_wrap(~Order.q)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):")+
  #scale_fill_viridis_c(name="Number of samples (log10):", breaks=c(1.8, 2.1, 2.4), labels=c(63, 126, 250))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))+
  ggtitle("20 km2 grids")

# make 10 km grid map
plot_dat_10.2 <- grids_10 %>%
  left_join(., full_prediction_dat2 %>%
              dplyr::filter(grid_size==10)) %>%
  mutate(type=case_when(Order.q==0 ~ "Rare species sensitive",
                        Order.q==2 ~ "Common species sensitive"))

ggplot()+
  geom_sf(data=plot_dat_10.2, aes(fill=log10(predicted_checklists)))+
  facet_wrap(~type)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):")+
  #scale_fill_viridis_c(name="Number of samples (log10):", breaks=c(1.8, 2.1, 2.4), labels=c(63, 126, 250))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))+
  ggtitle("10 km2 grids")







