# summarizing results
# This script follows the 'analysis' script
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
grids_15 <- st_read("Spatial data/15_km_grids_shape/study_extent_grids_15km.geojson")
grids_20 <- st_read("Spatial data/20_km_grids_shape/study_extent_grids_20km.geojson")
grids_25 <- st_read("Spatial data/25_km_grids_shape/study_extent_grids_25km.geojson")
grids_30 <- st_read("Spatial data/30_km_grids_shape/study_extent_grids_30km.geojson")

# Will present the results for just 2019
# Can present results for 2014 as well
# in supplementary, but the main results will be for 2019 only in the main paper
# Obviously all the years are possible
# Just going to pick a year for now

year_name="2019"

# First will check the partial dependence data to make sure the
# completeness analysis 'makes sense' for both the mean completeness and the total completeness
# now read in the partial dependence data for this year
pd_dat <- readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_30_0.RDS")) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_30_2.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_25_0.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_25_2.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_20_0.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_20_2.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_15_0.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/partial_dependence_data/pd_", year_name, "_15_2.RDS"))) %>%
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
observed_prediction_dat <- readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_30_0_0.95.RDS")) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_30_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_25_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_25_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_20_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_20_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_15_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_15_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_10_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_10_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_5_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_5_2_0.95.RDS"))) %>%
  mutate(type=case_when(Order.q==0 ~ "Rare species sensitive",
                        Order.q==2 ~ "Common species sensitive"))

# Now plot the main results of the 'number of checklists'
# needed to sample biodiversity
# Three different options currently
# Option 1
ggplot(observed_prediction_dat, aes(x=type, y=predicted_checklists, fill=type))+
  geom_violin()+
  stat_summary(fun='mean', geom='point', size=2, col='black')+
  coord_flip()+
  facet_wrap(~grid_size)+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Number of checklists")+
  scale_fill_brewer(palette="Set1")+
  guides(fill=FALSE)

# Option 2
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

# Option 3
ggplot(observed_prediction_dat, aes(x=as.factor(grid_size), y=predicted_checklists, fill=type))+
  geom_violin(position=position_dodge())+
  coord_flip()+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Grain size (km2)")+
  ylab("Number of checklists to meet 95% completeness")+
  scale_fill_brewer(palette="Set1")

# Get the summary results
# for the results text in paper
# get summary results
observed_prediction_dat %>%
  group_by(grid_size, type) %>%
  summarize(mean=mean(predicted_checklists),
            sd=sd(predicted_checklists))

# Plot the number of checklists needed as a function of grid size
observed_prediction_dat %>%
  group_by(grid_size, type) %>%
  summarize(mean=mean(predicted_checklists),
            sd=sd(predicted_checklists)) %>%
  ggplot(., aes(x=grid_size, y=mean, color=type))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote("Grain size " (~km^2~"")))+
  ylab("Number of checklists to meet 95% completeness")+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank())


# full prediction data (mainly for spatial plotting)
full_prediction_dat <- readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_30_0_0.95.RDS")) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_30_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_25_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_25_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_20_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_20_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_15_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_15_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_10_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_10_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_5_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_5_2_0.95.RDS"))) %>%
  mutate(type=case_when(Order.q==0 ~ "Rare species sensitive",
                        Order.q==2 ~ "Common species sensitive"))

# summary of full prediction dat
full_prediction_dat %>%
  group_by(grid_size, type) %>%
  dplyr::filter(complete.cases(predicted_checklists)) %>%
  summarize(mean=mean(predicted_checklists),
            sd=sd(predicted_checklists))

# make 20 km grid map
plot_dat_20 <- grids_20 %>%
  left_join(., full_prediction_dat %>%
              dplyr::filter(grid_size==20))

ggplot()+
  geom_sf(data=plot_dat_20, aes(fill=log10(predicted_checklists)))+
  facet_wrap(~type)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):")+
  #scale_fill_viridis_c(name="Number of samples (log10):", breaks=c(1.8, 2.1, 2.4), labels=c(63, 126, 250))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))+
  ggtitle(bquote("20 "~km^2~"grids"))

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


# Read in SEM results to summarize
sem_results <- readRDS(paste0("Results/sem_results/", year_name, "_30_0.RDS")) %>%
  bind_rows(readRDS(paste0("Results/sem_results/", year_name, "_30_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/sem_results/", year_name, "_25_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/sem_results/", year_name, "_25_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_20_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_20_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_15_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_15_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_10_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_10_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_5_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_5_2_0.95.RDS"))) %>%
  mutate(type=case_when(Order.q==0 ~ "Rare species sensitive",
                        Order.q==2 ~ "Common species sensitive"))




