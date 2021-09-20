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
library(piecewiseSEM)

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

# get overall summary stats for 2019
# read in data
summarize_data_used_function <- function(grid_size){
  
  bird_dat <- readRDS(paste0("Data/bcr31_2019_data.RDS")) %>%
    left_join(., readRDS(paste0("Data/grid_lookups/", grid_size, "km_grid_lookup_bcr31_2019.RDS")))
  
  grid_summary <- bird_dat %>%
    group_by(grid_id) %>%
    summarize(number_checklists=length(unique(SAMPLING_EVENT_IDENTIFIER)),
              total_SR=length(unique(COMMON_NAME))) %>%
    dplyr::filter(number_checklists>=25)
  
  df_summary <- data.frame(number_grids=nrow(grid_summary),
                           total_checklists=length(unique(bird_dat$SAMPLING_EVENT_IDENTIFIER)),
                           total_SR=length(unique(bird_dat$COMMON_NAME)))
  
  }

overall_data_summary <- bind_rows(lapply(c(5, 10, 15, 20, 25, 30), summarize_data_used_function))

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
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_30_1_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_25_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_25_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_25_1_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_20_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_20_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_20_1_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_15_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_15_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_15_1_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_10_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_10_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_10_1_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_5_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_5_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/observed_prediction/v2_observed_", year_name, "_5_1_0.95.RDS"))) %>%
  mutate(type=case_when(Order.q==0 ~ "Rare species sensitive",
                        Order.q==1 ~ "Common species sensitive",
                        Order.q==2 ~ "Dominant species sensitive")) %>%
  mutate(type=factor(type, levels=c("Dominant species sensitive", "Common species sensitive", "Rare species sensitive")))

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
observed_prediction_all <- ggplot(observed_prediction_dat, aes(x=as.factor(grid_size), y=predicted_checklists, fill=as.factor(grid_size)))+
  geom_violin()+
  stat_summary(fun='mean', geom='point', size=2, col='black')+
  coord_flip()+
  facet_wrap(~type)+
  scale_y_log10()+
  scale_x_discrete(labels=c(5, 10, 15, 20, 25, 30), limits=c("30", "25", "20", "15", "10", "5"))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote("Grain size " (~km^2~"")))+
  ylab("Number of checklists to meet 95% completeness")+
  scale_fill_brewer(palette="Set1")+
  guides(fill=FALSE)

observed_prediction_all
ggsave("Figures/observed_checklists_necessary_all.png", height=5, width=6.5, units="in")

# Option 2 again
# after we show that dominant and 'common' species are both very similar!
# so now remove 'common species sensitive'
observed_prediction <- ggplot(observed_prediction_dat %>%
                                dplyr::filter(type != "Common species sensitive"), aes(x=as.factor(grid_size), y=predicted_checklists, fill=as.factor(grid_size)))+
  geom_violin()+
  stat_summary(fun='mean', geom='point', size=2, col='black')+
  coord_flip()+
  facet_wrap(~type)+
  scale_y_log10()+
  scale_x_discrete(labels=c(5, 10, 15, 20, 25, 30), limits=c("30", "25", "20", "15", "10", "5"))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote("Grain size " (~km^2~"")))+
  ylab("Number of checklists to meet 95% completeness")+
  scale_fill_brewer(palette="Set1")+
  guides(fill=FALSE)

observed_prediction
ggsave("Figures/observed_checklists_necessary.png", height=5, width=6.5, units="in")

# Option 3
ggplot(observed_prediction_dat, aes(x=as.factor(grid_size), y=predicted_checklists, fill=type))+
  geom_violin(position=position_dodge())+
  coord_flip()+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote("Grain size " (~km^2~"")))+
  ylab("Number of checklists to meet 95% completeness")+
  scale_fill_brewer(palette="Set1")

ggsave("Figures/observed_checklists_necessary.png", height=5, width=6.5, units="in")

# Get the summary results
# for the results text in paper
# get summary results
# mean number of checklists and sd
observed_prediction_dat %>%
  group_by(grid_size, type) %>%
  summarize(mean=mean(predicted_checklists),
            sd=sd(predicted_checklists),
            N=length(unique(grid_id)),
            min=min(predicted_checklists),
            max=max(predicted_checklists))

# Plot the number of checklists needed as a function of grid size
predicted_vs_grain_size <- observed_prediction_dat %>%
  dplyr::filter(type != "Common species sensitive") %>%
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
  theme(legend.title=element_blank())+
  theme(legend.position="bottom")

predicted_vs_grain_size
ggsave("Figures/checklists_needed_vs_grain_size.png", height=5, width=6.5, units="in")

# put the two together
observed_prediction + ggtitle("A") + predicted_vs_grain_size + ggtitle("B") + plot_layout(ncol=1)

ggsave("Figures/observed_predictions_AND_grain_size.png", height=7.8, width=6.2, units="in")

# full prediction data (mainly for spatial plotting)
full_prediction_dat <- readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_30_0_0.95.RDS")) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_30_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_30_1_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_25_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_25_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_25_1_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_20_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_20_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_20_1_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_15_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_15_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_15_1_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_10_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_10_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_10_1_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_5_0_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_5_2_0.95.RDS"))) %>%
  bind_rows(readRDS(paste0("Results/full_prediction/v2_full_", year_name, "_5_1_0.95.RDS"))) %>%
  mutate(type=case_when(Order.q==0 ~ "Rare species sensitive",
                        Order.q==1 ~ "Common species sensitive",
                        Order.q==2 ~ "Dominant species sensitive")) %>%
  mutate(type=factor(type, levels=c("Dominant species sensitive", "Common species sensitive", "Rare species sensitive")))

# summary of full prediction dat
full_prediction_dat %>%
  group_by(grid_size, type) %>%
  dplyr::filter(complete.cases(predicted_checklists)) %>%
  summarize(mean=mean(predicted_checklists),
            sd=sd(predicted_checklists),
            N=length(unique(grid_id)),
            R2=mean(model_r2))

full_prediction_dat %>%
  group_by(grid_size, type) %>%
  dplyr::filter(complete.cases(predicted_checklists)) %>%
  summarize(mean=mean(predicted_checklists),
            sd=sd(predicted_checklists),
            N=length(unique(grid_id)),
            R2=mean(model_r2)) %>%
  group_by(type) %>%
  summarize(mean_r2=mean(R2),
            min_r2=min(R2),
            max_r2=max(R2))

# test the distribution of predicted checklists needed
# for the observed grid cells and the predicted grid cells
full_prediction_dat %>%
  dplyr::filter(type != "Common species sensitive") %>%
  left_join(., observed_prediction_dat %>%
              dplyr::filter(type != "Common species sensitive") %>%
              dplyr::select(grid_id, grid_size) %>%
              distinct() %>%
              mutate(orange="Sampled")) %>%
  replace_na(list(orange="Unsampled")) %>%
  ggplot(., aes(x=predicted_checklists, fill=orange))+
  geom_density(alpha=0.8)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(type~grid_size, scales="free")+
  scale_fill_brewer(palette="Set1")+
  scale_x_log10()+
  theme(legend.title=element_blank())+
  xlab("Predicted checklists to meet 95% completeness")+
  ylab("Density")

ggsave("Figures/sampled_vs_unsampled_predictions.png", width=8.8, height=7.2, units="in")


# Now make a map showing the number of sampled
# needed to sample species richness in space
# Will do this for both 10 km2 grain size (and present these in the main results)
# and also 20 km grain size and present this in supplementary
# first for 10 km grain size
# make 10 km grid map
plot_dat_10 <- grids_10 %>%
  left_join(., full_prediction_dat %>%
              dplyr::filter(grid_size==10))

ggplot()+
  geom_sf(data=plot_dat_10, aes(fill=log10(predicted_checklists)))+
  facet_wrap(~type)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):", breaks=c(1.5, 2.0, 2.8), labels=c(32, 100, 631))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))+
  ggtitle(bquote("10 "~km^2~"grain size"))

ggsave("Figures/10_km_sampling_effort_map_all.png", height=6.6, width=6.9, units="in")

plot_dat_10 <- grids_10 %>%
  left_join(., full_prediction_dat %>%
              dplyr::filter(type !="Common species sensitive") %>%
              dplyr::filter(grid_size==10))

ggplot()+
  geom_sf(data=plot_dat_10, aes(fill=log10(predicted_checklists)))+
  facet_wrap(~type)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):", breaks=c(1.5, 2.0, 2.8), labels=c(32, 100, 631))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))+
  ggtitle(bquote("10 "~km^2~"grain size"))

ggsave("Figures/10_km_sampling_effort_map.png", height=6.6, width=6.9, units="in")

# Now make a similar map but for psecies richness
# based on the random forest for species richness
# will put the two map figures together in powerpoint!
SR_prediction <- readRDS(paste0("Results/SR_prediction/", year_name, "_10_0.RDS"))

sr_plot_dat_10 <- grids_10 %>%
  left_join(., SR_prediction) %>%
  mutate(title="Species richness")

ggplot()+
  geom_sf(data=sr_plot_dat_10, aes(fill=log10(predicted_SR)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Species richness (log10):", breaks=c(1.9, 2.0, 2.13), labels=c(79, 100, 135))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))+
  facet_wrap(~title)+
  theme(axis.text.y=element_blank())+
  theme(axis.ticks.y=element_blank())

ggsave("Figures/10_km_species_richness_map.png", height=6.4, width=6.6, units="in")

# Correlation between species richness
# and the number of checklists needed in each grid cell
sr_vs_predicted_checklists <- full_prediction_dat %>%
  dplyr::filter(grid_size==10) %>%
  dplyr::filter(type != "Common species sensitive") %>%
  dplyr::select(grid_id, type, predicted_checklists) %>%
  left_join(., SR_prediction %>%
              dplyr::select(grid_id, predicted_SR))

ggplot(sr_vs_predicted_checklists, aes(x=predicted_checklists, y=predicted_SR, color=type))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_x_log10()+
  scale_y_log10()+
  scale_color_brewer(palette="Set1")+
  theme(legend.position="none")+
  theme(legend.title=element_blank())+
  xlab("Predicted checklists to meet 95% completeness (log10)")+
  ylab("Predicted species diversity (log10)")

ggsave("Figures/predicted_checklists_vs_predicted_SR.png", width=6.75, height=3, units="in")

# make 20 km grid map
# for supplementary to show similar visual patterns
plot_dat_20 <- grids_20 %>%
  left_join(., full_prediction_dat %>%
              dplyr::filter(grid_size==20))

ggplot()+
  geom_sf(data=plot_dat_20, aes(fill=log10(predicted_checklists)))+
  facet_wrap(~type)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):", breaks=c(1.5, 2.0, 2.8), labels=c(32, 100, 631))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))+
  ggtitle(bquote("20 "~km^2~"grain size"))

ggsave("Figures/20_km_sampling_effort_map.png", height=6.6, width=6.9, units="in")

# Now make a similar map but for psecies richness
# based on the random forest for species richness
# will put the two map figures together in powerpoint!
SR_prediction <- readRDS(paste0("Results/SR_prediction/", year_name, "_20_0.RDS"))

sr_plot_dat_20 <- grids_20 %>%
  left_join(., SR_prediction) %>%
  mutate(title="Species richness")

ggplot()+
  geom_sf(data=sr_plot_dat_20, aes(fill=log10(predicted_SR)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Species richness (log10):", breaks=c(2.05, 2.15, 2.25), labels=c(112, 141, 178))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))+
  facet_wrap(~title)+
  theme(axis.text.y=element_blank())+
  theme(axis.ticks.y=element_blank())

ggsave("Figures/20_km_species_richness_map.png", height=6.4, width=6.6, units="in")



# look at one example sem result
sem_results <- readRDS(paste0("Results/sem_results/2019_20_0.RDS"))
plot(sem_results)


# Read in SEM results to summarize
# quick function
summarize_sem <- function(year_name, grid_resolution){
  
  sem_results <- readRDS(paste0("Results/sem_results/", year_name, "_", grid_resolution, "_", order, ".RDS"))
  
  df <- data.frame(summary(sem_results)$coefficients) %>%
    mutate(year=year_name) %>%
    mutate(grain_size=grid_resolution) 
}

sem_results <- bind_rows(lapply(c(30, 25, 20, 15, 10, 5), function(x){summarize_sem(2019, x)}))


# get 'mean' Std.Estimate to plot
# manually in powerpoint
mean_results <- sem_results %>%
  unite(combination, Response, Predictor, remove=FALSE) %>%
  group_by(combination) %>%
  summarize(mean_response=mean(Std.Estimate),
            sd_response=sd(Std.Estimate))

sem_results %>%
  mutate(Predictor=case_when(Predictor=="total_SR" ~ "Species richness",
                             Predictor=="urban" ~ "Urban cover",
                             Predictor=="heterogeneity" ~ "Heterogeneity",
                             Predictor=="water" ~ "Water cover",
                             Predictor=="tree" ~ "Tree cover")) %>%
  mutate(Response=case_when(Response=="log.number_checklists" ~ "Number of checklists",
                            Response=="total_SR" ~ "Species richness")) %>%
  unite(combination, Predictor, Response, sep=" -> ", remove=FALSE) %>%
  ggplot(., aes(x=grain_size, y=Std.Estimate))+
  geom_point()+
  facet_wrap(~combination, scales="free_y")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab(bquote("Grain size " (~km^2~"")))+
  ylab("Standardized psem estimate")+
  theme(strip.text=element_text(size=7.5))

ggsave("Figures/sem_grain_size_results.png", width=8.0, height=6.7, units="in")



