# summarize results

library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)

results_20km <- readRDS("Results/20_km_grid_prediction_results.RDS") %>%
  mutate(q=paste0("q=", as.character(q)))


ggplot(results_20km, aes(x=coverage, y=predicted_checklists, group=coverage, fill=q))+
  geom_violin()+
  stat_summary(fun='mean', geom='point', size=2, col='black')+
  coord_flip()+
  facet_wrap(q~year, ncol=6)+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Coverage")+
  ylab("Number of checklists")+
  scale_x_discrete(limits=c("85%", "90%", "95%", "100%"), 
                   breaks=c("85%", "90%", "95%", "100%"), 
                   labels=c("85%", "90%", "95%", "100%"))+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x=element_text(size=6))

ggsave("Figures/prelim_20km_fig.png", width=8.8, height=6.9, units="in")

results_20km %>%
  dplyr::select(q, year, model_r2) %>%
  distinct() %>%
  group_by(q) %>%
  summarize(mean_r2=mean(model_r2))
  
grids_20 <- st_read("Spatial data/20_km_grids_shape/study_extent_grids_20km.geojson")

plot_dat <- grids_20 %>%
  left_join(., results_20km, by="grid_id") %>%
  dplyr::filter(q %in% c("q=0", "q=2")) %>%
  dplyr::filter(coverage=="95%")

ggplot()+
  geom_sf(data=plot_dat, aes(fill=log10(predicted_checklists)))+
  facet_wrap(q~year, ncol=3)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):", breaks=c(1.8, 2.1, 2.4), labels=c(63, 126, 250))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))
  
ggsave("Figures/prelim_20km_fig_maps.png", width=8.8, height=6.9, units="in")

# how many grids have met the number of checklists needed
plot_dat2 <- grids_20 %>%
  left_join(., results_20km, by="grid_id") %>%
  dplyr::filter(q %in% c("q=0", "q=2")) %>%
  dplyr::filter(coverage=="95%") %>%
  mutate(sampled=ifelse(number_checklists>predicted_checklists, "Yes", "No"))

ggplot()+
  geom_sf(data=plot_dat2, aes(fill=sampled))+
  facet_wrap(q~year, ncol=3)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_d(name="Number of samples (log10):", breaks=c(1.8, 2.1, 2.4), labels=c(63, 126, 250))+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))

results_20km %>%
  dplyr::select(predicted_checklists, q, coverage, grid_id, year) %>%
  dplyr::filter(coverage=="95%") %>%
  dplyr::filter(year==2019) %>%
  pivot_wider(names_from="q", values_from="predicted_checklists") %>%
  ggplot(., aes(x=`q=0`, y=`q=2`))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")

ggplot(results_20km, aes(x=predicted_checklists, y=number_checklists))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(q~year, ncol=6)+
  scale_y_log10()+
  scale_x_log10()
  





results_5km <- readRDS("Results/5_km_grid_prediction_results.RDS") %>%
  mutate(q=paste0("q=", as.character(q)))


ggplot(results_5km, aes(x=coverage, y=predicted_checklists, group=coverage, fill=q))+
  geom_violin()+
  stat_summary(fun='mean', geom='point', size=2, col='black')+
  coord_flip()+
  facet_wrap(q~year, ncol=6)+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Coverage")+
  ylab("Number of checklists")+
  scale_x_discrete(limits=c("85%", "90%", "95%", "100%"), 
                   breaks=c("85%", "90%", "95%", "100%"), 
                   labels=c("85%", "90%", "95%", "100%"))+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x=element_text(size=6))

ggsave("Figures/prelim_5km_fig.png", width=8.8, height=6.9, units="in")

results_5km %>%
  dplyr::select(q, year, model_r2) %>%
  distinct() %>%
  group_by(q) %>%
  summarize(mean_r2=mean(model_r2))

grids_5 <- st_read("Spatial data/5_km_grids_shape/study_extent_grids_5km.geojson")

plot_dat <- grids_5 %>%
  left_join(., results_5km, by="grid_id") %>%
  dplyr::filter(q=="q=2") %>%
  dplyr::filter(coverage=="95%")

ggplot()+
  geom_sf(data=plot_dat, aes(fill=log10(predicted_checklists)))+
  facet_wrap(~year, ncol=3)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_viridis_c(name="Number of samples (log10):")+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(size=6))


