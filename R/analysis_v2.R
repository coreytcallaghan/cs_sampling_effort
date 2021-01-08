# Now we know the number of checklists necessary to meet
# a specified sampling threshold
# for a number of grid cells
# this script will read in these data (processed from "run_grid_analysis_iNEXT.R")
# and make some plots and then run a model


library(ggplot2)
library(dplyr)
library(readr)
library(sf)
library(ggcorrplot)
library(GGally)
library(randomForest)

##
dat <- readRDS(paste0("Intermediate grid level data/iNEXT/", grid_size, "km_bcr31_", year, ".RDS"))

preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_size, "km.csv")) %>%
  dplyr::select(-`system:index`, -`.geo`)

# make some summary figures to help understand the data better
grid_ex <- dat %>%
  dplyr::filter(grid_id==4)

dat %>%
  dplyr::filter(coverage=="95%") %>%
  dplyr::filter(complete.cases(.)) %>%
  ggplot(., aes(x=x, y=number_checklists, group=as.factor(percent_cutoff)))+
  geom_point()+
  geom_smooth()+
  #scale_x_log10()+
  #scale_y_log10()+
  facet_wrap(~percent_cutoff)


dat %>%
  left_join(., preds, by="grid_id") %>%
  ggplot(., aes(x=Order.q, y=Estimate, group=grid_id, color=heterogeneity))+
  geom_line(size=0.05)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

dat %>%
  left_join(., preds, by="grid_id") %>%
  ggplot(., aes(x=Order.q, y=Estimate, group=grid_id, color=log10(number_checklists)))+
  geom_line(size=0.05)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

dat %>%
  dplyr::filter(Order.q %in% c(0, 2)) %>%
  left_join(., preds, by="grid_id") %>%
  ggplot(., aes(x=number_checklists, y=Estimate, color=log10(number_checklists)))+
  geom_point()+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~Order.q, scales="free_y")+
  xlab("Number of observed checklists")

dat %>%
  dplyr::filter(Order.q %in% c(0, 2)) %>%
  left_join(., preds, by="grid_id") %>%
  ggplot(., aes(x=number_checklists, y=Estimate, color=log10(total_SR)))+
  geom_point()+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~Order.q, scales="free_y")+
  xlab("Number of observed checklists")

dat %>%
  dplyr::filter(Order.q %in% c(0, 2)) %>%
  left_join(., preds, by="grid_id") %>%
  ggplot(., aes(x=heterogeneity, y=Estimate, color=log10(total_SR)))+
  geom_point()+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~Order.q, scales="free_y")+
  xlab("Heterogeneity")

dat %>%
  dplyr::filter(Order.q %in% c(0, 2)) %>%
  left_join(., preds, by="grid_id") %>%
  ggplot(., aes(x=number_checklists, y=total_SR))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~Order.q, scales="free")+
  xlab("Number of observed checklists")+
  ylab("Observed species richness")

filtered_dat <- dat %>%
  dplyr::filter(Order.q %in% c(0, 2)) %>%
  left_join(., preds, by="grid_id") %>%
  dplyr::select(number_checklists, Order.q, Estimate, total_SR,
                heterogeneity, `tree-coverfraction`,
                `urban-coverfraction`, `water-permanent-coverfraction`) %>%
  rename(tree=`tree-coverfraction`) %>%
  rename(urban=`urban-coverfraction`) %>%
  rename(water=`water-permanent-coverfraction`)

# see relationship
filtered_dat %>%
  dplyr::filter(Order.q==0) %>%
  dplyr::select(-Order.q) %>%
  cor(.) %>%
  ggcorrplot(lab=TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ggtitle(paste0(year, "; ", grid_size, "km; q=0"))

filtered_dat %>%
  dplyr::filter(Order.q==2) %>%
  dplyr::select(-Order.q) %>%
  cor(.) %>%
  ggcorrplot(lab=TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ggtitle(paste0(year, "; ", grid_size, "km; q=2"))



