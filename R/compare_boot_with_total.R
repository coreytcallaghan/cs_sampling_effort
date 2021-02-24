# compare the bootstrapped estimates with the
# total estimates

library(tidyverse)
library(ggplot2)

# just for 2014
# and grid size = 20

year=2014
grid_size=20

total <- readRDS(paste0("Intermediate grid level data/sampling_profile/", grid_size, "km_bcr31_", year, ".RDS"))

boot <- readRDS(paste0("Intermediate grid level data/sampling_profile_boot/", grid_size, "km_bcr31_", year, ".RDS"))

# get the average estimate
# across the bootstrapped repeated measures
summary <- boot %>%
  group_by(grid_id, Order.q) %>%
  summarize(mean_Estimate=mean(Estimate),
            sd_estimate=sd(Estimate))

# correlation between the mean estimate
# when only sampling 10 checklists at a time
# and the total observed estimate of completeness
summary %>%
  left_join(., total, by=c("grid_id", "Order.q")) %>%
  dplyr::filter(Order.q %in% c(0, 2.0)) %>%
  ggplot(., aes(x=mean_Estimate, y=Estimate, color=log10(number_checklists)))+
  geom_point()+
  xlab("bootstrapped mean estimate")+
  ylab("total estimate")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~Order.q, scales="free")

preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_size, "km.csv")) %>%
  dplyr::select(-`system:index`, -`.geo`)

# relationship between heterogeneity
# and bootstrapped estimate
summary %>%
  left_join(., preds, by="grid_id") %>%
  dplyr::filter(Order.q %in% c(0, 2.0)) %>%
  ggplot(., aes(x=heterogeneity, y=mean_Estimate))+
  geom_point()+
  facet_wrap(~Order.q, scales="free")+
  geom_smooth(method="lm")


total %>%
  left_join(., preds, by="grid_id") %>%
  dplyr::filter(Order.q %in% c(0, 2.0)) %>%
  ggplot(., aes(x=heterogeneity, y=Estimate))+
  geom_point()+
  facet_wrap(~Order.q, scales="free")+
  geom_smooth(method="lm")





