# compare the bootstrapped estimates with the
# total estimates

library(tidyverse)
library(ggplot2)

# just for 2014
# and grid size = 20

year=2019
grid_size=20

total <- readRDS(paste0("Intermediate grid level data/sampling_profile/", grid_size, "km_bcr31_", year, ".RDS"))

boot <- readRDS(paste0("Intermediate grid level data/sampling_profile_boot/", grid_size, "km_bcr31_", year, ".RDS"))

# get the average estimate
# across the bootstrapped repeated measures
summary <- boot %>%
  group_by(grid_id, Order.q) %>%
  summarize(mean_Estimate=mean(Estimate),
            sd_estimate=sd(Estimate)) %>%
  mutate(type=case_when(Order.q==0 ~ "Rare species sensitive",
                        Order.q==2 ~ "Dominant species sensitive"))

# correlation between the mean estimate
# when only sampling 10 checklists at a time
# and the total observed estimate of completeness
summary %>%
  left_join(., total, by=c("grid_id", "Order.q")) %>%
  dplyr::filter(Order.q %in% c(0, 2.0)) %>%
  ggplot(., aes(x=mean_Estimate, y=Estimate, color=log10(number_checklists)))+
  geom_point()+
  xlab("Bootstrapped mean sampling completeness")+
  ylab("Total observed sampling completness")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~type, scales="free")+
  scale_color_viridis_c(name="Number of checklists (log10)")+
  theme(legend.position="bottom")

ggsave("Figures/mean_completeness_vs_total_completeness.png", height=5.7, width=7.1, units="in")

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
  geom_smooth(method="lm")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Bootstrapped estimate (mean)")


total %>%
  left_join(., preds, by="grid_id") %>%
  dplyr::filter(Order.q %in% c(0, 2.0)) %>%
  ggplot(., aes(x=heterogeneity, y=Estimate))+
  geom_point()+
  facet_wrap(~Order.q, scales="free")+
  geom_smooth(method="lm")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Total observed estimate")





