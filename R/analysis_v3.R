# This script is used to assess the relationships
# between completeness/sample coverage
# and predictor variables
# there is a large nested design to test here

# packages
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(sf)
library(ggcorrplot)
library(GGally)
library(randomForest)
library(patchwork)


# read in the data
summarize_data_function <- function(year, grid_size){
  
  dat <- readRDS(paste0("Intermediate grid level data/sampling_profile_boot/", grid_size, "km_bcr31_", year, ".RDS")) %>%
    dplyr::filter(Order.q %in% c(0, 2))
  
  preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_size, "km.csv")) %>%
    dplyr::select(-`system:index`, -`.geo`)
  
  # summarized data
  # across the bootstrap estimates
  summarized_data <- dat %>%
    dplyr::filter(Estimate>0) %>%
    group_by(Order.q, grid_id, sample_level) %>%
    summarize(mean_completeness=mean(Estimate),
              sd_completeness=sd(Estimate))
  
  # test to see if the completeness values
  # are correlated among
  # bootstrap levels 
  summarized_data %>%
    dplyr::filter(Order.q==0) %>%
    ungroup() %>%
    dplyr::select(grid_id, sample_level, mean_completeness) %>%
    pivot_wider(names_from=sample_level, values_from=mean_completeness) %>%
    dplyr::select(-grid_id) %>%
    ggpairs(.)+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    ggtitle("q=0")
  
  ggsave(paste0("Figures/sampling_design_assessment/", year, "_", grid_size, "_q=0.png"),
         width=7.4, height=7.1, units="in")
  
  summarized_data %>%
    dplyr::filter(Order.q==2) %>%
    ungroup() %>%
    dplyr::select(grid_id, sample_level, mean_completeness) %>%
    pivot_wider(names_from=sample_level, values_from=mean_completeness) %>%
    dplyr::select(-grid_id) %>%
    ggpairs(.)+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    ggtitle("q=2")
  
  ggsave(paste0("Figures/sampling_design_assessment/", year, "_", grid_size, "_q=2.png"),
         width=7.4, height=7.1, units="in")
  
  # let's now use 10 checklists as our cutoff
  # and save this dataframe for further analysis
  filtered_dat <- summarized_data %>%
    dplyr::filter(sample_level==10) %>%
    left_join(., preds, by="grid_id") %>%
    dplyr::select(Order.q, mean_completeness,
                  heterogeneity, `tree-coverfraction`,
                  `urban-coverfraction`, `water-permanent-coverfraction`) %>%
    rename(tree=`tree-coverfraction`) %>%
    rename(urban=`urban-coverfraction`) %>%
    rename(water=`water-permanent-coverfraction`) %>%
    mutate(grid_size=grid_size) %>%
    mutate(year=year) %>%
    left_join(., dat %>% 
                dplyr::select(grid_id, number_checklists, total_SR) %>%
                distinct()) %>%
    mutate(q=paste0("q=", Order.q))
  
  return(filtered_dat)
  
}

dat_20 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 20)}))
dat_10 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 10)}))
  

# look for correlation among predictor/habitat variables
dat_20 %>%
  ungroup() %>%
  dplyr::filter(year==2014) %>%
  dplyr::select(4:7) %>%
  cor(.) %>%
  ggcorrplot(., lab=TRUE)


# grid size 20 results
het <- ggplot(dat_20, aes(x=heterogeneity, y=mean_completeness, 
                   group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Bootstrapped coverage estimate")+
  xlab("Habitat heterogeneity")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

het

urb <- ggplot(dat_20, aes(x=urban, y=mean_completeness, 
                          group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Bootstrapped coverage estimate")+
  xlab("Urban cover")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)
  
urb

water <- ggplot(dat_20, aes(x=water, y=mean_completeness, 
                            group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Bootstrapped coverage estimate")+
  xlab("Water cover")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

water

tree <- ggplot(dat_20, aes(x=tree, y=mean_completeness, 
                           group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Bootstrapped coverage estimate")+
  xlab("Tree cover")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

tree
  
het + ggtitle("Grid size=20km") + urb + water + tree + plot_layout(ncol=2)

 
# do the patterns stay the same at a smaller grid size?
het <- ggplot(dat_10, aes(x=heterogeneity, y=mean_completeness, 
                          group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Bootstrapped coverage estimate")+
  xlab("Habitat heterogeneity")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

het

urb <- ggplot(dat_10, aes(x=urban, y=mean_completeness, 
                          group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Bootstrapped coverage estimate")+
  xlab("Urban cover")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

urb

water <- ggplot(dat_10, aes(x=water, y=mean_completeness, 
                            group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Bootstrapped coverage estimate")+
  xlab("Water cover")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

water

tree <- ggplot(dat_10, aes(x=tree, y=mean_completeness, 
                           group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Bootstrapped coverage estimate")+
  xlab("Tree cover")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

tree

het + ggtitle("Grid size = 10km") + urb + water + tree + plot_layout(ncol=2)

# do the patterns stay the same at a smaller grid size?
het <- ggplot(dat_10, aes(x=heterogeneity, y=number_checklists, 
                          group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Number of samples (log10)")+
  scale_y_log10()+
  xlab("Habitat heterogeneity")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

het

urb <- ggplot(dat_10, aes(x=urban, y=number_checklists, 
                          group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Number of samples (log10)")+
  scale_y_log10()+
  xlab("Urban cover")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

urb

water <- ggplot(dat_10, aes(x=water, y=number_checklists, 
                            group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Number of samples (log10)")+
  scale_y_log10()+
  xlab("Water cover")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

water

tree <- ggplot(dat_10, aes(x=tree, y=number_checklists, 
                           group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  ylab("Number of samples (log10)")+
  scale_y_log10()+
  xlab("Tree cover")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

tree

het + ggtitle("Grid size = 10km") + urb + water + tree + plot_layout(ncol=2)
  
# look at the relationship between completeness
# and the number of checklists used to sample there
ggplot(dat_20, aes(x=number_checklists, y=mean_completeness,
                   group=year, color=q))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~q, scales="free")

analysis_dat <- dat_20 %>%
  bind_rows(dat_10)


# Now make a function to run a random forest
# so that I can make predictions
# into the entire landscape
# separately for each year and each grid size
# run a function for a given level of q
modelling_function <- function(year, grid_size){
  
  preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_size, "km.csv")) %>%
    dplyr::select(-`system:index`, -`.geo`)
  
  different_order <- function(q){
    
    filtered_dat <- analysis_dat %>%
      ungroup() %>%
      dplyr::filter(Order.q==2) %>%
      dplyr::filter(year==2014) %>%
      dplyr::filter(grid_size==20) %>%
      dplyr::select(mean_completeness, heterogeneity,
                    tree, urban, water, number_checklists)
    
    filtered_dat %>%
      cor(.) %>%
      ggcorrplot(lab=TRUE)+
      theme_bw()+
      theme(axis.text=element_text(color="black"))+
      xlab("")+
      ylab("")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ggtitle(paste0(year, "; ", grid_size, "km; q=", q))
    
    # run a random forest model
  set.seed(123)
  samp <- sample(nrow(filtered_dat), 0.8 * nrow(filtered_dat))
  train <- filtered_dat[samp, ]
  test <- filtered_dat[-samp, ]
  
  mod <- randomForest(log10(number_checklists) ~ ., data = train)
  
  mod
  
  pred <- data.frame(predicted_checklists=10^predict(mod, newdata = test)) %>%
    mutate(observed_checklists=test$number_checklists)
  
  ggplot(pred, aes(x=predicted_checklists, y=observed_checklists))+
    geom_point()+
    scale_x_log10()+
    scale_y_log10()+
    geom_smooth(method="lm")+
    theme_bw()+
    theme(axis.text=element_text(color="black"))
  
  r2 <- pred %>%
    lm(predicted_checklists ~ observed_checklists, data=.) %>%
    summary() %>%
    .$r.squared
  
  # write a function for different levels of completeness
  different_completeness <- function(completeness){
    # now create dataframe to predict on, using all grid cells
    newdata <- preds %>%
      dplyr::select(heterogeneity, `tree-coverfraction`,
                    `urban-coverfraction`, `water-permanent-coverfraction`) %>%
      rename(tree=`tree-coverfraction`) %>%
      rename(urban=`urban-coverfraction`) %>%
      rename(water=`water-permanent-coverfraction`) %>%
      mutate(mean_completeness=completeness)
    
    
    full_prediction <- data.frame(predicted_checklists=10^predict(mod, newdata=newdata)) %>%
      mutate(grid_id=preds$grid_id) %>%
      mutate(coverage=paste0(100*completeness, "%")) %>%
      mutate(q=q) %>%
      mutate(year=year) %>%
      mutate(grid_size=grid_size) %>%
      mutate(model_r2=r2) %>%
      left_join(., dat %>% 
                  dplyr::select(grid_id, number_checklists) %>%
                  distinct(), by="grid_id")
    
    return(full_prediction)
    
  }
  
  # apply this function over different levels of completeness
  completeness_results <- bind_rows(lapply(c(.85, .9, .95, 1), different_completeness))
  
  
}

  

  # see relationship
 temp_dat 
  
  
  # fit a gam
  g1_mod <- mgcv::gam(log10(number_checklists) ~ s(Estimate, k=3) + s(log10(total_SR), k=3), data=filtered_dat)
  
  newdata <- filtered_dat %>%
    dplyr::select(total_SR) %>%
    mutate(Estimate=completeness)
  
  pred <- data.frame(predicted_checklists=10^predict(g1_mod, newdata = newdata)) %>%
    mutate(observed_checklists=filtered_dat$number_checklists)
  
 
  
  
  mod <- randomForest(log10(number_checklists) ~ ., data = train)
  
  mod
  
  pred <- data.frame(predicted_checklists=10^predict(mod, newdata = test)) %>%
    mutate(observed_checklists=test$number_checklists)
  
  ggplot(pred, aes(x=predicted_checklists, y=observed_checklists))+
    geom_point()+
    scale_x_log10()+
    scale_y_log10()+
    geom_smooth(method="lm")+
    theme_bw()+
    theme(axis.text=element_text(color="black"))
  
  r2 <- pred %>%
    lm(predicted_checklists ~ observed_checklists, data=.) %>%
    summary() %>%
    .$r.squared
  
  # write a function for different levels of completeness
  different_completeness <- function(completeness){
    # now create dataframe to predict on, using all grid cells
    newdata <- preds %>%
      dplyr::select(heterogeneity, `tree-coverfraction`,
                    `urban-coverfraction`, `water-permanent-coverfraction`) %>%
      rename(tree=`tree-coverfraction`) %>%
      rename(urban=`urban-coverfraction`) %>%
      rename(water=`water-permanent-coverfraction`) %>%
      mutate(Estimate=completeness)
    
    
    full_prediction <- data.frame(predicted_checklists=10^predict(mod, newdata=newdata)) %>%
      mutate(grid_id=preds$grid_id) %>%
      mutate(coverage=paste0(100*completeness, "%")) %>%
      mutate(q=q) %>%
      mutate(year=year) %>%
      mutate(grid_size=grid_size) %>%
      mutate(model_r2=r2) %>%
      left_join(., dat %>% 
                  dplyr::select(grid_id, number_checklists) %>%
                  distinct(), by="grid_id")
    
    return(full_prediction)
    
  }
  
  # apply this function over different levels of completeness
  completeness_results <- bind_rows(lapply(c(.85, .9, .95, 1), different_completeness))
  
}

# apply this function over different levels of q
results <- bind_rows(lapply(c(0, 2), different_order))




















############################################
############################################
############# OLD ##########################
############################################
  
  
  # make some summary figures to help understand the data better
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
  
  
  
}
