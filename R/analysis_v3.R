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
library(pdp)
library(vip)
library(piecewiseSEM)
library(mice)


# read in the data
summarize_data_function <- function(year, grid_size){
  
  dat <- readRDS(paste0("Intermediate grid level data/sampling_profile_boot/", grid_size, "km_bcr31_", year, ".RDS")) %>%
    dplyr::filter(Order.q %in% c(0, 2))
  
  dat2 <- readRDS(paste0("Intermediate grid level data/sampling_profile/", grid_size, "km_bcr31_", year, ".RDS")) %>%
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
  
  # ggsave(paste0("Figures/sampling_design_assessment/", year, "_", grid_size, "_q=0.png"),
  #        width=7.4, height=7.1, units="in")
  
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
  
  # ggsave(paste0("Figures/sampling_design_assessment/", year, "_", grid_size, "_q=2.png"),
  #        width=7.4, height=7.1, units="in")
  
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
    mutate(q=paste0("q=", Order.q)) %>%
    left_join(., dat2 %>%
                dplyr::select(Estimate, grid_id, Order.q) %>%
                rename(total_completeness=Estimate))
  
  return(filtered_dat)
  
}

dat_20 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 20)}))
dat_10 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 10)}))
dat_5 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 5)}))  

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

  
# look at the relationship between completeness
# and the number of checklists used to sample there
ggplot(dat_20, aes(x=number_checklists, y=mean_completeness,
                   group=year, color=q))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~q, scales="free")+
  geom_smooth(method="lm")

analysis_dat <- dat_20 %>%
  bind_rows(dat_10)


# plot the relationship between the different landscape characteristics
# and the number of checklists
# and the number of total eBird checklists there
# if there is a positive relationship then that means
# birders are selecting the locations with the most species/mean completeness
het_lists <- ggplot(dat_10, aes(x=heterogeneity, y=number_checklists, 
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

het_lists

urb_lists <- ggplot(dat_10, aes(x=urban, y=number_checklists, 
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

urb_lists

water_lists <- ggplot(dat_10, aes(x=water, y=number_checklists, 
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

water_lists

tree_lists <- ggplot(dat_10, aes(x=tree, y=number_checklists, 
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

tree_lists

het_lists + ggtitle("Grid size = 10km") + urb_lists + water_lists + tree_lists + plot_layout(ncol=2)


# plot the relationship between the mean completeness
# and the number of total eBird checklists there
# if there is a positive relationship then that means
# birders are selecting the locations with the most species/mean completeness
ggplot(dat_10, aes(y=mean_completeness, x=number_checklists, 
                   group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  xlab("Number of samples (log10)")+
  scale_x_log10()+
  ylab("Bootstrapped coverage estimate")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)

ggplot(dat_20, aes(y=mean_completeness, x=number_checklists, 
                   group=year, color=q))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~q, scales="free")+
  scale_color_brewer(palette="Set1")+
  xlab("Number of samples (log10)")+
  scale_x_log10()+
  ylab("Bootstrapped coverage estimate")+
  geom_smooth(method="lm", se=FALSE)+
  guides(color=FALSE)











# Now make a function to run some different analyses
# for each year/grid size resolution
# this will save out results from various things
analysis_function <- function(year_name, grid_resolution, data){
  
  preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_resolution, "km.csv")) %>%
    dplyr::select(-`system:index`, -`.geo`)
  
  # first predict the number of checklists
  # necessary to meet different levels of sampling
  # for each grid
  # but the number of checklists, including the total SR observed
  # temp <- analysis_dat %>%
  #   ungroup() %>%
  #   dplyr::filter(year==year_name) %>%
  #   dplyr::filter(grid_size==grid_resolution) %>%
  #   dplyr::filter(Order.q==order) %>%
  #   dplyr::select(total_completeness, mean_completeness, total_SR, number_checklists)
  # 
  # mod1 <- lm(log10(number_checklists) ~ mean_completeness + total_SR,
  #                   data=temp)
  # summary(mod1)
  # newdata <- temp %>%
  #   mutate(total_completeness=0.95)
  # 
  # temp$needed_checklists <- predict(mod1, newdata=newdata)
  # 
  # temp <- temp %>%
  #   mutate(needed_checklists=10^needed_checklists)
  
  different_order <- function(order){
    
    filtered_dat <- data %>%
      ungroup() %>%
      dplyr::filter(Order.q==order) %>%
      dplyr::filter(year==year_name) %>%
      dplyr::filter(grid_size==grid_resolution) %>%
      dplyr::select(mean_completeness, heterogeneity,
                    tree, urban, water, number_checklists,
                    total_SR, total_completeness)
    
    filtered_dat %>%
      cor(.) %>%
      ggcorrplot(lab=TRUE)+
      theme_bw()+
      theme(axis.text=element_text(color="black"))+
      xlab("")+
      ylab("")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ggtitle(paste0(year_name, "; ", grid_resolution, "km; q=", order))
    
    # run a random forest model
  set.seed(123)
  samp <- sample(nrow(filtered_dat), 0.8 * nrow(filtered_dat))
  train <- filtered_dat[samp, ]
  test <- filtered_dat[-samp, ]
  
  mod <- randomForest(log10(number_checklists) ~ ., data = train)
  
  mod
  
  vip(mod)
  partial(mod, pred.var="total_completeness", plot=TRUE, plot.engine="ggplot2")
  partial(mod, pred.var="mean_completeness", plot=TRUE, plot.engine="ggplot2")
  
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
  
  partial_dependence <- data.frame(partial(mod, pred.var="total_completeness")) %>%
    mutate(variable="total_completeness") %>%
    rename(completeness=total_completeness) %>%
    mutate(number_checklists=10^yhat) %>%
    mutate(Order.q=order) %>%
    mutate(year=year_name) %>%
    mutate(grid_size=grid_resolution) %>%
    bind_rows(data.frame(partial(mod, pred.var="mean_completeness")) %>%
                mutate(variable="mean_completeness") %>%
                rename(completeness=mean_completeness) %>%
                mutate(number_checklists=10^yhat) %>%
                mutate(Order.q=order) %>%
                mutate(year=year_name) %>%
                mutate(grid_size=grid_resolution))
  
  saveRDS(partial_dependence, paste0("Results/partial_dependence_data/pd_", year_name, "_", grid_resolution, "_", order, ".RDS"))
  
  # write a function to make a prediction at different levels of completeness
  different_completeness <- function(completeness){
    
    # first make a prediction based on
    # the observed data
    newdata <- filtered_dat %>%
      mutate(total_completeness=completeness)
    
    prediction <- data.frame(predicted_checklists=10^predict(mod, newdata=newdata)) %>%
      bind_cols(data %>%
               ungroup() %>%
               dplyr::filter(Order.q==order) %>%
               dplyr::filter(year==year_name) %>%
               dplyr::filter(grid_size==grid_resolution) %>%
               dplyr::select(mean_completeness, heterogeneity,
                             tree, urban, water, number_checklists,
                             total_SR, total_completeness, grid_id,
                             grid_size, year, Order.q))
      
    saveRDS(prediction, paste0("Results/observed_prediction/observed_", year_name, "_", grid_resolution, "_", order, "_", completeness, ".RDS"))
      
    # now create dataframe to predict on, using all grid cells
    # need to impute some things as we don't know everything though
    # as we don't know the total SR
    # the mean completeness
    # or the total completeness in each grid cell
    # that we want to predict to
    apple <- preds %>%
      dplyr::select(grid_id, heterogeneity, `tree-coverfraction`,
                    `urban-coverfraction`, `water-permanent-coverfraction`) %>%
      rename(tree=`tree-coverfraction`) %>%
      rename(urban=`urban-coverfraction`) %>%
      rename(water=`water-permanent-coverfraction`) %>%
      left_join(., filtered_dat)
    
    imp <- mice(apple, m=10, maxit=50, meth="pmm", seed=610)
    
    imp_list <- mice::complete(imp, "long")
    
    newdata2 <- imp_list %>%
      group_by(grid_id) %>%
      summarize(mean_completeness=mean(mean_completeness),
                number_checklists=mean(number_checklists),
                total_SR=mean(total_SR),
                total_completeness=mean(total_completeness)) %>%
      left_join(apple %>%
                  dplyr::select(1:5))
    
    
    full_prediction <- data.frame(predicted_checklists=10^predict(mod, newdata=newdata2)) %>%
      mutate(grid_id=preds$grid_id) %>%
      mutate(coverage=paste0(100*completeness, "%")) %>%
      mutate(Order.q=order) %>%
      mutate(year=year_name) %>%
      mutate(grid_size=grid_resolution) %>%
      mutate(model_r2=r2) %>%
      left_join(., dat %>% 
                  dplyr::select(grid_id, number_checklists) %>%
                  distinct(), by="grid_id")
    
    saveRDS(full_prediction, paste0("Results/full_prediction/full_", year_name, "_", grid_resolution, "_", order, "_", completeness, ".RDS"))
    
  }
  
  # apply this function over different levels of completeness
  lapply(c(.85, .9, .95, 1), different_completeness)
  
  
}

  lapply(c(0, 2), different_order)
  
}
  
lapply(c(2014:2019), function(x){analysis_function(x, 20, dat_20)})

lapply(c(2014:2019), function(x){analysis_function(x, 10, dat_10)})
  
lapply(c(2014:2019), function(x){analysis_function(x, 5, dat_5)})  
  
  
















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
