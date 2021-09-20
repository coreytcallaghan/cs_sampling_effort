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
    dplyr::filter(Order.q %in% c(0, 1, 2))
  
  dat2 <- readRDS(paste0("Intermediate grid level data/sampling_profile/", grid_size, "km_bcr31_", year, ".RDS")) %>%
    dplyr::filter(Order.q %in% c(0, 1, 2))
  
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
    ggtitle("Rare species sensitive")
  
  # manually go into function to save this figure
  # ggsave(paste0("Figures/bootstap_sample_size_", year, "_", grid_size, "_q=0.png"), width=7.4, height=7.1, units="in")
  
  summarized_data %>%
    dplyr::filter(Order.q==2) %>%
    ungroup() %>%
    dplyr::select(grid_id, sample_level, mean_completeness) %>%
    pivot_wider(names_from=sample_level, values_from=mean_completeness) %>%
    dplyr::select(-grid_id) %>%
    ggpairs(.)+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    ggtitle("Dominant species sensitive")
  
  # ggsave(paste0("Figures/bootstap_sample_size_", year, "_", grid_size, "_q=2.png"), width=7.4, height=7.1, units="in")
  
  # let's now use 10 checklists as our cutoff
  # and save this dataframe for further analysis
  filtered_dat <- summarized_data %>%
    dplyr::filter(sample_level==10) %>%
    left_join(., preds, by="grid_id") %>%
    dplyr::select(Order.q, mean_completeness,
                  heterogeneity, `tree-coverfraction`,
                  `urban-coverfraction`, `water-seasonal-coverfraction`) %>%
    rename(tree=`tree-coverfraction`) %>%
    rename(urban=`urban-coverfraction`) %>%
    rename(water=`water-seasonal-coverfraction`) %>%
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

dat_30 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 30)}))
dat_25 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 25)}))
dat_20 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 20)}))
dat_15 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 15)}))
dat_10 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 10)}))
dat_5 <- bind_rows(lapply(c(2014:2019), function(x){summarize_data_function(x, 5)}))  

# look for correlation among predictor/habitat variables
dat_20 %>%
  ungroup() %>%
  dplyr::filter(year==2019) %>%
  dplyr::select(4:7) %>%
  cor(.) %>%
  ggcorrplot(., lab=TRUE)


# pick a year
year_name=2019


# shape some data into something we can plot with
temp <- dat_5 %>%
  dplyr::filter(year==year_name) %>%
  bind_rows(dat_10 %>%
              dplyr::filter(year==year_name)) %>%
  bind_rows(dat_15 %>%
              dplyr::filter(year==year_name)) %>%
  bind_rows(dat_20 %>%
              dplyr::filter(year==year_name)) %>%
  bind_rows(dat_25 %>%
              dplyr::filter(year==year_name)) %>%
  bind_rows(dat_30 %>%
              dplyr::filter(year==year_name)) %>%
  mutate(type=case_when(Order.q==0 ~ "Rare species sensitive",
                        Order.q==1 ~ "Common species sensitive",
                        Order.q==2 ~ "Dominant species sensitive")) %>%
  dplyr::filter(type != "Common species")


# make four plots
# one for each of our landcover variables
het <- ggplot(temp, aes(x=heterogeneity, y=mean_completeness, 
                          group=grid_size, color=as.factor(grid_size)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~type, scales="free")+
  scale_color_brewer(palette="Set1", name=bquote("Grain size " (~km^2~"")))+
  ylab("Standardized sampling completeness")+
  xlab("Habitat heterogeneity")+
  geom_smooth(method="lm", se=FALSE)+
  theme(legend.position="none")
  

het

urb <- ggplot(temp, aes(x=urban, y=mean_completeness, 
                        group=grid_size, color=as.factor(grid_size)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~type, scales="free")+
  scale_color_brewer(palette="Set1", name=bquote("Grain size " (~km^2~"")))+
  ylab("Standardized sampling completeness")+
  xlab("Urban cover")+
  geom_smooth(method="lm", se=FALSE)+
  theme(legend.position="none")

urb

water <- ggplot(temp, aes(x=water, y=mean_completeness, 
                        group=grid_size, color=as.factor(grid_size)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~type, scales="free")+
  scale_color_brewer(palette="Set1", name=bquote("Grain size " (~km^2~"")))+
  ylab("")+
  xlab("Water cover")+
  geom_smooth(method="lm", se=FALSE)+
  theme(legend.position="none")

water

tree <- ggplot(temp, aes(x=tree, y=mean_completeness, 
                          group=grid_size, color=as.factor(grid_size)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~type, scales="free")+
  scale_color_brewer(palette="Set1", name=bquote("Grain size " (~km^2~"")))+
  ylab("")+
  xlab("Tree cover")+
  geom_smooth(method="lm", se=FALSE)+
  theme(legend.position="bottom")

tree

het + water + urb + tree + plot_layout(ncol=2)

ggsave("Figures/bootstrapped_completeness_vs_variables.png", width=8.8, height=6.8, units="in")

# run linear models to test strength of relationship
lms <- temp %>%
  group_by(grid_size, Order.q) %>%
  group_modify(~ broom::tidy(lm(scale(mean_completeness) ~ scale(heterogeneity) + 
                                  scale(urban) + scale(tree) + scale(water), data = .x))) %>%
  bind_cols(., temp %>%
              group_by(grid_size, Order.q) %>%
              group_modify(~ broom::confint_tidy(lm(scale(mean_completeness) ~ scale(heterogeneity) + 
                                                       scale(urban) + scale(tree) + scale(water), data = .x))) %>%
              ungroup() %>%
              dplyr::select(conf.low, conf.high)) %>%
  mutate(variable=case_when(term=="scale(mean_completeness)" ~ "Standardized sampling completeness",
                            term=="scale(urban)" ~ "Urban cover",
                            term=="scale(heterogeneity)" ~ "Heterogeneity",
                            term=="scale(tree)" ~ "Tree cover",
                            term=="scale(water)" ~ "Water cover")) %>%
  dplyr::filter(complete.cases(variable)) %>%
  mutate(type=case_when(Order.q==0 ~ "Rare species sensitive",
                        Order.q==1 ~ "Common species sensitive",
                        Order.q==2 ~ "Dominant species sensitive"))

common <- lms %>%
  dplyr::filter(type=="Common species sensitive") %>%
  ggplot(., aes(x=variable, y=estimate))+
  facet_wrap(~grid_size, ncol=2)+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0))+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  ylab("Standardized parameter estimate")+
  xlab("")+
  theme(strip.text=element_text(size=8))+
  ggtitle("Common species sensitive")

common

dominant <- lms %>%
  dplyr::filter(type=="Dominant species sensitive") %>%
  ggplot(., aes(x=variable, y=estimate))+
  facet_wrap(~grid_size, ncol=2)+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0))+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  ylab("Standardized parameter estimate")+
  xlab("")+
  theme(strip.text=element_text(size=8))+
  ggtitle("Dominant species sensitive")

dominant

rare <- lms %>%
  dplyr::filter(type=="Rare species sensitive") %>%
  ggplot(., aes(x=variable, y=estimate))+
  facet_wrap(~grid_size, ncol=2)+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0))+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  ylab("Standardized parameter estimate")+
  xlab("")+
  theme(strip.text=element_text(size=8))+
  ggtitle("Rare species sensitive")

rare

dominant+common+rare+plot_layout(ncol=1)

ggsave("Figures/multiple_linear_regression_completeness.png", width=6.3, height=8.8, units="in")

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
het_lists <- ggplot(temp, aes(x=heterogeneity, y=number_checklists, 
                          group=grid_size, color=as.factor(grid_size)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1", name=bquote("Grain size " (~km^2~"")))+
  ylab("Number of samples (log10)")+
  scale_y_log10()+
  xlab("Habitat heterogeneity")+
  geom_smooth(method="lm", se=FALSE)+
  theme(legend.position="none")

het_lists

urb_lists <- ggplot(temp, aes(x=urban, y=number_checklists, 
                                group=grid_size, color=as.factor(grid_size)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1", name=bquote("Grain size " (~km^2~"")))+
  ylab("Number of samples (log10)")+
  scale_y_log10()+
  xlab("Urban cover")+
  geom_smooth(method="lm", se=FALSE)+
  theme(legend.position="none")

urb_lists

water_lists <- ggplot(temp, aes(x=water, y=number_checklists, 
                                  group=grid_size, color=as.factor(grid_size)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1", name=bquote("Grain size " (~km^2~"")))+
  ylab("Number of samples (log10)")+
  scale_y_log10()+
  xlab("Water cover")+
  geom_smooth(method="lm", se=FALSE)+
  theme(legend.position="none")

water_lists

tree_lists <- ggplot(temp, aes(x=tree, y=number_checklists, 
                                 group=grid_size, color=as.factor(grid_size)))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1", name=bquote("Grain size " (~km^2~"")))+
  ylab("Number of samples (log10)")+
  scale_y_log10()+
  xlab("Tree cover")+
  geom_smooth(method="lm", se=FALSE)+
  theme(legend.position="bottom")

tree_lists

het_lists + urb_lists + water_lists + tree_lists + plot_layout(ncol=2)

ggsave("Figures/number_of_samples_vs_variables.png", width=8.8, height=6.8, units="in")


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
# This function includes the mean completeness value in the random forest analysis
analysis_function <- function(year_name, grid_resolution, data){
  
  preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_resolution, "km.csv")) %>%
    dplyr::select(-`system:index`, -`.geo`)
  
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
    # using the defaults
    mod <- randomForest(log10(number_checklists) ~ ., replace=FALSE, data = filtered_dat)
    
    mod
   
    vip(mod)
    # partial(mod, pred.var="total_completeness", plot=TRUE, plot.engine="ggplot2")
    # partial(mod, pred.var="mean_completeness", plot=TRUE, plot.engine="ggplot2")
    
    r2 <- mean(mod$rsq)
    
    partial_dependence <- data.frame(partial(mod, pred.var="total_completeness", train=filtered_dat)) %>%
      mutate(variable="total_completeness") %>%
      rename(completeness=total_completeness) %>%
      mutate(number_checklists=10^yhat) %>%
      mutate(Order.q=order) %>%
      mutate(year=year_name) %>%
      mutate(grid_size=grid_resolution) %>%
      bind_rows(data.frame(partial(mod, pred.var="mean_completeness", train=filtered_dat)) %>%
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
                    dplyr::select(1:5)) %>%
        mutate(total_completeness=completeness)
      
      
      full_prediction <- data.frame(predicted_checklists=10^predict(mod, newdata=newdata2)) %>%
        mutate(grid_id=preds$grid_id) %>%
        mutate(coverage=paste0(100*completeness, "%")) %>%
        mutate(Order.q=order) %>%
        mutate(year=year_name) %>%
        mutate(grid_size=grid_resolution) %>%
        mutate(model_r2=r2) %>%
        left_join(., data %>% 
                    dplyr::filter(year==year_name) %>%
                    ungroup() %>%
                    dplyr::select(grid_id, number_checklists) %>%
                    distinct(), by="grid_id")
      
      saveRDS(full_prediction, paste0("Results/full_prediction/full_", year_name, "_", grid_resolution, "_", order, "_", completeness, ".RDS"))
      
    }
    
    # apply this function over different levels of completeness
    lapply(c(.85, .9, .95, 1), different_completeness)
    
    
  }
  
  lapply(c(0, 1, 2), different_order)
  
}

lapply(c(2014:2019), function(x){analysis_function(x, 30, dat_30)})
lapply(c(2014:2019), function(x){analysis_function(x, 25, dat_25)})
lapply(c(2014:2019), function(x){analysis_function(x, 20, dat_20)})
lapply(c(2014:2019), function(x){analysis_function(x, 15, dat_15)})
lapply(c(2014:2019), function(x){analysis_function(x, 10, dat_10)})
lapply(c(2014:2019), function(x){analysis_function(x, 5, dat_5)})

# Now do a different function that is mostly repetitive
# but does not do the imputation
# and also does not include the mean completeness value in the random forests
# Now make a function to run some different analyses
# for each year/grid size resolution
# this will save out results from various things
# This function also performs the SEM approach as well
analysis_function_v2 <- function(year_name, grid_resolution, data){
  
  preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_resolution, "km.csv")) %>%
    dplyr::select(-`system:index`, -`.geo`)
  
  different_order <- function(order){
    
    filtered_dat <- data %>%
      ungroup() %>%
      dplyr::filter(Order.q==order) %>%
      dplyr::filter(year==year_name) %>%
      dplyr::filter(grid_size==grid_resolution) %>%
      dplyr::select(heterogeneity, tree, urban, water, 
                    number_checklists, total_completeness)
    
    filtered_dat %>%
      cor(.) %>%
      ggcorrplot(lab=TRUE)+
      theme_bw()+
      theme(axis.text=element_text(color="black"))+
      xlab("")+
      ylab("")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ggtitle(paste0(year_name, "; ", grid_resolution, "km; q=", order))
    
    # run a psem to better understand the patterns
    # among coverage, observed SR, checklists
    # and the habitat variables
    
    # first get a df together with transformations
    # for modelling
    psem_dat <- data %>%
      ungroup() %>%
      dplyr::filter(Order.q==order) %>%
      dplyr::filter(year==year_name) %>%
      dplyr::filter(grid_size==grid_resolution) %>%
      dplyr::select(heterogeneity, tree, urban, water, total_SR,
                    number_checklists, total_completeness) %>%
      mutate(log.number_checklists=log(number_checklists)) %>%
      mutate(logit.sampling_coverage=car::logit(total_completeness))
    
    psem1 <- psem(
      lm(log.number_checklists ~ total_SR + urban + heterogeneity, data=psem_dat),
      lm(total_SR ~ heterogeneity + urban + water + tree, data=psem_dat)
    )
    summary(psem1, .progressBar = FALSE)
    plot(psem1)
    
    saveRDS(psem1, paste0("Results/sem_results/", year_name, "_", grid_resolution, "_", order, ".RDS"))
    
    # run a random forest model
    mod <- randomForest(log10(number_checklists) ~ ., replace=FALSE, data = filtered_dat)
    
    mod
    
    vip(mod)
    # partial(mod, pred.var="total_completeness", plot=TRUE, plot.engine="ggplot2")
    # partial(mod, pred.var="mean_completeness", plot=TRUE, plot.engine="ggplot2")
    
    r2 <- mean(mod$rsq)

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
      
      saveRDS(prediction, paste0("Results/observed_prediction/v2_observed_", year_name, "_", grid_resolution, "_", order, "_", completeness, ".RDS"))
      
      # now create dataframe to predict on, using all grid cells
      # need to impute some things as we don't know everything though
      # as we don't know the total SR
      # the mean completeness
      # or the total completeness in each grid cell
      # that we want to predict to
      apple <- preds %>%
        dplyr::select(grid_id, heterogeneity, `tree-coverfraction`,
                      `urban-coverfraction`, `water-seasonal-coverfraction`) %>%
        rename(tree=`tree-coverfraction`) %>%
        rename(urban=`urban-coverfraction`) %>%
        rename(water=`water-seasonal-coverfraction`) %>%
        left_join(., filtered_dat) %>%
        mutate(total_completeness=completeness)
      
      full_prediction <- data.frame(predicted_checklists=10^predict(mod, newdata=apple)) %>%
        mutate(grid_id=preds$grid_id) %>%
        mutate(coverage=paste0(100*completeness, "%")) %>%
        mutate(Order.q=order) %>%
        mutate(year=year_name) %>%
        mutate(grid_size=grid_resolution) %>%
        mutate(model_r2=r2) %>%
        left_join(., data %>% 
                    dplyr::filter(year==year_name) %>%
                    ungroup() %>%
                    dplyr::select(grid_id, number_checklists) %>%
                    distinct(), by="grid_id")
      
      saveRDS(full_prediction, paste0("Results/full_prediction/v2_full_", year_name, "_", grid_resolution, "_", order, "_", completeness, ".RDS"))
      
    }
    
    # apply this function over different levels of completeness
    lapply(c(.85, .9, .95, 1), different_completeness)
    
    
  }
  
  lapply(c(0, 1, 2), different_order)
  
}

lapply(c(2014:2019), function(x){analysis_function_v2(x, 30, dat_30)})
lapply(c(2014:2019), function(x){analysis_function_v2(x, 25, dat_25)})  
lapply(c(2014:2019), function(x){analysis_function_v2(x, 20, dat_20)})
lapply(c(2014:2019), function(x){analysis_function_v2(x, 15, dat_15)})  
lapply(c(2014:2019), function(x){analysis_function_v2(x, 10, dat_10)})
lapply(c(2014:2019), function(x){analysis_function_v2(x, 5, dat_5)})  


# Write a function that predicts species richness
# given the landcover variables
# this mimics the random forest used to predict total number of checklists
# in the above function
# but is just focused on species richness
# that will be later used to make maps
predict_richness <- function(year_name, grid_resolution, data){
  
  preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_resolution, "km.csv")) %>%
    dplyr::select(-`system:index`, -`.geo`)
  
  different_order <- function(order){
    
    filtered_dat <- data %>%
      ungroup() %>%
      dplyr::filter(Order.q==order) %>%
      dplyr::filter(year==year_name) %>%
      dplyr::filter(grid_size==grid_resolution) %>%
      dplyr::select(heterogeneity, tree, urban, water, 
                    number_checklists, total_SR) %>%
      mutate(number_checklists=log10(number_checklists))

    # run a random forest model
    mod <- randomForest(log10(total_SR) ~ ., replace=FALSE, data = filtered_dat)
    
    mod
    
    r2 <- mean(mod$rsq)
    
    apple <- preds %>%
      dplyr::select(grid_id, heterogeneity, `tree-coverfraction`,
                    `urban-coverfraction`, `water-seasonal-coverfraction`) %>%
      rename(tree=`tree-coverfraction`) %>%
      rename(urban=`urban-coverfraction`) %>%
      rename(water=`water-seasonal-coverfraction`) %>%
      left_join(., filtered_dat) %>%
      mutate(number_checklists=mean(filtered_dat$number_checklists))
    
    SR_prediction <- data.frame(predicted_SR=10^predict(mod, newdata=apple)) %>%
      mutate(grid_id=preds$grid_id) %>%
      mutate(Order.q=order) %>%
      mutate(year=year_name) %>%
      mutate(grid_size=grid_resolution) %>%
      mutate(model_r2=r2) %>%
      left_join(., data %>% 
                  dplyr::filter(year==year_name) %>%
                  ungroup() %>%
                  dplyr::select(grid_id, number_checklists, total_SR) %>%
                  distinct(), by="grid_id")
    
    saveRDS(SR_prediction, paste0("Results/SR_prediction/", year_name, "_", grid_resolution, "_", order, ".RDS"))
    
  }
  
  lapply(c(0, 1, 2), different_order)
  
}

lapply(c(2014:2019), function(x){predict_richness(x, 30, dat_30)})
lapply(c(2014:2019), function(x){predict_richness(x, 25, dat_25)})  
lapply(c(2014:2019), function(x){predict_richness(x, 20, dat_20)})
lapply(c(2014:2019), function(x){predict_richness(x, 15, dat_15)})  
lapply(c(2014:2019), function(x){predict_richness(x, 10, dat_10)})
lapply(c(2014:2019), function(x){predict_richness(x, 5, dat_5)})










