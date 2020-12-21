# This script is to predict out the number of checklists needed to sample
# each grid at a specific level
# it works as a function that takes year


library(ggplot2)
library(dplyr)
library(readr)
library(sf)
library(ggcorrplot)
library(GGally)
library(randomForest)


# function to predict richness
predict_checklists <- function(year, grid_size) {
  
  
  dat <- readRDS(paste0("Intermediate grid level data/sampling_profile/", grid_size, "km_bcr31_", year, ".RDS"))
  
  preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_size, "km.csv")) %>%
    dplyr::select(-`system:index`, -`.geo`)
  
  # run a function for a given level of q
  different_order <- function(q){
    
    filtered_dat <- dat %>%
      dplyr::filter(Order.q==q) %>%
      left_join(., preds, by="grid_id") %>%
      dplyr::select(number_checklists, Estimate,
                    heterogeneity, `tree-coverfraction`,
                    `urban-coverfraction`, `water-permanent-coverfraction`) %>%
      rename(tree=`tree-coverfraction`) %>%
      rename(urban=`urban-coverfraction`) %>%
      rename(water=`water-permanent-coverfraction`)
    
    # see relationship
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
  results <- bind_rows(lapply(c(0, 1, 2), different_order))
  
  
}


final_results_20 <- bind_rows(lapply(c(2014:2019), function(x){predict_checklists(x, 20)}))

saveRDS(final_results_20, "Results/20_km_grid_prediction_results.RDS")


final_results_10 <- bind_rows(lapply(c(2014:2019), function(x){predict_checklists(x, 10)}))

saveRDS(final_results_10, "Results/10_km_grid_prediction_results.RDS")


final_results_5 <- bind_rows(lapply(c(2014:2019), function(x){predict_checklists(x, 5)}))

saveRDS(final_results_5, "Results/5_km_grid_prediction_results.RDS")



### A second version of the analysis
### flipping the response and predictor variables
# function to predict richness
predict_completeness <- function(year, grid_size) {
  
  
  dat <- readRDS(paste0("Intermediate grid level data/sampling_profile/", grid_size, "km_bcr31_", year, ".RDS"))
  
  preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_size, "km.csv")) %>%
    dplyr::select(-`system:index`, -`.geo`)
  
  # run a function for a given level of q
  different_order <- function(q){
    
    filtered_dat <- dat %>%
      dplyr::filter(Order.q==q) %>%
      left_join(., preds, by="grid_id") %>%
      dplyr::select(number_checklists, Estimate,
                    heterogeneity, `tree-coverfraction`,
                    `urban-coverfraction`, `water-permanent-coverfraction`) %>%
      rename(tree=`tree-coverfraction`) %>%
      rename(urban=`urban-coverfraction`) %>%
      rename(water=`water-permanent-coverfraction`)
    
    # see relationship
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
    
    
    mod <- randomForest(Estimate ~ ., data = train)
    
    mod
    
    pred <- data.frame(predicted_estimate=predict(mod, newdata = test)) %>%
      mutate(observed_estimate=test$Estimate)
    
    ggplot(pred, aes(x=predicted_estimate, y=observed_estimate))+
      geom_point()+
      #scale_x_log10()+
      #scale_y_log10()+
      geom_smooth(method="lm")+
      theme_bw()+
      theme(axis.text=element_text(color="black"))
    
    r2 <- pred %>%
      lm(predicted_estimate ~ observed_estimate, data=.) %>%
      summary() %>%
      .$r.squared
    
    # write a function for different levels of checklists
    different_checklists <- function(checklists){
      # now create dataframe to predict on, using all grid cells
      newdata <- preds %>%
        dplyr::select(heterogeneity, `tree-coverfraction`,
                      `urban-coverfraction`, `water-permanent-coverfraction`) %>%
        rename(tree=`tree-coverfraction`) %>%
        rename(urban=`urban-coverfraction`) %>%
        rename(water=`water-permanent-coverfraction`) %>%
        mutate(number_checklists=checklists)
      
      
      full_prediction <- data.frame(predicted_estimate=predict(mod, newdata=newdata)) %>%
        mutate(grid_id=preds$grid_id) %>%
        mutate(checklists=checklists) %>%
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
    checklist_results <- bind_rows(lapply(seq(10, 1000, by=10), different_checklists))
    
  }
  
  # apply this function over different levels of q
  results <- bind_rows(lapply(c(0, 1, 2), different_order))
  
  
}











