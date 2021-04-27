# This script is to run the grid-level analysis at different spatial resolutions
# and for different years
# basically a couple of functions to perform this with lots happening inside of them

# packages
library(iNEXT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

source("R/functions_from_Chao_2020.R")

options(tidyverse.quiet = TRUE)

# calculate grid-level coverage and checklist estimates
grid_analysis <- function(file_name, grid_size){
  
  # read in data
  bird_dat <- readRDS(paste0("Data/", file_name)) %>%
    left_join(., readRDS(paste0("Data/grid_lookups/", grid_size, "km_grid_lookup_", gsub("_data", "", file_name))))
  
  grid_summary <- bird_dat %>%
    group_by(grid_id) %>%
    summarize(number_checklists=length(unique(SAMPLING_EVENT_IDENTIFIER)),
              total_SR=length(unique(COMMON_NAME)))
  
  # write a function to apply to each grid
  summarize_coverage <- function(grid){
    
    message(paste0("Analyzing grid #: ", grid))
    
    dat <- bird_dat %>%
      dplyr::filter(grid_id==grid)
    
    # bootstrap this by only
    # selecting 20 eBird checklists
    # to get the estimate from
    # and repeat this 50 times
    boot_function <- function(draw_number, number_samples){
      
      random_checklists <- dat %>%
        dplyr::select(SAMPLING_EVENT_IDENTIFIER) %>%
        distinct() %>%
        sample_n(number_samples)
      
      random_dat <- dat %>%
        dplyr::filter(SAMPLING_EVENT_IDENTIFIER %in% random_checklists$SAMPLING_EVENT_IDENTIFIER)
      
      # create a matrix of species x site (sampling event)
      # using presence/absence only data
      # ignoring abundance data
      temp <- random_dat %>% 
        group_by(COMMON_NAME, SAMPLING_EVENT_IDENTIFIER) %>% 
        summarize(present=n()) %>%
        mutate(present=1) %>%
        pivot_wider(names_from=SAMPLING_EVENT_IDENTIFIER, values_from=present, values_fill=0) %>%
        column_to_rownames(var="COMMON_NAME") %>%
        as.data.frame()
      
      # convert this dataframe into data format for iNext
      temp_inext <- as.incfreq(temp)
      
      # now run the sampling completeness profile
      # for the data
      # specify knots
      q = seq(0,2,0.1)
      B <- 100
      
      sampling_profile <- sc_profile(temp_inext, datatype="incidence", q=q, B=B, conf=0.95) %>%
        mutate(grid_id=grid) %>%
        mutate(analysis="sampling_profile") %>%
        rename(y.lwr=LCL) %>%
        rename(y.upr=UCL) %>%
        mutate(draw=draw_number) %>%
        mutate(observed_SR=length(unique(random_dat$COMMON_NAME))) %>%
        mutate(sample_level=number_samples)
      
      return(sampling_profile)
    }
    
    results_5 <- bind_rows(lapply(c(1:50), function(x){boot_function(x, 5)}))
    results_10 <- bind_rows(lapply(c(1:50), function(x){boot_function(x, 10)}))
    results_15 <- bind_rows(lapply(c(1:50), function(x){boot_function(x, 15)}))
    results_20 <- bind_rows(lapply(c(1:50), function(x){boot_function(x, 20)}))
    
    results <- bind_rows(results_5,
                         results_10,
                         results_15,
                         results_20)
    return(results)
  }
  
  # now apply the function for any grid that has >25 eBird checklists
  grid_list <- bird_dat %>%
    group_by(grid_id) %>%
    summarize(number_checklists=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
    dplyr::filter(number_checklists>25) %>%
    .$grid_id
  
  summary <- bind_rows(lapply(grid_list, summarize_coverage)) %>%
    mutate(data=gsub("_data.RDS", "", file_name)) %>%
    mutate(spatial_resolution=grid_size) %>%
    left_join(., grid_summary) %>%
    mutate(Year=gsub("bcr31_", "", data))
  
  saveRDS(summary, paste0("Intermediate grid level data/sampling_profile_boot/", grid_size, "km_", gsub("_data", "", file_name)))
}

file_names <- c("bcr31_2014_data.RDS", "bcr31_2015_data.RDS", "bcr31_2016_data.RDS",
                "bcr31_2017_data.RDS", "bcr31_2018_data.RDS", "bcr31_2019_data.RDS")

lapply(file_names, function(x){grid_analysis(x, 30)})

lapply(file_names, function(x){grid_analysis(x, 25)})

lapply(file_names, function(x){grid_analysis(x, 20)})

lapply(file_names, function(x){grid_analysis(x, 15)})

lapply(file_names, function(x){grid_analysis(x, 10)})

lapply(file_names, function(x){grid_analysis(x, 5)})
