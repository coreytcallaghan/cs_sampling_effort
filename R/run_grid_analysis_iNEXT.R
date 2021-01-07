# This script is to run the grid-level analysis at different spatial resolutions
# and for different years
# basically a couple of functions to perform this with lots happening inside of them

# packages
library(iNEXT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

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
    
    cutoff_function <- function(cutoff){
    # create a matrix of species x site (sampling event)
    # using presence/absence only data
    # ignoring abundance data
    temp <- dat %>% 
      group_by(COMMON_NAME, SAMPLING_EVENT_IDENTIFIER) %>% 
      summarize(present=n()) %>%
      mutate(present=1) %>%
      group_by(COMMON_NAME) %>%
      mutate(total_obs=n()) %>%
      mutate(total_lists=length(unique(.$SAMPLING_EVENT_IDENTIFIER))) %>%
      mutate(percent_lists=(total_obs/total_lists)*100) %>%
      dplyr::filter(percent_lists>=cutoff) %>%
      dplyr::select(1:3) %>%
      pivot_wider(names_from=SAMPLING_EVENT_IDENTIFIER, values_from=present, values_fill=0) %>%
      column_to_rownames(var="COMMON_NAME") %>%
      as.data.frame()
    
    # convert this dataframe into data format for iNext
    temp_inext <- as.incfreq(temp)
    
    # now run iNext on the data
    # specify knots
    k <- ncol(temp)
    out <- iNEXT(temp_inext, datatype="incidence_freq", knots=k, nboot=100)
    
    # get the data
    data_from_inext <- fortify.iNEXT(out, type=2)
    
    # make a summary dataframe
    observed <- data_from_inext %>%
      dplyr::filter(method=="observed") %>%
      dplyr::select(4, 6:9) %>%
      rename(coverage=method)
    
    cov_100_percent <- data_from_inext %>%
      mutate(y=round(y, digits=4)) %>%
      dplyr::filter(y>=1.000) %>%
      slice(1) %>%
      dplyr::select(4, 6:9) %>%
      rename(coverage=method) %>%
      mutate(coverage="100%")
    
    cov_90_percent <- data_from_inext %>%
      dplyr::filter(y>0.9) %>%
      slice(1) %>%
      dplyr::select(4, 6:9) %>%
      rename(coverage=method) %>%
      mutate(coverage="90%")
    
    cov_95_percent <- data_from_inext %>%
      dplyr::filter(y>0.95) %>%
      slice(1) %>%
      dplyr::select(4, 6:9) %>%
      rename(coverage=method) %>%
      mutate(coverage="95%")
    
    full <- data.frame(coverage=c("observed", "90%", "95%", "100%"))
    
    summary_df <- bind_rows(observed, cov_90_percent, cov_95_percent, cov_100_percent) %>%
      mutate(grid_id=grid) %>%
      right_join(., full) %>%
      mutate(analysis="iNEXT") %>%
      mutate(percent_cutoff=cutoff)
    
    return(summary_df)
    
    }
    
    summary_final <- bind_rows(lapply(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), cutoff_function))
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
  
  saveRDS(summary, paste0("Intermediate grid level data/iNEXT/", grid_size, "km_", gsub("_data", "", file_name)))
}

file_names <- c("bcr31_2014_data.RDS", "bcr31_2015_data.RDS", "bcr31_2016_data.RDS",
                "bcr31_2017_data.RDS", "bcr31_2018_data.RDS", "bcr31_2019_data.RDS")

lapply(file_names, function(x){grid_analysis(x, 20)})




