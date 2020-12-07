

library(iNEXT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)


# read in data and join with grids
# start with 20 km grid cell size
# and one year of data
bird_dat <- readRDS("Data/bcr31_2016_data.RDS") %>%
  left_join(., readRDS("Data/grid_lookups/20km_grid_lookup_bcr31_2016.RDS"))

# summarize the number of samples within each grid
# get grid summary
grid_summary <- bird_dat %>%
  group_by(grid_id) %>%
  summarize(number_checklists=length(unique(SAMPLING_EVENT_IDENTIFIER)))

ggplot(grid_summary, aes(x=number_checklists))+
  geom_histogram(bins=50, fill="gray80", color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of eBird checklists")+
  ylab("Number of grids")

# get data for a well-sampled grid to test iNext out
ex_grid <- bird_dat %>%
  dplyr::filter(grid_id==289)

# create a matrix of species x site (sampling event)
# using presence/absence only data
# ignoring abundance data
temp <- ex_grid %>% 
  group_by(COMMON_NAME, SAMPLING_EVENT_IDENTIFIER) %>% 
  summarize(present=n()) %>%
  mutate(present=1) %>%
  pivot_wider(names_from=SAMPLING_EVENT_IDENTIFIER, values_from=present, values_fill=0) %>%
  column_to_rownames(var="COMMON_NAME") %>%
  as.data.frame()

# convert this dataframe into data format for iNext
temp_inext <- as.incfreq(temp)

# now run iNext on the data
# specify knots
k <- ncol(temp)
out <- iNEXT(temp_inext, datatype="incidence_freq", knots=k, nboot=20)

ggiNEXT(out, type=1)
ggiNEXT(out, type=2)
ggiNEXT(out, type=3)

# get the data
data_from_inext <- fortify.iNEXT(out, type=2)


# write a function to apply to each grid
summarize_coverage <- function(grid){
  
  message(paste0("Analyzing grid #: ", grid))
  
  dat <- bird_dat %>%
    dplyr::filter(grid_id==grid)
  
  # create a matrix of species x site (sampling event)
  # using presence/absence only data
  # ignoring abundance data
  temp <- dat %>% 
    group_by(COMMON_NAME, SAMPLING_EVENT_IDENTIFIER) %>% 
    summarize(present=n()) %>%
    mutate(present=1) %>%
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
  
  full <- data.frame(coverage=c("observed", "90%", "95%"))
  
  summary_df <- bind_rows(observed, cov_90_percent, cov_95_percent) %>%
    mutate(grid_id=grid) %>%
    right_join(., full)
    
  return(summary_df)
  
}

# now apply the function for any grid that has >25 eBird checklists
grid_list <- bird_dat %>%
  group_by(grid_id) %>%
  summarize(number_checklists=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
  dplyr::filter(number_checklists>25) %>%
  .$grid_id

summary <- bind_rows(lapply(grid_list, summarize_coverage))

summary %>%
  dplyr::filter(coverage=="observed") %>%
  left_join(., grid_summary) %>%
  ggplot(., aes(x=x, y=y))+
  geom_point()+
  xlab("Number of eBird checklists")+
  ylab("Observed sample coverage")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

summary %>%
  dplyr::filter(coverage=="90%") %>%
  left_join(., grid_summary) %>%
  ggplot(., aes(x=x, y=number_checklists))+
  geom_point(alpha=0.4)+
  xlab("Number of eBird checklists necessary for 90% sample coverage")+
  ylab("Total number of checklists in that grid cell")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")

summary %>%
  dplyr::filter(coverage=="90%") %>%
  left_join(., grid_summary) %>%
  ggplot(., aes(x=x, y=number_checklists))+
  geom_point(alpha=0.4)+
  xlab("Number of eBird checklists necessary for 95% sample coverage")+
  ylab("Total number of checklists in that grid cell")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")

summary %>%
  dplyr::filter(coverage!="observed") %>%
  ggplot(., aes(x=coverage, y=x))+
  geom_violin()+
  coord_flip()

summary %>%
  dplyr::filter(coverage=="90%") %>%
  left_join(., grid_summary) %>%
  ggplot(., aes(x=x, y=number_checklists))+
  geom_point(alpha=0.4)+
  xlab("Number of eBird checklists necessary for 90% sample coverage")+
  ylab("Total number of checklists in that grid cell")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")
