library(iNEXT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)

source("R/functions_from_Chao_2020.R")

options(tidyverse.quiet = TRUE)

#pull out the data for one year and grid size
file_name <- "bcr31_2019_data.RDS"
grid_size <- 20

# read in data for the above years and grid size
bird_dat <- readRDS(paste0("Data/", file_name)) %>%
    left_join(., readRDS(paste0("Data/grid_lookups/", grid_size, "km_grid_lookup_", gsub("_data", "", file_name))))
head(bird_dat)

#summarize per grid
grid_summary <- bird_dat %>%
  group_by(grid_id) %>%
  summarize(number_checklists=length(unique(SAMPLING_EVENT_IDENTIFIER)),
                                total_SR=length(unique(COMMON_NAME)))
head(grid_summary)

#write a function to pull out estimateD and sc_profile for each grid
gridFun <- function(grid){
  
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

# now run the sampling completeness profile
# for the data
# specify knots
q = c(0,1,2)
B <- 100
sampling_profile <- sc_profile(temp_inext, datatype="incidence", q=q, B=B, conf=0.95)

#and now using estimateD to get qD
out.inc <- iNEXT(temp_inext, q=c(0,1,2), datatype="incidence_freq")
#ggiNEXT(out.inc, type=1, facet.var="order")
#ggiNEXT(out.inc, type=2, facet.var="order")
sampling_profile2 <- estimateD(as.numeric(temp_inext), datatype="incidence_freq",base="coverage", 
                              level=0.95, conf=0.95)

#combine all output into one
sampling_profile <- cbind(sampling_profile,sampling_profile2)
sampling_profile$grid_id <- grid
return(sampling_profile)

}

#apply the above function to each grid - just for grids with at least 10 checklists
grid <- grid_summary$grid_id[grid_summary$number_checklists>=10]
output <- plyr::ldply(grid,gridFun)

#add on summary statistics too
output <- inner_join(output,grid_summary)

#and also add on land use data
preds <- read_csv(paste0("Data/predictor_data_for_grids/stats_", grid_size, "km.csv")) %>%
  dplyr::select(-`system:index`, -`.geo`)
output <- inner_join(output,preds)


#look at some correlations among everything
names(output)[which(names(output)=="Estimate")] <- "sampling_coverage"
output$log.number_checklists <- log(output$number_checklists)
names(output) <- gsub("-","_",names(output))

#lets look for q=0
output0 <- subset(output,order==0)#keeping it simple, intitially
pairs(output0[,c("sampling_coverage","t","qD",
                 "log.number_checklists","total_SR","heterogeneity")])

# #predict the number of checklists for Estimate = 95 using qD
# lm1 <- lm(log(number_checklists) ~ sampling_coverage + qD, data=output0)
# summary(lm1)
# car::avPlots(lm1)
# newdata <- output0
# newdata$sampling_coverage <- 0.95
# newdata$needed_checklists <- predict(lm1,newdata=newdata)
# qplot(needed_checklists,qD,data=newdata)+scale_x_log10()
# 
# #predict land use effects on diversity
options(na.action = "na.fail")
lm1 <- lm(total_SR ~ bare_coverfraction + crops_coverfraction + grass_coverfraction + heterogeneity + urban_coverfraction + shrub_coverfraction + water_permanent_coverfraction +water_seasonal_coverfraction + tree_coverfraction,data=output0)
dd <- MuMIn::dredge(lm1)
subset(dd, delta < 2)



#link models together using piecewise sem
library(piecewiseSEM)
psem1 = psem(
  lm(sampling_coverage ~ total_SR + shrub_coverfraction, data=output0),
  lm(log.number_checklists ~ total_SR + urban_coverfraction, data=output0),
  lm(total_SR ~ heterogeneity + urban_coverfraction + shrub_coverfraction,data=output0)
)

summary(psem1, .progressBar = FALSE)
plot(psem1)


#and for q=2
output2 <- subset(output,order==2)

# #predict land use effects on diversity
options(na.action = "na.fail")
lm1 <- lm(total_SR ~ bare_coverfraction + crops_coverfraction + grass_coverfraction + heterogeneity + urban_coverfraction + shrub_coverfraction + water_permanent_coverfraction +water_seasonal_coverfraction + tree_coverfraction,data=output2)
dd <- MuMIn::dredge(lm1)
subset(dd, delta < 2)

#link models together using piecewise sem
library(piecewiseSEM)
psem1 = psem(
  lm(sampling_coverage ~ log.number_checklists + urban_coverfraction, 
     data=output2),
  lm(log.number_checklists ~ total_SR + urban_coverfraction, data=output2),
  lm(total_SR ~ heterogeneity + urban_coverfraction + shrub_coverfraction,data=output2)
)

summary(psem1, .progressBar = FALSE)
plot(psem1)
