# This script is used to get data for 5 BCRs
# to run species distribution models
# the data are collated only for the years
# 2018

# for each BCR I'll write out all data as an RDS
# and I'll write out all the unique sites as an RDS
# which will be processed in GEE

## packages
library(readr)
library(bigrquery)
library(dbplyr)
library(dplyr)
library(tidyr)
library(lubridate)

# create connection with online database
con <- DBI::dbConnect(bigrquery::bigquery(),
                      dataset= "ebird",
                      project="ebird-database",
                      billing="ebird-database")

# create ebird table
ebird <- tbl(con, 'ebird_qa_may_2020')

## extract data
bcr_31 <- ebird %>%
  dplyr::filter(BCR_CODE==31) %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(OBSERVATION_DATE > "2014-01-01") %>%
  dplyr::filter(OBSERVATION_DATE < "2019-12-31") %>%
  collect(n=Inf)

dat <- bcr_31 %>%
  dplyr::filter(CATEGORY %in% c("species", "issf", "domestic")) %>%
  dplyr::filter(PROTOCOL_TYPE %in% c("Stationary", "Traveling",
                                     "Area", "Random"))

ropi <- bcr_31 %>%
  dplyr::filter(COMMON_NAME=="Rock Pigeon")

analysis_data <- dat %>%
  dplyr::filter(!CATEGORY %in% "domestic") %>%
  bind_rows(ropi)

df_2014 <- analysis_data %>%
  dplyr::filter(OBSERVATION_DATE >= "2014-01-01") %>%
  dplyr::filter(OBSERVATION_DATE <= "2014-12-31")

saveRDS(df_2014, "Data/bcr31_2014_data.RDS")

df_2015 <- analysis_data %>%
  dplyr::filter(OBSERVATION_DATE >= "2015-01-01") %>%
  dplyr::filter(OBSERVATION_DATE <= "2015-12-31")

saveRDS(df_2015, "Data/bcr31_2015_data.RDS")

df_2016 <- analysis_data %>%
  dplyr::filter(OBSERVATION_DATE >= "2016-01-01") %>%
  dplyr::filter(OBSERVATION_DATE <= "2016-12-31")

saveRDS(df_2016, "Data/bcr31_2016_data.RDS")

df_2017 <- analysis_data %>%
  dplyr::filter(OBSERVATION_DATE >= "2017-01-01") %>%
  dplyr::filter(OBSERVATION_DATE <= "2017-12-31")

saveRDS(df_2017, "Data/bcr31_2017_data.RDS")

df_2018 <- analysis_data %>%
  dplyr::filter(OBSERVATION_DATE >= "2018-01-01") %>%
  dplyr::filter(OBSERVATION_DATE <= "2018-12-31")

saveRDS(df_2018, "Data/bcr31_2018_data.RDS")

df_2019 <- analysis_data %>%
  dplyr::filter(OBSERVATION_DATE >= "2019-01-01") %>%
  dplyr::filter(OBSERVATION_DATE <= "2019-12-31")

saveRDS(df_2019, "Data/bcr31_2019_data.RDS")

locality_data <- analysis_data %>%
  dplyr::select(LOCALITY_ID, LATITUDE, LONGITUDE) %>%
  distinct()

saveRDS(locality_data, "Data/unique_locality_data.RDS")

library(sf)

locality_shape <- locality_data %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326)

st_write(locality_shape, "Data/unique_locality_shapefile/unique_localities.shp")


