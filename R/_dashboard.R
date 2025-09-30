### Load Libraries

library(here)
library(tidyverse)
library(sf)
library(readr)




#Value Box Prep
source(here::here("R/_commonR.r"))

# Load the data
all_drifts <- readRDS(here("data", "AllDrifts_BaseEffort.rds"))
deployDetails <- read_csv("data", "Deployment Details - deployDetails.csv")

# Extract the start and end dates
start_date <- deployDetails$Deployment_Date[1]
end_date <- deployDetails$Recovery_Date[1]

#Extract start and end lat/long
start_lat <- deployDetails$Deployment_Latitude[1]
start_lon <- deployDetails$Deployment_Longitude[1]
end_lat <- deployDetails$Recovery_Latitude[1]
end_lon <- deployDetails$Recovery_Longitude[1]

duration <- deployDetails$DeployDuration_days # Duration of deployment/recording

recording <- deployDetails$RecordDuration_hrs # Distance traveled
