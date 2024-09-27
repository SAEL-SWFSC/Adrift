#Import Deployment Details (new and old), merge, and save as a useable dataframe in output folder

#See about direct access to document from Google Drive
# https://github.com/RVerse-Tutorials/GoogleDrive1


#For Old Deployments, keep relevant columns

oldDeploys <- read_csv(here("data", "Deployment Details - deployDetails.csv"), show_col_types = FALSE)
oldDeploys <- subset(oldDeploys, oldDeploys$Platform== 'drift')
oldDeploys$ChannelNumber_2 <- as.numeric(oldDeploys$ChannelNumber_2)
oldDeploys <- oldDeploys[, c(1:28, 32:41, 50:52, 60:63)]

#Read New Deployments, keep relevant columns
newDeploys <- read_csv(here("data", "Deployment Details - NEW DEPLOYMENT TO SAVE.csv"), show_col_types = FALSE)
newDeploys <- subset(newDeploys, newDeploys$Platform== 'drift')
x <- newDeploys[ !(newDeploys$`Drift#` %in% "EXAMPLE-000"), ]
newDeploys <- x[, c(6:33, 37:46, 55:57, 64:67)]

#Rename column names with those from oldDeploys
colnames(newDeploys)<- colnames(oldDeploys)

#Bind newDeploys and oldDeploys to make a single Deployment Dataframe
deployDetails <- rbind(oldDeploys, newDeploys)

#Add New Columns
deployDetails$DutyCycle <- if_else(
  deployDetails$RecordingDuration_m != "Continuous",  
  paste(deployDetails$RecordingDuration_m, "min for every", 
        deployDetails$RecordingInterval_m), "Continuous"
  )

#Remove buoys that were cancelled (never deployed)
deployDetails <-deployDetails %>%
  filter(!Status %in% c("Canceled", "In Prep"))

#Change all "Lost" and "Sunk" to "Failed"
deployDetails <- deployDetails %>%
  mutate(Status = case_when(
    Status == "Sunk" | Status == "Lost" ~ "Failed",
    Status == "Complete" ~ "Complete",
    Status == "Unusable" ~ "Unusable"
  ))


#Put Date/Time columns in POSIXct format
#Next: Need to tidy up, put into a prettier function, set decimals to 2
deployDetails$Deployment_Date <- parse_date_time(deployDetails$Deployment_Date, 
                                                  orders=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S'), 
                                                  truncate=3)
deployDetails$Recovery_Date <- parse_date_time(deployDetails$Recovery_Date, 
                                                 orders=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S'), 
                                                 truncate=3)
deployDetails$Data_Start <- parse_date_time(deployDetails$Data_Start, 
                                               orders=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S'), 
                                               truncate=3)
deployDetails$Data_End <- parse_date_time(deployDetails$Data_End, 
                                            orders=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S'), 
                                            truncate=3)

deployDetails$DeployDuration_days <- difftime(deployDetails$Recovery_Date, deployDetails$Deployment_Date, units = 'days')
deployDetails$RecordDuration_hrs <- difftime(deployDetails$Data_End, deployDetails$Data_Start, units = 'hours')

write.csv(deployDetails, here("output", "deployDetails.csv"))



