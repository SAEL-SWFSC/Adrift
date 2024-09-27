

#Create summary of hourly probability of detection by species, separated by season
#Inputs are binned data from the "formatBinnedPresence" function (in ADRIFT_report reportPlotFunctions)
#Examples of how to use the function at the end 
#Anne Simonis
#2024 May 09

# Updated 2024-08 by Taiki to fix issue with max prob > 1


library(here)
library(dplyr)
library(marmap)
library(lubridate)
library(ggplot2)
library(ggnewscale)
library(viridis)
library(sjPlot)


DailyProbCall<-function(Call=NULL,BinnedData){
#   BinnedData$BWPres<-NA
# Summary<-BinnedData %>%
#   mutate(Site = case_when(
#     DeploymentSite %in% c('Crescent City','HUM') ~ 'Humboldt',
#     DeploymentSite %in% c('HMB','SF','SFB') ~ 'San Francisco',
#     DeploymentSite %in% c('MBY','MOB') ~ 'Morro Bay',
#     DeploymentSite %in% c('ORE')~ 'Oregon')) %>%
#   mutate(Date=as.Date(UTC),Hour=hour(UTC))%>%
#   group_by(Date,Site) %>%
#   summarise(HourlyEffort=sum(pctEff),
#             BWPres=length(which(call==Call))) 
# 
# BinnedData$BWPres[is.na(BinnedData$BWPres)]<-0
# 
# Combined<-Summary %>%
#   mutate(Month=month(Date)) %>%
#   mutate(Season = factor(case_when(
#     Month %in% c(3,4,5,6) ~ 'Upwelling',
#     Month %in% c(7,8,9,10,11) ~'Post-Upwelling',
#     Month %in% c(12,1,2) ~'Winter')))%>%
#   group_by(Site,Season) %>%
#   reframe( Mean_Max=paste(round(mean(BWPres/HourlyEffort),2),' ','(',round(max(BWPres/HourlyEffort),2),')',sep=""))
#     # BWDailyProb_Mean = round(mean(BWPres/HourlyEffort),2),
#   #           BWDailyProb_sd= round(sd(BWPres/HourlyEffort),2),
#   #           # BWDailyProb_25 = quantile((BWPres/HourlyEffort),.25),
#   #           # BWDailyProb_75 = quantile((BWPres)/HourlyEffort,.75),
#   #           BWDailyProb_max = round(max(BWPres/HourlyEffort),2),
#            # ,)
#  
# Combined$Call<-Call
# return(Combined)
    BinnedData$BWPres<-NA
    Summary<-BinnedData %>%
        mutate(Site = case_when(
            DeploymentSite %in% c('Crescent City','HUM') ~ 'Humboldt',
            DeploymentSite %in% c('HMB','SF','SFB') ~ 'San Francisco',
            DeploymentSite %in% c('MBY','MOB') ~ 'Morro Bay',
            DeploymentSite %in% c('ORE')~ 'Oregon'),
            Month=month(UTC),
            Season = factor(case_when(
                Month %in% c(3,4,5,6) ~ 'Upwelling',
                Month %in% c(7,8,9,10,11) ~'Post-Upwelling',
                Month %in% c(12,1,2) ~'Winter'))) %>%
        group_by(Site, Season) %>%
        summarise(HourlyEffort=sum(pctEff),
                  BWPres=length(which(call==Call)),
                  Mean_Max=as.character(round(sum(BWPres) / sum(HourlyEffort), 2)))
    
    Summary$HourlyEffort <- NULL
    Summary$BWPres <- NULL
    Summary$Call<-Call
    return(Summary)
}

# #Examples of how to use the function
# SumMs<-DailyProb(Species='MS',BinnedData=bwBinMs_adrift)
# SumZc<-DailyProb(Species='ZC',BinnedData=bwBinZc_adrift)
# SumBb<-DailyProb(Species='BB',BinnedData=bwBinBb_adrift)
# SumMc<-DailyProb(Species='MC',BinnedData=bwBin39_adrift)
# 


