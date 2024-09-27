#Evaluate variation in beaked whale detections within clustered deployments
#Started with Morro Bay (June 2022, March 2023, and Nov 2023; 
#Omitted June 2023 drifters as they did not sample beaked whale habitat


library(ggplot2)
library(gridExtra)
library(data.table)
library(ggmap)
library(osmdata)

#Load Effort and Detection Data
effortBase <- readRDS(here('data/AllDrifts_BaseEffort.rds'))
badPascal <- c('PASCAL_014')
gps <- readRDS(here('data/AllDeploymentGPS.rds'))
effortBase <- effortBase[!effortBase$DriftName %in% badPascal, ]
effortMin <- effortToBins(effortBase, bin='min')
effortMin_adrift <- filter(effortMin, grepl('ADRIFT', DriftName))

bwData_adrift <- readRDS(here('data/bw/bwData_adrift.rds'))

#Focus on Ziphius
bwBinZc_adrift <- formatBinnedPresence(filter(bwData_adrift, species == 'ZC'),
                                       effort=effortMin_adrift,
                                       bin='min',
                                       gps=gps)
BinnedData<-bwBinZc_adrift  
Species<-'ZC'
BinnedData$BWPres<-NA

#Filter based on CCC survey periods that sampled beaked whale habitat
June22<-BinnedData %>%
  filter(DeploymentSite %in% c('MBY','MOB')) %>%
  mutate(Date=as.Date(UTC),Hour=hour(UTC))%>%
  filter(month(Date)==6, year(Date)==2022) %>%
  group_by(DriftName,Date,Hour) %>%
  summarise(Zc_MinPerHour=length(which(species==Species))) %>%
  mutate(DateTime=as.POSIXct(paste(Date,Hour,sep=" "),format='%Y-%m-%d %H'))

March23<-BinnedData %>%
   filter(DeploymentSite %in% c('MBY','MOB')) %>%
    mutate(Date=as.Date(UTC),Hour=hour(UTC))%>%
    filter(month(Date)==3, year(Date)==2023) %>%
    group_by(DriftName,Date,Hour) %>%
    summarise(Zc_MinPerHour=length(which(species==Species))) %>%
  mutate(DateTime=as.POSIXct(paste(Date,Hour,sep=" "),format='%Y-%m-%d %H'))

Nov23<-BinnedData %>%
  filter(DeploymentSite %in% c('MBY','MOB')) %>%
  mutate(Date=as.Date(UTC),Hour=hour(UTC))%>%
  filter(month(Date)==11, year(Date)==2023) %>%
  group_by(DriftName,Date,Hour) %>%
  summarise(Zc_MinPerHour=length(which(species==Species))) %>%
  mutate(DateTime=as.POSIXct(paste(Date,Hour,sep=" "),format='%Y-%m-%d %H'))

#Create Plots
June22Plot<-ggplot(June22,aes(DateTime,Zc_MinPerHour,color=DriftName))+
  geom_line()+
  theme_bw()+
  ylab('Minutes per hour w/Zc detections')+
  ylim(0,60)+
  ggtitle('Zc:June 2022')

March23Plot<-ggplot(March23,aes(DateTime,Zc_MinPerHour,color=DriftName))+
  geom_line()+
  theme_bw()+
  ylab('Minutes per hour w/Zc detections')+
  ylim(0,60)+
  ggtitle('Zc:March 2023')

Nov23Plot<-ggplot(Nov23,aes(DateTime,Zc_MinPerHour,color=DriftName))+
  geom_line()+
  theme_bw()+
  ylab('Minutes per hour w/Zc detections')+
  ylim(0,60)+
  ggtitle('Zc:November 2023')

grid.arrange(June22Plot,March23Plot,Nov23Plot,nrow=3)

ViolinJune22<-ggplot(June22,aes(factor(Date),Zc_MinPerHour,color=DriftName))+
  geom_violin()+ylab('Minutes per Hour w/Zc Detections')+
  xlab('Date')+
  ggtitle('Zc:June 2022')+
  ylim(0,50)

ViolinMarch23<-ggplot(March23,aes(factor(Date),Zc_MinPerHour,color=DriftName))+
  geom_violin()+ylab('Minutes per Hour w/Zc Detections')+
  xlab('Date')+
  ggtitle('Zc:March 2023')+
  ylim(0,50)

ViolinNov23<-ggplot(Nov23,aes(factor(Date),Zc_MinPerHour,color=DriftName))+
  geom_violin()+ylab('Minutes per Hour w/Zc Detections')+
  xlab('Date')+
  ggtitle('Zc:November 2023')+
  ylim(0,50)

grid.arrange(ViolinJune22,ViolinMarch23,ViolinNov23,nrow=3)



# ##heatmap not working yet 
# JuneBinned<-filter(BinnedData,DriftName %in% paste0('ADRIFT_0',19:26))
# 
# data<-JuneBinned
# # generate bins for the x, y coordinates
# xbreaks <- seq(floor(min(data$Latitude)), ceiling(max(data$Latitude)), by = 0.01)
# ybreaks <- seq(floor(min(data$Longitude)), ceiling(max(data$Longitude)), by = 0.01)
# 
# # allocate the data points into the bins
# data$latbin <- xbreaks[cut(data$Latitude, breaks = xbreaks, labels=F)]
# data$longbin <- ybreaks[cut(data$Longitude, breaks = ybreaks, labels=F)]
# 
# # # Summarise the data for each bin
# # datamat <- data[, list(average_rate_per_night = mean(average_rate_per_night)), 
# #                 by = c("latbin", "longbin")]
# datamat <- data %>% 
#   group_by(latbin, longbin)%>%
#    mutate(total_min = length(Species=='ZC'))
# 
# # Merge the summarised data with all possible x, y coordinate combinations to get 
# # a value for every bin
# datamat <- merge(setDT(expand.grid(latbin = xbreaks, longbin = ybreaks)), datamat, 
#                  by = c("latbin", "longbin"), all.x = TRUE, all.y = FALSE)
# # Fill up the empty bins 0 to smooth the contour plot
# datamat[is.na(average_min), ]$average_min <- 0
# 
# 
# ggplot(datamat,aes(latbin,longbin,fill=total_min))+geom_tile()
