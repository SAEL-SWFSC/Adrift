
# #Load packages and background data/functions
# library(here)
# knitr::opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE)
# source(here::here("R/_commonR.r"))
# source(here::here("R/reportPlotFunctions.r"))

#Prep Daily Probability dataset 
load(here("data", "Bm", "blueBin_adrift.rdata"))
binA<-bmAHourly_adrift %>%
  mutate (Season = markSeason(bmAHourly_adrift$UTC),
          Region = map_chr(bmAHourly_adrift$Latitude, find_region)
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(call == "A")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binA<- binA[,c(1, 2, 5, 6)]
binA$CallType <- "A"
binA <- binA[!is.na(binA$Region),]

binB<-bmBHourly_adrift%>%
  mutate (Season = markSeason(bmBHourly_adrift$UTC),
          Region = map_chr(bmBHourly_adrift$Latitude, find_region),
          CallType = "B"
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(call == "B")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binB<- binB[,c(1, 2, 5, 6)]
binB$CallType <- "B"
binB <- binB[!is.na(binB$Region),]


binD<-bmDHourly_adrift%>%
  mutate (Season = markSeason(bmDHourly_adrift$UTC),
          Region = map_chr(bmDHourly_adrift$Latitude, find_region),
          CallType = "D"
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(call == "D")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binD<- binD[,c(1, 2, 5, 6)]
binD$CallType <- "D"
binD <- binD[!is.na(binD$Region),]

dfSummary <- rbind(binA, binB, binD)

detTbl <- dfSummary %>%
  pivot_wider(names_from = "Season", values_from = c("HourlyProb", "n"))%>%
  mutate (Region = fct_relevel(Region, c("Oregon", "Humboldt", "San Francisco", "Morro Bay")))%>%
  arrange(Region)%>%
  gt(
    groupname_col = "CallType",
    rowname_col = "Region"
  )%>%
  DetTableTheme508()
detTbl

# gtsave(detTbl, filename = here("output", "blue_detTbl.html"))

