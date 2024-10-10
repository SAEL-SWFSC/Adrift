
# #Load packages and background data/functions
# library(here)
# knitr::opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE)
# source(here::here("R/_commonR.r"))
# source(here::here("R/reportPlotFunctions.r"))

load(here("data", "humpback", "humpBin_adrift.rdata"))
binSong<-hmpBinSong_adrift %>%
  mutate (Season = markSeason(hmpBinSong_adrift$UTC),
          Region = map_chr(hmpBinSong_adrift$Latitude, find_region)
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(call == "Song")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binSong<- binSong[,c(1, 2, 5, 6)]
binSong$CallType <- "Song"
binSong <- binSong[!is.na(binSong$Region),]

binSocial<-hmpBinSoc_adrift%>%
  mutate (Season = markSeason(hmpBinSoc_adrift$UTC),
          Region = map_chr(hmpBinSoc_adrift$Latitude, find_region),
          CallType = "B"
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(call == "Social")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binSocial<- binSocial[,c(1, 2, 5, 6)]
binSocial$CallType <- "Social"
binSocial <- binSocial[!is.na(binSocial$Region),]


binUnid <- hmpBinReg_adrift%>%
  mutate (Season = markSeason(hmpBinReg_adrift$UTC),
          Region = map_chr(hmpBinReg_adrift$Latitude, find_region),
          CallType = "Unidentified"
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(call == "unidentified")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binUnid<- binUnid[,c(1, 2, 5, 6)]
binUnid$CallType <- "Unidentified"
binUnid <- binUnid[!is.na(binUnid$Region),]

dfSummary <- rbind(binSong, binSocial, binUnid)

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
# gtsave(detTbl, filename = here("output", "hump_detTbl.html"))

