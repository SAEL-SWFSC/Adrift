
# #Load packages and background data/functions
# library(here)
# knitr::opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE)
# source(here::here("R/_commonR.r"))
# source(here::here("R/reportPlotFunctions.r"))

load(here("data", "Pm", "pmBin_adrift.rdata"))
binClick<-pmBinClick_adrift %>%
  mutate (Season = markSeason(pmBinClick_adrift$UTC),
          Region = map_chr(pmBinClick_adrift$Latitude, find_region)
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(call == "Clicks")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binClick<- binClick[,c(1, 2, 5, 6)]
binClick$CallType <- "Regular Clicks"
binClick <- binClick[!is.na(binClick$Region),]

binSlow<-pmBinSlow_adrift%>%
  mutate (Season = markSeason(pmBinSlow_adrift$UTC),
          Region = map_chr(pmBinSlow_adrift$Latitude, find_region),
          CallType = "Slow Clicks"
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(call == "Slow Clicks")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binSlow<- binSlow[,c(1, 2, 5, 6)]
binSlow$CallType <- "Slow Clicks"
binSlow <- binSlow[!is.na(binSlow$Region),]




dfSummary <- rbind(binClick, binSlow)

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

# gtsave(detTbl, filename = here("output", "sperm_detTbl.html"))

