

#Load packages and background data/functions
# library(here)
# knitr::opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE)
# source(here::here("R/_commonR.r"))
# source(here::here("R/reportPlotFunctions.r"))

load(here("data", "gray", "grayBin_adrift.rdata"))
Species <- "Er"
binData <- grayBin_adrift
Bin<-binData%>%
  mutate(Region = case_when(
    DeploymentSite %in% c('Crescent City','HUM') ~ 'Humboldt',
    DeploymentSite %in% c('HMB','SF','SFB') ~ 'San Francisco',
    DeploymentSite %in% c('MBY','MOB') ~ 'Morro Bay',
    DeploymentSite %in% c('ORE')~ 'Oregon'))

Bin$season <- markSeason(binData$UTC)
Bin<- Bin[,c(4, 7, 11, 12)]


dfSummary <- Bin%>%
  group_by(season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(species == Species)),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )

dfSummary<- dfSummary[,c(1, 2, 5, 6)]

detTbl <- dfSummary %>%
  pivot_wider(names_from = "season", values_from = c("HourlyProb", "n"))%>%
  mutate (Region = fct_relevel(Region, c("Oregon", "Humboldt", "San Francisco", "Morro Bay")))%>%
  arrange(Region)%>%
  gt(
    groupname_col = "Species",
    rowname_col = "Region",
  )%>%
  DetTableTheme508()
detTbl
# 
# gtsave(detTbl, filename = here("output", "gray_detTbl.html"))



