#Written by Kaitlin Palmer March 2024
#Modified by Shannon Rankin to use here() for reproducibility and to merge images (line226) 
#Modified by Shannon Rankin again to change lat/longs to better pair with Southall et al 2023 (May 2024

library(ggOceanMaps)
library(mapdata)
library(sf)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(here)

# This script creates a figure of the different regions drifters are used in
# and combines it with a table created from the deployment site sheet from the
# deployment information. Assumes that add deployment locations are organized
# North to south. Currently lat limits are hard coded but these should ultimately
# go into the deployment sheet for sanity and efficiency. 



# plotting will crash otherwise
sf_use_s2(FALSE) 

# Get world regions to plot
world <- ne_countries(scale = "medium", returnclass = "sf")


#eez boundaries from: https://www.marineregions.org/gazetteer.php?p=details&id=8456
eezUS = read_sf(here("data/USeez/eez.shp"))
eezMexico = read_sf(here("data/MexicoEEZ/eez.shp"))

crop_factor <- st_bbox(c(xmin = -130, 
                         xmax = -105, 
                         ymax = 50, 
                         ymin = 22.9),
                       crs = st_crs(eezMexico))
eezMexico <- st_crop(eezMexico, crop_factor)

#test
ggplot(eezMexico)+geom_sf()



# Filter polygons west of -110 degrees longitude
# Create a bounding box for the area west of -110 degrees longitude
bbox <- st_bbox(c(xmin = -115, xmax = -160, 
                  ymin = 0, ymax = 90), 
                crs = st_crs(eezUS))
# Crop the EEZ polygons to the area west of -110 degrees longitude
eez_west <- st_crop(eezUS, bbox)


# Basemap with wet coast and bathymetry, state lines and EEZ
# p<- basemap(limits = c(-130, -117, 30, 50),
#             bathy.style = "rcb", grid.col = NA) 
# Trouble downloading high res grid, site down try again later

p<-basemap(limits = c(-130, -105, 20, 50), bathymetry = TRUE,
           grid.col = NA)+
  geom_sf(data = world)+
  #geom_sf(data=eez_west,inherit.aes = FALSE)+
  #geom_sf(data=eezMexico,inherit.aes = TRUE)+
  coord_sf(xlim = c(-130, -105), ylim = c(20, 50), expand = FALSE)+
  guides(fill="none")


# Test
p

#########################################################################
# Cut up the regions 
##########################################################################
# siteList = read.csv(here("data/Deployment Details - Region.csv")) [,4:8]
# # siteList = read.csv(here("data/Deployment Details - Site List.csv"))  
# siteList<-siteList[1:15,]
# tableData = siteList[,c(3:5)]
# tableData$Color <- ""
# tableData= tableData[,c(4,1,2,3)]

# Shannon's revised regions
siteList = read.csv(here("data/DrifterRegions.csv"))
# need to drop the so cal minimum bc of weird shape
siteList$Lat_min[siteList$Region.Code == 'SCB'] <- 30
tableData <- siteList[c('Region.Code', 'Adrift.Regions', 'Description')]
tableData$Color <- ''
tableData <- tableData[c('Color', 'Region.Code', 'Adrift.Regions', 'Description')]
colnames(tableData)[3] <- 'ADRIFT.Regions'

# Add latitude limits for each  
# siteList$lat_min = NA
# siteList$lat_max = NA
# 
# # dear me....
# siteList$lat_max[1]<-49
# siteList$lat_max[2]<-46.2469
# siteList$lat_max[3]<-42
# siteList$lat_max[4]<-40.74
# siteList$lat_max[5]<-40.44
# siteList$lat_max[6]<-38.90
# siteList$lat_max[7]<-37.77
# siteList$lat_max[8]<-37.463
# siteList$lat_max[9]<-36.800
# siteList$lat_max[10]<-35.36
# siteList$lat_max[11]<-34.448
# siteList$lat_max[12]<-34.05
# siteList$lat_max[13]<-33.46
# siteList$lat_max[14]<-32.7157
# siteList$lat_max[15]<-28.19
# 
# 
# siteList$lat_min[1:14]<- siteList$lat_max[2:15]
# siteList$lat_min[15]<-22.9

# siteList$lat_min[13]<-0 #runs into the southern EEZ

#Shannon Add this to modify selection


#End Shannon's Mods
# branded_colors = data.frame(color = c("#00798c", "#d1495b", "#66a182",
#                                       "#F3F5F4", "#C2EABD", "#7E52A0",
#                                       "#C0BABC","#05B2DC"))
# 
# branded_colors = data.frame(color = c("#edae49","#00798c", "#d1495b",
#                                       "#66a182","#2e4057","#8d96a3",
#                                       "#F3F5F4", "#C2EABD","#CD533B",
#                                       "#033860","#7E52A0","#A37C40",
#                                       "#C1D2BD","#C7AC92", "#C0BABC",
#                                       "#3D2B3D","#05B2DC"))
branded_colors = data.frame(color = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
                                      '#0072B2', '#D55E00', '#CC79A7', '#45c929', 
                                      '#9e3e04', '#5b33e5'))

# Crete all the areas and regions, add to table
lonMin = -160; lonMax = -100 # max extents

for(ii in seq_len(nrow(siteList))) {
  
  # Pull latitudes from site list, hopefully this will be in the actual
  # spreadsheet to avoid hard-coding
  
  latMax = siteList$Lat_max[ii]
  latMi = siteList$Lat_min[ii]
  
  # dummy matrix of extents
  aa = matrix(c(lonMax,lonMin,lonMin,lonMax,lonMax,
                latMi,latMi,latMax,latMax,latMi),,2)
  
  # Create rectangle to filter
  Poly_Coord_df = data.frame(lon = aa[,1],
                             lat = aa[,2])
  
  # Convert the box to a sfc object so it can be intersected with the eez 
  # regions
  region <- Poly_Coord_df %>% 
    st_as_sf(coords = c("lon", "lat"), 
             crs = st_crs(eez_west)) %>% 
    st_bbox() %>% 
    st_as_sfc()
  
  # Intersect with the eez to create actual region, USA or mexico
  if(siteList$Region.Code[ii] %in% c('BCN', 'BCS')) {
      region = st_intersection(region, eezMexico)
  } else {
      region = st_intersection(region, eez_west)
  }
  
  # Add the region to the plot
  p=p +geom_sf(data = region, 
               fill= branded_colors$color[ii],
               alpha =.9)
  
}

# add the limits back in
p =p + coord_sf(xlim = c(-130, -105), ylim = c(20, 50), expand = FALSE)
p
####################################
## Combine figure and table
#########################################

library(ggpubr)

# Create a table to display, create color column and reorganize

######################################################
### MOVED tableData CREATION TO DATA LOADING ABOVE ###
######################################################

# Summary table plot, medium orange theme
stable.p <- ggtexttable(tableData, rows = NULL, 
                        theme = ttheme("classic",  base_size = 12))



for(ii in 1:nrow(tableData)){
  
  stable.p <- table_cell_bg(stable.p, row = ii+1, 
                            column = 1, 
                            fill= as.character(branded_colors$color[ii]))
  
}

##########################################################
# Export the images
#########################################################

library(ragg) # to make figure sizes consistant 

p<-p+theme(plot.margin = unit(c(0,0.2,0,1), 'lines'))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# Arrange the plots on the same page
imageFile<- ggarrange(p, stable.p,
                      ncol = 2, nrow = 1,
                      heights = c(1, 1),
                      widths = c(1, 1))

ragg::agg_jpeg(here("output/DrifterRegions3.jpg"), 
               width = 40, 
               height = 30, 
               units = "cm", res = 300)
print(imageFile)
dev.off()






ragg::agg_jpeg(here("output/DrifterRegions3.jpg"), 
               width = 15, 
               height = 20, 
               units = "cm", res = 300)
print(p)
dev.off()


ragg::agg_jpeg(here("output/DrifterRegionsTable3.jpg"), 
               width = 25, 
               height = 15, 
               units = "cm", res = 300)
print(stable.p)
dev.off()


##########################################################
# Merge the images
#########################################################
library(magick)
regionMap <- magick::image_read(here("output/DrifterRegions3.jpg"))%>%
  image_scale("450")%>%
  image_border("white", "10x20")
regionTable <- magick::image_read(here("output/DrifterRegionsTable3.jpg"))%>%
  image_scale("1100")%>%
  image_trim(fuzz = 0) %>%
  image_border("white","10x45")

DrifterRegionFigure <- image_append(c(regionMap, regionTable))
  
print(DrifterRegionFigure, info=FALSE)
image_write(DrifterRegionFigure, path = here("figs/DrifterRegions.jpg"), format = "jpg")
