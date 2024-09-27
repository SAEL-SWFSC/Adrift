# Example of updating existing WCA
library(sf)
library(here)
# this has column Area_Name and geometry, so data to replace should match
old <- readRDS('./data/map/WindCallBoundary.RData')
# point to new .shp file
newFile <-  '~/../Downloads/Shapefiles_OREGON_Wind_Energy_Proposed_Lease_Areas_GIS_Data/OREGON_Wind_Energy_Proposed_Lease_Areas_2024_BOEM.shp'
new <- read_sf(newFile)
new
#if aboev has a "Z" dimension we need to remove it with this line
# new <- st_zm(new, drop=TRUE)
# one of the column names should be the name of the region, it is not
# consistent so easiest way is to look at it
View(new)
# set this to whatever the region name column was
nameColumn <- 'Name'
# subset to just region column, then rename to "Area_Name"
new <- new[nameColumn]
colnames(new) <- c('Area_Name', 'geometry')
new <- st_transform(new, crs=st_crs(old))
# if updating existing region we need to remove them from old
old <- old[!old$Area_Name %in% new$Area_Name, ]
combined <- rbind(old, new)
plot(combined)
# save to whatever name you want. most convenient to not need to update other
# stuff would be to overwrite "WindCallBoundary.RData", but I'd back that old
# one up first just in case
saveRDS(combined, file='./data/map/WindCallBoundary.RData')
