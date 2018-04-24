library(ggplot2)
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(dplyr)

#Read in outfall locations
#outfalls <- read.csv("Outfall locations (2017-7-5) (1).csv")
lidar <- raster("lidar_hs.tif")

#Read in watershed shapefile and fortify
ia.watersheds.shp <- readOGR(dsn= getwd(), layer = "ia.watersheds.shapefile")
ia.watersheds.f <-fortify(ia.watersheds.shp)

#Subset data to list of 103 major municipal facilities
#Maj.Muni <- outfalls %>% filter(Permit.Type == "MUNICIPAL" & Class == "MAJOR")
#write.csv(Maj.Muni, "Major_Municipal_Outfalls.csv")

#Read in modified list of 103 major municipal facilities
Maj.Muni <- read.csv("Major_Municipal_Outfalls.csv")

#Setting projection, projecting it, converting to points
sr <- "+proj=longlat +e11ps=wgs84"
lidar.wgs84 <- projectRaster(lidar, crs = sr)
lidar.p <- rasterToPoints(lidar.wgs84)

#Turn points into datafram to use in ggplot
lidar.df <- data.frame(lidar.p)

#Make appropriate column headings
colnames(lidar.df) <- c("long", "lat","MAP")
lidar.df$MAP[lidar.df$MAP == 0] <- NA
lidar.df$MAP[lidar.df$MAP == 0.0] <- NA

#Create Iowa basemap
basemap <- ggplot() + theme_void() + geom_raster(data=lidar.df, aes(x=long, y=lat, fill=MAP)) +
  scale_fill_gradient2(low="black", mid="goldenrod4", high="bisque1", midpoint=179.1, 
                       na.value = NA, limits=c(170,188.2)) + theme(legend.position = "none")
basemap

#Build complete map with points of 103 facilities and watershed outlines
Watershed.pt.map <- basemap + geom_polygon(data= ia.watersheds.f, aes(x=long, y=lat, group=group), 
                                       color=alpha("black", 0.3), fill=NA) + 
                              geom_point(data=Maj.Muni, aes(x=Longitude, y=Latitude)) 

Watershed.pt.map

#Safe map as image
ggsave(file = "Watershed.pt.map.jpg", dpi = 1200, scale = 1.5)





