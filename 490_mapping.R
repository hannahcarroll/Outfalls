library(ggplot2)
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(dplyr)

#Read in outfall locations
#outfalls <- read.csv("Outfall locations (2017-7-5) (1).csv")
lidar <- raster("lidar_hs.tif")


#Read in county map, fortify and subset for Iowa
CountyLine <- readOGR(dsn= "/Users/lindsaykaymack/Documents/ENSCI 490/Flow Data Form 1", 
                      layer = "cb_2016_us_county_500k")

IowaCounties <- subset(CountyLine, STATEFP %in% "19")
IowaCounties.F <- fortify(IowaCounties)

#Subset data to list of 103 major municipal facilities
#Maj.Muni <- outfalls %>% filter(Permit.Type == "MUNICIPAL" & Class == "MAJOR")
#write.csv(Maj.Muni, "Major_Municipal_Outfalls.csv")

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

#Read in modified list of 103 major municipal facilities
Maj.Muni <- read.csv("Major_Municipal_Outfalls.csv")

#Build map with points of 103 facilities
Outfall.Loc <- basemap +geom_polygon(data=IowaCounties.F, aes(x=long, y=lat, group=group), 
                                     color=alpha("black", 0.2), fill = NA) +
  geom_point(data=Maj.Muni, aes(x=Longitude, y=Latitude)) + 
  geom_text(data=Maj.Muni, aes(x=Longitude, y=Latitude, label=Facility.Name), size=3, 
            check_overlap = TRUE) 
  
Outfall.Loc

#Safe map as image
ggsave(file = "IA_103_MajorMuniciapalFacilitiesw/names.jpg", dpi = 1200, scale = 1.5)





