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
basemap <- ggplot() + theme_void(legend=true) + geom_raster(data=lidar.df, aes(x=long, y=lat, fill=MAP)) +
  scale_fill_gradient2(low="#000000", mid="#969696", high="#ffffff", midpoint=179.1, 
                       na.value = NA, limits=c(170,188.2)) + theme(legend.position = "bottom")
basemap

#Read in modified list of 103 major municipal facilities
Maj.Muni <- read.csv("Major_Municipal_Outfalls.csv")

#Build map with points of 103 facilities
Outfall.Loc <- basemap + geom_polygon(data=IowaCounties.F, aes(x=long, y=lat, group=group), 
                                      color=alpha("black", 0.2), fill = NA) +
  geom_point(data=Maj.Muni, aes(x=Longitude, y=Latitude)) + labs(title="") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=24)) + 
  north(data=ia.counties.f) + scalebar(data=ia.counties.f, dist=50,
                            dd2km = TRUE, model="WGS84", location="bottomleft") 
  
Outfall.Loc
ggsave(file = "Facilities.jpg", dpi = 1200, scale = 1.5)

#Build map with weather stations
Station.Loc <- basemap + geom_polygon(data=IowaCounties.F, aes(x=long, y=lat, group=group), 
                                      color=alpha("black", 0.2), fill = NA) +
  geom_point(data=station.list, aes(x=long, y=lat), color="#084081") +
  north(data=ia.counties.f) + scalebar(data=ia.counties.f, dist=50, 
                                       dd2km = TRUE, model="WGS84", location="bottomleft") +
  labs(title="Locations of Weather Stations") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))

ggsave(file = "WeatherStationsblue.jpg", dpi = 1200, scale = 1.5)

#Build map with both facilitlies and weather stations
Combined.Loc <- basemap + geom_polygon(data=IowaCounties.F, aes(x=long, y=lat, group=group), 
                                      color=alpha("black", 0.2), fill = NA) +
  geom_point (data=station.list, aes(x=long, y=lat), color="#0868ac", fill = NA) +
  geom_point(data=Maj.Muni, aes(x=Longitude, y=Latitude), color=alpha("black")) + labs(title="") + 
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=24)) + 
  north(data=ia.counties.f) + scalebar(data=ia.counties.f, dist=50,
                                       dd2km = TRUE, model="WGS84", location="bottomleft") +
  scale_color_manual(values=c("#0868ac","black"),labels=c("Weather Stations","Facility Locations"))

ggsave(file = "FacilitiesandWeatherStations.jpg", dpi = 1200, scale = 1.5)

