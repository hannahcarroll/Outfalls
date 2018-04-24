library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(rvest)

#Read in watershed shapefile and fortify
ia.watersheds.shp <- readOGR(dsn= getwd(), layer = "ia.watersheds.shapefile")
ia.watersheds.f <-fortify(ia.watersheds.shp)

#Plot watershed
ia.watersheds.plot <- ggplot() + 
  geom_polygon(data= ia.watersheds.f, aes(x=long, y=lat, group=group), color="black", fill=NA)
ia.watersheds.plot
