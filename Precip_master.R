library(plyr)
library(dplyr)
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)

###################################################################################
#Precip data

# How I created the dataset:
# Six files of daily ranfall data were generated using NOAA Climate Data Center
# This code reads those six files in as one data frame. Note that the code is a bit
# different for csv vs xlsx.

#file.list.precip <- list.files(pattern='*.csv')
#precip.data <- do.call(rbind,lapply(file.list.precip,read.csv))

# Write the combined data sets into one file
#write.csv(x=precip.data, file="daily precip.csv", row.names=FALSE, col.names = TRUE)

# Now we work from that file
precip.data <- read.csv(file="daily precip.csv", header=TRUE)

# This is a layer of all of the counties in the US and its territories
#counties <- readOGR(dsn = getwd(), layer = "cb_2015_us_county_500k")

# Transform to the same projection as our other data
#counties.wgs84 <- spTransform(counties, CRS("+proj=longlat +ellps=WGS84"))

# Note that we subset a spatial polygons data frame differently from a regular data frame
# The State fip code for Iowa is 19, so we subset using that.
#ia.counties <- counties.wgs84[counties.wgs84@data$STATEFP == 19, ]

# Create a new layer and then work from that
#writeOGR(ia.counties, dsn=getwd(), layer="iowa.counties.shapefile", driver="ESRI Shapefile")

ia.counties <- readOGR(dsn=getwd(), layer="iowa.counties.shapefile")
ia.counties.f <- fortify(ia.counties)

#Read in watershed shapefile and fortify
ia.watersheds.shp <- readOGR(dsn= getwd(), layer = "ia.watersheds.shapefile")
ia.watersheds.f <-fortify(ia.watersheds.shp)

#Read in modified list of 103 major municipal facilities
Maj.Muni <- read.csv("Major_Municipal_Outfalls.csv")

# Try plottting without the coord_map argument and see how it looks
# If this locks up your computer, use coord_quickmap() instead of coord_map().
messy.map <- ggplot() + theme_void() +
  geom_polygon(data=ia.counties.f, aes(x=long, y=lat, group=group), color="grey15", fill=NA) +
  geom_polygon(data=ia.watersheds.f, aes(x=long, y=lat, group=group), 
               color="grey15", fill="grey90", alpha=0.6) +
  geom_point(data=precip.data, aes(x=Long, y=Lat), color="red") +
# You will need to call your major municipal dataset in the next line  
  geom_point(data=Maj.Muni, aes(x=Longitude, y=Latitude), 
             color="black", size=3) + coord_map()

# This map is too messy to present, but it's useful for us to see how things are laid
# out spatially.

#Extract the numerical coordinates and combine by column (Lat,Long)
coords <- cbind(Maj.Muni$Longitude, Maj.Muni$Latitude)
coords <- cbind(precip.data$Long, precip.data$Lat)

#Turn into spacial points data frame
maj.muni.spdf <- SpatialPointsDataFrame(coords, Maj.Muni)
precip.data.spdf <- SpatialPointsDataFrame(coords, precip.data)

#Project and set coordinate reference system
proj4string(maj.muni.spdf) = CRS("+proj=longlat +datum=WGS84")
proj4string(ia.counties) = CRS("+proj=longlat +datum=WGS84")
proj4string(precip.data.spdf) = CRS("+proj=longlat +datum=WGS84")

# This proj4string(ia.counties) = CRS("+proj=longlat +datum=WGS84") command
# throws an error, but it's okay - we're transforming to the same thing
# it already was, just changing the wording a little so our identicalCRS call doesn't fail.

# This returns a list of data frames. The empty data frames are counties with no
# outfalls - and hence there is no need for precip data there.
bycounty <- over(ia.counties, maj.muni.spdf, returnList = TRUE)

# For the point-to-point operation:

#Transform to planar coordinates
maj.muni.planar <- spTransform( maj.muni.spdf, CRS( "+init=epsg:3347" )) 


# Hint: Rgeos has a super easy function for finding the nearest point to a point.
# Here is the basic form:
maj.muni.planar$data$nearest_in_precip.data.spdf <- apply(gDistance(maj.muni.planar, 
                                               precip.data.spdf, byid=TRUE), 1, which.min)
summary(maj.muni.planar)

maj.muni.planar$data$nearest_in_precip.data.spdf
coords <- cbind(maj.municipal$Longitude, maj.municipal$Latitude)
maj.muni.spdf <- SpatialPointsDataFrame(coords, maj.municipal)

proj4string(spdf) = CRS("+proj=longlat +datum=WGS84")
proj4string(ia.counties) = CRS("+proj=longlat +datum=WGS84")
# This proj4string(ia.counties) = CRS("+proj=longlat +datum=WGS84") command
# throws an error, but it's okay - we're transforming to the same thing
# it already was, just changing the wording a little so our identicalCRS call doesn't fail.

bycounty <- over(ia.counties, spdf, returnList = TRUE)

# Hint: Rgeos has a super easy function for finding the nearest point to a point.
# Here is the basic form:
library(rgeos)
set1$nearest_in_set2 <- apply(gDistance(set1sp, set2sp, byid=TRUE), 1, which.min)
