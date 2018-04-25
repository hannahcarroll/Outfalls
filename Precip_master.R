pkgs <- c("ggmap", "rgdal", "rgeos", "maptools", "plyr", "dplyr", "tidyr", "tmap", "ggplot2", 
       "RColorBrewer", "maptools", "maps", "sp", "raster", "rgeos",
       "rvest", "readr", "SpatioTemporal", "rnoaa", "usethis") 

# If it wants to restart R, tell it yes. The work will be saved.
# Install the packages
install.packages(pkgs) # warning: this may take a number of minutes

# Run this code to load them into memory
lapply(pkgs, library, character.only = TRUE)

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
coords.muni <- cbind(Maj.Muni$Longitude, Maj.Muni$Latitude)
coords.precip <- cbind(precip.data$Long, precip.data$Lat)

#Turn into spacial points data frame
maj.muni.spdf <- SpatialPointsDataFrame(coords.muni, Maj.Muni)
precip.data.spdf <- SpatialPointsDataFrame(coords.precip, precip.data)

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
precip.data.planar <- spTransform( precip.data.spdf, CRS( "+init=epsg:3347" ))

# Nearest weather station to the outfall
maj.muni.spdf@data$nearest.station <- apply(gDistance(maj.muni.planar, precip.data.planar, byid=TRUE), 1, which.min)


maj.muni.stations <- cbind(Maj.Muni, Maj.Muni[nearest.precip.data,], 
                      apply(nearest.precip.data, 1, function(x) sort(x, decreasing=F)[1]))


######################################3
# We need precip data for all of Iowa
# Package rnoaa
# This uses a unique key that must be requested from NOAA


# Doing this saves it to the global environment so that you can use it in any R session
usethis::edit_r_environ()
# In the window that opens, paste in this:
# NOAA_KEY=your_noaa_key (without quotation marks)
# Then save it and restart your R session and reload packages

# All stations in Iowa that have the GHCND dataset available
station.locations <- ncdc_stations(datasetid='GHCND', locationid="FIPS:19", datatypeid = "PRCP",
                        limit = 1000, startdate = '1987-01-01', enddate = '2017-12-31')

# Pull out the columns we need
station.list <- station.locations$data[ ,c(4:5, 7, 9)]
station.list <- station.list[c(2,3,1,4)]
names(station.list) <- c("name", "id", "lat", "long")

station.list <- station.list %>%
  separate(id, c("dataset", "station.id"), ":")

station.locations.all <- ggplot() + theme_void() + geom_polygon(data=ia.counties.f, 
                                aes(x=long, y=lat, group=group), color="black", fill=NA) +
  geom_point(data=station.list, aes(x=long, y=lat), color="green4")