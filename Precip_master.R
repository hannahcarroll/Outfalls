pkgs <- c("ggmap", "rgdal", "rgeos", "maptools", "plyr", "dplyr", "tidyr", "tmap", "ggplot2", 
       "RColorBrewer", "maptools", "maps", "sp", "raster", "rgeos",
       "rvest", "readr", "SpatioTemporal", "rnoaa", "usethis", "devtools", "ggsn") 

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
ia.counties <- spTransform(ia.counties, CRS("+proj=longlat +datum=WGS84")) 
proj4string(precip.data.spdf) = CRS("+proj=longlat +datum=WGS84")

# This returns a list of data frames. The empty data frames are counties with no
# outfalls - and hence there is no need for precip data there.
bycounty <- over(ia.counties, maj.muni.spdf, returnList = TRUE)

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

# This is no longer needed
#station.list <- station.list %>%
#  separate(id, c("dataset", "station.id"), ":")

station.locations.all <- ggplot() + theme_void() + geom_polygon(data=ia.counties.f, 
                                aes(x=long, y=lat, group=group), color="black", fill=NA) +
  geom_point(data=station.list, aes(x=long, y=lat), color="green4") +
  north(data=ia.counties.f) + scalebar(data=ia.counties.f, dist=50, 
                                       dd2km = TRUE, model="WGS84", location="bottomleft") +
  labs(title="Locations of Weather Stations") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))


#Turn into spatial points data frame
station.list.spdf <- SpatialPointsDataFrame(coords.stations, station.list)

#Project and set coordinate reference system
proj4string(station.list.spdf) = CRS("+proj=longlat +datum=WGS84")

# Code from: https://www.nceas.ucsb.edu/scicomp/usecases/AssignClosestPointsToPoints
#  Define these vectors, used in the loop.

   closest.station.vec <- vector(mode = "numeric",length = nrow(maj.muni.spdf))
   min.dist.vec     <- vector(mode = "numeric",length = nrow(maj.muni.spdf))

# Get the vector index of the weather station closest to each outfall.
# Use the spDistsN1 function to compute the distance vector between each
# field station site and all of the temperature stations. Then, find and
# retain the actual temperature, and the index of the closest temperature
# to each transect station.
#
# spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
#
# where:
#         pointList   : List of candidate points.
#         pointToMatch: Single point for which we seek the closest point in pointList.
#         longlat     : TRUE  computes Great Circle distance in km,
#                       FALSE computes Euclidean distance in units of input geographic coordinates
#
# We use Great Circle distance to increase distance calculation accuracy at high latitudes
# See the discussion of distance units in the header portion of this file
#
# minDistVec stores distance from the closest temperature station to each density measurement point.
# closestSiteVec stores the index of the closest temperature station to each density measurement point.
#
   for (i in 1 : nrow(maj.muni.spdf))
   {
      distVec <- spDistsN1(station.list.spdf,maj.muni.spdf[i,],longlat = TRUE)
     min.dist.vec[i] <- min(distVec)
      closest.station.vec[i] <- which.min(distVec)
   }
#
# Create the Temperature Assignment table: merge the temperature point list with the transect point list
# into a five-column table by merging the temperature point and transect point lists.
#
   PointAssignstation <- as(station.list.spdf[closest.station.vec,]$id,"character")
   FinalTable = data.frame(coordinates(maj.muni.spdf),maj.muni.spdf,
                                    closest.station.vec,min.dist.vec,PointAssignstation)

# Now get the data:
stations <- as.list(unique(PointAssignstation))   

# Generate lists of ISO formatted dates for both daily and monthly flow data
# length.out corresponds to the number of days or months from the date of the first
# record in the corresponding outfall datset to the last.
dates.daily <- c(seq(as.Date("2003-09-01"), by="day", length.out = 5267))
dates.monthly <- c(seq(as.Date("1987-01-01"), by="month", length.out = 373))

# Initialie variables (function needs something blank to write into)
precip.daily <- NULL
precip.monthly <- NULL

# Length of the lists of dates on lines 169 and 170
vals.monthly <- 1:373
vals.daily <- 1:5267

# A variable describing the lengths
n.m    <- length(vals.monthly)
n.d <- length(vals.daily)

# The cool bit. This code generates a unique name for each dataset we call from NOAA
# paste means "make words out of this instead of calling an object"
data.month  <- paste("precip.monthly",    1:n.m,     sep="")
data.day <- paste("precip.daily", 1:n.d, sep="")

# Monthly:
tic()
precip <- NULL
 for (i in 1:length(dates.monthly))
 { precip <- try(ncdc(datasetid='GHCND', datatypeid = "PRCP", stationid = stations[1:99],
                        limit = 1000, startdate = paste(dates.monthly[i]), 
                     enddate = paste(dates.monthly[i]), includemetadata=FALSE))
        assign(data.month[i], precip)
 Sys.sleep(0.2)
 }
# We can use faster code now that we're done with ftp
precip.monthly.list <- lapply(ls(pattern="precip.monthly[0-9]+"), function(x) get(x))
monthly.precip.data <- lapply(precip.monthly.list, function (x) x["data"])
monthly.precip.df <- ldply(monthly.precip.data, data.frame)
write.csv(x=monthly.precip.df, file="monthlyprecipdata.csv", row.names = FALSE)
     rm(list=(paste(data.month)))
     rm(monthly.precip.data)
     rm(precip.monthly.list)
toc()

# Daily:
tic()
precip <- NULL
 for (i in 1:length(dates.daily))
 { precip <- try(ncdc(datasetid='GHCND', datatypeid = "PRCP", stationid = stations[1:99],
                        limit = 1000, startdate = paste(dates.daily[i]), 
                     enddate = paste(dates.daily[i]), includemetadata=FALSE))
        assign(data.day[i], precip)
 Sys.sleep(0.2)
 }
# We can use faster code now that we're done with ftp
precip.daily.list <- lapply(ls(pattern="precip.daily[0-9]+"), function(x) get(x))
daily.precip.data <- lapply(precip.daily.list, function (x) x["data"])
daily.precip.df <- ldply(daily.precip.data, data.frame)
write.csv(x=daily.precip.df, file="dailyprecipdata.csv", row.names = FALSE)
     rm(list=(paste(data.day)))
     rm(daily.precip.data)
     rm(precip.daily.list)
toc()
