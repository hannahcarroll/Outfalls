library(readxl)
library(dplyr)
library(plyr)

###################################################################################
#Flow data
#Create data frame for daily data
#file.list <- list.files(pattern = '*.xlsx')
#df.list <- lapply(file.list, read_excel)
#daily.df <- ldply(df.list, data.frame)

#Save as .csv
#write.csv(daily.df, "daily flow.csv")
daily.flow <- read.csv(file="daily flow.csv", header=TRUE)

#Create data frame for monthly data
#file.list2 <- list.files(pattern = '*.xlsx')
#df.list2 <- lapply(file.list2, read_excel)
#monthly.df <- ldply(df.list2, data.frame)

#Save as .csv
#write.csv(monthly.df, "monthly flow.csv")

monthly.flow <- read.csv(file="monthly flow.csv", header=TRUE)

# Get the precip information:
dailyprecip <- read.csv(file="dailyprecipdata.csv", header=TRUE)
monthlyprecip <- read.csv(file="monthlyprecipdata.csv", header=TRUE)

library(SpatioTemporal)
daily.flow$Date <- as.Date(daily.flow$Mon.Date, "%m/%d/%Y")
daily.flow <- daily.flow[,c(1:4,14,5:13)]
daily.flow$Daily.Value[daily.flow$Daily.Value > 131.6] <- NA
daily.flow$Daily.Value[daily.flow$Daily.Value == 0] <- NA
daily.flow$Facility.Type <- as.factor(daily.flow$Facility.Type)

# Clean this up for use in your talk
daily.plot <- ggplot(data=daily.flow, aes(x=Date, y=Daily.Value, color=Facility.Name)) + 
  geom_line() + theme_gray(base_size = 12) + theme(legend.position="none") + 
  labs(x ="Date", y ="Daily Flow (mgd)", title="Daily Flow by Facility and Treatment Type") + 
  facet_wrap(~Facility.Type, scales="fixed") 

# Create an index of which facility gets which weather data
station.matching.daily <- FinalTable[ ,c(6,54)]

# Append that information to the flow data

daily.flow2 <- merge(daily.flow, station.matching.daily, by="Facility.Name", fill=TRUE)

# Make the column names match
# Remember that the [15] is a column index
colnames(daily.flow2)[15] <- "data.station"  

# Next, reformat the dates in daily.precip.df to match those of daily.flow2
##### Note that you will first need to split the column at T.
##### Look at Precip_master lines 105 and 106 for the form to use.
##### Date format info here: https://www.stat.berkeley.edu/~s133/dates.html

# Make the column names match

# Now add in the precip values by merging daily.flow2 and daily.precip.df 
# (hint: what are the two pieces of information we need to match up the rows?)
daily.flow3 <- # use the form: by=c(" ", " "))

# Now we create the time series

# Follow this form for daily.flow3, making sure to include both flow and precip columns
  # when performing lapply operation.
  
# A time series is a special kind of object in R. We'll create them with package zoo.
library(zoo)

# Split by facility name using df2 <- split(df, df$Factor.to.Split.by)
daily <- split(daily.flow2, daily.flow2$Facility.Name)

ts.by.facility.daily <- lapply(daily, function(x) zoo(x[,7],as.Date(x$Date)))

# Now repeat for monthly, making sure not to overwrite any of the names for daily