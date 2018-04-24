library(readxl)
library(dplyr)
library(plyr)
library(magrittr)
library(ggplot2)

#Create data frame for daily data
file.list <- list.files(pattern = '*.xlsx')
df.list <- lapply(file.list, read_excel)

daily.df <- ldply(df.list, data.frame)

#Create data frame for monthly data
file.list <- list.files(pattern = '*.xlsx')
df.list <- lapply(file.list, read_excel)

monthly.df <- ldply(df.list, data.frame)

#Plot data (point, smooth, line, facet)
#Points
daily.plot1 <- ggplot(daily.df, aes(Mon.Date, Daily.Value)) + theme_minimal() + 
  geom_point(color='dodgerblue4') + ggtitle("Daily Flow Data") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(x = "Monitoring Date", y = "Daily Flow Value (MGD)")

daily.plot1

monthly.plot1 <- ggplot(monthly.df, aes(End.Date, Avg)) + theme_minimal() + 
  geom_point(color='dodgerblue4', position="jitter") + ggtitle("Monthly Flow Data") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(x = "Monitoring Date", y = "Monthly Average Flow Value (MGD)")

monthly.plot1

#Smooth
daily.plot2 <- ggplot(daily.df, aes(Mon.Date, Daily.Value)) + theme_minimal() + 
  geom_smooth() + ggtitle("Daily Flow Data") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(x = "Monitoring Date", y = "Daily Flow Value (MGD)")

daily.plot2

monthly.plot2 <- ggplot(monthly.df, aes(End.Date, Avg)) + theme_minimal() + 
  geom_smooth()+ ggtitle("Monthly Flow Data") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(x = "Monitoring Date", y = "Monthly Average Flow Value (MGD)")

monthly.plot2

#Line
daily.plot3 <- ggplot(daily.df, aes(Mon.Date, Daily.Value)) + theme_minimal() + 
  geom_line() + ggtitle("Daily Flow Data") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(x = "Monitoring Date", y = "Daily Flow Value (MGD)")

daily.plot3

monthly.plot3 <- ggplot(monthly.df, aes(End.Date, Avg)) + theme_minimal() + 
  geom_line()+ ggtitle("Monthly Flow Data") +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(x = "Monitoring Date", y = "Monthly Average Flow Value (MGD)")

monthly.plot3

#Facet
daily.plot4 <- ggplot(daily.df, aes(Mon.Date, Daily.Value)) + theme_minimal() + 
  geom_point() + facet_wrap( ~ Facility.Type)
