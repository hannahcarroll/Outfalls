#Assignment: ENSCI 490 week 6

library(readxl)
library(dplyr)
library(plyr)
library(magrittr)
library(ggplot2)
options(max.print=1000000)

#Combine data sets
file.list <- list.files(pattern = '*.xlsx')
df.list <- lapply(file.list, read_excel)

dnr.df <- ldply(df.list, data.frame)

#Create multiple data sets for each facility type (ACTIVATED SLUDGE, 
#AERATED LAGOON, TRICKLING FILTER, ROTATING BIOLOGICAL CONTACTOR)

Typ.AS <- subset(dnr.df, Facility.Type %in% "ACTIVATED SLUDGE")

Typ.AL <- subset(dnr.df, Facility.Type %in% "AERATED LAGOON")

Typ.TF <- subset(dnr.df, Facility.Type %in% "TRICKLING FILTER")

Typ.RBC <- subset(dnr.df, Facility.Type %in% "ROTATING BIOLOGICAL CONTACTOR")

#Create multple data sets for each monitoring location (RAW WASTE OR FINAL EFFLUENT(FLOW), 
#RAW WASTE, TOTAL RAW WASTE, FINAL EFFLUENT, COMBINED EFFLUENT, EFFLUENT AFTER DISINFECTION, 
#MECHANICAL PLANT INFLUENT)
Loc.RWFE <- subset(dnr.df, Mon.Locat %in% "RAW WASTE OR FINAL EFFLUENT(FLOW)")

Loc.RW <- subset(dnr.df, Mon.Locat %in% "RAW WASTE")

Loc.TRW <- subset(dnr.df, Mon.Locat %in% "TOTAL RAW WASTE")

Loc.FE <- subset(dnr.df, Mon.Locat %in% "FINAL EFFLUENT")

Loc.CE <- subset(dnr.df, Mon.Locat %in% "COMBINED EFFLUENT")

Loc.EAD <- subset(dnr.df, Mon.Locat %in% "EFFLUENT AFTER DISINFECTION")

Loc.MPI <- subset(dnr.df, Mon.Locat %in% "MECHANICAL PLANT INFLUENT")

#Create data set with just Cedar Rapids data
CedRap.df <- subset(dnr.df, Facility.Name %in% "CEDAR RAPIDS CITY OF STP")

#Plot Cedar Rapids data, choose theme and apply color schemes
CedRap.plot <- ggplot(CedRap.df, aes(Mon.Date, Daily.Value)) + theme_minimal() + 
  geom_point(color='darkslategray') +
  ggtitle("Cedar Rapids") + 
  theme(axis.title.x = element_text(color = "darkslategray"),
        axis.title.y = element_text(color = "darkslategray")) +  
  theme(plot.title = element_text(color = "darkslategray", hjust = 0.5, face = "bold")) +
  labs(x = "Monitoring Date", y = "Daily Flow Value (MGD)")
  
CedRap.plot
        