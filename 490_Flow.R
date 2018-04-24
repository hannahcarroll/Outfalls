library(readxl)
library(dplyr)
library(ggplot2)

file.list <- list.files(pattern = '*.xlsx' )
df.list <- lapply(file.list, read_excel)

flow.df <- ldply(df.list, data.frame)

librsummary(flow.df$Daily.value)