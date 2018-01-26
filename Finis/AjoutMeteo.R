rm(list=objects())
graphics.off()


setwd("~/StatML/DataProjet/AggregatedData")
load("AggratedData2016.RData")


Data <- Data2016

## Une fonction de normalisation de l'heure

ConvertHour <- function(time_stamp)
{
  if( minute(time_stamp) >30)
  {
    return(( hour(time_stamp) + 1) %% 24)
  }
  return( hour(time_stamp))
}

## Ajout de la météo 

library(riem)
library(lubridate)
library(tidyverse)
library(weathermetrics)

minDate <- min(Data$Day)-2
maxDate <- max(Data$Day)+2

Meteo <- riem_measures(station = "MDW", date_start = as.character(minDate), date_end = as.character(maxDate))
Meteo$tmpf <- fahrenheit.to.celsius(Meteo$tmpf)
Meteo$Day <- date(Meteo$valid)
Meteo$Month <- month(Meteo$valid)
Meteo$Year <- year(Meteo$valid)

HeureNorm <- sapply(Meteo$valid,ConvertHour)
Meteo$Hour <- as.array(HeureNorm)

sumMeteo <- summarise(group_by(Meteo, Day, Hour),temp = mean(tmpf, na.rm = TRUE), pluvio = mean(p01i, na.rm = TRUE))
DatawithMeteo <- left_join(Data,sumMeteo, by= c("Hour" ="Hour", "Day" ="Day"))


### éliminer les NA
DatawithMeteo$pluvio[is.na(DatawithMeteo$pluvio)] <-0 # On suppose qu'il n'y a pas de pluie

MeanMonth <- summarise(group_by(Meteo,Year, Month,Hour),mean_temp = mean(tmpf, na.rm = TRUE))
DatawithMeteo$Month <- month(DatawithMeteo$Day)
DatawithMeteo$Year <- month(DatawithMeteo$Day)
DatawithMeteo <- left_join(DatawithMeteo,MeanMonth, by= c("Year"="Year","Hour" ="Hour", "Month" ="Month"))
DatawithMeteo$temp[is.na(DatawithMeteo$temp)] <- DatawithMeteo$mean_temp[is.na(DatawithMeteo$temp)]


Data2016 <- DatawithMeteo[,- (8:10)]

save(Data2016,file = "AggratedData2016_WithM.RData")


