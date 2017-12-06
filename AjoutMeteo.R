load("~/statML/Projet/ProjetDataMining/AggratedData2013.RData")

## Une fonction de normalisation de l'heure

ConvertHour <- function(time_stamp)
{
  if( minute(time_stamp) >30)
  {
    return( hour(time_stamp) + 1)
  }
  return( hour(time_stamp))
}

## Ajout de la météo 

library(riem)
library(lubridate)
library(tidyverse)
library(weathermetrics)

minDate <- min(Data2013perHour$Day)
maxDate <- max(Data2013perHour$Day)

Meteo <- riem_measures(station = "MDW", date_start = as.character(minDate), date_end = as.character(maxDate))
Meteo$tmpf <- fahrenheit.to.celsius(Meteo$tmpf)
Meteo$Day <- date(Meteo$valid)
Meteo$Month <- month(Meteo$valid)

HeureNorm <- sapply(Meteo$valid,ConvertHour)
Meteo$Hour <- as.array(HeureNorm)

sumMeteo <- summarise(group_by(Meteo, Day, Hour),temp = mean(tmpf), pluvio = mean(p01i))
DatawithMeteo <- left_join(Data2013perHour,sumMeteo, by= c("Hour" ="Hour", "Day" ="Day"))


### éliminer les NA

DatawithMeteo$pluvio[is.na(DatawithMeteo$pluvio)] <-0 # On suppose qu'il n'y a pas de pluie

MeanMonth <- summarise(group_by(Meteo,Month,Hour),temp = mean(tmpf))
save(DatawithMeteo,file = "AggratedData2013.RData")


