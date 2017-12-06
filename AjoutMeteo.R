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

minDate <- min(Data2013perHour$Day)
maxDate <- max(Data2013perHour$Day)

riem_networks()
Meteo <- riem_measures(station = "MDW", date_start = as.character(minDate), date_end = as.character(maxDate))
Day <- date(Meteo$valid)
Meteo$Day <- Day

HeureNorm <- sapply(Meteo$valid,ConvertHour)
Meteo$Hour <- as.array(HeureNorm)

sumMeteo <- summarise(group_by(Meteo, Day, Hour),temp = mean(tmpf), pluvio = mean(p01i))
sumMeteo$temp <- (sumMeteo$temp -32)/1.8  ## Conversion Celsius
Data2013withMeteo <- left_join(Data2013perHour,sumMeteo, by= c("Hour" ="Hour", "Day" ="Day"))

### éliminer les températures 

Data2013withMeteo$pluvio[is.na(Data2013withMeteo$pluvio)] <-0
