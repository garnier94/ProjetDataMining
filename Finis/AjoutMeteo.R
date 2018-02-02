rm(list=objects())
graphics.off()

## Ce fichier sert à créer une data.frame contenant les données météo d'une année à Chicago

## Ajout de la météo 

library(riem)
library(lubridate)
library(tidyverse)
library(weathermetrics)

## Gestion des dates

year = 2014 #L'année ou l'on veut prendre la météo
minDate = as.POSIXct(strptime( paste(year, "01-01 00:00", sep = "-"), "%Y-%m-%d %H:%M"  ) )
maxDate = as.POSIXct(strptime( paste(year+1, "01-01 01:00", sep = "-"), "%Y-%m-%d %H:%M"  ) )
listDate = seq(minDate, maxDate, by = "hour")
dftime = data.frame(Time = listDate)

#Importation et selection des paramètre utiles pour la météo

Meteo <- riem_measures(station = "MDW", date_start = minDate, date_end = ceiling_date(maxDate , unit='day'))
Meteo$Time <- round_date( Meteo$valid ,unit ='hour')
Meteo$tmpf <- fahrenheit.to.celsius(Meteo$tmpf) # A enlever pour les américains
MeteoData <- summarise(group_by(Meteo, Time),temp = mean(tmpf, na.rm = TRUE), pluvio = mean(p01i, na.rm = TRUE))

#Jointure et traitement des données manquantes
MeteoData <- left_join(dftime, MeteoData, by = ("Time"="Time"))

MeteoData$pluvio[is.na(MeteoData$pluvio)] <-0 # Pas d'info = pas de pluie

## Pour la température, on remplace les NA par la température moyenne sur le mois et l'heure considér
MeteoData$Month <- month(MeteoData$Time)
MeteoData$Hour <- hour(MeteoData$Time)
MeanMonth <- summarise(group_by(MeteoData, Month, Hour), tempmean = mean(temp,na.rm = TRUE ) )

MeteoData <- left_join(MeteoData,MeanMonth, by= c( "Hour" ="Hour", "Month" ="Month"))
MeteoData$temp[is.na(MeteoData$temp)] <- MeteoData$tempmean[is.na(MeteoData$temp)]


MeteoData <- MeteoData[,- (4:6)]
#TODO Sauvegarde MeteoDato


