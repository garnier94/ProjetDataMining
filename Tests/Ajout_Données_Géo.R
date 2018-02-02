rm(list= objects())
library(rgdal)
library(raster)
library(tidyverse)
library(DivvyBikeProject)
library(riem)
library(weathermetrics)
library(lubridate)

#Importation des données géographiques 
illinoisCR <- readOGR(path.expand("~/StatML/Projet/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")
#illinoisCR <- readOGR(path.expand("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")  

# Importation des stations
Stations <-  read_csv("~/StatML/DataProjet/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Stations_2014-Q3Q4.csv")
Stations.map <- Stations

#Trips2014
load("~/StatML/Projet/ProjetDataMining/Package/AggratedData2014.RData")

year = 2014

Joined <- groupByGeospatialData(Data, illinoisCR, Stations.map)
Meteo <- getMeteo(year, save_data = FALSE)
ListDays <- getSpecialDays(year, save_data = FALSE)

dis <- summarise(group_by(Joined, district))
minDate <- as.POSIXct(strptime( paste(year, "01-01 00:00", sep = "-"), "%Y-%m-%d %H:%M"  ) )
maxDate <- as.POSIXct(strptime( paste(year+1, "01-01 01:00", sep = "-"), "%Y-%m-%d %H:%M"  ) )
listDate <- seq(minDate, maxDate, by = "hour")

Join <- Joined[,c(1,4)]

expansion <- expand(Join, Time = listDate, district )
Data <- left_join(expansion, Joined , by = c("district","Time"))

Data$Day <-  format(Data$Time, "%Y-%m-%d" )
Data$Hour <- hour(Data$Time)
Data$nbE[which(is.na(Data$nbE))] <- 0
Data$nbS[which(is.na(Data$nbS))] <- 0
Data$diff <- Data$nbE -Data$nbS
ListDays$Day <- format( ListDays$Day, "%Y-%m-%d")
Data <- left_join(Data, ListDays, by = "Day")


DataFinal <- left_join(Data, Meteo, by = "Time")

