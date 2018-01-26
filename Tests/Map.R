rm(list= objects())
library(rgdal)
library(raster)
library(tidyverse)

#--------------------
#Stations2017 <- read.csv("~/StatML/DataProjet/Divvy_Stations_2017_Q1Q2.csv")
Stations2017 <- read.csv("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Données/Divvy_Trips_2017_Q1Q2/Divvy_Stations_2017_Q1Q2.csv")
Stations2017.map <- Stations2017
#station_map <- readOGR(path.expand("~/StatML/DataProjet/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Stations_2014_Q3Q4"),"Divvy_Stations_2015")
station_map <- readOGR(path.expand("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Données/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Stations_2014_Q3Q4"),"Divvy_Stations_2015")

ecart = 0.09

minLat = min(Stations2017$latitude)-ecart-0.25
maxLat = max(Stations2017$latitude)+ecart
minLon = min(Stations2017$longitude)-ecart
maxLon = max(Stations2017$longitude )+ecart

is_in_Border= function(polygone)
{
  classe = extent(polygone)
  return((classe@xmin > minLon ) && (classe@xmax < maxLon) && (classe@ymin > minLat ) && (classe@ymax < maxLat))
}


#illinoisCR <- readOGR(path.expand("~/StatML/Projet/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")
illinoisCR <- readOGR(path.expand("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")   
  
#plot(illinoisCR)
#illinoisCR@data
lon = length(illinoisCR)
liste = rep.int(FALSE,lon)
for(i in 1:lon)
{
  liste[i] <- is_in_Border(illinoisCR[i,])  
}
ChicagoR <- illinoisCR[liste,]

plot.new()
plot(ChicagoR, axes= F, xlab ="", ylab="")
axis(2, ylim=c(minLat, maxLat))
axis(1, xlim=c(minLon, maxLon))
plot(station_map, col='red', add=T)

### Stations :
coordinates(Stations2017.map) <- ~ longitude + latitude
proj4string(Stations2017.map) <- proj4string(ChicagoR)
Stations2017$district <-over(Stations2017.map, ChicagoR)$SLDLST

District13 <- Stations2017[which(is.na(Stations2017$district)),]

coordinates(District13)<- ~ longitude + latitude


plot(ChicagoR, axes= F, xlab ="", ylab="")
axis(2, ylim=c(minLat, maxLat))
axis(1, xlim=c(minLon, maxLon))
plot(Stations2017.map, col='red', add= T)
plot(District13, add= T, col ="blue")

