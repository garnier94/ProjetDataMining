rm(list= objects())
library(rgdal)
library(raster)
library(tidyverse)

#--------------------
Stations2017 <- read.csv("~/StatML/DataProjet/Divvy_Stations_2015.csv")
station_map <- readOGR(path.expand("~/StatML/DataProjet/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Stations_2014_Q3Q4"),"Divvy_Stations_2015")


ecart = 0.09

minLat = min(Stations2017$latitude)-ecart
maxLat = max(Stations2017$latitude)+ecart
minLon = min(Stations2017$longitude)-ecart
maxLon = max(Stations2017$longitude )+ecart

is_in_Border= function(polygone)
{
  classe = extent(polygone)
  return((classe@xmin > minLon ) && (classe@xmax < maxLon) && (classe@ymin > minLat ) && (classe@ymax < maxLat))
}


illinoisCR <- readOGR(path.expand("~/StatML/Projet/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")
  
  
plot(illinoisCR)
illinoisCR@data
lon = length(illinoisCR)
liste = rep.int(FALSE,lon)
for(i in 1:lon)
{
  liste[i] <- is_in_Border(illinoisCR[i,])  
}
ChicagoR <- illinoisCR[liste,]

plot.new()
plot(ChicagoR, axes= F, xlab ="", ylab="", add= T)
axis(2, ylim=c(minLat, maxLat))
axis(1, xlim=c(minLon, maxLon))
plot(station_map, col='red')

