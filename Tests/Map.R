library(rgdal)
library(raster)
library(tidyverse)

#Chargement de la carte:
illinois <- readOGR(path.expand("~/statML/Projet/ProjetDataMining/Map"),"tl_2016_17_tract")
plot(illinois)
class(illinois)


#View attributes:
illinois@data
CookCounty <- illinois[which(illinois$COUNTYFP=="031"),]


#-----------------------------------------------------------------------------------------------

stations <- readOGR(path.expand("~/statML/Projet/ProjetDataMining/data/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Stations_2014_Q3Q4"), "Divvy_Stations_2015")


plot(CookCounty)
plot(stations, add=TRUE,col ='red')

#--------------------
Stations2017 <- read.csv("~/statML/Projet/ProjetDataMining/data/Divvy_Stations_2015.csv")

minLat = min(Stations2017$latitude)-0.1
maxLat = max(Stations2017$latitude)+0.1
minLon = min(Stations2017$longitude)-0.1
maxLon = max(Stations2017$longitude )+0.1

is_in_Border= function( census_tract)
{
  classe = extent(census_tract)
  return((classe@xmin > minLon ) && (classe@xmax < maxLon) && (classe@ymin > minLat ) && (classe@ymax < maxLat))
}

lon = length(CookCounty)
liste = rep.int(FALSE,lon)
for(i in 1:lon)
{
  liste[i] <- is_in_Border(CookCounty[i,])  
}

Chicago <- CookCounty[liste,]

plot(Chicago)
plot(stations, add=TRUE,col ='red')

### On n'observe pas la même densité
crs(Chicago)
# +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 
crs(stations)
#+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0

#On doit reprojeter 
stationsRepro <- spTransform(stations,crs(Chicago))
crs(stationsRepro)
crs(Chicago)

plot(Chicago)
plot(stationsRepro, add = T, col = 'red')

##Testons autre chose :
illinoisCR <- readOGR(path.expand("~/statML/Projet/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")
plot(illinoisCR)
illinoisCR@data
lon = length(illinoisCR)
liste = rep.int(FALSE,lon)
for(i in 1:lon)
{
  liste[i] <- is_in_Border(illinoisCR[i,])  
}
ChicagoR <- illinoisCR[liste,]
plot(ChicagoR)