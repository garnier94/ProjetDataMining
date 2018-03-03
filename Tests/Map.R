rm(list= objects())
library(rgdal)
library(raster)
library(tidyverse)

#--------------------
Stations2017 <- read.csv("~/StatML/DataProjet/Divvy_Stations_2017_Q1Q2.csv")
#Stations2017 <- read.csv("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Données/Divvy_Trips_2017_Q1Q2/Divvy_Stations_2017_Q1Q2.csv")
Stations2017.map <- Stations2017
station_map <- readOGR(path.expand("~/StatML/DataProjet/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Stations_2014_Q3Q4"),"Divvy_Stations_2015")
#station_map <- readOGR(path.expand("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Données/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Stations_2014_Q3Q4"),"Divvy_Stations_2015")

ecart = 0.15

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
#illinoisCR <- readOGR(path.expand("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")   
  
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
plot(ChicagoR, axes= F, xlab ="", ylab="")
axis(2, ylim=c(minLat, maxLat))
axis(1, xlim=c(minLon, maxLon))
plot(station_map, col='light blue', add=T)

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


###Génération des cartes de l'énoncé

Stations2014 <- read.csv("~/StatML/DataProjet/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Stations_2014-Q3Q4.csv")
Stations2015 <- read.csv("~/StatML/DataProjet/Divvy_Stations_2015.csv")
Stations2016 <- read.csv("~/StatML/DataProjet/Divvy_Stations_2016_Q4.csv")

plot.new()
plot(ChicagoR, axes= F, xlab ="", ylab="", main = "Extension du réseau de stations entre 2014 et 2017")
axis(2, ylim=c(minLat, maxLat))
axis(1, xlim=c(minLon, maxLon))
colors_t = c('green','yellow', 'orange', 'red')
Stat = list(Stations2017,Stations2016, Stations2015, Stations2014)
Stat[1]
for( i in 1:4 )
{
  Stat.map  <- as.data.frame(Stat[i])
  coordinates(Stat.map) <- ~ longitude + latitude
  proj4string(Stat.map) <- proj4string(ChicagoR)
  plot(Stat.map, col=colors_t[i], add=T, pch = 4  )
}

legend(-87.4, 42, legend=c("2014", "2015", "2016", "2017"),
       col=colors_t,pch = 4, cex=0.8)

#____________________________________________________________________
# Un nouveau plot : fréquentation en 2016

load("~/StatML/Projet/ProjetDataMining/AggratedData2016.RData")

Compil = summarise(group_by(Data, station),nb = sum(nbS))
Frequentation = left_join(Stations2016, Compil, by =  c("id"="station") )

summary(Frequentation$nb)

limite = c(500,2000,5000,20000)
couleur = c('dark blue','light blue', 'yellow','orange', 'red')

assign_col = function(j)
{
  for(i in 1:4)
  {
    if(j < limite[i]){return(couleur[i])}
  }
  return(couleur[i+1])
}

plot.new()
plot(ChicagoR, axes= F, xlab ="", ylab="", main = "Fréquentation des stations en 2016")
axis(2, ylim=c(minLat, maxLat))
axis(1, xlim=c(minLon, maxLon))
Stat.map <- Stations2016
Stat.map  <- as.data.frame(Stat[i])
coordinates(Stat.map) <- ~ longitude + latitude
proj4string(Stat.map) <- proj4string(ChicagoR)
plot(Stat.map, col=sapply(Frequentation$nb,assign_col), add=T, pch = 4  )
legend(-87.5, 42, title ="nombre de vélos empruntés"  ,legend=c("n < 500", " 500 < n < 2000", "2000 < n < 5000", "5000 < n < 20000", "< 20000"),
       col=couleur,pch = 4, cex=0.8)


