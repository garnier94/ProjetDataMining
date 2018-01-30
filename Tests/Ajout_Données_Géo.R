rm(list= objects())
library(rgdal)
library(raster)
library(tidyverse)



#Importation des données géographiques 

illinoisCR <- readOGR(path.expand("~/StatML/Projet/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")
#illinoisCR <- readOGR(path.expand("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")  

# Importation des stations
Stations2017 <- read.csv("~/StatML/DataProjet/Divvy_Stations_2017_Q1Q2.csv")
Stations.map <- Stations2017

#Trips2017:
load("~/StatML/DataProjet/AggregatedData/Full_data2017.RData")

# Gestion des cartes
coordinates(Stations.map) <- ~ longitude + latitude
proj4string(Stations.map) <- proj4string(illinoisCR)
Stations2017$district <-as.integer( over(Stations.map, illinoisCR)$SLDLST)

#Jointure Table
Datat <- left_join(Data2017,Stations2017, by=c("station"="id"))
Group  <- summarise(group_by(Datat, Day,  dow, weekday, Hour, district, temp, pluvio ), nbE = sum(nbE), nbS = sum(nbS)  ) 
Group$diff  <- Group$nbE - Group$nbS






Group$timestamp <-  as.POSIXct(strptime( paste(Group$Day, Group$Hour, sep = " "),"%Y-%m-%d %H")) 




#
Regio = Group[which(Group$district==21 & Group$Day <= "2017-05-10" & Group$Day >= "2017-05-01"  ),]
plot(Regio$timestamp, Regio$diff, type ='l' )
