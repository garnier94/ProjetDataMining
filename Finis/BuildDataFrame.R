rm(list= objects())
library(rgdal)
library(raster)
library(tidyverse)
library(DivvyBikeProject)
library(riem)
library(weathermetrics)
library(lubridate)

setwd("~/StatML/Projet/ProjetDataMining")



#Importation des données géographiques 
illinoisCR <- readOGR(path.expand("~/StatML/Projet/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")
#illinoisCR <- readOGR(path.expand("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Map/Congres"),"tl_2016_17_sldl")  

# Importation des stations
Stations <-  read_csv("~/StatML/DataProjet/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Stations_2014-Q3Q4.csv")

#2014
load("~/StatML/Projet/ProjetDataMining/Package/AggratedData2014.RData")

year = 2014

dat <- BuildDataSet(year, Data, illinoisCR, Stations, save_data = TRUE)

#2015
Stations <-  read_csv("~/StatML/DataProjet/Divvy_Stations_2015.csv")
Trips2015_1 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015-Q1.csv")
Trips2015_2 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015-Q2.csv")
Trips2015_3 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015_07.csv")
Trips2015_4 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015_08.csv")
Trips2015_5 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015_09.csv")
Trips2015_6 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015_Q4.csv")

Trips2015 <- rbind( Trips2015_1, Trips2015_2, Trips2015_3, Trips2015_4, Trips2015_5, Trips2015_6  )
rm( Trips2015_1, Trips2015_2, Trips2015_3, Trips2015_4, Trips2015_5, Trips2015_6  )
Data <- aggregateData(Trips2015,2015)
BuildDataSet(2015, Data, illinoisCR, Stations, save_data = TRUE)

#2016
Stations <-  read_csv("~/StatML/DataProjet/Divvy_Stations_2016_Q4.csv")
Trips2016_1 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_Q1.csv")
Trips2016_2 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_04.csv")
Trips2016_3 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_05.csv")
Trips2016_4 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_06.csv")
Trips2016_5 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q3.csv")
Trips2016_6 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q4.csv")

Trips2016 <- rbind( Trips2016_1, Trips2016_2, Trips2016_3, Trips2016_4, Trips2016_5, Trips2016_6  )
rm( Trips2016_1, Trips2016_2, Trips2016_3, Trips2016_4, Trips2016_5, Trips2016_6  )
Data <- aggregateData(Trips2016,2016)
BuildDataSet(2016, Data, illinoisCR, Stations, save_data = TRUE)

#2017
