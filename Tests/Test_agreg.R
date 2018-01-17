################################################################
############ Appel de la fonction d'aggrégation ################
################################################################

source("~/Documents/Orsay/M2/Data Mining/ProjetDataMining/Data_agregate.R")

setwd("~/Documents/Orsay/M2/Data Mining/ProjetDataMining/Données/Divvy_Stations_Trips_2014_Q1Q2")

# Données Trips
trips_2014 <- read.csv("Divvy_Trips_2014_Q1Q2.csv", header=T)


# Aggrégation : 

test_2014 <- data_aggregate(trips_2014)
