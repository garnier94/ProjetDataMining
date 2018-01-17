################################################################
#########################  DATA MINING  ########################
################################################################

pdf("~/Documents/Orsay/M2/Data Mining/Divvy_Stations_Trips_2013/projet.pdf") 

# Lien vers les donnees : https://www.divvybikes.com/system-data

# Donnees sur les utilisations des velos en libre service dans la ville de Chicago
# But : predire le nombre de velos dans chaque station par demi-heure ?

# Ajouter les donnees meteo, ajouter le type de jours

# Etapes : 
#     1) Se concentrer sur les donnees des trajets de 2013
#        - rajouter la variable a predire
#        - graphiques
#        - analyses
#     2) Rajoute une information avec la localisation
#     3) Regarder les autres annees


# I- Preparation de l'environnement de travail
################################################################

rm(list=objects())
graphics.off()

# Set Working Directory
#setwd("~/Documents/Orsay/M2/Data Mining/Divvy_Stations_Trips_2013")
setwd("~/statML/Projet/ProjetDataMining")

# Chargement des packages
library("tidyverse")
library("magrittr")
library("xts")

# II- Importation des donnees
################################################################

# Donnees Stations
 stations <- read.csv("data/Divvy_Stations_Trips_2013/Divvy_Stations_2013.csv", header=T)
# n_stat = length(stations[,1])
# Pour le moment je me limite aux donnees sur les trajets

# Creation du data file pour 2013 :

# Donnees Trips
trips <- read.csv("data/Divvy_Stations_Trips_2013/Divvy_Trips_2013.csv", header=T)

names(trips)
head(trips)
glimpse(trips) # dates dans le mauvais format
summary(trips)

n_trip <- length(trips[,1])

# III- Conversion en type "Date" de starttime et de stoptime
################################################################

Date <- as.POSIXct(strptime(trips$starttime,"%Y-%m-%d %H:%M"))
trips$starttime <- Date
rm(Date)

Date <- as.POSIXct(strptime(trips$stoptime,"%Y-%m-%d %H:%M"))
trips$stoptime <- Date
rm(Date)

#glimpse(trips)
summary(trips)

# Pour predire le nombre de velos par station par heures, je dois : 
#         1) faire une agregation par station et une agregation par heure
#         2) faire la somme des lignes et rajouter cette info dans une colonne des donnees
#         3) diviser mon echantillon en deux parties pour commencer le travail de prediction

# IV- Agregation par heure
################################################################

#On le fait d'abord pour le mois de Juillet 2013 :

Data0 <- mutate(trips, DayHour = format(starttime, "%Y-%m-%d %H"))
DateHeure <- as.POSIXct(strptime(Data0$DayHour,"%Y-%m-%d %H"))
Data0$DayHour <- DateHeure
rm(DateHeure)

by_hour <-group_by(Data0, DayHour)
TrajetPerHour <- summarise(by_hour,nb= n())

IndexJuillet <- which(TrajetPerHour$DayHour>="2013-07-01 00:00:00" & TrajetPerHour$DayHour <="2013-07-30 00:00:00" )
TrajetJuillet <- TrajetPerHour[IndexJuillet,]

plot(TrajetJuillet$DayHour, TrajetJuillet$nb, xlab = "Jour", ylab = "Nombre de trajets par heure",type = "l" )

#Essayons maintenant pour la demi heure:

Data1 <- mutate(trips, DayHour = format(starttime, "%Y-%m-%d %H"))

