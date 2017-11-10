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

# Chargement des packages
library("tidyverse")
library("magrittr")
library("lubridate")
library("graphics")

# II- Importation des donnees
################################################################


config = read.csv("config.csv")

setwd(toString(  config$loc_dir ))
# Donnees Stations
stations <- read.csv(toString(config$file_stations[1] ), header=T)
# Creation du data file pour 2013 :
trips <- read.csv(toString(config$file_trips[1]), header=T)

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

# IV- Agregation par heure : Quelques Tests
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

step = 15 
Data1 <- mutate(trips , TimePeriod = paste(format(starttime, "%Y-%m-%d %H"), step*minute(starttime)%/%step))
Data1$TimePeriod <- as.POSIXct(strptime(Data1$TimePeriod,"%Y-%m-%d %H %M"))

by_step <-group_by(Data1, TimePeriod)
TrajetPerStep <- summarise(by_step,nb= n())

Index <- which(TrajetPerStep$TimePeriod >="2013-09-01 00:00:00" & TrajetPerStep$TimePeriod <="2013-09-08 00:00:00" )
Trajet <- TrajetPerStep[Index,]
rm(Index)

plot(Trajet$TimePeriod,Trajet$nb, xlab = "Jour", ylab = "Nombre de trajets par 1/4 heure",type = "l" )
# On observe que le dimanche et le lundi semble se comporter de la même façon . Or le 2 septembre 2013 est un jour férie aux USA!  

rm(Data1)

# V- Agregation par station : Quelques Tests
############################################

Data2 <- mutate(trips, Hour = hour(trips$starttime), Day= format(starttime, "%Y-%m-%d" ))
Data2$Day <- as.POSIXct(strptime(Data2$Day,"%Y-%m-%d"))

TrajetStationSortant <- summarise(group_by(Data2, Day, from_station_id),nb=n())
TrajetStationEntrant <- summarise(group_by(Data2, Day, to_station_id),nb=n())

#On s'intéresse à la station Wood St And Milwaukee Ave:

station = 97

Index = which(TrajetStationEntrant$to_station_id == station)
InWoodStAndMilwaukeeAve = TrajetStationEntrant[Index,]
rm(Index)
Index = which(TrajetStationSortant$from_station_id == station)
OutWoodStAndMilwaukeeAve= TrajetStationSortant[Index,]

plot( InWoodStAndMilwaukeeAve$Day,InWoodStAndMilwaukeeAve$nb, col = "blue", xlab = "Date" , ylim = c(0,200), type ="l")
par(new = TRUE)
plot(OutWoodStAndMilwaukeeAve$Day, OutWoodStAndMilwaukeeAve$nb, col ="red", xlab = "Date",ylim = c(0,200), type = "l")
