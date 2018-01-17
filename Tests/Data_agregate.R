################################################################
##################Traitement des données #######################
################################################################

# But de la routine : 
# Faire une fonction qui prenne en entrée un fichier de données (tranche de 6 mois)
# Fait le traitement 
# Renvoie un fichier . RData des données aggrégées

# Preparation de l'environnement de travail
################################################################

rm(list=objects())
graphics.off()

# Chargement des packages
library("tidyverse")
library("lubridate")
library("timeDate")

data_aggregate <- function(data_trips){
  ######################################################
  # Input : 
  #   - un fichier csv détails des trajets
  # Output :
  #   - un fichier .RData avec les données aggrégées
  ######################################################
  
  # 1 - Conversion des dates en type 'date'  
  
  Date <- as.POSIXct(strptime(data_trips$starttime,"%Y-%m-%d %H:%M"))
  data_trips$starttime <- Date
  rm(Date)
  
  Date <- as.POSIXct(strptime(data_trips$stoptime,"%Y-%m-%d %H:%M"))
  data_trips$stoptime <- Date
  rm(Date)
  
  # 2 - Aggrégation par station et par heure
  
  Aggrega <- mutate(data_trips, Hour = hour(data_trips$starttime), Day = format(starttime, "%Y-%m-%d" ))
  Aggrega$Day <- as.POSIXct(strptime(Aggrega$Day, "%Y-%m-%d"))
  Aggrega$Day <- as.Date(Aggrega$Day)
  
  TrajetStationEntrant <- summarise(group_by(Aggrega, Day, Hour, station = to_station_id), nbE=n())
  TrajetStationSortant <- summarise(group_by(Aggrega, Day, Hour, station = from_station_id), nbS=n())
  
  DataperHour <- full_join(TrajetStationEntrant, TrajetStationSortant, by = c("Day", "station", "Hour" ))
  DataperHour[is.na(DataperHour)] <- 0
  
  return(DataperHour)
}  
