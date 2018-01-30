#Ce script sert à concatener les différentes différents plages de données pour les différentes années. 
#Il n'y a pas de découpage standard pour toute les années. En particulier pour les dates

# I- Preparation de l'environnement de travail
################################################################

rm(list=objects())
graphics.off()
setwd("~/StatML/DataProjet/AggregatedData")
# Chargement des packages
library("tidyverse")
library("magrittr")
library("lubridate")
library("graphics")
library("timeDate")

# II- Importation des donnees
################################################################

#2014:

Trips2014_1 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2014_Q1Q2.csv")
Trips2014_2 <- read.csv("~/StatML/DataProjet/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-07.csv")
Trips2014_3 <- read.csv("~/StatML/DataProjet/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-0809.csv")
Trips2014_4 <- read.csv("~/StatML/DataProjet/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q4.csv")

Trips2014 <- rbind(Trips2014_1,Trips2014_2,Trips2014_3,Trips2014_4)
rm(Trips2014_1,Trips2014_2,Trips2014_3,Trips2014_4)
#2015:

Trips2015_1 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015-Q1.csv")
Trips2015_2 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015-Q2.csv")
Trips2015_3 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015_07.csv")
Trips2015_4 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015_08.csv")
Trips2015_5 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015_09.csv")
Trips2015_6 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2015_Q4.csv")

Trips2015 <- rbind( Trips2015_1, Trips2015_2, Trips2015_3, Trips2015_4, Trips2015_5, Trips2015_6  )
rm( Trips2015_1, Trips2015_2, Trips2015_3, Trips2015_4, Trips2015_5, Trips2015_6  )

#2016:
Trips2016_1 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_Q1.csv")
Trips2016_2 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_04.csv")
Trips2016_3 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_05.csv")
Trips2016_4 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_06.csv")
Trips2016_5 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q3.csv")
Trips2016_6 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2016_Q4.csv")

Trips2016 <- rbind( Trips2016_1, Trips2016_2, Trips2016_3, Trips2016_4, Trips2016_5, Trips2016_6  )
rm( Trips2016_1, Trips2016_2, Trips2016_3, Trips2016_4, Trips2016_5, Trips2016_6  )

#2017
Trips2017_1 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2017_Q1.csv")
Trips2017_2 <- read.csv("~/StatML/DataProjet/Divvy_Trips_2017_Q2.csv")
Trips2017<- rbind(Trips2017_1,Trips2017_2)
rm(Trips2017_1,Trips2017_2)

#Ici on choisit l'année qu'on va transformer
trips <- Trips2014

# III- Conversion en type "Date" de starttime et de stoptime
################################################################

Date <- as.POSIXct(strptime(trips$starttime,"%m/%d/%Y %H:%M"))
# Change the prevbious line by this if year = 2017
#Date <- as.POSIXct(strptime(trips$start_time,"%m/%d/%Y %H:%M"))
trips$starttime <- Date
rm(Date)

Date <- as.POSIXct(strptime(trips$stoptime,"%m/%d/%Y %H:%M"))
# Change the prevbious line by this if year = 2017
#Date <- as.POSIXct(strptime(trips$end_time,"%m/%d/%Y %H:%M"))
trips$stoptime <- Date
rm(Date)

# VI Aggregation
#==============================

minDate = floor_date(min(trips$starttime), unit= "hour")
year = year(minDate)

# Trouvez une autre idée pour 2017
maxDate = as.POSIXct(strptime( paste(year, "12-31 23:00", sep = "-"), "%Y-%m-%d %H:%M"  ) )
listDate = seq(minDate, maxDate, by = "hour")

Data <- mutate(trips, Hour = hour(trips$starttime), Time = floor_date(trips$starttime, unit = "hour"))
Data2 <- mutate(trips, Hour = hour(trips$stoptime), Time = floor_date(trips$stoptime, unit = "hour"))
Data2 <- Data2[which(year(Data2$Time)== year),] # On élimine les données supérieures à l'année considérée

TrajetStationEntrant <- summarise(group_by(Data2, Time, Hour,station = to_station_id),nbE=n())
TrajetStationSortant <- summarise(group_by(Data, Time, Hour,station = from_station_id),nbS=n())

Data <- full_join(TrajetStationEntrant, TrajetStationSortant, by = c("Time","station","Hour" ))
Data <- mutate(Data, Day = format(Time, "%Y-%m-%d" ))

Data[is.na(Data)] <- 0
Data$nbE[which(is.na(Data$nbE))] <- 0
Data$nbS[which(is.na(Data$nbS))] <- 0

# V Controle des valeurs manquantes: 
#===================================

dftime = data.frame(Time = listDate)
TimeValues = summarise(group_by(Data,Time), ind = 1)
difftable =  left_join(dftime, TimeValues, by = "Time")
MissingValues <- data.frame( Time = difftable$Time[which(is.na(difftable$ind))])
MissingValues <- mutate(MissingValues, Day = format(Time, "%Y-%m-%d" ), Hour = hour(Time) )

save(Data, MissingValues, file=paste(  "AggratedData", year,".RData", sep = ""))
