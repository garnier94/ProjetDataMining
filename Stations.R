library(tidyverse)

Trips_2017_Q1 <- read_csv("~/statML/Projet/ProjetDataMining/data/Divvy_Trips_2017_Q1.csv")
Trips_2013 <- read_csv("~/statML/Projet/ProjetDataMining/data/Divvy_Stations_Trips_2013/Divvy_Trips_2013.csv")
Trips_2015_Q2 <- read_csv("~/statML/Projet/ProjetDataMining/data/Divvy_Trips_2015-Q2.csv")

Stat2015 <- summarise(group_by(Trips_2015_Q2,from_station_id,from_station_name), nb2015= n())
Stat2013 <- summarise(group_by(Trips_2013,from_station_id,from_station_name), nb2013= n())
Stat2017 <- summarise(group_by(Trips_2017_Q1,from_station_id,from_station_name), nb2017= n())
A <-full_join(Stat2015,Stat2013, by=c("from_station_id" = "from_station_id"))
A <-full_join(A,Stat2017, by = c("from_station_id"= "from_station_id"))


which(A$from_station_name.x != A$from_station_name.y)



Stations2015 <- read_csv("~/statML/Projet/ProjetDataMining/data/Divvy_Stations_2015.csv")
Stations2013 <- read_csv("~/statML/Projet/ProjetDataMining/data/Divvy_Stations_Trips_2013/Divvy_Stations_2013.csv")
Stations2017 <- read_csv("~/statML/Projet/ProjetDataMining/data/Divvy_Stations_2017_Q1Q2.csv")


Station20135 <- full_join(Stations2013,Stations2015,by=("id" ="id"))
Station201357 <- full_join(Station20135,Stations2017,by = ("id"="id") )

#Les stations qui ont changé de nom:
Anomalies <- Station201357[which(Station201357$name.x != Station201357$name.y  | Station201357$name.y != Station201357$name),]

Diff <-  which(Anomalies$latitude.x != Anomalies$latitude.y | Anomalies$latitude.y != Anomalies$latitude)


Difflat <- abs(Anomalies$latitude.x-Anomalies$latitude)
Difflat[is.na(Difflat)] <-0
Difflong <- abs(Anomalies$longitude.x-Anomalies$longitude)
Difflong[is.na(Difflong)] <- 0
Aberation <- Anomalies[which(Difflong+Difflat > 0.003),]

