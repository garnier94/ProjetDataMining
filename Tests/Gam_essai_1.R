library(mgcv)
library(lubridate)
library(DivvyBikeProject)

# Add district :
AddPos <- function(cur_data, list_dis )
{
  len = length(cur_data$district)
  a = vector(length = len)
  for(i in 1:len)
  {
      a[i]<-  which(list_dis == cur_data$district[i])
  }
  return(a)
}

plotperiod <- function( data_in, data_used , min_date, max_date , title = "Nb of rented bikes per day and per station", couleur = 'blue', stat = 12)
{
  period = which(data_in$Time < max_date & data_in$Time > min_date & data_in$district ==stat)
  plot(data_in$Time[period], data_in$nbEstat[period], main  = paste("District ", as.character(stat)) ,xlab = "Time",  ylab = title, type = 'l', col =couleur)
  lines(data_in$Time[period], data_used[period])
}



# Chargement des donnés

load("~/StatML/Projet/ProjetDataMining/FullData2014.RData")
data_2014 <- Data
load("~/StatML/Projet/ProjetDataMining/FullData2015.RData")
data_2015 <- Data
load("~/StatML/Projet/ProjetDataMining/FullData2016.RData")
data_2016 <- Data
rm(Data)


datatrain  <- rbind(data_2014,data_2015)
datatest <-  data_2016

datatrain$nbEstat <- datatrain$nbE/datatrain$nb_stations
datatrain$Week <- week(as.POSIXct(strptime(datatrain$Day, "%Y-%m-%d")))
datatest$nbEstat <- datatest$nbE/datatest$nb_stations
datatest$Week <- week(as.POSIXct(strptime(datatest$Day,"%Y-%m-%d")))


listdistrict = unique(datatrain$district)
# Pour l'instant , on ne s'interesse qu'aux districts  pour lesquels on a des données au moins pour 2015.
datatest <- datatest[which(is.element(datatest$district, listdistrict)),]

datatrain$districtpos <- AddPos(datatrain, listdistrict)
datatest$districtpos <- AddPos(datatest, listdistrict)


eq <- nbEstat ~ s(Hour)+s(districtpos) + s(dow, k= 8)+s(pluvio, k= 3) + s(temp) +s(Week)

g0 <- gam(eq, data = datatrain)
forecast<-predict(g0, newdata=datatest)

rmse(datatest$nbEstat, forecast)

par(mfrow=c(2,2))


for(i in 1:4)
{
  plotperiod(datatest,forecast, "2016-07-01", "2016-07-04", stat = 10 +i)
}


