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


eq <- nbEstat ~ s(Hour, k = 12)+s(districtpos) + s(dow, k= 8)+s(pluvio, k= 3) + s(temp) +s(Week)

g0 <- gam(eq, data = datatrain)
forecast<-predict(g0, newdata=datatest)

rmse(datatest$nbEstat, forecast)

par(mfrow=c(2,2))


for(i in 1:4)
{
  plotperiod(datatest,forecast, "2016-07-01", "2016-07-04", stat = 10 +i)
}


# Choix de K :
#=============
#Cette partie serait peut être à refaire:
# Ici on procède par validation simpple(TODO: remplacer par une  validation croisée)

list_k = 2*(5:10)
err_vect = vector(length = length(list_k))
for( k_hour in list_k)
{
  eq <- nbEstat ~ s(Hour, k = k_hour)+s(districtpos) + s(dow, k= 8)+s(pluvio, k= 3) + s(temp) +s(Week)
  g0 <- gam(eq, data = datatrain)
  forecast<-predict(g0, newdata=datatest)
  err_vect[k_hour] <- rmse(datatest$nbEstat, forecast)
}
par(mfrow=c(1,1))
plot(list_k,err_vect[list_k])
# idéal à partir de 12


#Heure?
list_k = 3*(5:10)
err_vect = vector(length = length(list_k))
for( k_hour in list_k)
{
  eq <- nbEstat ~ s(Hour, k = 12)+s(districtpos) + s(dow, k= 8)+s(pluvio, k= 3) + s(temp) +s(Week, k= k_hour)
  g0 <- gam(eq, data = datatrain)
  forecast<-predict(g0, newdata=datatest)
  err_vect[k_hour] <- rmse(datatest$nbEstat, forecast)
}
par(mfrow=c(1,1))
plot(list_k,err_vect[list_k])
#ne semble pas changer grand chose


list_k = 2*(5:10)
err_vect = vector(length = length(list_k))
for( k_hour in list_k)
{
  eq <- nbEstat ~ s(Hour, k = 12)+s(districtpos, k= k_hour) + s(dow, k= 8)+s(pluvio, k= 3) + s(temp) +s(Week)
  g0 <- gam(eq, data = datatrain)
  forecast<-predict(g0, newdata=datatest)
  err_vect[k_hour] <- rmse(datatest$nbEstat, forecast)
}
par(mfrow=c(1,1))
plot(list_k,err_vect[list_k])
# semble être meilleure quand k = 20

list_k = 2*(5:10)
err_vect = vector(length = length(list_k))
for( k_hour in list_k)
{
  eq <- nbEstat ~ s(Hour, k = 12)+s(districtpos, k= 20) + s(dow, k= 8)+s(pluvio, k= 3) + s(temp, k=k_hour) +s(Week)
  g0 <- gam(eq, data = datatrain)
  forecast<-predict(g0, newdata=datatest)
  err_vect[k_hour] <- rmse(datatest$nbEstat, forecast)
}
par(mfrow=c(1,1))
plot(list_k,err_vect[list_k])
#Ne semble pas affecter l'erreur

#### Prédiction de la différence:
#===================================
#On choisit le meilleure modèle, et on regarde pour les vélos sortants (ainsi que pour diff)
eqE <- nbEstat ~ s(Hour, k = 12)+s(districtpos, k= 20) + s(dow, k= 8)+s(pluvio, k= 3) + s(temp) +s(Week)
g0 <- gam(eqE, data = datatrain)
forecastE<-predict(g0, newdata=datatest)

rmse(datatest$nbEstat, forecast)

par(mfrow=c(2,2))

for(i in 1:4)
{
  plotperiod(datatest,forecast, "2016-07-01", "2016-07-08", stat = 10 +i)
}


# On essaie avex Sstat et les mêmes paramètres:
datatrain$nbSstat <- datatrain$nbS/datatrain$nb_stations
datatest$nbSstat <- datatest$nbS/datatest$nb_stations
eqS <- nbSstat ~ s(Hour, k = 12)+s(districtpos, k= 20) + s(dow, k= 8)+s(pluvio, k= 3) + s(temp) +s(Week)
g1 <- gam(eqS, data = datatrain)
forecastS<-predict(g1, newdata=datatest)
rmse(datatest$nbSstat, forecastS)
rmse(datatest$diff, forecastE-forecastS)
#0.357


##Et si on prédisait diff directemnt
eqdiff <- diff ~ s(Hour, k = 12)+s(districtpos, k= 20) + s(dow, k= 8)+s(pluvio, k= 3) + s(temp) +s(Week)
g2 <- gam(eqdiff, data = datatrain)
forecastdiff<-predict(g2, newdata=datatest)
rmse(datatest$diff, forecastdiff)
#0.357


