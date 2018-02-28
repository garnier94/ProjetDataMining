rm(list=objects())
graphics.off()
###############packages
library(rpart)
library(party)
library(randomForest)
library(Rborist)
library(magrittr)
library(DivvyBikeProject)
library(lubridate)
library(tidyverse)

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=10))
}

#chargement des donnees
load("~/StatML/Projet/ProjetDataMining/FullData2014.RData")
data_2014 <- Data
load("~/StatML/Projet/ProjetDataMining/FullData2015.RData")
data_2015 <- Data
load("~/StatML/Projet/ProjetDataMining/FullData2016.RData")
data_2016 <- Data
rm(Data)

datatrain  <- rbind(data_2014,data_2015)
#datatrain <- summarise(group_by(datatrain, Time, dow, Day,Hour,pluvio, temp), nbEstat =sum(nbE))

datatest <-  data_2016
#datatest <- summarise(group_by(datatest, Time,dow, Day,Hour,pluvio, temp), nbEstat =sum(nbE))

datatrain$nbEstat <- datatrain$nbE/datatrain$nb_stations
datatrain$Week <- week(as.POSIXct(strptime(datatrain$Day, "%Y-%m-%d")))
datatest$nbEstat <- datatest$nbE/datatest$nb_stations
datatest$Week <- week(as.POSIXct(strptime(datatest$Day,"%Y-%m-%d")))
datatrain$nbSstat <- datatrain$nbS/datatrain$nb_stations
datatest$nbSstat <- datatest$nbS/datatest$nb_stations

listdistrict = unique(datatrain$district)
# Pour l'instant , on ne s'interesse qu'aux districts  pour lesquels on a des données au moins pour 2015.
datatest <- datatest[which(is.element(datatest$district, listdistrict)),]


##############################################################################################################################
##########################################default random forest
##############################################################################################################################
eq <- nbEstat ~   Week  + dow  + Hour +pluvio +district
rf1 <- randomForest(eq, ntree=5, data=datatrain, importance=FALSE)
rf1.fitted <- predict(rf1,newdata=datatest)
rmse(datatest$nbEstat, rf1.fitted)
#0.503 /0.491 pour ntree = 5 
#0.574 sans pluvio1
#0.461 pour ntree = 10
#0.99 sans district
#0.768 sans temp
## Nos variables sont pertinentes

Cdistrict =14

period = which(datatest$Day <= "2016-06-25" & datatest$Day  >= "2016-06-18" & datatest$district == Cdistrict)
test_prediction <- predict(rf0, newdata = datatest[period,] )

plot(datatest$Time[period], datatest$nbEstat[period],  type ='l', col= 'red', xlab = "Time", ylab= "Différence de vélo par heure")
lines(datatest$Time[period], test_prediction,  type ='l', col='blue')

pperiod = which(datatest$Day <= "2014-06-25" & data_2014$Day  >= "2014-06-10" & data_2014$district == Cdistrict)
lines(data_2015$Time[period], data_2014$nbEstat[pperiod] ,  type ='l', col='green')


eq <- nbSstat ~   Week + district + dow + temp  + Hour +pluvio
rf0 <- randomForest(eq, ntree=5, data=datatrain, importance=FALSE)
rf0.fitted <- predict(rf0,newdata=datatest)
rmse(datatest$nbSstat, rf0.fitted)
#0.47

rmse(datatest$diff , rf1.fitted -rf0.fitted)
#0.397

eq <- diff ~   Week  + dow + temp  + Hour +pluvio
rf2 <- randomForest(eq, ntree=5, data=datatrain, importance=FALSE)
rf2.fitted <- predict(rf2,newdata=datatest)
rmse(datatest$diff, rf2.fitted)
#0.289
