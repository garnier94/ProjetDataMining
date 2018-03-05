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

#chargement des donnees
load("~/StatML/Projet/ProjetDataMining/FullData2014.RData")
data_2014 <- Data
load("~/StatML/Projet/ProjetDataMining/FullData2015.RData")
data_2015 <- Data
load("~/StatML/Projet/ProjetDataMining/FullData2016.RData")
data_2016 <- Data
load("~/StatML/Projet/ProjetDataMining/FullData2017.RData")
data_2017 <- Data
rm(Data)

datatrain  <- rbind(data_2014,data_2015, data_2017)
#datatrain <- summarise(group_by(datatrain, Time, dow, Day,Hour,pluvio, temp), nbEstat =sum(nbE), nbStat =  sum(nb_stations) )
#datatrain$nbEstat <- datatrain$nbEstat/datatrain$nbStat

datatest <-  data_2017
#datatest <- summarise(group_by(datatest, Time, dow, Day,Hour,pluvio, temp), nbEstat =sum(nbE), nbStat =  sum(nb_stations) )
#datatest$nbEstat <- datatest$nbEstat/datatest$nbStat


datatrain$nbEstat <- datatrain$nbE/datatrain$nb_stations
datatrain$Week <- week(as.POSIXct(strptime(datatrain$Day, "%Y-%m-%d")))
datatest$nbEstat <- datatest$nbE/datatest$nb_stations
datatest$Week <- week(as.POSIXct(strptime(datatest$Day,"%Y-%m-%d")))
datatrain$nbSstat <- datatrain$nbS/datatrain$nb_stations
datatest$nbSstat <- datatest$nbS/datatest$nb_stations

datatest$Year <- year(datatest$Time)
datatrain$Year <- year(datatrain$Time)

listdistrict = unique(datatrain$district)
# Pour l'instant , on ne s'interesse qu'aux districts  pour lesquels on a des données au moins pour 2015.
datatest <- datatest[which(is.element(datatest$district, listdistrict)),]

###QQ essais 
area =2
#datatrain <- datatrain[which(datatrain$district ==area),]
#datatest <- datatest[which(datatest$district ==area),]


##############################################################################################################################
##########################################default random forest
##############################################################################################################################
eq <- nbEstat ~   Week  + dow  + Hour + pluvio  + temp + Year +district
rf1 <- randomForest(eq, ntree=10, data=datatrain, importance=FALSE)
rf1.fitted <- predict(rf1,newdata=datatest)
rmse(datatest$nbEstat, rf1.fitted)
#0.36 maintenant
#0.305 avec 10 tree
#0.85 sans les districts(mistake)

list_district = c(2,9,14,12)
par(mfrow=c(2,2))
for( area in list_district)
{
  data_train <- datatrain[which(datatrain$district ==area),]
  data_test <- datatest[which(datatest$district ==area),]
  eq <- nbEstat ~  Week  + dow  + Hour + Year  +pluvio +temp
  rf0 <- randomForest(eq, ntree=10, data=data_train, importance=FALSE)
  rf0.fitted <- predict(rf0,newdata=data_test)
  print(rmse(rf0.fitted, data_test$nbEstat))
  print(rmse(rf1.fitted[which(datatest$district ==area)], datatest$nbEstat))
}

#0.108 0.08
#0.525 0.34
#0.145 0.13
#0.733 0.52


###Districts spécific:
area =9
rmse(datatest$nbEstat[which(datatest$district==area)], rf1.fitted[which(datatest$district==area)]) #0.116 pour 2


## Nos variables sont pertinentes

Cdistrict =14
list_district = c(2)
par(mfrow=c(1,1))

for( dis in list_district)
{
  period = which(datatest$Day <= "2017-02-15" & datatest$Day  >= "2017-02-01" & datatest$district == dis)
  test_prediction <- predict(rf0, newdata = datatest[period,] )
  plot(datatest$Time[period], datatest$nbEstat[period],  type ='l', col= 'red', xlab = "Time", ylab= "nbE_stat", main = paste( "District", dis) )
  lines(datatest$Time[period], test_prediction,  type ='l', col='blue')
  legend( x="topright", legend=c("Données", "Prédiction"),
         col=c("red", "blue"), lty=1, cex=0.8)
}

pperiod = which(datatest$Day <= "2014-06-25" & data_2014$Day  >= "2014-06-10" & data_2014$district == Cdistrict)
lines(data_2015$Time[period], data_2014$nbEstat[pperiod] ,  type ='l', col='green')

#=================sans pluvio
eq <- nbSstat ~  Week  + dow  + Hour   + temp + Year +district
rf0 <- randomForest(eq, ntree=5, data=datatrain, importance=FALSE)
rf0.fitted <- predict(rf0,newdata=datatest)
rmse(datatest$nbSstat, rf0.fitted)
#0.248

#=================sans temp
eq <- nbSstat ~  Week  + dow  + Hour + Year +district +pluvio
rf0 <- randomForest(eq, ntree=5, data=datatrain, importance=FALSE)
rf0.fitted <- predict(rf0,newdata=datatest)
rmse(datatest$nbSstat, rf0.fitted)
#0.407

#=================sans district
eq <- nbEstat ~  Week  + dow  + Hour + Year  +pluvio +temp
rf0 <- randomForest(eq, ntree=5, data=datatrain, importance=FALSE)
rf0.fitted <- predict(rf0,newdata=datatest)
rmse(datatest$nbEstat, rf0.fitted)
#0.188 ????

#=================sans dow
eq <- nbSstat ~  Week   + temp  + Hour + Year +district +pluvio
rf0 <- randomForest(eq, ntree=5, data=datatrain, importance=FALSE)
rf0.fitted <- predict(rf0,newdata=datatest)
rmse(datatest$nbSstat, rf0.fitted)
#0.405

#=================sans Year
eq <- nbSstat ~  Week + dow  + temp  + Hour  + district + pluvio
rf0 <- randomForest(eq, ntree=5, data=datatrain, importance=FALSE)
rf0.fitted <- predict(rf0,newdata=datatest)
rmse(datatest$nbSstat, rf0.fitted)
#0.322

#=================== Groupement géo dis_9
eq <- nbEstat ~  Week  + dow  + Hour + Year  +pluvio +temp
rf0 <- randomForest(eq, ntree=5, data=datatrain, importance=FALSE)
rf0.fitted <- predict(rf0,newdata=datatest)
rmse(datatest$nbEstat, rf0.fitted)
#0.349 pour le district 9 contre 0.543 en utilisant toutes les données
#0.0861 pour le district 2 contre 0.116 en utilisant toutes les données. 