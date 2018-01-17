# test RF sur le projet


rm(list=objects())
###############packages
library(rpart)
library(tree)
library(plotmo)
library(rpart.plot)
library(caret)
library(party)
library(randomForest)
library(Rborist)
library(magrittr)
rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=10))
}

#chargement des donnees
load("~/Documents/Orsay/M2/Data Mining/ProjetDataMining/AggratedData2013.RData")
DatawithMeteo[is.na(DatawithMeteo)] <- 0

##############################################################################################################################
##########################################default random forest
##############################################################################################################################
eq <- nbE ~ Day + Hour + station + temp + pluvio
rf0 <- randomForest(eq, ntree=5, data=DatawithMeteo, importance=FALSE)
rf0.fitted <- predict(rf0,newdata=DatawithMeteo)
rf0.forecast <- predict(rf0,newdata=DatawithMeteo)

names(rf0)
plot(rf0$mse) 
