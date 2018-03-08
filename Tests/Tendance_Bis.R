rm(list=objects())
graphics.off()

library(magrittr)
library(lubridate)
library(dplyr)
library(mgcv)
library(DivvyBikeProject)

## 2014
load("~/StatML/Projet/ProjetDataMining/FullData2014.RData")
data_2014 <- Data

## 2015
load("~/StatML/Projet/ProjetDataMining/FullData2015.RData")
data_2015 <- Data

## 2016
load("~/StatML/Projet/ProjetDataMining/FullData2016.RData")
data_2016 <- Data

## 2017
load("~/StatML/Projet/ProjetDataMining/FullData2017.RData")
data_2017 <- Data

rm(Data)

datatrain  <- rbind(data_2014,data_2015, data_2016)
datatrain <- summarise(group_by(datatrain, Time, dow, Day,Hour,pluvio, temp), nbE =sum(nbE), nb_Stations =  sum(nb_stations) )

datatest <-  data_2017
datatest <- summarise(group_by(datatest, Time, dow, Day,Hour,pluvio, temp), nbE =sum(nbE), nb_Stations =  sum(nb_stations) )


# Modèle linéaire pour la recherche de la tendance Sur nbE
#############################################################


lm_E <- lm(nbE~Time, data = datatrain)
tend_test <- predict.lm(lm_E,datatest)

plot(datatrain$Time, datatrain$nbE, type='l', xlab="Time", ylab = "Diff",main="Tendance de la variable nbEstat 2014-2016")
lines(datatrain$Time, lm_E$fitted, col='red')

plot(datatest$Time, datatest$nbE, type='l', xlab="Time", ylab = "Diff",main="Tendance de la variable nbSstat 2014-2017")
lines(datatest$Time, tend_test, col='red')

datatrain$nbE_detrend <- datatrain$nbE / lm_E$fitted
datatest$nbE_detrend <- datatest$nbE/ tend_test


# Saisonnalité(s) 
##############

Saisonnalité = function(data_i , w, Nfourier = 15 , var_name = "nbE_detrend" )
{
  data <- data_i 
  data$id <- as.numeric(as.character(rownames(data))) 
  for(i in c(1:Nfourier))
  {
    assign(paste("cos", i, sep=""),cos(w*data$id*i))
    assign(paste("sin", i, sep=""),sin(w*data$id*i))
  }
  cos<-paste('cos',c(1:Nfourier),sep="",collapse=",")                         
  sin<-paste('sin',c(1:Nfourier),sep="",collapse=",")
  data <- eval(parse(text=paste("data.frame(data,",cos,",",sin,")",sep="")))
  lm.fourier<-list()
  eq<-list()
  for(i in c(1:Nfourier))
  {
    cos<-paste(c('cos'),c(1:i),sep="")
    sin<-paste(c('sin'),c(1:i),sep="")
    fourier<-paste(c(cos,sin),collapse="+")
    eq[[i]]<-as.formula(paste("data$",var_name,"~",fourier,sep=""))
    lm.fourier[[i]]<-lm(eq[[i]],data=data)
  }
  return(lm.fourier)
}


## Création de la base de Fourier
# Saisonnalité annuelle = 24*365.2
# Saisonnalité journalière = 24
w <- 2*pi/(24*365.2) 
Nfourier <- 10

lm.fourier_E <- Saisonnalité(datatrain, w, Nfourier = 30) 
datatrain$nbE_htime <- datatrain$nbE_detrend / lm.fourier_E[[20]]$fitted
lm.fourier_test <- as.vector(lm.fourier_E[[20]]$fitted[1:8761])
datatest$nbE_htime <- datatest$nbE_detrend/lm.fourier_test

plot(datatrain$Time, datatrain$nbE_detrend, type='l', ylab="nbEstat" ,main="Saisonnalité journalière nbE")
lines(datatrain$Time, lm.fourier_E[[20]]$fitted, col='red')
plot(datatrain$Time, datatrain$nbE_htime, type='l', ylab="nbEstat" ,main="Saisonnalité journalière nbE")

lm.quot <- list()
lm_predict <- list()
datatrain$id_time <-0
datatrain$desaison <- -1 
datatest$desaison <- -1
for(jour in 1:8)
{
  w <- 2*pi/(24)
  dataday <- datatrain[which(datatrain$dow ==  jour),]
  dataday$id <- as.numeric(as.character(rownames(dataday))) 
  lm.quot[jour] <- Saisonnalité(dataday, w, Nfourier = 15, var_name = "nbE_htime" )[Nfourier]
  for(i in c(1:Nfourier))
  {
    assign(paste("cos", i, sep=""),cos(w*dataday$id*i))
    assign(paste("sin", i, sep=""),sin(w*dataday$id*i))
  }
  lm_predict[[jour]] <- predict.lm(lm.quot[[jour]], dataday)
  datatrain$desaison[which(datatrain$dow ==  jour)]<- lm_predict[[jour]]
  len = length(which(datatest$dow ==  jour))
  datatest$desaison[which(datatest$dow ==  jour)] <- lm_predict[[jour]][1:len]
}

aperiod = which(datatrain$Time >= "2016-09-10" & datatrain$Time <= "2016-09-18")

plot(datatrain[aperiod,]$Time, datatrain[aperiod,]$nbE_htime,type='l', xlab = "Time", ylab ="nbE corrigé de la tendance et de l'annualité", main =  "Saisonnalité journalière")
lines(datatrain[aperiod,]$Time,datatrain[aperiod,]$desaison, col = 'red')

period_2017 = which(datatest$Time >= "2017-09-10" & datatest$Time <= "2017-09-18")

plot(datatest[period_2017,]$Time, datatest[period_2017,]$nbE_htime,type='l', xlab = "Time", ylab ="nbE corrigé de la tendance et de l'annualité", main =  "Saisonnalité journalière 2017")
lines(datatest[period_2017,]$Time,datatest[period_2017,]$desaison, col = 'red')
plot(datatest$Time, datatest$reste, type = 'l', xlab = "Time", ylab = "Résidus", main = "Résidus pour 2017")



datatrain$reste <- datatrain$nbE_htime - datatrain$desaison
datatest$reste <- datatest$nbE_htime - datatest$desaison
plot(datatrain$Time, datatrain$reste, type = 'l')
datatest$Week <- week(datatest$Time)
datatrain$Week <- week(datatrain$Time)

equation <- reste ~ s(temp, k=5 ) +s(pluvio, k=6)+s(Week)+ te(Hour, temp, k=c(5, 5)) 
gamexp<-gam(equation, data=datatrain)
gamexp$forecast <- predict(gamexp, newdata=datatest)
summary(gamexp)
par(mfrow=c(1,2))
plot(gamexp)

datatest$prediction <- 0
pluspos <- which((datatest$desaison + gamexp$forecast) >= 0)
datatest$prediction[pluspos] <-  (tend_test * lm.fourier_test * (datatest$desaison + gamexp$forecast))[pluspos]
rmse(datatest$prediction, datatest$nbE) #310 #330 #309 

#290 avec WEEK
#286 avew Week +température bizarre

par(mfrow= c(1,1))
plot(datatest[period_2017,]$Time, datatest[period_2017,]$nbE,type='l', xlab = "Time", ylab ="nbE", main =  "Prédiction modèle saison/tendance 2017")
lines(datatest[period_2017,]$Time,datatest[period_2017,]$prediction, col = 'red')
period_2017 = which(datatest$Time >= "2017-01-17" & datatest$Time <= "2017-01-25")

