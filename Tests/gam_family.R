rm(list=objects())
###############packages
library(mgcv)
library(lubridate)
library(DivvyBikeProject)
library(tidyverse)
library(mgcViz)
library(mgcFam)

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


plotperiod <- function( data_in,data_in_spe, data_used , min_date, max_date , title = "Nb of rented bikes per day and per station", couleur = 'blue', stat = 12)
{
  period = which(data_in$Time < max_date & data_in$Time > min_date & data_in$district ==stat)
  plot(data_in$Time[period], data_in_spe[period], main  = paste("District ", as.character(stat)) ,xlab = "Time",  ylab = title, type = 'l', col =couleur)
  lines(data_in$Time[period], data_used[period])
}


###=======================================================================================================================================
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

datatrain  <- rbind(data_2014,data_2015, data_2016)
#datatrain <- summarise(group_by(datatrain, Time, dow, Day,Hour,pluvio, temp), nbEstat =sum(nbE))

datatest <-  data_2017
#datatest <- summarise(group_by(datatest, Time,dow, Day,Hour,pluvio, temp), nbEstat =sum(nbE))

datatrain$Week <- week(as.POSIXct(strptime(datatrain$Day, "%Y-%m-%d")))
datatrain$Year <- year(datatrain$Time)

datatest$Week <- week(as.POSIXct(strptime(datatest$Day,"%Y-%m-%d")))
datatest$Year <- year(datatest$Time)

#Correction de l'heure:
datatest$Hour <- hour(datatest$Time)
datatrain$Hour <- hour(datatrain$Time)

datatrain$nbEstat <- datatrain$nbE/datatrain$nb_stations
datatest$nbEstat <- datatest$nbE/datatest$nb_stations
datatrain$nbSstat <- datatrain$nbS/datatrain$nb_stations
datatest$nbSstat <- datatest$nbS/datatest$nb_stations

attach(datatrain)
datatrain <- datatrain[order(Time, district),]
detach(datatrain)

attach(datatest)
datatest <- datatest[order(Time, district),]
detach(datatest)

###===========================================================
#Un premier GAM
equation <- diff~s(Week,k=3)+s(Hour,k = 12) + te(Hour, temp, k=c(3, 5)) + s(district, k=10)+ s(temp)
gamexp<-gam(equation, data=datatrain) 
gamexp$forecast <- predict(gamexp, newdata=datatest)
rmse(gamexp$forecast, datatest$diff)
#0.346

plot(gamexp)

plotperiod(datatest , gamexp$forecast , "2017-09-04", "2017-09-06" , title = "diff", couleur = 'blue', stat = 9)

###=======================================================================================================================================
#Nombre de districts en 2014 & 2015
length(unique(data_2014$district)) #13
length(unique(data_2015$district)) #20
length(unique(data_2016$district)) #20

##Pics (ne fonctionne pas )
datatrain$pic <- 0
pic_time_pos <- which( datatrain$nbEstat >= 4 )
datatrain$pic[pic_time_pos] <- 1
pic_time_neg <- which( datatrain$nbSstat >= 4. )
datatrain$pic[pic_time_neg] <- datatrain$pic[pic_time_neg] - 1 

pic_time_pos_2016 = which( datatrain$pic == 1 & datatrain$Year == 2016)
pic_time_neg_2016 = which( datatrain$pic == -1. & datatrain$Year == 2016)

datatest$pic <- 0
datatest$pic[pic_time_pos_2016 - 365*24*33 - 3*24*28] <- 1
datatest$pic[pic_time_neg_2016 - 365*24*33 - 3*24*28] <- -1

###------------------------------------Test
equation <- nbEstat~s(Week,k=3)+s(Hour,k = 12) + te(Hour, temp, k=c(3, 5)) + s(district, k=10)+ s(temp)+pluvio
gamexp<-gam(equation, data=datatrain) 
gamexp$forecast <- predict(gamexp, newdata=datatest)
rmse(gamexp$forecast, datatest$nbEstat)
#0.838 avec pic
#0.833 sans 

plotperiod(datatest ,datatest$nbEstat, gamexp$forecast , "2017-09-04", "2017-09-10" , title = "diff", couleur = 'blue', stat = 9)

#------------------------------------
#Une autre façon  de voir les pics

dataglob <- summarise(group_by(datatrain,Time, Hour, dow), nbEstat =sum(nbE), nbSstat=sum(nbS))
dataday  <- summarise(group_by(dataglob, dow, Hour ), meanE = mean(nbEstat), meanS= mean(nbSstat) )

plot(dataday$meanE, type = 'l', col='blue', main = "Nb moyen d'entrées/sorties par heure et type de jours", ylab ="nombre"  )
lines(dataday$meanS, col ='red')

dataday$pic <- 0
dataday$pic[which(dataday$meanE > 600)] <- 1 
dataday$pic[which(dataday$meanE > 1000)] <- 2 
dataday$pic[which(dataday$meanE < 70)] <- -1 
dataday <- dataday[,- 3]
dataday <- dataday[,- 3]

datatest <- left_join(datatest, dataday, by = c('dow','Hour'))
datatrain<- left_join(datatrain, dataday, by = c('dow','Hour'))

datadistrict <- summarise(group_by(datatrain[which(datatrain$Year==2016),], district), nbE =sum(nbE))

attach(datadistrict)
datadistrict <- datadistrict[order(nbE),]
detach(datadistrict)

list_dist = unique(datadistrict$district)
datatrain$districtpos <- AddPos(datatrain, list_dist)
datatest$districtpos <- AddPos(datatest, list_dist)

#---------------------------------------------------------
equation <- nbEstat~s(Week,k=8)+s(Hour,k = 12) + te(Hour, temp, k=c(3, 5)) + s(districtpos, k=20)+ s(temp)+pluvio + pic
gamexp<-gam(equation, data=datatrain) 
gamexp$forecast <- predict(gamexp, newdata=datatest)
rmse(gamexp$forecast, datatest$nbEstat)

plotperiod(datatest ,datatest$nbEstat, gamexp$forecast , "2017-09-04", "2017-09-10" , title = "diff", couleur = 'blue', stat = 9)

#0.786 sans régularisation
#0.765 avec regularisation des districts mais sans classement (avec pic)
#0.751 avec classement & régularisation
#0.746 avec les temp corrigés

plot(gamexp)


#---------------------------------------------------------
equation <- nbEstat ~ s(Week,k=8)+s(Hour,k = 20) + te(Hour, temp, k=c(5, 5)) + s(districtpos, k=20)+ s(temp)+s(pluvio,k=3) +s(pic, k=3)
gamexp<-gam(equation, data=datatrain) 
gamexp$forecast <- predict(gamexp, newdata=datatest)
rmse(gamexp$forecast, datatest$nbEstat)

plotperiod(datatest ,datatest$nbEstat, gamexp$forecast , "2017-09-04", "2017-09-12" , title = "diff", couleur = 'blue', stat = 9)
#0.747 avec classement & régularisation (pos 1/0)
#0.746 avec pas (-1:2)
#0.744 avec les temp corrigé


plot(gamexp)

#----------------------------------------------------avec Year

equation <- nbEstat ~ s(Week,k=8)+s(Hour,k = 20) + te(Hour, temp, k=c(5, 5)) + s(districtpos, k=20)+ s(temp)+s(pluvio,k=3) + s(pic,k=3)
gamexp<-gam(equation, data=datatrain) 
gamexp$forecast <- predict(gamexp, newdata=datatest)
rmse(gamexp$forecast, datatest$nbEstat)

plotperiod(datatest ,datatest$nbEstat, gamexp$forecast , "2017-09-04", "2017-09-12" , title = "diff", couleur = 'blue', stat = 9)
#0.746 sans Year

plot(gamexp)



equation <- diff ~ s(Week,k=8)+s(Hour,k = 20) + te(Hour, temp, k=c(5, 5)) + s(districtpos, k=20)+ s(temp)+s(pluvio,k=3) +pic
gamexp<-gam(equation, data=datatrain) 
gamexp$forecast <- predict(gamexp, newdata=datatest)
rmse(gamexp$forecast, datatest$diff)

plotperiod(datatest ,datatest$diff, gamexp$forecast , "2017-09-04", "2017-09-12" , title = "diff", couleur = 'blue', stat = 9)

#0.346 avec pas (-1:2)


#-----------------------------------------

list_district = c(2,9,14,12)
par(mfrow=c(2,2))
for( area in list_district)
{
  #data_train <- datatrain[which(datatrain$district ==area),]
  data_test <- datatest[which(datatest$district ==area),]
  #equation <- nbEstat ~ s(Week,k=8)+s(Hour,k = 20) + te(Hour, temp, k=c(5, 5)) + s(temp)+s(pluvio,k=3) +s(pic, k=3)
  #gamexp<-gam(equation, data=data_train) 
  #gamexp$forecast <- predict(gamexp, newdata=data_test)
  print(rmse(gamexp$forecast[which(datatest$district ==area)], data_test$nbEstat))
  plotperiod(datatest ,datatest$nbEstat, gamexp$forecast , "2017-09-04", "2017-09-12" , title = "nbE_stat", couleur = 'red', stat = area)
}

#   Secteur only         Full data
#===================================
# 2 |     0.131         0.389
# 9 |     0.792         1
# 14 |     0.337        0.31
# 12 |     1.18         1.92