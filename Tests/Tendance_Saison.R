######################################################
############# Tendance et Saisonnalité ###############
######################################################

rm(list=objects())
graphics.off()

library(magrittr)
#library(DivvyBikeProject)
library(lubridate)
library(dplyr)

# Chargement des données
########################

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

data_all <- rbind(data_2014,data_2015,data_2016)
rm(data_2014,data_2015,data_2016)

data_all$nbEstat <- data_all$nbE / data_all$nb_stations
data_all$nbSstat <- data_all$nbS / data_all$nb_stations

data_all_nbE <- summarise(group_by(data_all, Time, dow, Day,Hour,pluvio, temp), nbEstat=sum(nbE))
data_all_nbS <- summarise(group_by(data_all, Time, dow, Day,Hour,pluvio, temp), nbSstat=sum(nbS))

#plot(data_all$Time, data_all$diff, type='l', main="Variable diff de 2014 à 2017")



# Modèle linéaire pour la recherche de la tendance
##################################################

## Sur nbEstat
lm_E <- lm(nbEstat~Time, data=data_all_nbE)
lm_S <- lm(nbSstat~Time, data=data_all_nbS)

plot(data_all_nbE$Time, data_all_nbE$nbEstat, type='l', xlab="Time", ylab = "Diff",main="Tendance de la variable nbEstat 2014-2017")
lines(data_all_nbE$Time, lm_E$fitted, col='red')

plot(data_all_nbS$Time, data_all_nbS$nbSstat, type='l', xlab="Time", ylab = "Diff",main="Tendance de la variable nbSstat 2014-2017")
lines(data_all_nbS$Time, lm_S$fitted, col='red')

#rm(lm_S,lm_E)

## Variables sans tendance
data_all_nbE$nbEstat_detrend <- data_all_nbE$nbEstat/lm_E$fitted
data_all_nbS$nbSstat_detrend <- data_all_nbS$nbSstat/lm_S$fitted

plot(data_all_nbE$Time, data_all_nbE$nbEstat_detrend, type='l',main="Série nbEstat sans sa tendance")
plot(data_all_nbS$Time, data_all_nbS$nbSstat_detrend, type='l',main="Série nbSstat sans sa tendance")

lm0_test <- lm(nbEstat_detrend~Time, data=data_all_nbE)
plot(data_all_nbE$Time, data_all_nbE$nbEstat_detrend, type='l',ylab="nbEstat_detrend",main="Série nbEstat sans sa tendance")
lines(data_all_nbE$Time, lm0_test$fitted, col='red')

lm1_test <- lm(nbSstat_detrend~Time, data=data_all_nbS)
plot(data_all_nbS$Time, data_all_nbS$nbSstat_detrend, type='l',ylab="nbSstat_detrend",main="Série nbSstat sans sa tendance")
lines(data_all_nbS$Time, lm1_test$fitted, col='red')


# Saisonnalité 
##############

data_all_nbE$id <- as.numeric(as.character(rownames(data_all_nbE))) 
data_all_nbS$id <- as.numeric(as.character(rownames(data_all_nbS))) 

attach(data_all_nbE)
data_all_nbE <-  data_all_nbE[order(Time),]
detach(data_all_nbE)

attach(data_all_nbS)
data_all_nbS <-  data_all_nbS[order(Time),]
detach(data_all_nbS)

## Création de la base de Fourier
# Saisonnalité annuelle = 24*365.2
# Saisonnalité journalière = 24
w <- 2*pi/(24*365.2) 
Nfourier <- 15
for(i in c(1:Nfourier))
{
  assign(paste("cos", i, sep=""),cos(w*data_all_nbE$id*i))
  assign(paste("sin", i, sep=""),sin(w*data_all_nbE$id*i))
}

## Insertion de la base de fourier dans la data.frame
cos<-paste('cos',c(1:Nfourier),sep="",collapse=",")                         
sin<-paste('sin',c(1:Nfourier),sep="",collapse=",")
paste("data.frame(data_all_nbE,",cos,",",sin,")",sep="")
paste("data.frame(data_all_nbS,",cos,",",sin,")",sep="")

data_all_nbE <- eval(parse(text=paste("data.frame(data_all_nbE,",cos,",",sin,")",sep="")))
data_all_nbS <- eval(parse(text=paste("data.frame(data_all_nbS,",cos,",",sin,")",sep="")))


lm.fourier_E<-list()
lm.fourier_S<-list()
eq_E<-list()
eq_S<-list()
for(i in c(1:Nfourier))
{
  cos<-paste(c('cos'),c(1:i),sep="")
  sin<-paste(c('sin'),c(1:i),sep="")
  fourier<-paste(c(cos,sin),collapse="+")
  eq_E[[i]]<-as.formula(paste("data_all_nbE$nbEstat_detrend~",fourier,sep=""))
  eq_S[[i]]<-as.formula(paste("data_all_nbS$nbSstat_detrend~",fourier,sep=""))
  lm.fourier_E[[i]]<-lm(eq_E[[i]],data=data_all_nbE)
  lm.fourier_S[[i]]<-lm(eq_S[[i]],data=data_all_nbS)
}

adjR<-function(x){summary(x)$adj.r.squared}

unlist(lapply(lm.fourier_E,adjR))
unlist(lapply(lm.fourier_S,adjR))

adjR_E<-unlist(lapply(lm.fourier_E,function(x){summary(x)$adj.r.squared}))
adjR_S<-unlist(lapply(lm.fourier_S,function(x){summary(x)$adj.r.squared}))

plot(adjR_E,type='b',pch=20,xlab='K',ylab='adjusted R-squared')
plot(adjR_S,type='b',pch=20,xlab='K',ylab='adjusted R-squared')

plot(data_all_nbE$nbEstat_detrend, type='l', ylab="nbEstat_detrend" ,main="Saisonnalité annuelle nbEstat")
lines(lm.fourier_E[[8]]$fitted, col='red')

plot(data_all_nbS$nbSstat_detrend, type='l',ylab="nbSstat_detrend" ,main="Saisonnalité annuelle nbSstat")
lines(lm.fourier_S[[8]]$fitted, col='red')

data_all_nbE$nbEstat_noseason <- data_all_nbE$nbEstat_detrend / lm.fourier_E[[8]]$fitted
data_all_nbS$nbSstat_noseason <- data_all_nbS$nbSstat_detrend / lm.fourier_S[[8]]$fitted

plot(data_all_nbE$nbEstat_noseason, type = 'l', ylab="nbEstat_noseason", main="Variable nbEstat sans saisonnalité annuelle")
plot(data_all_nbS$nbSstat_noseason, type = 'l', ylab="nbSstat_noseason", main="Variable nbSstat sans saisonnalité annuelle")

rm(cos1,cos2,cos3,cos4,cos5,cos6,cos7,cos8,cos9,cos10,cos11,cos12,cos13,cos14,cos15)
rm(sin1,sin2,sin3,sin4,sin5,sin6,sin7,sin8,sin9,sin10,sin11,sin12,sin13,sin14,sin15)

data_all_nbE <- data_all_nbE[,-c(10:39)]
data_all_nbS <- data_all_nbS[,-c(10:39)]

rm(w,Nfourier,i,cos,sin,lm0_test,lm1_test,fourier)


## VISUALISATION
a <- ymd("2014-07-07")
b <- ymd("2014-07-14")
sel <- which(data_all_nbE$Time >= a & data_all_nbE$Time <= b)
o <- order(data_all_nbE$Time[sel])

# Zoom sur un jour
plot(data_all_nbE$Time[sel[o]], data_all_nbE$nbEstat_detrend[sel[o]], type='l', xlab="", 
     ylab = "", main="")


## Deuxième saisonnalité sur un jour

data_all_nbE_3 <- data_all_nbE[which(data_all_nbE$dow==3),]
data_all_nbE_3$id <- as.numeric(as.character(rownames(data_all_nbE_3))) 

data_all_nbS_3 <- data_all_nbS[which(data_all_nbS$dow==3),]
data_all_nbS_3$id <- as.numeric(as.character(rownames(data_all_nbS_3))) 

w <- (2*pi)/24 
Nfourier <- 15
for(i in c(1:Nfourier))
{
  assign(paste("cos", i, sep=""),cos(w*data_all_nbE_3$id*i))
  assign(paste("sin", i, sep=""),sin(w*data_all_nbE_3$id*i))
}

## Insertion de la base de fourier dans la data.frame
cos<-paste('cos',c(1:Nfourier),sep="",collapse=",")                         
sin<-paste('sin',c(1:Nfourier),sep="",collapse=",")
paste("data.frame(data_all_nbE_3,",cos,",",sin,")",sep="")
paste("data.frame(data_all_nbS_3,",cos,",",sin,")",sep="")

data_all_nbE_3 <- eval(parse(text=paste("data.frame(data_all_nbE_3,",cos,",",sin,")",sep="")))
data_all_nbS_3 <- eval(parse(text=paste("data.frame(data_all_nbS_3,",cos,",",sin,")",sep="")))


lm.fourier_E<-list()
lm.fourier_S<-list()
eq_E<-list()
eq_S<-list()
for(i in c(1:Nfourier))
{
  cos<-paste(c('cos'),c(1:i),sep="")
  sin<-paste(c('sin'),c(1:i),sep="")
  fourier<-paste(c(cos,sin),collapse="+")
  eq_E[[i]]<-as.formula(paste("data_all_nbE_3$nbEstat_noseason~",fourier,sep=""))
  eq_S[[i]]<-as.formula(paste("data_all_nbS_3$nbSstat_noseason~",fourier,sep=""))
  lm.fourier_E[[i]]<-lm(eq_E[[i]],data=data_all_nbE_3)
  lm.fourier_S[[i]]<-lm(eq_S[[i]],data=data_all_nbS_3)
}

adjR<-function(x){summary(x)$adj.r.squared}

unlist(lapply(lm.fourier_E,adjR))
unlist(lapply(lm.fourier_S,adjR))

adjR_E<-unlist(lapply(lm.fourier_E,function(x){summary(x)$adj.r.squared}))
adjR_S<-unlist(lapply(lm.fourier_S,function(x){summary(x)$adj.r.squared}))

plot(adjR_E,type='b',pch=20,xlab='K',ylab='adjusted R-squared')
plot(adjR_S,type='b',pch=20,xlab='K',ylab='adjusted R-squared')

plot(data_all_nbE_3$nbEstat_noseason, type='l', ylab="nbEstat_noseason" ,main="Saisonnalité journalière nbEstat")
lines(lm.fourier_E[[2]]$fitted, col='red')

plot(data_all_nbS_3$nbSstat_noseason, type='l',ylab="nbSstat_noseason" ,main="Saisonnalité journalière nbSstat")
lines(lm.fourier_S[[2]]$fitted, col='red')

data_all_nbE_3$nbEstat_end<- data_all_nbE_3$nbEstat_noseason / lm.fourier_E[[2]]$fitted
data_all_nbS_3$nbSstat_end <- data_all_nbS_3$nbSstat_noseason / lm.fourier_S[[2]]$fitted

plot(data_all_nbE_3$nbEstat_end, type = 'l', ylab="nbEstat_end", main="Variable nbEstat sans saisonnalité journalière")
plot(data_all_nbS_3$nbSstat_end, type = 'l', ylab="nbSstat_end", main="Variable nbSstat sans saisonnalité journalière")


# Graphiques cool
#################

## 1) Différentiel selon le jour de la semaine
diff_moy_dow <- tapply(data_all$diff,as.factor(data_all$dow),mean)
barplot(diff_moy_dow, col="palegoldenrod", beside=F)
plot(diff_moy_dow, type='b', pch=20, col="palevioletred", main="différentiel selon le jour de la semaine")

rm(diff_moy_dow)

## 2) Influence de la température toutes stations confondues
library(ggplot2)
library(ggthemes)
ggplot(data_all, aes(x=temp, y=nbEstat, color = temp))+
  geom_point() + scale_color_gradient(low="blue", high="red")+ theme_bw() 

## 3) Influence de la température sur le district 14
data_14 <- data_all[which(data_all$district==14),]
ggplot(data_14, aes(x=temp, y=nbE_stat, color = temp))+
  geom_point() + scale_color_gradient(low="blue", high="red")+ theme_bw() 
rm(data_14)

## 4) Zoom sur une partie des données 

data_2014$nbEstat <- data_2014$nbE / data_2014$nb_stations
data_2014$nbSstat <- data_2014$nbS / data_2014$nb_stations
data_9_14 <- data_2014[which(data_2014$district==4),]

a <- ymd("2014-07-07")
b <- ymd("2014-07-14")
sel <- which(data_9_14$Time >= a & data_9_14$Time <= b)
o <- order(data_9_14$Time[sel])

# Zoom sur une semaine pour le rapport
plot(data_9_14$Time[sel[o]], data_9_14$nbEstat[sel[o]], type='l', xlab="", 
     ylab = "", main="NbEstat et NbSstat du 07-07-2014 au 13-07-2014")
lines(data_9_14$Time[sel[o]], data_9_14$nbSstat[sel[o]], col='red')
legend(x="topright",legend=c("nbEstat","nbSstat"),text.col=c("black","red"),pch=c(16,15),col=c("black","red"))

# Zoom sur une journée pour le rapport

data_9_14_j <- data_9_14[which(data_9_14$Time > "2014-11-27 00:00:00 CST" & data_9_14$Time <= "2014-11-28 00:00:00 CST"),]

data_9_14_j$Time <- data_9_14_j$Time+(3600*1)

plot(data_9_14_j$Time, data_9_14_j$nbEstat, type='l', xlab="", 
     ylab = "", main="NbEstat et NbSstat au 07-07-2014")
lines(data_9_14_j$Time, data_9_14_j$nbSstat, col='red')
legend(x="topright",legend=c("nbEstat","nbSstat"),text.col=c("black","red"),pch=c(16,15),col=c("black","red"))


plot(data_2014$Time[sel[o]], data_2014$nbS_stat[sel[o]], type='l', xlab="", 
     ylab = "nbSstat", main="nbSstat du 2014-07-07 au 2014-07-13")

plot(data_2014$Time[sel[o]], data_2014$diff[sel[o]], type='l', xlab="", 
     ylab = "diff", main="Diff du 2014-07-07 au 2014-07-13")

## Plusieurs cycles = plusieurs saisonnalités (semaine et jour)
rm(a, b, sel, o)


# Analyse des corrélations
##########################

## Corrélation des variables numériques
coltypes <- lapply(data_all, class)%>%unlist()
cor(data_all[, which(coltypes=='numeric')-1])
rm(coltypes)

acf(data_all$diff)
acf(data_all$diff, lag.max=10*24) #correlation hebdommadaire
pacf(data_all$diff, lag.max=10*24)

# Test pour visualiser la stationnarité
#######################################

library("xts")

data.xts<-xts(data_all$diff, order.by = data_all$Time)
hour <- as.factor(.indexhour(data.xts))
day  <- as.factor(.indexday(data.xts))
week <- as.factor(.indexweek(data.xts))
month<- as.factor(.indexmon(data.xts))

conso_horaire<-tapply(data.xts, hour, mean)
conso_journ<-tapply(data.xts, day, mean)
conso_hebdo<-tapply(data.xts, week, mean)
conso_mensuelle<-tapply(data.xts, month, mean)

plot(conso_journ, type="l", main="Diff journalier", xlab="jour", ylab='Diff')
abline(v=366, col='red', lty=2)
abline(v=366+365, col='red', lty=2)
abline(v=366+365+365, col='red', lty=2)
legend('topright', 'nouvel an', lty=2, col='red')

