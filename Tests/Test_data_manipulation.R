#############################################
# Test premier traitement sur les données : #
#############################################

# 0 - Préparation de l'environnement :
######################################

rm(list=objects())
graphics.off()

library(magrittr)
library(DivvyBikeProject)
library(lubridate)
library(dplyr)

# Chargement des deux fichiers de données qui nous intéresse pour le moment 

#load("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/FullData2014.RData")
load("~/StatML/Projet/ProjetDataMining/FullData2015.RData")

data_2014 <- Data

load("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/FullData2015.RData")
data_2015 <- Data
#data_2015 <- data_2015[complete.cases(data_2015[,2]),] # à utiliser si NA's

rm(Data)

#-------------------------------------------------------------------------------------------------------

# I - Cours 1 : Importation, manipulation et nettoyage des données :
####################################################################

# Obtenir le type des variables de la data frame
str(data_2014) ## même chose pour 2015

# Stat de base
summary(data_2014)

# Sélection sur les lignes  
## Ne marche plus depuis que la variable diff a été normalisé
test_selec <- filter(data_2014, diff >= 6.5) ## diff plus grand que 6.5, 28 occurences
test_select_1 <- filter(data_2014, diff >= 6.5, dow == 7) ## diff plus grand que 6.5 : 
## samedi-7/dimanche-3
## différentiel important lorsqu'on est en début de semaine
## (le mardi-3/mercredi-4/jeudi-3 // le lundi-5/vendredi-5)

## Quelques stats 2014 : 2 400 000 trajets | 300 stations
## Quelques stats 2015 : 3 183 439 trajets | 474 stations

rm(test_selec, test_selec_2015) ## Nettoyage

#-------------------------------------------------------------------------------------------------------

# II - TP 1 : Manipulation de données :
#######################################

# Regroupement des tables 2014/2015 dans data_all
data_all <- rbind(data_2014, data_2015) 
plot(data_all$Time, data_all$diff, type='l')

# Zoom sur une partie des données 
## Ici : exemple avec le mois de juillet (07)
a <- ymd("2014-07-01")
b <- ymd("2014-08-01")
sel <- which(data_all$Time >= a & data_all$Time <= b)
o <- order(data_all$Time[sel])
plot(data_all$Time[sel[o]], data_all$diff[sel[o]], type='l')
## Plusieurs cycles = plusieurs saisonnalités (semaine et jour)
rm(a, b, sel, o)

# Graphique diff par district 
## Changer NumDistricts[i] pour voir le district i 
NumDistricts <- sort(as.matrix(distinct(data_all[,2])))

sel <- which(data_all$district == NumDistricts[4])
o <- order(data_all$Time[sel])

plot(data_all$Time[sel[o]], data_all$diff[sel[o]], col="black", type='l', xlab='Date', ylab='Diff')

rm(NumDistricts, o, sel)

#-------------------------------------------------------------------------------------------------------
# Graphiques à récupérer :
## Changement des stations (Tests/Test2013.R)
## Zones géographiques (Tests/Map.R)
#-------------------------------------------------------------------------------------------------------

# III - TP 2 : Visualisation :
##############################

# Modèle linéaire pour chercher la tendance
lm0 <- lm(diff ~ Time+nb_stations+dow+temp+pluvio+district, data=data_2014)
summary(lm0)
plot(data_2014$Time, data_2014$diff, type='l')
lines(data_2014$Time, lm0$fitted, col='red')
## Données centrées autour de zéro, pas vraiment de tendance 
rm(lm0)

# Quelques plot pour se rendre compte des relations entre variables explicatives/
#variable cible
plot(data_2014$dow, data_2014$diff, pch=16, cex=0.5)
plot(data_2014$nb_stations, data_2014$diff, pch=16, cex=0.5)
plot(data_2014$Time, data_2014$diff, pch=16, cex=0.5) ## linéaire
plot(data_2014$temp, data_2014$diff, pch=16, cex=0.5) ## intéressant 
plot(data_2014$pluvio, data_2014$diff, pch=16, cex=0.5)

# Diff moyen : deux exemples
diff_moy_dow <- tapply(data_2014$diff,as.factor(data_2014$dow),mean)
barplot(diff_moy_dow, col="palegoldenrod", beside=F)
plot(diff_moy_dow, type='b', pch=20, col="palevioletred")

diff_moy_dow_hour <- tapply(data_2014$diff, as.factor(data_2014$Hour):as.factor(data_2014$dow),mean)
matplot(diff_moy_dow_hour, type='l', col="red", lty=1, xlab='Hour/dow', ylab='diff')

rm(diff_moy_dow, diff_moy_dow_hour)

# Analyse des corrélations
## exemple avec la pluvio
cor(data_2014$pluvio, data_2014$diff)
## avec les variables numériques
coltypes <- lapply(data_2014, class)%>%unlist()
cor(data_2014[, which(coltypes=='numeric')-1])

acf(data_2014$diff, lag.max=52)
pacf(data_2014$diff, lag.max=52)

rm(coltypes)

# Graphique de la température
library(ggplot2)
library(ggthemes)
ggplot(data_2014, aes(x=temp, y=diff, color = temp))+
  geom_point() + scale_color_gradient(low="blue", high="red")+ theme_bw() 

#-------------------------------------------------------------------------------------------------------

# IV - TP 3 : Régression linéaire :
###################################

# Le seul intérêt du code qui suit est l'utilisation du summary pour récupérer les 
# résidus du modèle et le forecasting
lm2 <- lm(diff~temp+I(temp^2), data=data_2014)
lm2.summary<-summary(lm2)
lm2.summary
lm2.summary$adj.r.squared
o=order(data_2014$temp)
lines(data_2014$temp[o],lm2$fitted[o],col='red',lwd=2)
plot(data_2014$temp, lm2$residuals, pch=20)
lm2.forecast <- predict(lm2,newdata=data_2015)
rmse<-function(eps) {return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))}
rmse(data_2015$diff-lm2.forecast) ## ici égal à zéro car modèle non approprié

rm(o, lm2, lm2.summary, lm2.forecast, rmse)

# Création de la base de Fourier
w <- 2*pi/52.2
Nfourier <- 20
for(i in c(1:Nfourier))
{
  assign(paste("cos", i, sep=""),cos(w*Time*i))
  assign(paste("sin", i, sep=""),sin(w*Time*i))
}

plot(cos1,type='l')
plot(cos10,type='l')

rm(cos1, cos2, cos3, cos4, cos10, cos11, cos12, cos13, cos14, cos15, cos16, cos17, cos18, cos19)
rm(cos20, cos5, cos6, cos7, cos8, cos9)
rm(sin1, sin2, sin3, sin4, sin5, sin6, sin7, sin8, sin9, sin10, sin11, sin12, sin13, sin14)
rm(sin15, sin16, sin17, sin18, sin19, sin20)

rm(i, Nfourier, w)

# modèle pour la moyenne et modèle pour la variance (avec les cycles temporels et la température)

