#####################################
############  PROJET R  #############
### Consommation électrique Zone 2 ##
######## Palmier & Valladier ########
#####################################

#####################################
###Preparation des donnees

###Preparation de l'environnement de travail
rm(list=objects())
graphics.off()

#setwd("~/Fac/M1/ProjetR - annee2") Arnaud
setwd("/Volumes/CAMILLE/Projet") #Camille

#Chargement des packages
library("magrittr")
library("timeDate")
library("xts")
library("stats")
library("mgcv")

###Creation du data file:
data<-read.table("data_elec0.txt", header=T, sep='\t')
n = length(data[,1])

###Conversion de la premiere colonne en type "date":
Date = as.POSIXct(strptime(data$date,"%Y-%m-%d %H:%M:%S"))
plot(Date, pch='.')
plot(diff(Date)) #sauts : dates bien implementees
data$date <- Date
#on remarque qu'il y a des trous dans les donnees
rm(Date)

#####################################
###Choix de la station 2
sd    = sd(data$Zone2)
mean  = mean(data$Zone2)

plot(data$date,data$Zone2, type='l', ylab="", main="consommation electrique de la zone 2")
#on observe plusieurs saisonnalites

boxplot(data$Zone2)  #les valeurs extremes ne sont que superieures
hist(data$Zone2)

### Format xts - aggregation heure, jour, semaine, mois
zone2.xts<-xts(data$Zone2, order.by = data$date)
hour <- as.factor(.indexhour(zone2.xts))
day  <- as.factor(.indexday(zone2.xts))
week <- as.factor(.indexweek(zone2.xts))
month<- as.factor(.indexmon(zone2.xts))

conso_horaire<-tapply(zone2.xts, hour, mean)
conso_journ<-tapply(zone2.xts, day, mean)
conso_hebdo<-tapply(zone2.xts, week, mean)
conso_mensuelle<-tapply(zone2.xts, month, mean)

#####################################
###Consomation horaire:
boxplot(data$Zone2 ~ data$Hour,col="lightblue",pch=20,cex=0.5, main="consommation par heure de la zone 2", xlab="Heure")
#on voit que la longueur des boites est toujours a peu pres la meme 
#elle augmente quand meme un peu entre entre 16h et 18h inclus
#il n'y a que des valeurs extremes superieures concentrees surtout en minuit et 10h
plot(100*conso_horaire/mean(conso_horaire), main="consommation horaire en pourcentage par rapport a la moyenne", 
     type="b", xlab='heures', ylab='')
abline(h=100)
#la conso est faible la nuit, plus elevee en journee a partir de 9h, encore plus en soiree et s'effondre entre minuit et une heure


#####################################
###Conso moyennee par jour:
plot(conso_journ, type="l", main="consommation journaliere", xlab="jour", ylab='consommation')
abline(v=366, col='red', lty=2)
abline(v=366+365, col='red', lty=2)
abline(v=366+365+365, col='red', lty=2)
legend('topright', 'nouvel an', lty=2, col='red')
#on distingue mieux les saisonnalites : annuelle et hebdomadaire

#####################################
###Par jour de la semaine
boxplot(data$Zone2 ~ data$dow,col="lightblue",pch=20,cex=0.5, main="consommation par jour de la zone 2", xlab="Heure")
#variance a peu pres constante sauf plus elevee le lundi, boite a moustache basse plus longue dus a la fin du we
#variance du we legerement plus faible

#####################################
###Par type de jour
consoJ <- tapply(data$Zone2, data$Day, mean)
plot(1:31,consoJ, type='b', main='Moyenne de la serie par jour', xlab='jour du mois', ylab='consommation')
#Notre activite ne depend pas du jour du mois => information non pertinente


consoJSpe <- tapply(data$Zone2, data$daytype, mean)
plot(1:24, 100*sort(consoJSpe)/mean, main='consommation par type de jour en pourcentage par rapport a la moyenne',
     type='b', xlab='', ylab='consommation', xaxt="n")
axis(1, at=seq(1, 24, by=1), labels = FALSE)
text(seq(1, 24, by=1), par("usr")[3] - 3, labels = names(sort(consoJSpe)), srt = 45, pos = 2, xpd = TRUE, cex=0.5)
sepx <- c(0.5, 4.5, 13.5, 18.5, 24.5)
sepy <- c(75, 85, 100, 107, 122)
rect(sepx[1], sepy[1], sepx[2], sepy[2], border='orange')
rect(sepx[2], sepy[2], sepx[3], sepy[3], border='green')
rect(sepx[3], sepy[3], sepx[4], sepy[4], border='blue')
rect(sepx[4], sepy[4], sepx[5], sepy[5], border='red')
abline(h=100, lty=2, col='red')

#on met en evidence un profil de consommation suivant les evenements
#comme les jours feries et les week-ends qui ont une plus faible consommation
#on en deduit que notre zone contient un certain nombre d'entreprises
#qui sont plus actives la semaine et certains jours particuliers
#comme par exemple le jour des elections

#Avec un boxplot:
test <- as.factor(data$daytype)
test2<- factor(test, names(sort(consoJSpe)))

boxplot(data$Zone2 ~ test2, col="lightblue",pch=20,cex=0.5, main="consommation par jours speciaux", 
        xlab="", las=2, at=1:24, varwidth=F)
#les plus grandes variances sont sur les jours normaux. Les jours speciaux ont une variances nettement plus faible,
#excepte le Memorial Day, le President Day et le MLK Birthday: ces jours sont donc plus dur a prevoir et a ajuster
#On pense qu'il sera donc plus facile de prevoir la consommation des jours speciaux montrant une faible variance.
#Cette faible variance s'explique aussi par le fait qu'il y a beaucoup moins de jours speciaux que de jours normaux.

rm(test, test2)

#####################################
##Regardons le profil par heure de certain jours speciaux : la repartition change

#Consommation la plus basse: Memorial Day:
dataN   <- subset(data, daytype == 'USMemorialDay', select = c(date, Zone2))
N.xts   <- xts(dataN$Zone2, order.by = dataN$date)
hourN   <- as.factor(.indexhour(N.xts))
meanMD  <- tapply(N.xts, hourN, mean)

#Consommation de plus basse variance: le nouvel an:
dataN   <- subset(data, daytype == 'USNewYearsDay', select = c(date, Zone2))
N.xts   <- xts(dataN$Zone2, order.by = dataN$date)
hourN   <- as.factor(.indexhour(N.xts))
meanNY  <- tapply(N.xts, hourN, mean)

#Consommation jours normaux du lundi au vendredi:
dataN   <- subset(data, daytype == 'lundi'|daytype =='mardi'|daytype =='mercredi'
                  |daytype =='jeudi'|daytype =='vendredi',select = c(date, Zone2)) 
N.xts   <- xts(dataN$Zone2, order.by = dataN$date)
hourN   <- as.factor(.indexhour(N.xts))
meanWD  <- tapply(N.xts, hourN, mean)

#Consommation du WE:
dataN   <- subset(data, daytype == 'samedi' | daytype == 'dimanche', select = c(date, Zone2))
N.xts   <- xts(dataN$Zone2, order.by = dataN$date)
hourN   <- as.factor(.indexhour(N.xts))
meanWE  <- tapply(N.xts, hourN, mean)

#Consommation la pus haute: president Day
dataN   <- subset(data, daytype == c('USPresidentsDay'), select = c(date, Zone2))
N.xts   <- xts(dataN$Zone2, order.by = dataN$date)
hourN   <- as.factor(.indexhour(N.xts))
meanPD  <- tapply(N.xts, hourN, mean)

#Representation graphique
plot(dataN$date[1:24], meanMD, type='l', main="Profil horaire suivant differents types de jours", xlab='heures', 
     ylab='conso', col='orange', ylim=c(100000,240000))
lines(dataN$date[1:24], meanWD, col='blue')
lines(dataN$date[1:24], meanWE, col='green')
lines(dataN$date[1:24], meanPD, col='red')
lines(dataN$date[1:24], meanNY, col='purple')
legend('bottomright', c('Memorial Day', 'Nouvel An', 'Week ends', 'en semaine', 'Presidents Day'), lty = c(1,1,1,1,1), 
       col=c('orange', 'purple', 'green', 'blue', 'red'), cex=0.8)

#On remarque bien des profils de consommation par heure differents! Nous en prendrons compte dans notre 
#modele de prevision!

rm(sepx, sepy, dataN, N.xts, hourN, meanWE, meanWD, meanPD, meanMD, meanNY)



#####################################
###Conso hebdo
plot(conso_hebdo, type="l",main="conso hebdo", xlab="nbre de semaine")
abline(v=53, col='red', lty=2)
abline(v=53+53, col='red', lty=2)
abline(v=53+53+53+53, col='red', lty=2)
legend('topright', 'nouvel an', lty=2, col='red')
#on voit mieux la saisonnalite annuelle ainsi que la tendance a la hausse

#####################################
###Conso mensuelle
plot(conso_mensuelle, type='l', main="conso mensuelle")
#on voit bien les evolutions de la consommatin durant l'année
boxplot(data$Zone2 ~ data$Month,col="lightblue",pch=20,cex=0.5, main="consommation par mois de la zone 2", xlab="Mois")
#on remarque une plus grande dispersion en ete qu'en hiver avec des valeurs extremes concentrees dans les 
#periodes de transition entre hiver/printemps et ete/automne
#ces ecarts peuvent s'expliquer par des jours anormalement froids ou anormalement chauds


#Comparaison avec la dispersion de la temperature
boxplot(data$Station9 ~ data$Month)
#pourtant la dispersion des temperatures est plus faible en ete. Cela s'explique par le comportement
#des americains vis a vis de la climatisation. Celle ci est reglee en moyenne sur 70 F, elle tourne donc a fond
#la moitie de la journee.En zoomant sur la consommation en ete, on peut voir une tres forte oscillation
#journaliere!

#Exemple :
data$date[4500]
plot(data$date[4500:(4500+7*24)], data$Zone2[4500:(4500+7*24)], type='l', xlab='', ylab='consommation',
     main='exemple de variation de consommation en juillet 2004')


#####################################
##Evolution de la consommation au fil des ans
annee1 <- data[1:8784,]
annee2 <- data[8785:16872,]
annee3 <- data[16873:24961,]
annee4 <- data[24962:n,]
  
Annee1.M<-tapply(annee1$Zone2, as.factor(format(annee1$date, "%m")), mean)
Annee2.M<-tapply(annee2$Zone2, as.factor(format(annee2$date, "%m")), mean)
Annee3.M<-tapply(annee3$Zone2, as.factor(format(annee3$date, "%m")), mean)
Annee4.M<-tapply(annee4$Zone2, as.factor(format(annee4$date, "%m")), mean)
plot(1:12, Annee1.M, type='b', lwd=2, ylim=c(140000, 250000), main='consommation moyenne par mois et par annee', 
     xlab='mois', ylab='consommation')
lines(1:12, Annee2.M, col=2, type='b', lwd=2)
lines(1:12, Annee3.M, col=3, type='b', lwd=2)
lines(1:12, Annee4.M, col=4,type='b', lwd=2)
legend("topright", c("Annee 1", "Annee 2", "Annee 3", "Annee 4"), col = c("black", 2, 3, 4), lty = c(1,1,1,1))

#on observe une augmentation progressive de la consommation de 2004 a 2007
#avec un choc au mois de fevrier 2007, coherent avec l'abondance de valeurs extremes sur les boxplots

#Pour s'en convaincre:
mean(annee1$Zone2)
mean(annee2$Zone2)
mean(annee3$Zone2)
mean(annee4$Zone2)
#on a quand meme une stagnation entre 2005 et 2006 mais la tendance semble etre a la hausse.


#on nettoie
rm(hour, day, week, month, conso_hebdo, conso_horaire, conso_journ, conso_mensuelle, consoJ, consoJSpe)
rm(Annee1.M, Annee2.M, Annee3.M, Annee4.M)
rm(annee1, annee2, annee3, annee4)

##Remarque: on voit bien qu'il n'y a pas le meme nombre d'observations suivant les annees => donnees manquantes




#####################################
###AUTO CORRELATION
acf(data$Zone2) #correlation par cycle de 24h tres visible
acf(data$Zone2, lag.max = 10*24) #on remarque egalement une correlation hebdommadaire
acf(data$Zone2, lag.max = 10*24, plot=FALSE)[24]
acf(data$Zone2, lag.max = 10*24, plot=FALSE)[7*24]
#peu de tendance donc quand on va l'enlever l'acf restera sensiblement la meme - ligne 334

pacf(data$Zone2, lag.max=10*24)
#on retrouve la correlation a 24h sur une semaine
#mais qui s'effondre apres une semaine (il n'est pas pertinent de prevoir a un horizon de plus d'une semaine)
#la correlation journaliere a horizon superieur a une semaine semble donc artificielle
pacf(data$Zone2, lag.max = 10*24, plot=FALSE)[24]
pacf(data$Zone2, lag.max = 10*24, plot=FALSE)[7*24]





#####################################
##### RECHERCHE D'UNE TENDANCE ######
#####################################


week <- as.factor(.indexweek(zone2.xts))
conso_hebdo<-tapply(zone2.xts, week, mean)
plot(conso_hebdo, type="l",main="conso hebdo")

#Packages:
library("datasets")
library('graphics')
library('grDevices')


#####################################
###Noyau gaussien

h     <- 24*7*52   #fenetre annuelle
noyau <- ksmooth(1:n, data$Zone2, kernel=c("normal"), bandwidth=h)  #calcul du noyau, assez long a charger (30s)
reg <- lm(noyau$y~noyau$x)
summary(reg)

coef1 <- reg$coefficients[2]
plot(data$Time, noyau$y, type='l') 
lines(data$Time, reg$coeff[1] + reg$coeff[2]*data$Time, col='red', type='l', lwd=2)
legend('bottomright', c('tendance par noyau gaussien', 'tendance par regression sur le noyau'), 
       col=c('black', 'red'), lty=c(1,1,1))

#la pente par heure est faible 0.43

#Prenons une fenetre plus petite
#####################################
h     <- 24*7*26   #fenetre 6 mois
noyau <- ksmooth(1:n, data$Zone2, kernel=c("normal"), bandwidth=h)  #calcul du noyau, 20s

plot(data$Time, noyau$y, type='l') 
reg <- lm(noyau$y~noyau$x)
summary(reg)
lines(data$Time, reg$coeff[1] + reg$coeff[2]*data$Time, col='red', type='l')
legend('topright', c('tendance par noyau gaussien', 'tendance par regression sur le noyau'), 
       col=c('black', 'red'), lty=c(1,1,1))

coef2 <- reg$coefficients[2]

abs(coef1-coef2)/coef1  #1% de decalage
#on trouve bien la meme tendance


###Avec les donnees moyennees par semaines?
#####################################
nw = length(conso_hebdo)
h  = 52                   #nombre de semaines par an
noyau <- ksmooth(1:nw, conso_hebdo, kernel=c("normal"), bandwidth=h)  #calcul du noyau, instantanne

reg <- lm(noyau$y~noyau$x)
summary(reg)
plot(noyau, type='l')
lines(noyau$x, reg$coeff[1] + reg$coeff[2]*noyau$x, col='red', type='l')

coef3 <- reg$coefficients[2]
test  <- 7*24*coef1        #le coef 1 est par heures, le coef3 est par semaine
#on fait la conversion pour les comparer
abs(coef3-test)/test
#erreur de 0.7%, la serie de la consommation moyenne par semaine nous permet de bien estimer la tendance
#ce qui est logique car la tendance est un estimateur a temps long
#On garde cette methode 

#Representation graphique
plot(data$Time, data$Zone2, main='tendance par noyau gaussien sur un an', xlab='date', ylab='consommation', type='l')
lines(data$Time, reg$coeff[1] + reg$coeff[2]/(7*24)*data$Time, col='red', type='l', lwd=2)
abline(h=reg$coefficients[1], lty=2, col='orange')
legend('bottomright', c("tendance a l'origine", 'tendance par regression sur le noyau'), 
       col=c('orange', 'red'), lty=c(2,1))

#Nettoyage
rm(coef1, coef2, coef3, conso_hebdo, h, noyau, nw, test, week, zone2.xts)




###############################################
###Auto correlation sans tendance 
acf(data$Zone2-reg$coeff[1] + reg$coeff[2]/(7*24)*data$Time, main='ACF zone 2 sans tendance') 
#correlation par cycle de 24h tres visible
pacf(data$Zone2-reg$coeff[1] + reg$coeff[2]*data$Time, lag.max=10*24, main='PACF zone 2 sans tendance')
#on retrouve sensiblement la meme chose voir remarque precedente

rm(reg)


#####################################
# CREATION D'UN MODELE DE PREVISION #
#####################################

#Separation des donnees en deux
sep <- which(data$date == '2007-01-01 00:00:00')

data0a <- data[1:sep,]        #donnees d'apprentissage
data0b <- data[-c(1:sep),]    #donnees de test 


#####################################
###Fonction RMSE et MAPE pour les calculs d'erreur
rmse<-function (eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function (y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
}

fit.error <- function(data=reg.station){
  lapply(data, function(x){rmse(data0a$Zone2 - x$fitted)})%>%unlist
}

fit.mape <- function(data=reg.station){
  lapply(data, function(x){mape(data0a$Zone2, x$fitted )})%>%unlist
}

forecast.mape <- function(data = reg.forecast){
  lapply(data, function(x){mape(data0b$Zone2, x)})%>%unlist
}

forecast.error <- function(data=reg.forecast){
  lapply(data, function(x){rmse(data0b$Zone2 - x)})%>%unlist
}

adj.rsquare<-function(data = reg.station){
  lapply(data,function(x){summary(x)$adj.r.squared})%>%unlist
}



#####################################
##REGRESSION LINEAIRE SUR LES STATIONS

NStation   <- 11
ColStation <- c(29:39)
Station    <- c()

#mise en place des regressions lineaires:
reg.station  <- list()
reg.forecast <- list()
eq          <- list()
for (i in c(1:NStation)){
  Station[i]       <- paste('Time+Station',i,sep = "")
  eq[[i]]          <- as.formula(paste("Zone2~",Station[i],sep=""))
  reg.station[[i]]  <- lm(eq[[i]],data=data0a) 
  reg.forecast[[i]] <- predict(reg.station[[i]],data0b)
}


#R Ajuste:
plot(adj.rsquare(), type='b',pch=20, main=paste("Adjusted rsquared error"), xlab='N° de Station', ylab='consommation')  
#coefficient de correlation faible, pas satisfaisant du tout!


#Erreur de fitting:
plot(fit.mape(), xlab="Station", ylab="mape_error", type='b', pch=20, main=paste("ERREUR DE FITTAGE"))
par(new=TRUE)
plot(fit.error(), xlab="Station", ylab='', type='b', pch=20, col='red', axes=FALSE)
axis(side=4, col='dark red', col.axis='red', ylab="rmse_error")
legend("bottomright", c("mape fit","rmse fit"), col=c('black','red'), lty=c(1,1))

#33000 pr la somme des carres residuels : mauvais
#idem pour les erreurs de previsions


#Erreur de prevision:
plot(forecast.mape(), xlab='Station', ylab=" ", type='b', pch=20, main="ERREUR DE PREVISION")
par(new=TRUE)
plot(forecast.error(), xlab=" ", ylab=" ", type='b', col='red', pch=20, axes=FALSE)
axis(side=4, col='dark red', col.axis='red')
legend("bottomright", c("mape forecast","rmse forecast"), col=c('black','red'), lty=c(1,1))

#Mais etonnament ont a "que" 16% d'erreur d'ajustement et de prevision


#Au lieu de faire une regression lineaire sur une seule station, on va etoffer notre modele
rm(Station, eq, reg.station, reg.forecast)


plot(data$Station5, data$Zone2)
#La regression est non pertinente car les donnees de consommation ne sont pas du tout lineaires
#La regression simple est donc ininteressante
#On voit plutot que c'est lineaire par morceaux => on va creer une variable station tronquee



#FIRST, INTERESSONT NOUS AU CAS DU TRUNC.65:


#####################################
##STATION TRONQUEES A 65 FAHRENHEIT

#On a choisi 65F comme etant le point d'intersection des deux parties lineaires

Station.trunc.65 <- paste('Station', c(1:NStation),".trunc.65", sep="", collapse=",")

for(i in c(1:NStation)){
  assign(paste("Station",i,".trunc.65",sep=""), pmax(data[,ColStation[i]]-65,0))  
}

#On met les nouvelles donnees dans note data frame:
data <- eval(parse(text=paste("data.frame(data,",Station.trunc.65,")",sep="")))

#On nettoie
rm(Station1.trunc.65,Station2.trunc.65,Station3.trunc.65,Station4.trunc.65,Station5.trunc.65,
   Station6.trunc.65,Station7.trunc.65,Station8.trunc.65,Station9.trunc.65,Station10.trunc.65,Station11.trunc.65)

#On actualise nos donnees
data0a <- data[1:sep,]
data0b <- data[-c(1:sep),]

### On recommence la regression:
reg.station  <- list()
reg.forecast <- list()
eq           <- list()

for (j in c(1:NStation)){
  Station.trunc.65[j] <- paste("Time+Station",j,"+Station",j,".trunc.65",sep="")
  eq[[j]]             <- as.formula(paste("Zone2~",Station.trunc.65[j],sep=""))
  reg.station[[j]]    <- lm(eq[[j]], data=data0a)
  reg.forecast[[j]]   <- predict(reg.station[[j]], data0b)   #la ca coince...  MERDE!!!
}


#R Ajuste:
plot(adj.rsquare(),type='b',pch=20,main=paste("Adjusted rsquared error"))  #pas loin des 0.5, ca monte!
which.max(adj.rsquare())
#la 9 a le meilleur R2 qui est beaucoup mieux que ce que l'on avait trouve avec le precedent modele


#Erreur de fitting:
plot(fit.mape(), xlab="Station", ylab="mape_error", type='b', pch=20, main=paste("ERREUR DE FITTAGE"))
par(new=TRUE)
plot(fit.error(), xlab="Station", ylab='', type='b', pch=20, col='red', axes=FALSE)
axis(side=4, col='dark red', col.axis='red', ylab="rmse_error")
legend("topright", c("mape fit","rmse fit"), col=c('black','red'), lty=c(1,1))
#la 5 et 9 sont bon candidats


#Erreur de prevision:
plot(forecast.mape(), xlab='Station', ylab=" ", type='b', pch=20, main="ERREUR DE PREVISION")
par(new=TRUE)
plot(forecast.error(), xlab=" ", ylab=" ", type='b', col='red', pch=20, axes=FALSE)
axis(side=4, col='dark red', col.axis='red')
legend("topright", c("mape forecast","rmse forecast"), col=c('black','red'), lty=c(1,1))
#La 9 a visiblement la meilleur erreur de prevision a 12%


#On nettoie:
rm(Station.trunc.65)
rm(eq, reg.forecast, reg.station)
rm(Station1,Station2,Station3,Station4,Station5,Station6,Station7,Station8,Station9,Station10,Station11)


#####################################
#Validation du choix de la station par modele GAM

Station.gam  <- c()
gam.station  <- list()
gam.forecast <- list()
eq           <- list()

for (j in c(1:NStation)){
  Station.gam[j]    <- paste("Station",j,sep="")
  eq[[j]]           <- as.formula(paste("Zone2~Time+s(",Station.gam[j],",k=10, bs='cr')",sep=""))
  gam.station[[j]]  <- gam(eq[[j]], data=data0a)
  gam.forecast[[j]] <- predict(gam.station[[j]], data0b)  
}

R.sq <- lapply(gam.station, function(x){summary(x)$r.sq})%>%unlist
plot(R.sq,type='b',pch=20,main=paste("Adjusted rsquared error"))
#la 9 semble etre toujours la meilleure

#Erreur d'ajustement :
fit.map <- lapply(gam.station, function(x){mape(data0a$Zone2,x$fitted.values)})%>%unlist
fit.r   <- lapply(gam.station, function(x){rmse(data0a$Zone2 - x$fitted.values)})%>%unlist
plot(fit.map, xlab="Station", ylab="mape_error", type='b', pch=20, main=paste("ERREUR DE FITTAGE"))
par(new=TRUE)
plot(fit.r, xlab="Station", type='b', pch=20, col='red', axes=FALSE)
axis(side=4, col='dark red', col.axis='red', ylab="rmse_error")
legend("topright", c("mape fit","rmse fit"), col=c('black','red'), lty=c(1,1))
#les stations : 3, 5, 9 et 11 sont les meilleurs


#Erreur de prevision:
forecast.map <- lapply(gam.forecast, function(x){mape(data0b$Zone2,x)})%>%unlist
forecast.r   <- lapply(gam.forecast, function(x){rmse(data0b$Zone2 - x)})%>%unlist

plot(forecast.map, xlab='Station', ylab=" ", type='b', pch=20, main="ERREUR DE PREVISION")
par(new=TRUE)
plot(forecast.r, xlab=" ", ylab=" ", type='b', col='red', pch=20, axes=FALSE)
axis(side=4, col='dark red', col.axis='red')
legend("topright", c("mape forecast","rmse forecast"), col=c('black','red'), lty=c(1,1))
#seules la 9 et la 11 se demarquent des autres 

opt <- which.min(forecast.map) #On garde la 9

forecast.map[opt] #11.93, on gagne 0.1% sur le coefficient R2

formule.gam <- paste("s(Station",opt,",k=10, bs='cr')", sep='')

rm(eq, fit.map, fit.r, forecast.map, forecast.r, gam.forecast, gam.station, Station.gam, R.sq)


#Nous avons egalement fait un modele polynomiale (dans un autre fichier R) mais 
#le modele est tres mauvais



#####################################
###REGRESSION LINEAIRE SUR LES COUPLES DE STATIONS

#Dans cette partie, il faut faire attention a optimiser 
#nos commandes pour ne pas surcharger la memoire:

Station      <- matrix(nrow = NStation, ncol = NStation)
reg.station  <-list()
reg.forecast <-list()
eq           <-list()


for (i in c(1:NStation)){
  for(j in c(1:NStation)){
      Station[i,j]       <- paste("s(Station",i,",k=10, bs='cr')+s(Station",j,",k=10, bs='cr')",sep = "")
  }
}


R.squ           = matrix(ncol=NStation, nrow=NStation)
fit.err         = R.squ
forecast.err    = R.squ
fit.map         = R.squ
forecast.map    = R.squ


for(i in c(1:NStation)){
  for(j in c(1:NStation)){
    eq                   <- as.formula(paste("Zone2~Time+",Station[j,i],sep=""))
    reg.station          <- gam(eq, data=data0a)
    reg.forecast         <- predict(reg.station, data0b)
    R.squ[i,j]           <- summary(reg.station)$r.sq
    fit.err[i,j]         <- rmse(data0a$Zone2 - reg.station$fitted)
    forecast.err[i,j]    <- rmse(data0b$Zone2 - reg.forecast)
    fit.map[i,j]         <- mape(data0a$Zone2, reg.station$fitted )
    forecast.map[i,j]    <- mape(data0b$Zone2, reg.forecast)
  }
} 

R.squ
which.max(R.squ)    #55 ie 5*11 ou 11*5 ie le couple (5,11)
R.squ[5,11]         #0.49 pour la station 9 contre la 0.54 pour le couple (5,11)
fit.err             #min a 22608 pour (5,11)
fit.map             #min a 11.71 pour la (5,11)
forecast.err        #min a 23897 pour le couple (5,11)
forecast.map        #min a 11.61 pour le couple (5,11)


opt <- which.min(forecast.map)

formule.gam.double <- "s(Station5, k=10, bs='cr')+s(Station11, k=10, bs='cr')"

#on gagne 0.3% de prevision par rapport au modele a une station
#de plus on remarque que la station 9 n'est pas dans le couple selectionne
#on utilisera le couple de station (5,11) dans la suite du projet

rm(fit.err, fit.map, forecast.err, forecast.map, Station, reg.forecast, reg.station, R.squ, eq, opt)

#Nous avons egalement suivi la meme demarche pour trouver un triplet de station
#R2 ameliore de 0.4%, on decide de garder le couple (5,11).




#####################################
##RECHERCHE D'UNE SAISONNALITE
##SERIE DE FOURIER
#pulsation
W         <- 2*pi/(24*365)  #saisonnalite annuelle
Nfourier  <- 50
cos       <- c()
sin       <- c()


names <- names(data)

#Creation cos et sin
for(i in c(1:Nfourier)){
  cos[i]  <- paste('cos',i,sep="")
  data    <- data.frame(data,cos(W*data$Time*i))
}

for(i in c(1:Nfourier)){
  sin[i]  <- paste('sin',i,sep='')
  data    <- data.frame(data,sin(W*data$Time*i))
}


names(data) <- c(names, cos, sin)

#Variables cos et sin dans data donc on actualise
data0a <- data[1:sep,]
data0b <- data[-c(1:sep),]


reg.fourier          <- list()
eq                   <- list()
reg.fourier.forecast <- list()
fourier              <- c()


fourier[1] <- paste("Time+",formule.gam.double,'+',cos[1],"+",sin[1], sep='')
#fourier[1] <- paste("Time+Station9+Station9.trunc.65+",cos[1],"+",sin[1], sep='')

for(i in 2:Nfourier){
  fourier[i] <- paste(fourier[i-1],"+",cos[i],"+",sin[i], sep='')
}

for(i in c(1:Nfourier)){
  eq[[i]] <- as.formula(paste("Zone2~", fourier[i], sep = "")); 
  reg.fourier[[i]]          <- gam(eq[[i]], data = data0a)
  reg.fourier.forecast[[i]] <- predict(reg.fourier[[i]], data0b)
}

fit.map      <- fit.mape(data=reg.fourier)
forecast.map <- forecast.mape(data=reg.fourier.forecast)

plot(fit.map, type='l', xlab='harmoniques de Fourier', ylab="taux d'erreur", 
     main="Mise en evidence de l'harmonique optimale a la prevision", ylim=c(fit.map[50]-0.1,fit.map[1]+0.05))
lines(forecast.map, type='l', col='red')
axis(side=4, col='dark red', col.axis='red')
fmin <- which.min(forecast.map)
abline(v=fmin)
points(x=fmin, y=forecast.map[fmin], pch='+', col='red', cex=2)
text(x=fmin, y=forecast.map[fmin]-0.05,labels=forecast.map[fmin]%>%round(digits=3))
legend("topright", c('erreur de fitting', 'erreur de prevision'), col=c('black', 'red'), lty=c(1,1))
abline(h=forecast.map[fmin], col='red', lty=2)

#c'est la 6eme harmonique de Fourier qui nous donne la meilleure erreur de prevision de:
forecast.map[fmin]  #11%

Foulm <- reg.fourier[[fmin]]
Foulm
reg.fourier[[6]]

#On a deja reussi a ameliorer notre modele.
lt <- length(data)
names(data)[151]
names(data)[132]
names(data)[101]
names(data)[82]
data <- data[-c(132:151)]
data <- data[-c(82:101)]

data0a <- data[1:sep,]
data0b <- data[-c(1:sep),]

formule.fourier  <- fourier[fmin]


plot(data0a$date, data0a$Zone2, type='l', main="Superposition de la consommation reelle et du modele de prevision \n 
     sur les donnees d'apprentissage", xlab='Time', ylab='Consommation')
lines(data0a$date,Foulm$fitted.values, col='red') 
legend("bottomright", c("donnees d'apprentissage","modele"), col=c('black','red'), lty=c(1,1))
#bon ajustement, meme si on attrape pas les valeurs extremes (surtout les valeurs basses)
#on ne prend pas encore compte des variations journalieres

plot(data0b$date, data0b$Zone2, type='l', main="Superposition des donnees test au modele de prevision", xlab="date")
lines(data0b$date, predict(Foulm, data0b), col='red')
legend("topright", c("donnees test","modele"), col=c('black','red'), lty=c(1,1))
#On ajuste mal les valeurs extremes, il nous faudrait prendre en compte
#l'influence de la consommation horaire
#on sous estime egalement les valeurs en debut d'annee, notament celle du choc de fevrier

rm(fit.map, forecast.map, reg.fourier, reg.fourier.forecast, eq, fourier, names, W)


plot(data0a$Station9, data0a$Zone2 - Foulm$fitted, pch=20)
#on ne distingue ni tendance ni saisonnalite 
#il semble qu'on a extrait une bonne partie de l'information donnee par les temperatures

plot(data0b$Station9, data0b$Zone2 - predict(Foulm, data0b), pch=20)
#alors qu'ici une forme se degage pour les valeurs tres basse < 20 F et hautes >70 F





#####################################
##ON COMPLETE LE MODELE


#Rappel : formule.gam.double = couple de stations (5,11)
eq        <- as.formula(paste("Zone2~Time + s(Hour, by=daytype) + s(Toy, bs='cc')+", 
                              formule.gam.double, sep=""))
#eq        <- as.formula(paste("Zone2~ s(Hour, by=daytype) + s(Toy, bs='cc')+", formule.fourier, sep=""))
#+ Station9 + Station9.trunc.65 

#Modele gam
reg       <- gam(eq, data=data0a)
predict   <- predict(reg, data0b)

summary(reg)


R              <- summary(reg)$r.sq                  #0.82  net amelioration!   0.816 sans la station 9
fit.er         <- rmse(data0a$Zone2 - reg$fitted)    #14293
forecast.er    <- rmse(data0b$Zone2 - predict)       #16361
fit.map        <- mape(data0a$Zone2, reg$fitted )    #6.94
forecast.map   <- mape(data0b$Zone2, predict)        #7.35  


#Ajustement:
plot(data0a$date, data0a$Zone2, type='l')
lines(data0a$date, reg$fitted.values, col='red')

#Prediction:
plot(data0b$date, data0b$Zone2, type='l')
lines(data0b$date, predict, col='red')
#on pourrait predire mieux les valeurs extremes


plot(data0b$Zone2, predict)
abline(a=0, b=1, col='red')
#ca se superpose pas tout a fait a la droite y=x
#on a un decalage pour les valeurs basses et hautes, on a une dispersion sur les faibles consommations

plot(reg, select=1, shade=TRUE)
gam.check(reg, old.style = F)

gam.check(reg)

#Nettoyage
rm(fit.er, fit.map, forecast.er, forecast.map, predict, reg)




#######################################
#### PREVISION A HORIZON 1 SEMAINE ####
#######################################

h=sep-24*365

p=7*24

reg       <- gam(eq, data=data0a[h:sep,])
predict   <- predict(reg, data0b[1:p,])

summary(reg)

R              <- summary(reg)$r.sq                        #0.83  
fit.er         <- rmse(data0a[h:sep,]$Zone2 - reg$fitted)  #13220
forecast.er    <- rmse(data0b[1:p,]$Zone2 - predict)       #12936
fit.map        <- mape(data0a[h:sep,]$Zone2, reg$fitted )  #6.25
forecast.map   <- mape(data0b[1:p,]$Zone2, predict)        #7.09 

#petite amelioration

#Ajustement:
plot(data0a[h:sep,]$date, data0a[h:sep,]$Zone2, type='l')
lines(data0a[h:sep,]$date, reg$fitted.values, col='red')

#Prediction:
plot(data0b[1:p,]$date, data0b[1:p,]$Zone2, type='l')
lines(data0b[1:p,]$date, predict, col='red')



#######################################
### TEST DU MODELE AVEC DATA_ELEC01 ###
#######################################


newdata <- read.table("data_elec1.txt", header=T, sep='\t')
nw      <- length(newdata[,1])

###Conversion de la premiere colonne en type "date":
Date = as.POSIXct(strptime(newdata$date,"%Y-%m-%d %H:%M:%S"))
newdata$date <- Date
#Nettoyage
rm(Date)

###Modele gam
reg       <- gam(eq, data=data)
predict   <- predict(reg, newdata)

summary(reg)

R              <- summary(reg)$r.sq                  #0.82  (pas d'amelioration)
fit.er         <- rmse(data$Zone2 - reg$fitted)      #14762
forecast.er    <- rmse(newdata$Zone2 - predict)      #19489
fit.map        <- mape(data$Zone2, reg$fitted )      #7.01
forecast.map   <- mape(newdata$Zone2, predict)       #8.69 


#Ajustement:
plot(data$date, data$Zone2, type='l')
lines(data$date, reg$fitted.values, col='red')

#Prediction:
plot(newdata$date, newdata$Zone2, type='l', main="donnees relles de 2008 par rapport au modele de prevision",
     xlab='dates', ylab='consommation')
lines(newdata$date, predict, col='red')
legend("bottomleft", c("consommation relle", "modele"), lty=c(1,1), col=c("black", "red"))
#on est decale vers le bas par rapport a la consommation reelle
#la tendance est sous estimee

plot(reg, select=1, shade=TRUE)
gam.check(reg, old.style = F)


plot(newdata$Zone2, predict)
abline(a=0, b=1, col='red')
#On voit bien ici qu'on sous-estime la consommation réelle en 2008.






######################################
## ETUDE DES RESIDUS: MODELE SARIMA ##
######################################

#On reprend le modele avec data0a et data0b:

reg          <- gam(eq, data=data0a)
predict      <- predict(reg, data0b)

head(reg$residuals)

length(c(reg$fit, predict))
data$residus <- data$Zone2 - c(reg$fit, predict)

data0a <- data[1:sep,]
data0b <- data[-c(1:sep),]

plot(data0b$Time, data0b$residus, type='l')

#On voit de la saisonnalite et une tendance
#Il reste de l'information non exploitee
#On n'a pas un bruit blanc
#On va reduire le nombre d'observations pour que le modele tourne sur nos ordinateurs

#Séparation de data0b (2007) en deux parties
sep2        <- length(data0b[,1])/2
res.detude  <- data0b$residus[1:sep2] #donnees d'apprentissage

acf(res.detude)
pacf(res.detude)
#On retrouve de la saisonnalite, on va donc differentier nos residus 
#Pour enlever une des saisonnalites, on a pris deux lag : 24 et 7*24

diff.hebdo <- diff(res.detude, lag = 7*24)
diff.24    <- diff(res.detude, lag = 24)

donnee <- diff.hebdo
#donnee <- diff.24

plot(donnee, type='l')

acf(donnee)
pacf(donnee)

acf(donnee, lag.max = 10*7*24)
pacf(donnee, lag.max = 7*24*10)

###Interressons nous a diff.hebdo :
#Estimation des parametres du modele
acf(donnee, lag.max = 80)
abline(v=70)
q=16 #pas bon theoriquement mais a prendre en pratique
Q=1  

pacf(donnee)
p=4
pacf(donnee, lag.max = 7*24*10)
P=5  

#Ici d=0 car on a deja differentie une fois nos residus!
# D=1 pour differentier la saisonnalite
d=0
D=1

#model.arima <- arima(donnee, order = c(p,d,q), seasonal = list(order=c(P,D,Q), period=7*24),method='CSS')
#Ce modele arima ne fonctionne pas car il y a trop de parametres!




######################################
###### MODELE SARIMA PAR HEURES ######
######################################

res.h  <- subset(data0b, Hour == 1, select = 112)
test   <- rep(x = 0, length(res.h[,1]))
res.h  <- data.frame(res.h, rep(test, 23))
res.h  <- data.frame(res.h, test, test, test, test, test, test, test, test, test, test, test,
                     test, test, test, test, test, test, test, test, test, test, test, test)

#Boucle sur les heures
for(i in 2:24){
  res.h[,i]  <- subset(data0b, Hour == i, select = 112)
}

names(res.h)  <- 1:24

###Estimation des parametres du modele : 
d=1 #on differentie une fois
D=0 #plus de saisonnalite dans le modele par heure (but)

#Pour tous les autres parametres, on va regarder l'heure 15
#Meme si les coefficients de consommation sont differents selon les heures de la journee,
#le schema reste le meme (+ ou - un pour chaque parametre)

#L'heure 15 :
acf(diff(res.h$'15', lag=7), lag.max = 7*10)
q=2
Q=4

pacf(diff(res.h$'15', lag=7), lag.max = 7*10)
p=1
P=4 

####Construction des 24 modeles sarima
model.h  <- list()
for(i in 1:24){
  model.h[[i]] <- arima(res.h[,i], order = c(p,d,q), seasonal = list(order=c(P,D,Q), period=7),method='CSS')
}

#Regardons le summary du premier modele pour voir si ca fonctionne correctement
summary(model.h[[1]])
summary(model.h[[1]]$residuals) #ok

#Prediction avec le modele 1
predict(model.h[[1]],n.ahead=7)
#Le modele predit 600 avec une erreur de 10 000, il est donc inexploitable.
#Il y a beaucoup de valeurs extremes dans nos donnees et le regroupement par heure
#est beaucoup trop sensible a ces phenomenes


#Pour resoudre ce probleme on peut essayer de filtrer les jours atypiques : 

test  <- hist(res.detude, plot = F)

h = 7*24
noyau <- ksmooth(1:(2*sep2), data0b$residus, kernel=c("normal"), bandwidth=h)  #calcul du noyau, 20s
plot(noyau$y, type='l') 

plot(data0b$residus,type='l')
lines(noyau$y, col='red')

mean2 <- mean(abs(data0b$residus-noyau$y))

#On veut bloquer les residus entre le noyau +/- moyenne
residus.centre <- rep(0, 2*sep2)

###Representation
test <- pmin(data0b$residus, mean2 + noyau$y)

plot(test, type='l')

test2 <- pmax(test, noyau$y - mean2)

plot(test2, type='l')
lines(noyau$y, col='red', lwd=2)

plot(diff(test2), type='l')

#On recommence le modele de prediction

data0b$residus.filtre <- test2
res.h                 <- subset(data0b, Hour == 1, select = 113)
test                  <- rep(x = 0, length(res.h[,1]))
res.h                 <- data.frame(res.h, test, test, test, test, test, test, test, test, test, test, test,
                                    test, test, test, test, test, test, test, test, test, test, test, test)

for(i in 2:24){
  res.h[,i]  <- subset(data0b, Hour == i, select = 113)
}

names(res.h)  <- 1:24

### Estimation des parametres
d=1
D=0

plot(diff(res.h$'15', lag=7), type='l')
h = 7*24
noyau <- ksmooth(1:(2*sep2), diff(res.h$'15', lag=7), kernel=c("normal"), bandwidth=h)  #calcul du noyau, 20s
plot(noyau$y, type='l')
mm <- which.min(noyau$y[1:200])
abline(v=mm)
#On enleve les premieres valeurs aberrantes

###Tendance:
coeff.reg.res <- lm(noyau$y[mm:(2*sep2)]~data0b$Time[mm:(2*sep2)])$coeff
lines(1:(2*sep2), data0b$Time*coeff.reg.res[2] + coeff.reg.res[1], col='red')
#On a reussi a enlever toute la tendance de notre modele

eq.test  <- as.formula(residus~sin1+sin2+sin3+sin4+sin5+sin6+cos1+cos2+cos3+cos4+cos5+cos6+s(Hour, by=daytype)+Time+s(Station5, k=10, bs='cr')+s(Station11, k=10, bs='cr'))
reg.test <- gam(eq.test, data=data0b)
summary(reg.test)

plot(data0b$residus, type='l')
lines(1:(2*sep2), reg.test$fitted.values, col='red')

###Estimation des parametres pour 15h
acf(diff(res.h$'15', lag=7), lag.max = 7*10)
q=10
Q=10

pacf(diff(res.h$'15', lag=7), lag.max = 7*10)
p=8
P=5 

### Construction des 24 modeles
model.h  <- list()
for(i in 1:1){
  model.h[[i]] <- arima(res.h[,i], order = c(p,d,q), seasonal = list(order=c(P,D,Q), period=7),method='CSS')
}

summary(model.h[[1]])

summary(model.h[[1]]$residuals)

predict(model.h[[1]],n.ahead=7)
#Nous n'avons pas reussi a ameliorer notre modele.

