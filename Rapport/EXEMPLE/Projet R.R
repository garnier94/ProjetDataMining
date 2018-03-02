#####################################
##  PROJET R
#####################################

#####################################
##Preparation des donnees

###Preparation de l'environnement de travail
rm(list=objects())
graphics.off()

library("magrittr")
library("timeDate")
library("xts")

setwd("~/Fac/M1/ProjetR - annee2")
###Creation du data file:
data<-read.table("data_elec0.txt", header=T, sep='\t')
n = length(data[,1])

###Conversion de la premiere colonne en type "date":
Date = as.POSIXct(strptime(data$date,"%Y-%m-%d %H:%M:%S"))
plot(Date, pch='.')
plot(diff(Date)) #sauts : dates bien implementees
data$date <- Date

#il y a des trous :( 



###################################################
###fonction RMSE et MAPE
###Calcul d'erreurs

rmse<-function (eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function (y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
}







#######################################
##Choix de la station 2
sd    = sd(data$Zone2)
mean  = mean(data$Zone2)
rm(sd, mean)              #REFLEXE: bien nettoyer son envirronement de travail au fur et a mesure

plot(data$date,data$Zone2, type='l', ylab="", main="consommation electrique de la zone 2")
#a premiere vu on observe une saisonnalite annuelle

boxplot(data$Zone2)  #les valeurs extremes ne sont que superieures
hist(data$Zone2)


zone2.xts<-xts(data$Zone2, order.by = data$date)
hour <- as.factor(.indexhour(zone2.xts))
day  <- as.factor(.indexday(zone2.xts))
week <- as.factor(.indexweek(zone2.xts))
month<- as.factor(.indexmon(zone2.xts))

conso_horaire<-tapply(zone2.xts, hour, mean)
conso_journ<-tapply(zone2.xts, day, mean)
conso_hebdo<-tapply(zone2.xts, week, mean)
conso_mensuelle<-tapply(zone2.xts, month, mean)


###Consomation horaire:
boxplot(data$Zone2 ~ data$Hour,col="lightblue",pch=20,cex=0.5, main="consommation par heure de la zone 2", xlab="Heure")
#on voit que la largeur des boites est toujours a peu pres la meme => ??
#il n'y a que des valeurs extremes superieures concentrees surtout en minuit et 10h
plot(conso_horaire, main="consommation horaire", type="b")


###Conso journaliere moyennee par jour:
plot(conso_journ, type="l", main="conso journaliere")

jour <- as.factor(format(data$date,"%d"))
consoJ <-tapply(data$Zone2,jour,mean)
plot(consoJ,type='b',pch=20, main='Moyenne de la serie par jour')
#l'information est elle pertinente? j'en suis pas sur
#bizarre cette saisonnalite quand meme...


###Conso hebdo:
plot(conso_hebdo, type="l",main="conso hebdo", xlab="nbre de semaine")
#ca permet d'y voir un peu plus clair sur la consommation globalle


###Conso mensuelle
plot(conso_mensuelle, type='l', main="conso mensuelle")
#on voit bien les evolutions de la consommatin durant l'année
boxplot(data$Zone2 ~ data$Month,col="lightblue",pch=20,cex=0.5, main="consommation par mois de la zone 2", xlab="Mois")

#on voit ça en filtrant notre data frame avec Toy
annee1 <- data[1:8784,]
annee2 <- data[8785:16872,]
annee3 <- data[16873:24961,]
annee4 <- data[24962:n,]
  
Annee1.M<-tapply(annee1$Zone2, as.factor(format(annee1$date, "%m")), mean)
Annee2.M<-tapply(annee2$Zone2, as.factor(format(annee2$date, "%m")), mean)
Annee3.M<-tapply(annee3$Zone2, as.factor(format(annee3$date, "%m")), mean)
Annee4.M<-tapply(annee4$Zone2, as.factor(format(annee4$date, "%m")), mean)
plot(1:12, Annee1.M, type='b', lwd=2, ylim=c(140000, 250000), main='consommation moyenne par mois et par annee', xlab='mois', ylab='consommation')
lines(1:12, Annee2.M, col=2, type='b', lwd=2)
lines(1:12, Annee3.M, col=3, type='b', lwd=2)
lines(1:12, Annee4.M, col=4,type='b', lwd=2)
legend("topright", c("Annee 1", "Annee 2", "Annee 3", "Annee 4"), col = c("black", 2, 3, 4), lty = c(1,1,1,1))

#on observe une augmentation progressive de la consommation de l'annee1 a l'annee 4
#avec un choc au mois de fevrier 2007.
#Pour s'en convaincre:
mean(annee1$Zone2)
mean(annee2$Zone2)
mean(annee3$Zone2)
mean(annee4$Zone2)
#on a quand meme une stagnation entre 2005 et 2006 mais la tendance semble etre a la hausse.


#on netoie
rm(hour, day, week, month, conso_hebdo, conso_horaire, conso_journ, conso_mensuelle, jour, consoJ)
rm(Annee1.M, Annee2.M, Annee3.M, Annee4.M)

##Remarque: on voit bien qu'il n'y a pas le meme nombre d'observation suivant les annees => donnees manquantes






#################################
##RECHERCHE D'UNE TENDANCE:

week <- as.factor(.indexweek(zone2.xts))
conso_hebdo<-tapply(zone2.xts, week, mean)
plot(conso_hebdo, type="l",main="conso hebdo")

#remarque:
plot(zone2.xts, type='l', major.format = "%Y-%m-%d") #ne marche pas, encore les trous??

###MOYENNE MOBILE
h=24*7*52   #la fenetre est de 6 mois a droite, 6 mois a gauche
MM<-filter(data$Zone2, filter=array(1/h, dim=h), method=c("convolution"))
plot(data$Time, MM, type='l') 
reg<-lm(MM~data$Time)
summary(reg)
lines(data$Time, reg$coeff[1] + reg$coeff[2]*data$Time, col='red', type='l')
reg$coefficients
#la pente est faible 0.5 mais quand meme!

#il se passe quoi pour une fenetre differente??

h=24*7*26   #la fenetre est de 3 mois a droite, 3 mois a gauche
MM<-filter(data$Zone2, filter=array(1/h, dim=h), method=c("convolution"))
plot(data$Time, MM, type='l') 
reg<-lm(MM~data$Time)
summary(reg)
lines(data$Time, reg$coeff[1] + reg$coeff[2]*data$Time, col='red', type='l')
reg$coefficients
#kiff kiff meme resultat, avec un R adjusted plus faible
#donc, on garde??
rm(h,MM,reg, week, conso_hebdo)
rm(zone2.xts)





##################################################
###CREATION D'UN MODELE DE PREVISION
##################################################

data0a <- data[1:25000,]
data0b <- data[-c(1:25000),]




##################################################
##REGRESSION LINEAIRE SUR LES STATIONS

#mise en place des regression lineaire:
Station     <- c()
lm.station  <- list()
lm.forecast <- list()
eq          <- list()
for (i in c(1:11)){
  Station[i]<-paste('Station',i,sep="")
  eq[[i]]<-as.formula(paste("Zone2~",Station[i],sep=""))
  lm.station[[i]]<-lm(eq[[i]],data=data0a) 
  lm.forecast[[i]]<-predict(lm.station[[i]],data0b)
}


#R Ajuste:
adj.rsquare<-lapply(lm.station,function(x){summary(x)$adj.r.squared})%>%unlist
plot(adj.rsquare,type='b',pch=20,main=paste("Adjusted rsquared error"))  #c'est super faible... pas satisfaisant
#la 5 a le meilleur R
#qui est vraiment tres faible!


#Somme des carres residuel: erreur sur nos donnees et erreur de prevision
fit.error <- lapply(lm.station, function(x){rmse(data0a$Zone2 - x$fitted)})%>%unlist
forecast.error <- lapply(lm.forecast, function(x){rmse(data0b$Zone2 - x)})%>%unlist

plot(fit.error, xlab="Station", ylab="rmse_error", type='b', pch=20, main=paste("SOMME DES CARRES RESIDUELS"))
par(new=TRUE)
plot(forecast.error, xlab=" ", ylab=" ", type='b', col='red', pch=20, axes=FALSE)
legend("bottomright", c("rmse fit","rmse predict"), col=c('black','red'), lty=c(1,1))



#Erreur relative (mape): erreur sur nos donnees et erreur de prevision
fit.mape <- lapply(lm.station, function(x){mape(data0a$Zone2, x$fitted )})%>%unlist
forecast.mape <- lapply(lm.forecast, function(x){mape(data0b$Zone2, x)})%>%unlist
plot(fit.mape, xlab="Station", ylab="mape_error", type='b', pch=20, main=paste("ERREUR DE PREVISION"))
par(new=TRUE)
plot(forecast.mape, xlab=" ", ylab=" ", type='b', col='red', pch=20, axes=FALSE)
legend("bottomright", c("mape fit","mape predict"), col=c('black','red'), lty=c(1,1))
#c'est beaucoup 15% d'erreur, on veut faire mieux

#DANS TOUS LES CAS, LA STATION 5 A QUE C'EST LA MIEUX BABY! A QUE JOHNY POWER LE 5!
#En vrai les ecarts sont pas ouf

#Au lieu de faire une regression lineaire sur une seule station, on va la faire sur un couple de station
rm(Station, eq, lm1.forecast, lm.station, adj.rsquare, fit.error, fit.mape, forecast.error, forecast.mape)

#BUT FIRST, INTERESSONT NOUS AU CAS DU TRUNC.65:





#################################################
##STATION TRONQUEES A 65 FAHRENHEIT
NStation   <- 11
ColStation <- c(29:39)

Station.trunc.65 <- paste('Station', c(1:NStation), ".trunc.65", sep="", collapse=",")
Station          <- paste('Station', c(1:NStation), sep="", collapse=",")

for(i in c(1:NStation)){
  assign(paste("Station",i,".trunc.65",sep=""), pmax(data[,ColStation[i]]-65,0))   
  #on prend data0a car on veut se servir de data0a pour prevoir data0b
  #assign(paste("Station",i,sep=""), data[,ColStation[i]]) pas besoin en fait!
}

#on met les nouvelles donnees dans note data frame:
data<-eval(parse(text=paste("data.frame(data,",Station.trunc.65,")",sep="")))
#on nettoie!
rm(Station1.trunc.65,Station2.trunc.65,Station3.trunc.65,Station4.trunc.65,Station5.trunc.65,
   Station6.trunc.65,Station7.trunc.65,Station8.trunc.65,Station9.trunc.65,Station10.trunc.65,Station11.trunc.65)
#on actualise!
data0a <- data[1:25000,]
data0b <- data[-c(1:25000),]

#et c'est reparti, comme en 40, oui ma bonne dame!
lm.station  <- list()
lm.forecast <- list()
eq          <- list()

for (j in c(1:NStation)){
  Station[j] <- paste("Station",j,"+Station",j,".trunc.65",sep="")
  eq[[j]]    <- as.formula(paste("Zone2~",Station[j],sep=""))
  lm.station[[j]]  <- lm(eq[[j]], data=data0a)
  lm.forecast[[j]] <- predict(lm.station[[j]], data0b)   #la ca coince...  MERDE!!!
}


#R Ajuste:
adj.rsquare<-lapply(lm.station,function(x){summary(x)$adj.r.squared})%>%unlist
plot(adj.rsquare,type='b',pch=20,main=paste("Adjusted rsquared error"))  #pas loin des 0.5, ca monte!
#la 9 a le meilleur R
#qui est beaucoup mieux qu'avant, ohoo!


#Somme des carres residuel: erreur sur nos donnees et erreur de prevision
fit.error <- lapply(lm.station, function(x){rmse(data0a$Zone2 - x$fitted)})%>%unlist
forecast.error <- lapply(lm.forecast, function(x){rmse(data0b$Zone2 - x)})%>%unlist

plot(fit.error, xlab="Station", ylab="rmse_error", type='b', pch=20, main=paste("SOMME DES CARRES RESIDUELS"))
par(new=TRUE)
plot(forecast.error, xlab=" ", ylab=" ", type='b', col='red', pch=20, axes=FALSE)
legend("topright", c("rmse fit","rmse predict"), col=c('black','red'), lty=c(1,1))
#la 9 toujours!


#Erreur relative (mape): erreur sur nos donnees et erreur de prevision
fit.mape <- lapply(lm.station, function(x){mape(data0a$Zone2, x$fitted )})%>%unlist
forecast.mape <- lapply(lm.forecast, function(x){mape(data0b$Zone2, x)})%>%unlist
plot(fit.mape, xlab="Station", ylab="mape_error", type='b', pch=20, main=paste("ERREUR DE PREVISION"))
par(new=TRUE)
plot(forecast.mape, xlab=" ", ylab=" ", type='b', col='red', pch=20, axes=FALSE)
legend("topright", c("mape fit","mape predict"), col=c('black','red'), lty=c(1,1))

 #Meilleure station: 9 bien joue! TADAAAA!


#Et on netoie:
rm(Station, Station.trunc.65)
rm(adj.rsquare, eq, fit.error, fit.mape, forecast.error, forecast.mape, lm.forecast, lm.station)
#rm(Station1,Station2,Station3,Station4,Station5,Station6,Station7,Station8,Station9,Station10,Station11)

#Maintenant, on cherche a estimer la saisonnalite avec Fourier:







###########################################
##RECHERCHE D'UNE SAISONNALITE
##SERIE DE FOURIER
#pulsation
W<-2*pi/(24*365)
Nfourier<-50
for(i in c(1:Nfourier)){
  assign(paste("cos",i,sep=""),cos(W*data$Time*i))
  assign(paste("sin",i,sep=""),sin(W*data$Time*i))
}

cos <- paste('cos',c(1:Nfourier), sep = "", collapse = ","); 
sin <- paste('sin',c(1:Nfourier), sep = "", collapse = ",");
data <- eval(parse(text = paste("data.frame(data,",cos,",",sin,")",sep = "")));
data0a <- data[1:25000,]
data0b <- data[-c(1:25000),]


start <- Sys.time ()

lm.fourier          <- list()
eq                  <- list()
lm.fourier.forecast <- list()

for(i in c(1:Nfourier)){
  cos     <- paste(c('cos'), c(1:i), sep = "")
  sin     <- paste(c('sin'), c(1:i), sep = "")
  fourier <- paste(c(cos,sin), collapse = "+")
  #on avait trouve la station 9!
  eq[[i]] <- as.formula(paste("Zone2~ Station9 + Station9.trunc.65 +", fourier, sep = "")); 
  lm.fourier[[i]]          <- lm(eq[[i]], data = data0a)
  lm.fourier.forecast[[i]] <- predict(lm.fourier[[i]], data0b)
}

temps.calcul <- Sys.time () - start

fit.mape      <- lapply(lm.fourier,function(x){mape(data0a$Zone2,x$fitted)}) %>% unlist
forecast.mape <- lapply(lm.fourier.forecast, function(x){mape(data0b$Zone2, x)}) %>% unlist

plot(fit.mape, type='l')
lines(forecast.mape, type='l', col='red')
abline(v=6)
#c'est la 6eme harmonique de Fourier qui nous donne la meilleur erreur de PREVISION de:
forecast.mape[6]  #11.49!

#on peut donc nettoyer tout ca:
rm(cos, sin, fit.mape, forecast.mape, temps.calcul, lm.fourier, lm.fourier.forecast, eq, fourier, start)

#et trouver une putain de façon de nettoyer tous ces putains de cos et de sin qui servent a rien





##################################################
##REGRESSION LINEAIRE SUR LES COUPLES DE STATIONS

#dans cette partie, il faut faire attention a optimiser nos commandes pour ne pas surcharger la memoire:

#A toi de jouer!
Station <- list()
lm.station <-list()
lm.forecast <-list()
eq <-list()
for (i in c(1:11)){
  for(j in c(1:11)){
    
  }
}

for (j in c(1:NStation)){
  Station[j] <- paste("Station",j,"+Station",j,".trunc.65",sep="")
  eq[[j]]    <- as.formula(paste("Zone2~",Station[j],sep=""))
  lm.station[[j]]  <- lm(eq[[j]], data=data0a)
  lm.forecast[[j]] <- predict(lm.station[[j]], data0b)   #la ca coince...  MERDE!!!
}







###################################
###AUTO CORRELATION
acf(data$Zone2) #correlation par cycle de 24h tres visible
acf(data$Zone2, lag.max = 10*24) #on remarque egalement une correlation hebdommadaire
acf(data$Zone2, lag.max = 10*24, plot=FALSE)[24]
acf(data$Zone2, lag.max = 10*24, plot=FALSE)[7*24]

pacf(data$Zone2, lag.max=10*24)
#so...
pacf(data$Zone2, lag.max = 10*24, plot=FALSE)[24]
pacf(data$Zone2, lag.max = 10*24, plot=FALSE)[7*24]


plot(lag(data$Zone2[25:n],24),data$Zone2[1:(n-24)], pch="x")             #graphe des auto correlation journaliere
plot(lag(data$Zone2[(24*7+1):n],24*7),data$Zone2[1:(n-24*7)], pch="x")   #graphe des auto correlation hebdomadaire

#QU'EN DIRE???

CorrJ.lm <- lm(Zone2[1:(n-24)]~ lag(Zone2[25:n],24), data=data)
CorrJ.lm
summary(CorrJ.lm)  
#c'est pas ouf comme resultat


CorrH.lm <- lm(Zone2[1:(n-24*7)]~lag(Zone2[(24*7+1):n],7*24), data=data)
CorrH.lm
summary(CorrH.lm)
#encore moins ouf

#si les donnees de la veille peuvent nous permettre de prevoir la consommation du lendemain
#ce n'est pas avec une vieille regression lineaire que ca va le faire...

rm(CorrH.lm, CorrJ.lm)






#le reste c'est du blabla pour l'instant



###################################
##SPLITTAGE DES DONNEES
data0a<-data[1:head(which(data$Year==2007),1),]
data0b<-data[-c(1:head(which(data$Year==2007),1)),]








#REMARQUE: trou entre la ligne 10320 et 10321 de 169 mesures
#####################################
##REMPLISSAGE DU PREMIER TROU

DA<-data[1:10320,]
DB<-data[-(1:10320),]
plot(DA$date, DA$Zone2, type='l')
plot(DB$date, DB$Zone2, type='l')

coupeA.xts<-xts(DA$Zone2, order.by = DA$date)
plot(coupeA.xts)  #pourquoi ca veut pas plot???!!!!! comment ca 1 observation??








zone2.zoo = zoo(data$Zone2,order.by = data$date)
###Consommation par periode:
plot(zone2.zoo[1:7*24],type="l", las=1, main="Zone2 sur la 1ere semaine", ylab="", xlab="Time")
plot(zone2.zoo[1:30*24],type="l", las=2, main="Zone2 sur le 1er mois", ylab="", xlab="Time")
plot(zone2.zoo[1:365*24],type="l", las=2, main="Zone2 sur la 1ere annee", ylab="", xlab="Time")
plot(zone2.zoo[(1+365*24):2*365*24], las=2, main="Zone2 sur la 2eme annee", ylab="", xlab="Time")
#marche, il doit y avoir un trou dans les donnees
rm(zone2.zoo)


###Avec Dygraphs
library(dygraphs)
Station1.xts  = xts(data$Station1, order.by = Date)
zone2.xts.sd  = zone2.xts/sd(zone2.xts)
Station1.xts.sd = Station1.xts/sd(Station1.xts)
time.series   = cbind(zone2.xts.sd, Station1.xts.sd)   #marche pas... encore a cause des trous?
dygraph(time.series)

rm(Station1.xts, Station1.xts.sd, zone2.xts.sd)




