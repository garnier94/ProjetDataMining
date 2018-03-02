########### Projet R ###########

# kaggle.com : compet de data science
# modele de prevision, demarche de selection, validation, benchmarks, implementation efficace
# 10 pages sur R Markdown
# modeles additifs non lineaire BAM, visualisation de donnees, open data, methodes de validation (croisée 
# V-fold, boostrap, simulation de prévision)
# initiative et recherche internet ! (données micro, rayonnement, meteo, super Bowl aux USA)
# package caret et mgcv

# Données : 
# Toy = temps qui s'écoule en une année
# Daytype = jours fériés ou non
# Zone : conso en kilo watt
# Station météo degres F (conversition package R)
# séparateur : tabulation (read.table)
# conso dependant de la journée (bureau) et de la temp (clim ou chauffage)

rm(list=objects()) #on nettoie l'environnement
graphics.off() #nettoyer les graphiques 

#packages
library(timeDate)
library(zoo)
library(xts)
library(magrittr)

# Acquision des données

data = read.table("data_elec0.txt",header = TRUE,sep='\t',dec='.') 

#convertir la colonne Date en type date
typeof(data[1,1])
Date = as.POSIXct(strptime(data$date,"%Y-%m-%d %H:%M:%S"))
plot(Date, pch='.')
plot(diff(Date)) #sauts : dates bien implementees
data$date <- Date
typeof(Date)

summary(data)
plot(data$date,data$Zone2,pch='.')
pairs(data[,]) #trop large

#consommation par mois
month <- as.factor(format(data$date,"%m"))
monthlyconso <-tapply(data$Zone2,month,mean)
plot(monthlyconso,type='b',pch=20, main='Moyenne de la série par mois')
#On élimine la 4, 9

#choix de la zone 2
plot(data$Zone2, type='l')

#Consommation en fonction de la temperature
plot(data$Station1,data$Zone2)
plot(data$Station1^2,data$Zone2^2)
#regression lineaire - mettre I() !!!!
out <- lm(data$Zone2^2~data$Station1^2+data$Station2^2+data$Station3^2+data$Station4^2+data$Station5^2+data$Station6^2+data$Station7^2+data$Station8^2+data$Station9^2+data$Station10^2+data$Station11^2 , data=data)
summary(out)
anova(out) #selection de variable

cor(data[,29:39]) #temperatures correlees car les points sont proches, inversion de X'X = modele instable car vp proches de 0
#probleme de surapprentissage
#On peut en faire la moyenne ou n'en garder qu'un

#faire aussi la conso en fonction du temps. Faire un modele de regression par jours, mois ou heures 

#heuristique de Mallows ! on rajoute au risque empirique une penalite 1/n ||Y-X*betachap||^2 + 2*sigma^2*(p/n)
#sigma^2 non connu --> var E* avec E* = Y-X*betachap

#validation croisee : n modeles sauf dans la lineaire ou il n'en faut qu'un seul (une seule regression)

#graphe residu en fonction de x1 : on veut des residus centrés, plus de dependance en x sinon 
#informations oubliees. Il faut faire une regression à noyau, moyenne des residus dans une zone locale
#Il faut rajouter un terme au carre dans la regression = on a recup toutes l'infos

#1) Representation
#2) Modeles de regression (date, station)

plot(data$Month, data$Zone2)
out1 <-lm(data$Zone2~data$Month)
summary(out1)

# gam dans mgct


######## Séance 2 #########
##### selection modele regression #####


#erreur relative moyenne

mape<-function(y,ychap){return (round(100*mean(abs(y-ychap)/abs(y)),digits=2))}

#création de série de Fourier :

Data0 = read.table("data_elec0.txt",header = TRUE,sep='\t',dec='.') 

#convertir la colonne Date en type date
typeof(Data0[1,1])
Data0$date <- as.POSIXct(strptime(Data0$date,"%Y-%m-%d %H:%M:%S"))

#pulsation
w<-2*pi/(24*365)
Nfourier<-50
for(i in c(1:Nfourier))
{
  assign(paste("cos",i,sep=""),cos(w*Data0$Time*i))
  assign(paste("sin",i,sep=""),sin(w*Data0$Time*i))
}
  
objects()
plot(cos1,type='l')

cos<-paste('cos',c(1:Nfourier),sep="",collapse=",")
sin<-paste('sin',c(1:Nfourier),sep="",collapse=",")  

Data0<-eval(parse(text=paste("data.frame(Data0,",cos,",",sin,")",sep="")))

###lag

Zone2.24<-Data0$Zone2
Zone2.24[25:nrow(Data0)]<-Data0$Zone2[1:(nrow(Data0)-24)]
Data0<-data.frame(Data0,Zone2.24)
plot(Data0$Station1,Data0$Zone2,pch=".")

### truncated temperatures - base permettant d'estimer toutes les fonctions lineaires
#en V

for(i in c(1:11))
{
  assign(paste("Station",i,".trunc.65",sep=""),pmax(Data0$Station1-65,0))
}

station.trunc<-paste("Station",c(1:11),".trunc.65",collapse=",",sep="")
Data0<-eval(parse(text=paste("data.frame(Data0,",station.trunc,")",sep="")))

### separation des donnees en deux parties (test)

Data0a<-Data0[1:head(which(Data0$Year==2007),1),]
Data0b<-Data0[-c(1:head(which(Data0$Year==2007),1)),]

dim(Data0a)
dim(Data0b)

test<-lm(Zone2~cos1+sin1,data=Data0)
plot(Data0$date,Data0$Zone2,pch='.')
lines(Data0$date,test$fitted.values,col='red')

test<-lm(Zone2~cos1+sin1+cos2+sin2+Station1.trunc.65+Station2,data=Data0)
plot(Data0$date,Data0$Zone2,pch='.')
lines(Data0$date,test$fitted.values,col='red')

####

Nfourier<-30
lm.fourier<-list()
eq<-list()
for(i in c(1:Nfourier))
{
  cos<-paste(c('cos'),c(1:i),sep="")
  sin<-paste(c('sin'),c(1:i),sep="")
  fourier<-paste(c(cos,sin),collapse="+")
  eq[[i]]<-as.formula(paste("Zone2~",fourier,sep=""))
  lm.fourier[[i]]<-lm(eq[[i]],data=Data0a)
}

names(test)

adj.rsquare<-lapply(lm.fourier,function(x){summary(x)$adj.r.squared})%>%unlist
fit.error<-lapply(lm.fourier,function(x){sqrt(mean((Data0a$Zone2-x$fitted)^2))})%>%unlist
fit.mape<-lapply(lm.fourier,function(x){mape(Data0a$Zone2,x$fitted)})%>%unlist

forecast.mape<-lapply(lm.fourier,function(x){mape(Data0b$Zone2,predict(x,newdata=Data0b))})%>%unlist

plot(adj.rsquare,type='b',pch=20)
plot(fit.error,type='b',pch=20)

plot(fit.mape,type='b',pch=20,ylim=range(forecast.mape,fit.mape))
#erreur apprentissage (surapprentissage, plus de valeur = diminution)
lines(forecast.mape,col='red',type='b',pch=20)
#l'erreur de prévision ne tend plus vers zéro-optimum autour de 12/13
abline(v=which.min(forecast.mape),col='red')
#min de l'erreur de prévision-6
##erreur de prévision : methode de selection de modele objective

#données avec bruit, deux parties : apprentissage et test
#serie de Fourier : saisonnalité, pas de bruit que l'on reproduit dans le futur
#mauvais car les oscillations entre la serie et le bruit peuvent être en décalés (2 fois plus d'erreur)
#selection du modele avec la courbe en rouge pour éviter le surapprentissage






