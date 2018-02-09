#############################################
# Test premier traitement sur les données : #
#############################################

# 0 - Préparation de l'environnement :
######################################

rm(list= objects())

library(magrittr)

# Chargement des deux fichiers de données qui nous intéresse pour le moment 

load("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Package/DataFinal2014.RData")
data_2014 <- DataFinal

load("~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Package/DataFinal2015.RData")
data_2015 <- DataFinal

rm(DataFinal)

#-------------------------------------------------------------------------------------------------------

# I - Cours 1 : Importation, manipulation et nettoyage des données :
####################################################################

# Obtenir le type des variables de la data frame
str(data_2014) ## même chose pour 2015

# Stat de base
summary(data_2014)

# Sélection sur les lignes  
test_selec <- filter(data_2014, diff >= 150) ## diff plus grand que 150, 24 occurences
test_select_1 <- filter(data_2014, diff >= 150, dow == 4) ## diff plus grand que 150 : jamais le w-e,
## ou les jours fériés - différentiel important lorsqu'on est en milieu de semaine
## (plus le mardi-5/mercredi-10/jeudi-6 que le lundi-0/vendredi-3)

test_selec_2015 <- filter(data_2015, diff >= 150) ## diff plus grand que 150, 93 occurences
test_select_1_2015 <- filter(data_2015, diff >= 150, dow == 8) ## diff plus grand que 150 : jamais le w-e,
## ou les jours fériés - différentiel important lorsqu'on est en milieu de semaine
## (plus le mardi-22/mercredi-21/jeudi-21 que le lundi-14/vendredi-15)

## Quelques stats 2014 : 2 400 000 trajets | 300 stations
## Quelques stats 2015 : 3 183 439 trajets | 474 stations

rm(test_selec, test_selec_2015, test_select_1, test_select_1_2015) ## Nettoyage

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
NumDistricts <- sort(as.matrix(distinct(data_all[,2])))

col <- c('dark red', 'red', 'salmon', 'orange', 'magenta' ,'pink', 'yellow3', 'yellow', 'yellowgreen', 'green',
       'aquamarine1', 'azure', 'blue', 'royalblue', 'purple', 'dark blue', 'grey', 'dark green',
       'black','white')

sel <- which(data_all$district == NumDistricts[1])
o <- order(data_all$Time[sel])

plot(data_all$Time[sel[o]], data_all$diff[sel[o]], col=col[1],type='l', xlab='Date', ylab='Diff')

for(i in c(2:length(NumDistricts))){
  sel <- which(data_all$district == NumDistricts[i])
  print(NumDistricts[i])
  lines(data_all$Time[sel[o]], data_all$diff[sel[o]], col=col[i], cex=0.5) 
}