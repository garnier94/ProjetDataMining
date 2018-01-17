#####################################
# Test sur la recherche de profil : #
#####################################

rm(list=objects())
graphics.off()

# Working directory
setwd("~/Documents/Orsay/M2/Data Mining/ProjetDataMining/Données")

# Base de données 2013
Trips2013 <- read.csv("Divvy_Stations_2013.csv")

# J'enleve les variables qualitatives et les variables au format date
Trips2013 <- Trips2013[,-c(1,2,5,6,7)]

# Test PCA : Analyse composantes principales

test1_pca <- prcomp(Trips2013, scale=TRUE)
summary(test1_pca)
print(test1_pca)


plot(test1_pca, type = "l")

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(test1_pca, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

# rajouter moyenne des trajets par jours

##################################################################

Trips.stand <- scale(Trips2013)  # To standarize the variables

# K-Means
k.means.fit <- kmeans(Trips.stand, 20)

#library(cluster)
clusplot(Trips.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

