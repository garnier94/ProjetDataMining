## Ce fichier regroupe les commandes nécessaires à l'installation du Package DivvyBikeProject
library(devtools)
library(roxygen2)

#Changer le path pour l'endroit ou se trouve le package
setwd("~/StatML/Projet/ProjetDataMining/Package/")

document()
build(,quiet = T)
install()
