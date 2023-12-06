# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Run all the exercise 1 scripts

### Libraries ###
library(tidyverse)
library(knitr)

# Chemin vers le dossier contenant les scripts R
chemin_dossier <- "SCRIPTS"

# Liste des fichiers dans le dossier, triés par ordre alphabétique
scripts <- list.files(chemin_dossier, pattern = "\\.R$", full.names = TRUE)
#Run all the scripts
lapply(scripts, source, local = TRUE)