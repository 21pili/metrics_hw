# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Run all the exercise 1 scripts

### Libraries ###
library(tidyverse)
library(knitr)

# Path to R scripts
chemin_dossier <- "Exercise_1/SCRIPTS"

# R files list
scripts <- list.files(chemin_dossier, pattern = "\\.R$", full.names = TRUE)

#Run all the scripts
lapply(scripts, source, local = TRUE)
