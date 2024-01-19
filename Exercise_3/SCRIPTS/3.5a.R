# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 3.4c
# Run 3 first-stage regressions, each using one of the above in-
# struments separately. Report the 3 sets of results in the same
# table. Which instrument seems to be the strongest?


### Libraries
library(tidyverse)
library(readxl)
library(xtable)
library(stargazer)

### Imports
df <- read_csv("Exercise_3/INTERMEDIATE/crime_rate.csv") #nolint

### Core
# Estimates the linear model of question 3 and 6, using OLS estimator

first_mtw <- lm(business_crea ~ log(pop) + income + com_type + motorway, df) #nolint
first_pty <- lm(business_crea ~ log(pop) + income + com_type + party, df) #nolint
first_txr <- lm(business_crea ~ log(pop) + income + com_type + tax_rate, df) #nolint

### Exports
# Generate a latex table for of the regression
code_latex <- stargazer(first_mtw, first_pty, first_txr,
                        title = "First Stages - Exercise 3", #nolint
                        align = TRUE, type = "latex",
                        label = "first_stages",
                        out = "Exercise_3/OUTPUT/first_stages.tex",
                        font.size = "footnotesize",
                        omit = "F")
