# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 3.4c
# Discuss the validity of all potential instruments with
# Thoroughly discuss the validity of the IV candidates
# regarding the exclusion restriction.


### Libraries
library(tidyverse)
library(readxl)
library(xtable)
library(stargazer)

### Imports
df <- read_csv("Exercise_3/INTERMEDIATE/crime_rate.csv") #nolint

### Core
# Estimates the linear model of question 3 and 6, using OLS estimator
model_3 <- lm(crime_rate ~ business_crea + log(pop) + income + com_type + motorway + party + tax_rate, df) #nolint

### Exports
# Generate a latex table for of the regression
code_latex <- stargazer(model_3, title = "Extended model - Exercise 3", #nolint
                        align = TRUE, type = "latex",
                        label = "results3",
                        out = "Exercise_3/OUTPUT/extended_results.tex",
                        font.size = "small")
