# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 3.2
# Estimate the model in equation (7) using OLS



### Libraries
library(tidyverse)
library(readxl)
library(xtable)
library(stargazer)

### Imports
df <- read_csv("Exercise_3/INTERMEDIATE/crime_rate.csv") #nolint

### Core
# Estimates the linear model of question 3 and 6, using OLS estimator
model_2 <- lm(crime_rate ~ business_crea + log(pop) + income + com_type, df)

### Exports
# Generate a latex table for of the regression
code_latex <- stargazer(model_2, title = "Ordinary Least Square Estimation - Exercise 3", #nolint
                        align = TRUE, type = "latex",
                        label = "results2",
                        out = "Exercise_3/OUTPUT/results.tex")
