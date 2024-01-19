# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 3.4c
# For the instrument chosen in part (b), instead of manually run-
# ning the 2 stages, implement the IV estimator using the ivreg
# package. Is your answer the same or different to the answer in
# part (b) and why?


### Libraries
library(tidyverse)
library(readxl)
library(xtable)
library(stargazer)
library(ivreg)

### Imports
df <- read_csv("Exercise_3/INTERMEDIATE/crime_rate.csv") #nolint

### Core
# Using ivreg
# Exogenous | Endogenous | Instruments #nolint
ivreg_model <- ivreg(crime_rate ~ log(pop) + income + com_type | business_crea | motorway, data = df) #nolint

# Doing both regression manually
first_mtw <- lm(business_crea ~ log(pop) + income + com_type + motorway, df) #nolint
#Extract fitted values for motorway
fitted_business_crea <- predict(first_mtw)
# Run the second stage regression
second_stage <- lm(crime_rate ~ fitted_business_crea + log(pop) + income + com_type, df) #nolint

### Exports
# Generate a latex table for of the regression
code_latex <- stargazer(second_stage, ivreg_model,
                        title = "2SLS Regression with and without ivreg Package", #nolint
                        align = TRUE, type = "latex",
                        label = "2sls",
                        out = "Exercise_3/OUTPUT/2sls_mtw.tex",
                        font.size = "small",
                        omit = "F")
