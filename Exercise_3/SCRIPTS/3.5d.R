# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 4.5d
# Use all instruments you think are valid to implement the
# IV estimator using the ivreg package and report your
# results. How different are the results from part (c)?



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
ivreg_model2 <- ivreg(crime_rate ~ log(pop) + income + com_type | business_crea | motorway + tax_rate, data = df) #nolint

### Exports
# Generate a latex table for of the regression
code_latex <- stargazer(ivreg_model2,
                        title = "2SLS Regression With motorway and tax rate as Instruments", #nolint
                        align = TRUE, type = "latex",
                        label = "2sls_mtw_tr",
                        out = "Exercise_3/OUTPUT/2sls.tex",
                        font.size = "small",
                        omit = "F")
