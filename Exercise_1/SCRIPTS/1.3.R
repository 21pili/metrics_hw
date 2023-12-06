# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 1.3
# In order to understand the relative importance of each of these factors,
# we consider the following linear model:
# self_emp = beta_0 + beta_1 log(GDP) + beta_2 literacy + beta_3 agro_emp + u
# Estimate the model parameters using OLS, report your results and summarize them. #nolint



### Libraries
library(readxl)
library(xtable)
library(stargazer)

### Imports
df <- read_excel("Exercise_1/RAW/employment.xls") #nolint

### Core
# Estimate the linear model of question 3, using OLS estimator
model_3 <- lm(self_emp ~ log_gdp + literacy + agro_emp, df)

### Exports
# model_3 is actually exported in file 1.6.R to be
# displayed on the same table as the extended model.
