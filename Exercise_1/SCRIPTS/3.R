# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 3
# In order to understand the relative importance of each of these factors,
# we consider the following linear model:
# self_emp = beta_0 + beta_1 log(GDP) + beta_2 literacy + beta_3 agro_emp + u
# Estimate the model parameters using OLS, report your results and summarize them. #nolint



### Libraries
library(readxl)
library(xtable)

### Imports
df <- read_excel("Exercise_1/RAW/employment.xls") #nolint

### Core
# Estimate the linear model of question 3, using OLS estimator
model_3 <- lm(self_emp ~ log_gdp + literacy + agro_emp, df)

### Exports
# Generate a latex table for of the regression
latex <- xtable(model_3,
    caption = "Linear Model",
    label = "tab:model_3")
writeLines(print(latex), "Exercise_1/OUTPUT/model_3.tex") #nolint