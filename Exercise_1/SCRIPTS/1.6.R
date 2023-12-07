# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 1.6a
# Estimate a linear model that includes the expanded set of
# covariates using OLS. Report your results and interpret them.



### Libraries
library(readxl)
library(xtable)
library(stargazer)

### Imports
df <- read_excel("Exercise_1/RAW/employment.xls") #nolint

### Core
# Estimates the linear model of question 3 and 6, using OLS estimator
model_3 <- lm(self_emp ~ log_gdp + literacy + agro_emp, df)
model_6 <- lm(self_emp ~ log_gdp + literacy + agro_emp + gfce + stocks + bribery, df) #nolint
### Exports
# Generate a latex table for of the regression
code_latex <- stargazer(model_3, model_6, title = "Linear Regressions - Exercise 1", #nolint
                        align = TRUE, type = "latex", label = "results_1",
                        out = "Exercise_1/OUTPUT/results.tex")
