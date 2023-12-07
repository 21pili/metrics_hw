# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 1.5
# Implement this 3-step procedure and compare your estimate and
# standard error of —3 with those in question 3 where you used
# the standard OLS procedure. Report the results from each of
# the 3 steps. What do you observe? Is it consistent with what
# you would expect?


### Libraries
library(readxl)
library(stargazer)
library(readxl)

### Imports
df <- read_excel("Exercise_1/RAW/employment.xls") #nolint

### Core

# Step 1 : Identify control variables, here log_gdp and literacy and
# regress it on the dependent variable to get the residuals ^r.
step_1 <- lm(self_emp ~ log_gdp + literacy, df)

# Step 2 : Regress the variable of interest on the control
# variables and get the residuals û.
step_2 <- lm(agro_emp ~ log_gdp + literacy, df)

# Step 3 : Regress ˆr on û and get the coefficient of interest beta_3
step_3 <- lm(residuals(step_1) ~ residuals(step_2), df)

# Extract the coefficient of interest (beta_3)
beta_3 <- coef(step_3)[2]



### Exports
# Generate a latex table for of the regression
code_latex <- stargazer(model_3, model_6, title = "Linear Regressions - Exercise 1", #nolint
                        align = TRUE, type = "latex", label = "results_1",
                        out = "Exercise_1/OUTPUT/results.tex")
