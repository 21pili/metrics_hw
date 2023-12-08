# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 1.6a
# We are told that many of these variables, including the self-employment
# rates, are measured using sample surveys in each country. This implies
# that the variance of each observation is directly proportional to the
# sample size used in each country. What does this imply for the
# OLS estimator? Use this information to implement the weighted least
# squares estimator on the model with the expanded set of covariates.
# Compare your results with part (a) and explain any differences.



### Libraries
library(readxl)
library(tidyverse)
library(stargazer)
library(lmtest)

### Imports
df <- read_excel("Exercise_1/RAW/employment.xls") #nolint

### Core

# Performing a white test
# Data for extended model
df_6 <- df %>%
    select(
        self_emp, log_gdp,
        literacy, agro_emp,
        gfce, stocks, bribery
    ) %>%
    na.omit()
# Data for simple model
df_3 <- df %>%
    select(
        self_emp, log_gdp,
        literacy, agro_emp
    ) %>%
    na.omit()

### Core
# Estimates the linear model of question 3 and 6, using OLS estimator
model_6 <- lm(self_emp ~ log_gdp + literacy + agro_emp + gfce + stocks + bribery, df_6) #nolint
model_3 <- lm(self_emp ~ log_gdp + literacy + agro_emp, df_3) #nolint

## Calculate residuals for model_6 and model_3
residuals_model_6 <- residuals(model_6)
residuals_model_3 <- residuals(model_3)

# Fit the auxiliary regressions for White test
white_aux_model_6 <- lm(residuals_model_6^2 ~ log_gdp + literacy + agro_emp + gfce + stocks + bribery, df_6) #nolint
white_aux_model_3 <- lm(residuals_model_3^2 ~ log_gdp + literacy + agro_emp, df_3) #nolint

# Perform the White tests
white_test_6 <- bptest(white_aux_model_6)
white_test_3 <- bptest(white_aux_model_3)

# Get resutls
print(white_test_6)
print(white_test_3)

# Implementing the WLS
weighted_df <- df %>%
    mutate(
        self_emp = self_emp / sample_size,
        log_gdp = log_gdp / sample_size,
        literacy = literacy / sample_size,
        agro_emp = agro_emp / sample_size,
        gfce = gfce / sample_size,
        stocks = stocks / sample_size,
        bribery = bribery / sample_size
    ) %>%
    select(
        self_emp, log_gdp,
        literacy, agro_emp,
        gfce, stocks, bribery
    )

weighted_model <- lm(self_emp ~ log_gdp + literacy + agro_emp + gfce + stocks + bribery, weighted_df) #nolint

### Export
code_latex <- stargazer(weighted_model, title = "Weighted Least Squares Extended Estimation - Exercise 1", #nolint
                        align = TRUE, type = "latex", label = "WLS",
                        out = "Exercise_1/OUTPUT/wls.tex")
