# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 1.6a
# Estimate a linear model that includes the expanded set of
# covariates using OLS. Report your results and interpret them.



### Libraries
library(readxl)
library(tidyverse)
library(xtable)
library(stargazer)

### Imports
df <- read_excel("Exercise_1/RAW/employment.xls") #nolint

### Core
df <- df %>% filter(
    country_name != "China" & country_name != "India") %>%
    select(
    self_emp, log_gdp,
    literacy, agro_emp, pop_total
    ) %>%
    na.omit()

# Estimates the linear model of question 3 and 6, using OLS estimator
model_3 <- lm(self_emp ~ log_gdp + literacy + agro_emp, df)

# Show correlation between log_gdp and pop_total
df %>% ggplot(aes(x = pop_total, y = residuals)) +
    geom_point() +
    scale_size_continuous(range = c(1, 10)) +
    labs(
        x = "log_gdp",
        y = "self_emp (%)",
        size = "pop_total") +
    theme_light() +
    theme(
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)
    )

# Plot the residuals against the log_gdp a proxy for the size of the country



### Exports
# Generate a latex table for of the regression
