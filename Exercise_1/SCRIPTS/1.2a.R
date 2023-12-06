# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 1.2a
# Is it the case that self-employment is correlated with how rich a country is
#(in-terms of logGDP per-capita)? Present a scatter-plot that provides an
# answer. In the scatter-plot, we want the size of the scatter points to be
# proportional to the population of the countries.


### Libraries
library(tidyverse)
library(ggplot2)
library(readxl)

### Imports
df <- read_excel("Exercise_1/RAW/employment.xls") #nolint


### Core
# Regresses self_emp on log_gdp and gets coefficients of interest
regression <- lm(self_emp ~ log_gdp, df)
coefficients <- coef(regression)
intercept <- as.numeric(coefficients["(Intercept)"])
slope <- as.numeric(coefficients["log_gdp"])

# Generates the plot of self employment with respect to log gdp and population
plot <- df %>% ggplot(aes(x = log_gdp, y = self_emp, size = pop_total)) +
    geom_point() +
    geom_abline(intercept = intercept, slope = slope) +
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

# Computes correlation coefficients
cor_semp_gdp <- cor(df$log_gdp, df$self_emp, use = "complete.obs")


### Exports
ggsave("Exercise_1/OUTPUT/emp_wrt_gdp.png", plot = plot, height = 8, width = 10, units = "in") #nolint
