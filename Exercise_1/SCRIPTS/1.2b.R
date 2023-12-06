# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 1.2b
# Is it the case that countries with higher share of employment
# in agriculture also have higher self-employment rates? Present
# a similar scatter-plot as in the previous question as well as
# the empirical correlation coefficients corresponding to these
# two relationships to answer the question.


### Libraries
library(tidyverse)
library(ggplot2)
library(readxl)

### Imports
df <- read_excel("Exercise_1/RAW/employment.xls") #nolint


### Core
# Regresses self_emp on log_gdp and gets coefficients of interest
regression <- lm(self_emp ~ agro_emp, df)
coefficients <- coef(regression)
intercept <- as.numeric(coefficients["(Intercept)"])
slope <- as.numeric(coefficients["agro_emp"])

# Generates the plot of self employment with respect to log gdp and population
plot <- df %>% ggplot(aes(x = agro_emp, y = self_emp, size = pop_total)) +
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
cor_semp_agro <- cor(df$agro_emp, df$self_emp, use = "complete.obs")


### Exports
ggsave("Exercise_1/OUTPUT/emp_wrt_agro.png", plot = plot, height = 8, width = 10, units = "in") #nolint
