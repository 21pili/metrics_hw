# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 3.2
# After estimating the model in equation (7), you wonder whether
# enodegeneity is a problem. Why could the variable of interest,
# the business creation growth rate, be endogenous?


### Libraries
library(readxl)
library(xtable)
library(stargazer)

### Imports
df <- read_csv("Exercise_3/INTERMEDIATE/crime_rate.csv") #nolint

### Core
#Plots
plot <- df %>% ggplot(aes(x = iqr_income, y = crime_rate)) +
    geom_point() +
    labs(
        x = "iqr_income",
        y = "crime_rate (%)") +
    theme_light() +
    theme(
        axis.text = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30)
    )

ggsave("Exercise_3/OUTPUT/cr_iqr.png", plot = plot, height = 8, width = 10, units = "in") #nolint

plot <- df %>% ggplot(aes(x = iqr_income, y = business_crea)) +
    geom_point() +
    labs(
        x = "iqr_income",
        y = "business_crea_rate (%)") +
    theme_light() +
    theme(
        axis.text = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30)
    )

ggsave("Exercise_3/OUTPUT/bcr_iqr.png", plot = plot, height = 8, width = 10, units = "in") #nolint

cor_iqr_cr <- cor(df$crime_rate, df$iqr_income)
cor_iqr_bcr <- cor(df$business_crea, df$iqr_income)

