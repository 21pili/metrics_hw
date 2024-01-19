# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 3.4b
# Discuss the validity of all potential instruments with
# Use the correlation between tax rate and the three variables to
# assess their potential strength as instruments. Does one seem
# better suited as an instrument than the other? Why ?


### Libraries
library(tidyverse)
library(readxl)
library(xtable)
library(stargazer)

### Imports
df <- read_csv("Exercise_3/INTERMEDIATE/crime_rate.csv") #nolint

### Core
# Turns the party variable into a dummy variable
df <- df %>% mutate(
    right = ifelse(party == "Right", 1, 0)
)

#Plots
plot <- df %>% ggplot(aes(x = tax_rate, y = business_crea)) +
    geom_point() +
    labs(
        x = "tax_rate",
        y = "business_crea (%)") +
    theme_light() +
    theme(
        axis.text = element_text(size = 40),
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40)
    )

ggsave("Exercise_3/OUTPUT/bcr_tx.png", plot = plot, height = 8, width = 10, units = "in") #nolint

plot <- df %>% ggplot(aes(x = factor(motorway), y = business_crea)) +
    geom_point(position = position_jitter(width = 0.1, height = 0)) +
    labs(
        x = NULL,
        y = "business_crea (%)") +
    theme_light() +
    scale_x_discrete(labels = c("No motorway", "Motorway")) +
    theme(
        axis.text = element_text(size = 40),
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40)
    )

ggsave("Exercise_3/OUTPUT/bcr_mtw.png", plot = plot, height = 8, width = 10, units = "in") #nolint

plot <- df %>% ggplot(aes(x = factor(right), y = business_crea)) +
    geom_point(position = position_jitter(width = 0.1, height = 0)) +
    labs(
        x = NULL,
        y = "business_crea (%)") +
    theme_light() +
    scale_x_discrete(labels = c("Center and Left", "Right")) +
    theme(
        axis.text = element_text(size = 40),
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40)
    )

ggsave("Exercise_3/OUTPUT/bcr_pty.png", plot = plot, height = 8, width = 10, units = "in") #nolint


cor_txr_bcr <- cor(df$business_crea, df$tax_rate)
cor_mtw_bcr <- cor(df$business_crea, df$motorway)
cor_pty_bcr <- cor(df$business_crea, df$right)