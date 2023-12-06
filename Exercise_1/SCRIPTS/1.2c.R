# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 1.2c
# Based on the literacy variable, create a new variable which classifies
# countries into low literacy (< 50%), medium literacy (50% - 80%), and high
# literacy (> 80%). Then present a bar graph comparing the mean self-employment
# rates in each of these 3 literacy-based categories of countries.



### Libraries
library(tidyverse)
library(ggplot2)
library(readxl)

### Imports
df <- read_excel("Exercise_1/RAW/employment.xls") #nolint


### Core
# Create a new category for the literacy variable
literacy_levels <- c("Low Literacy", "Medium Literacy", "High Literacy")

df <- df %>%
  mutate(literacy_category = case_when(
    literacy < 50 ~ "Low Literacy",
    literacy >= 50 & literacy <= 80 ~ "Medium Literacy",
    literacy > 80 ~ "High Literacy"
  ))

# Summarize the data to get the mean of self-employment rate per category
summary <- df %>% group_by(
    literacy_category
    ) %>%
    summarise(
        average_self_emp = mean(self_emp, na.rm = TRUE)
    ) %>%
    ungroup()

# Generates the plot of self employment with respect to log gdp and population
plot <- summary %>% ggplot(aes(x = literacy_category, y = average_self_emp)) +
    geom_col() +
    scale_x_discrete(limits = c("Low Literacy", "Medium Literacy", "High Literacy")) + #nolint
    theme_light() +
    theme(
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 0),
        axis.title.y = element_text(size = 15)
    )

### Exports
ggsave("Exercise_1/OUTPUT/bar_emp_literacy.png", plot = plot, height = 8, width = 10, units = "in") #nolint