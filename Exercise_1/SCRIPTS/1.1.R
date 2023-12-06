# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 1.1
# Create a table of descriptive statistics of the variables in the dataset.

### Libraries
library(tidyverse)
library(readxl)
library(xtable)

### Imports
df <- read_excel("Exercise_1/RAW/employment.xls")

### Core

# Function to compute the number of NAs in a column
na_frequency <- function(x) {
  as.integer(mean(is.na(x)) * 217)
}

# Generates the summary
summary <- df %>%
  select(
    agro_emp, bribery, gfce,
    literacy, log_gdp, pop_total,
    self_emp, stocks, sample_size
  ) %>%
  summarize(
    var = names(.),
    median = sapply(., median, na.rm = TRUE),
    mean = sapply(., mean, na.rm = TRUE),
    min = sapply(., min, na.rm = TRUE),
    max = sapply(., max, na.rm = TRUE),
    sd = sapply(., sd, na.rm = TRUE),
    NAs = sapply(., na_frequency)
  ) %>%
  mutate(
    median = ifelse(median > 1000, sprintf("%.1e", median), sprintf("%.1f", median)), #nolint
    mean = ifelse(mean > 1000, sprintf("%.1e", mean), sprintf("%.1f", mean)),
    min = ifelse(min > 1000, sprintf("%.1e", min), sprintf("%.1f", min)),
    max = ifelse(max > 1000, sprintf("%.1e", max), sprintf("%.1f", max)),
    sd = ifelse(sd > 1000, sprintf("%.1e", sd), sprintf("%.1f", sd))
  )


### Exports

#Saves a csv
write.csv(summary, "Exercise_1/OUTPUT/summary.csv", row.names = FALSE)

#Generates latex code for pdf file
latex <- xtable(
        summary,
        caption = "Descriptive statistics",
        label = "desc")

#Saves latex file
writeLines(print(latex, include.rownames = FALSE, include.colnames = TRUE), "Exercise_1/OUTPUT/summary.tex") #nolint
