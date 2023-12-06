# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 3.1
# Compute the crime rate per household. Generate a table of
# descriptive statistics of all the variables that have been
# introduced so far. Comment on the descriptive statistics.

### Libraries
library(tidyverse)
library(xtable)

### Imports
df <- read_csv("Exercise_3/RAW/crime_data.csv")

### Core
# Clean by deleting the first column
df <- df[, -1]

# Create the column crime_rate
df <- df %>% mutate(
  crime_rate = nb_crimes / nb_households
)

# Function to compute the number of NAs in a column
na_frequency <- function(x) {
  as.integer(mean(is.na(x)) * 217)
}

# Generates the summary
summary <- df %>%
  select(
    business_crea,
    nb_crimes, nb_households,
    pop, income, crime_rate
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
    median = ifelse(median > 1000 | median < 0.1, sprintf("%.1e", median), sprintf("%.1f", median)), #nolint
    mean = ifelse(mean > 1000 | mean < 0.1, sprintf("%.1e", mean), sprintf("%.1f", mean)), #nolint
    min = ifelse(min > 1000 | min < 0.1, sprintf("%.1e", min), sprintf("%.1f", min)), #nolint
    max = ifelse(max > 1000 | max < 0.1, sprintf("%.1e", max), sprintf("%.1f", max)), #nolint
    sd = ifelse(sd > 1000 | sd < 0.1, sprintf("%.1e", sd), sprintf("%.1f", sd)) #nolint
  )


### Exports

#Generates latex code for pdf file
latex <- xtable(
        summary,
        caption = "Descriptive statistics",
        label = "desc2")

#Saves latex file
writeLines(print(latex, include.rownames = FALSE, include.colnames = TRUE), "Exercise_3/OUTPUT/summary.tex") #nolint

# Exports the cleaned table in INTERMEDIATE folder
write.csv(df, "Exercise_3/INTERMEDIATE/crime_rate.csv", row.names = FALSE)
