# author : Pierre Pili
# email : pierre.pili@etu.minesparis.psl.eu
# description : Question 3.4c
# Extract the ﬁtted values of any one of these ﬁrst-stage regressions and use them to run #nolint
# a second-stage regression. Assuming that the instrument is exogenous, are your results #nolint
# reliable? Why?


### Libraries
library(tidyverse)
library(readxl)
library(xtable)
library(stargazer)

### Imports
df <- read_csv("Exercise_3/INTERMEDIATE/crime_rate.csv") #nolint

### Core
first_mtw <- lm(business_crea ~ log(pop) + income + com_type + motorway, df) #nolint

#Extract fitted values for motorway
fitted_business_crea <- predict(first_mtw)

# Run the second stage regression
second_stage <- lm(crime_rate ~ fitted_business_crea + log(pop) + income + com_type, df) #nolint
### Exports
# Generate a latex table for of the regression
code_latex <- stargazer(second_stage,
                        title = "Second-Stage Regression with Motorway as Instrumental Variable", #nolint
                        align = TRUE, type = "latex",
                        label = "scnd_stage",
                        out = "Exercise_3/OUTPUT/second_stage_mtw.tex",
                        font.size = "small",
                        omit = "F")
