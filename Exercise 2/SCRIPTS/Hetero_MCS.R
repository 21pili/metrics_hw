###############################################################################

### AUTHOR: [PUT YOUR NAME HERE]
### PROJECT: Econometrics 1 Homework
### ORGANIZATION: PSE (M1, APE)
### EMAIL: [PUT YOUR EMAIL HERE]


### DESCRIPTION: This file contains code for running Monte Carlo Simulations to
###              understand the implications of heteroskedasticity on inference
###              when using the standard OLS estimator.


#### NOTE: The code gives a crude way of running Monte Carlo Simulations (MCS). 
####       Feel free to modify the code anyway you like as long as it executes
####       the same thing. See the document linked below for an alternate, 
####       more robust, way of coding MCS by creating a "function".

####       (https://www.schmidheiny.name/teaching/montecarlo2up.pdf)


###############################################################################

#install.packages("tidyverse")
library(tidyverse)

# Setting model parameters 
n_simulations <- 1000
n <- 100
beta_0 <- 0  # Setting beta_0 to 0
beta_1 <- 0  # Setting beta_1 to 0
alpha <- 0.05  # Significance level

# Storage for simulation results
total_beta_1 <- 0
total_se_beta_1 <- 0
rejections <- 0

set.seed(321)  # For reproducibility

for (i in 1:n_simulations) {
  X <- rnorm(n)  # Generate X
  epsilon <- rnorm(n)  # Generate standard normal errors
  u <- epsilon * sqrt(abs(X))  # Heteroskedastic error term
  
  # Generate Y based on the linear model
  Y <- beta_0 + beta_1 * X + u
  
  # OLS regression and hypothesis testing
  model <- lm(Y ~ X)
  beta_1_estimate <- coef(model)["X"]
  beta_1_se <- summary(model)$coefficients["X", "Std. Error"]
  p_value <- summary(model)$coefficients["X", "Pr(>|t|)"]
  
  # Accumulate beta_1 estimates and standard errors
  total_beta_1 <- total_beta_1 + beta_1_estimate
  total_se_beta_1 <- total_se_beta_1 + beta_1_se
  
  # Count if null hypothesis is rejected
  if (p_value < alpha) {
    rejections <- rejections + 1
  }
}

# Calculate averages and rejection probability
average_beta_1 <- total_beta_1 / n_simulations
average_se_beta_1 <- total_se_beta_1 / n_simulations
rejection_probability <- rejections / n_simulations

# Display average estimate and null rejection rate
cat("Average estimate of Beta_1:", average_beta_1, "\n")
cat("Average standard error of Beta_1:", average_se_beta_1, "\n")
cat("Rejection probability of the null hypothesis (beta_1 = 0):", rejection_probability, "\n")
