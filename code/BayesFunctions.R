## Functions for Bayesian melding analysis

# R. T. Gray

# This script contains functions useful for the cascade incidence analysis
# and results and figures generation. 

incFunc <- function(cascadeData, beta1, beta2, beta3, beta4) {
  # Function for can easily calculating the incidence values
  # for each sample of the beta values
  #
  # Args:
  #   cascadeData: Data frame containing the cascade data for each stage for each 
  #     year
  #   beta1, beta2, beta3, beta4: Value for each beta fixed over time
  # Returns:
  #   Returns: estimated incidence for each year in data
  # 
  # ----------------------------------------------------------------------
  
  return(beta1 * cascadeData$undiag  + beta2 * cascadeData$diag  + 
           beta3 * cascadeData$unsuppressed  + beta4 * cascadeData$suppressed)
}

WeightError <- function(dataValues, estimatedValues, dataError) {
  # Function to calculate the weight for a sample of the prior 
  # distribtions. This function is used in a Bayesian melding procedure to 
  # a model to incidence data. Assume a normal distribution for the 
  # likelihood.
  # 
  # Args:
  #   dataValues: Data we are fitting to
  #   estimatedValues: Estimated data values from model
  #   dataError: Percentage error in the data
  # Returns:
  #
  # ----------------------------------------------------------------------
  
  numData <- length(dataValues)
  
  # Differences between observed and simulated
  distance <- dataValues - estimatedValues 
  
  sigma <- dataValues * dataError / 100 # standard deviation: convert 
                                        # percentage to proportion
  
  mu <- rep(0, numData) # mean of x is zero it matches the data exactly
  
  # Calculate the weight assuming a normal likelihood
  weight <- prod(dnorm(distance, mu, sigma) / dnorm(mu, mu, sigma))
  return(weight)
}
