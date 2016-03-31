## Functions for Bayesian melding analysis

# R. T. Gray

# This script contains functions useful for the cascade incidence analysis
# and results and figures generation. 

# Analysis Functions
# ==================

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

# Plotting Functions
# ==================

parameterPlot <- function(paramter, priorsSamples, posteriorSamples, 
                          save = FALSE) {
  # This function generates a plot of the prior and posterior for each
  # parameter. Specificly set up for this project. Parameters and inputs 
  # must correspond to the incidence model.
  #
  # Args:
  #   parameter: Model input paramter to plot. 
  #     Only beta1, beta2, beta3, beta4 allowed
  #   priorSamples: Data frame with columns corresponding to samples from 
  #     the prior parameter distributions
  #   posteriorSamples: Data frame with columns corresponding to samples from 
  #     the posterior parameter distributions
  #   save: Set to true if you want to save the plot
  # Returns:
  #   Plot handle corresponding to the created plot
  #
  # ----------------------------------------------------------------------
  
  # Check input paramter is appropriate
  if (!(parameter %in% c("beta1", "beta2", "beta3", "beta4"))) {
    stop("Unknown parameter entered")
  }
  
  # Specify parameter labels
  labels <- c("beta1" = "Undiagnosed Beta",
              "beta2" = "Diagnosed Beta",
              "beta3" = "Unsuppressed Beta",
              "beta4" = "Suppressed Beta")
  
  priorDist <- priorFrame[, dist]
  postDist <- posteriorFrame[, dist]
  plotRange <- range(priorDist)
  plotStats <- paste("Mean, median, mode:   \n", 
                     "prior = ", 
                     toString(signif(mean(priorDist), digits = 2)), ", ",
                     toString(signif(median(priorDist), digits = 2)), ", ",
                     toString(signif(getmode(priorDist), digits = 2)), "   \n", 
                     "postior = ", 
                     toString(signif(mean(postDist), digits = 2)), ", ",
                     toString(signif(median(postDist), digits = 2)), ", ",
                     toString(signif(getmode(postDist), digits = 2)), "   \t\n", 
                     sep = "")
  
  postPlot <- ggplot(data = posteriorFrame, aes_string(x = dist)) +
    geom_density(data = priorFrame, fill = "black", alpha = 0.2) +
    geom_density(colour = "red", fill = "red", alpha = 0.1) +
    coord_cartesian(xlim = plotRange) +
    ylab("Density") + 
    xlab(labels[dist]) +
    annotate("text", label = plotStats, 
             x = Inf, y = Inf, hjust = 1, vjust = 1) +
    plotOpts
  
  if (save) {
    ggsave(file.path(resultsFolder, 
                     paste("parameter_distribution-", paramter, ".png",sep ="")),
           plot = postPlot, width = 12, height = 10, 
           units = "cm")
  }
}
