## Functions for Bayesian melding analysis

# R. T. Gray

# This script contains functions useful for the cascade incidence analysis
# and results and figures generation. 

# Analysis Functions
# ==================

# R needs a function for mode!
GetMode <- function(vector) {
  uniqueElements <- unique(vector)
  uniqueElements[which.max(tabulate(match(vector, uniqueElements)))]
}

IncFunc <- function(cascadeData, beta1, beta2, beta3, beta4) {
  # Function for can easily calculating the incidence values
  # for each sample of the beta values
  #
  # Args:
  #   cascadeData: Data frame containing the cascade data for each stage 
  #     for each year
  #   beta1, beta2, beta3, beta4: Value for each beta fixed over time
  # Returns:
  #   Returns: estimated incidence for each year in data
  # 
  # ----------------------------------------------------------------------
  
  return(beta1 * cascadeData$undiag  + 
           beta2 * cascadeData$diag  + 
           beta3 * cascadeData$unsuppressed  + 
           beta4 * cascadeData$suppressed)
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

# Generating results
# ==================

PropInfections <- function(cascadeData, betaValues) {
  # This function calculates the proportion of new infections due to each 
  # stage of the cascade for a given set of beta Values.
  #
  # Args:
  #   cascadeData: Data frame with annual estimates for each stage
  #   betaValues: Vector of corresponding beta values for each stage
  # Returns:
  #   A data frame with the proportion of new infections attributed to 
  #   each stage of the HIV cascade. 
  #
  # ----------------------------------------------------------------------
  
  # Apply proportion calculation to each year
  propResults <- t(apply(cascadeData, 1, 
                         function(x) x * betaValues / sum(x * betaValues)))
  
  # Convert to data frame and return
  propResults <- as.data.frame(cbind(cascadeBest$year, propResults))
  colnames(propResults)[1] <- "year"
  
  return(propResults)
}

NumInfections <- function(cascadeData, betaValues) {
  # This function calculates the number of new infections due to each 
  # stage of the cascade for a given set of beta Values.
  #
  # Args:
  #   cascadeData: Data frame with annual estimates for each stage
  #   betaValues: Vector of corresponding beta values for each stage
  # Returns:
  #   A data frame with the proportion of new infections attributed to 
  #   each stage of the HIV cascade. 
  #
  # ----------------------------------------------------------------------
  
  # Apply proportion calculation to each year
  numResults <- t(apply(cascadeData, 1, 
                         function(x) x * betaValues))
  
  # Convert to data frame and return
  numResults <- as.data.frame(cbind(cascadeBest$year, numResults))
  colnames(numResults)[1] <- "year"
  
  return(numResults)
}

# Plotting Functions
# ==================

# PlotOptions.R needs to be sourced for these functions to work

ParameterPlot <- function(parameter, priorsSamples, posteriorSamples, 
                          savefolder = NULL) {
  # This function generates a plot of the prior and posterior for each
  # parameter. Specifically set up for this project. Parameters and inputs 
  # must correspond to the incidence model.
  #
  # Args:
  #   parameter: Model input paramter to plot. 
  #     Only beta1, beta2, beta3, beta4 allowed
  #   priorSamples: Data frame with columns corresponding to samples from 
  #     the prior parameter distributions
  #   posteriorSamples: Data frame with columns corresponding to samples 
  #     from the posterior parameter distributions
  #   savefolder: (Optional) Set to true if you want to save the plot
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
  
  priorDist <- priorFrame[, parameter]
  postDist <- posteriorFrame[, parameter]
  plotRange <- range(priorDist)
  plotStats <- paste("Mean, median, mode:   \n", 
                     "prior = ", 
                     toString(signif(mean(priorDist), digits = 2)), ", ",
                     toString(signif(median(priorDist), digits = 2)), ", ",
                     toString(signif(GetMode(priorDist), digits = 2)), 
                     "   \n", 
                     "postior = ", 
                     toString(signif(mean(postDist), digits = 2)), ", ",
                     toString(signif(median(postDist), digits = 2)), ", ",
                     toString(signif(GetMode(postDist), digits = 2)), 
                     "   \t\n", 
                     sep = "")
  
  postPlot <- ggplot(data = posteriorFrame, aes_string(x = parameter)) +
    geom_density(data = priorFrame, fill = "black", 
              alpha = 0.2) +
    geom_density(colour = "red", fill = "red", 
              alpha = 0.1) +
    coord_cartesian(xlim = plotRange) +
    ylab("Density") + 
    xlab(labels[parameter]) +
    annotate("text", label = plotStats, 
             x = Inf, y = Inf, hjust = 1, vjust = 1) +
    plotOpts
  
  if (!is.null(savefolder)) {
    ggsave(file.path(resultsFolder, 
                     paste("parameter_distribution-", paramter, ".png", 
                           sep ="")),
           plot = postPlot, width = 12, height = 10, 
           units = "cm")
  }
  
  return(postPlot)
}

PercentInc <- function(cascadeStage, percentFrame, years = NULL, 
                       xlimits = NULL, savefolder = NULL) {
  # This function generates a plot of the distribution in the percentage of
  # infections acquired from each stage of the HIV cascade. This function 
  # is specifically set up for this project. Parameters and inputs 
  # must correspond to the incidence model outputs.
  #
  # Args:
  #   cascadeStage: String specifying the stage we are plotting. 
  #     Must be "undiagnosed", "diagnosed", "unsuppressed", "suppressed".
  #   percentFrame: Data frame in long format with year values for 
  #     percentage of infections for each HIV stage and sample. 
  #     Must contain columns year, stage, percentage.
  #   years: (Optional) Vector specifying years we are interested in. 
  #     Default is all years in percentFrame. 
  #   xlimits: (Optional) Specify range of x values to plot.
  #   savefolder: (Optional) Specify if you want to save the plot by 
  #     setting to the folder where plot will be save.
  # Returns:
  #   Plot handle corresponding to the created plot.
  #
  # ----------------------------------------------------------------------
  
  # Check input paramter is appropriate
  if (!(cascadeStage %in% c("undiagnosed", "diagnosed", "unsuppressed", 
                            "suppressed"))) {
    stop("Unknown parameter entered")
  }
  
  # Extract data we want from percentFrame
  if (is.null(years)) {
    percentData <- filter(percentFrame, stage == cascadeStage)
    numYears <- 0
    saveStr <- "All-"
  } else {
    percentData <- filter(percentFrame, stage == cascadeStage, 
                          year %in% years)
    numYears <- length(years)
    saveStr <- paste(toString(min(years)), "-", toString(max(years)), "-", 
                     sep = "")
  }
  
  # Create plot
  if (numYears != 1) {
  distPlot <- ggplot(data = percentData, 
                     aes(x = percentage, group = year, 
                         colour = factor(year))) + 
    geom_line(size = 1.2, stat = "density") + 
    scale_colour_brewer(palette = "RdYlBu", name = "Year") +  
    ylab("Density") + 
    xlab(paste("Percentage acquired from ", cascadeStage)) +
    coord_cartesian(xlim = xlimits) +
    plotOpts + theme(legend.position = "right")
  } else {
    # Only a single year plotted so remove legend and fix colour
    distPlot <- ggplot(data = percentData, aes(x = percentage)) + 
      geom_density(size = 1.2, colour = "blue", 
                   fill = "blue", alpha = 0.2) +
      ylab("Density") + 
      xlab(paste("Percentage acquired from ", cascadeStage
                 , "in", toString(years))) +
      coord_cartesian(xlim = xlimits) +
      plotOpts
    
    # Overwrite save string with single year
    saveStr <- paste(toString(years), "-", sep = "")
  }
  
  # Save if requested 
  if (!is.null(savefolder)) {
    ggsave(file.path(savefolder, 
                     paste("Incidence_distribution_",
                           saveStr, 
                           cascadeStage,
                           ".png",sep ="")),
           plot = distPlot, width = 12, height = 10, 
           units = "cm")
  }
  
  # Return plot handle
  return(distPlot)
}

