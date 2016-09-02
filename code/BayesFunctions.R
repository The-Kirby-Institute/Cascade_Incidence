## Functions for Bayesian melding analysis

# R. T. Gray

# This script contains functions useful for the cascade incidence analysis
# and results and figures generation. 

# Analysis Functions ------------------------------------------------------

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
  #   Estimated incidence for each year in data
  # 

    return(beta1 * cascadeData$undiagnosed  + 
             beta2 * cascadeData$diagnosed  + 
             beta3 * cascadeData$unsuppressed  + 
             beta4 * cascadeData$suppressed)
}

IncFuncTV <- function(cascadeData, beta1start, beta1end, 
                      beta2start, beta2end,
                      beta3start, beta3end,
                      beta4start, beta4end) {
  # Function for can easily calculating the incidence values
  # for each sample of the beta values
  
  numYears <- nrow(cascadeData)
  beta1 <- seq(beta1start, beta1end, length = numYears)
  beta2 <- seq(beta2start, beta2end, length = numYears)
  beta3 <- seq(beta3start, beta3end, length = numYears)
  beta4 <- seq(beta4start, beta4end, length = numYears)
  
  return(beta1 * cascadeData$undiagnosed  + 
           beta2 * cascadeData$diagnosed  + 
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
  #   Corresponding weight for set of parameters
  #
  
  numData <- length(dataValues)
  
  # Differences between observed and simulated
  distance <- dataValues - estimatedValues 
  
  # if (length(dataError) == 1) {
    # Assume error is for entire time period
    sigma <- dataValues * dataError / 100 # standard deviation: convert 
                                        # percentage to proportion
  # } else {
  #   # Assume error is for each time point (allows variation in error)
  #   # In this case dataError is entered as an upper bound on the data values
  #   sigma <- (dataError - dataValues) # standard deviation
  # }
  
  mu <- rep(0, numData) # mean of x is zero it matches the data exactly
  
  # Calculate the weight assuming a normal likelihood
  weight <- prod(dnorm(distance, mu, sigma) / dnorm(mu, mu, sigma))
  return(weight)
}

PriorSample <- function(samples, dist, parameters) {
  # Function which produces samples from a prior specified by 
  # a distribution and appropriate set of parameters. 
  #
  # Args:
  #   samples: Number of samples to take from priors
  #   dist: String specifying the prior distribution type
  #   parameters: Vector of values specifying the shape of dist.
  #
  # Returns: 
  #   Vector of samples from the specified distribution.
  #
  
  if (dist == "unif") {
    return(runif(samples, parameters[1], parameters[2]))
  } else if (dist == "beta") {
    return(rbeta(samples, parameters[1], parameters[2]))
  } else if (dist == "triangle") {
    # Assume triangle library already loaded
    return(rtriangle(samples, parameters[1], parameters[2], parameters[3]))
  } else if (dist == "lognorm") {
    return(rlnorm(samples, parameters[1], parameters[2]))
  } else if (dist == "truncln") {
    # Assume EnvStats package already loaded 
    return(rlnormTrunc(samples, parameters[1], parameters[2], max = 1))
  } else {
    stop("Unknown distribution")
  }
}

SuppressedOption <- function(option) {
  # Function used to return specifications of prior for f4.
  #
  # Args:
  #   option: Specified option. 
  #
  # Returns:
  #   List with the distribution type and associated parameters.
  #
  
  # Beta distribution
  if (option == "option1") {
    return(list(dist = "beta", params = c(0.85, 10.76))) # Original
  } else if (option == "option2"){
    return(list(dist = "beta", params = c(1, 18)))
    # Triangle distribution
  } else if (option == "option3"){
    return(list(dist = "triangle", params = c(0, 0.3, 0.05)))
  } else if (option == "option4"){
    return(list(dist = "triangle", params = c(0, 0.45, 0.15)))
  } else if (option == "option5"){
    return(list(dist = "triangle", params = c(0, 0.6, 0.3)) )
  # Truncated lognormal distribution 
  } else if (option == "option6"){
    return(list(dist = "truncln", params = c(log(0.04), 1.2)))
  } else if (option == "option7"){
    return(list(dist = "truncln", params = c(log(0.1), 1.2)))
  } else if (option == "option8"){
    return(list(dist = "truncln", params = c(log(0.25), 1.2)))
  } else {
    stop("Unknown option for suppressed beta prior")
  }
}

# Generating results -----------------------------------------------------

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
  
  # Apply proportion calculation to each year
  propResults <- t(apply(cascadeData, 1, 
                         function(x) x * betaValues / sum(x * betaValues)))
  
  # Convert to data frame and return
  propResults <- as.data.frame(cbind(cascadeBest$year, propResults))
  colnames(propResults)[1] <- "year"
  
  return(propResults)
}

PropInfectionsTV <- function(cascadeData, betaValuesStart, betaValuesEnd) {
  # This function calculates the proportion of new infections due to each 
  # stage of the cascade for a given set of beta Values.
  
  # Apply proportion calculation to each year
  # propResults <- t(apply(cascadeData, 1, 
  #                        function(x) x * betaValues / 
  #                           sum(x * betaValues)))
  
  numYears <- nrow(cascadeData)
  betaValuesDf <- data.frame(beta1 = seq(betaValuesStart[1], 
                              betaValuesEnd[1], length = numYears),
                             beta2 = seq(betaValuesStart[2], 
                              betaValuesEnd[2], length = numYears),
                             beta3 = seq(betaValuesStart[3], 
                              betaValuesEnd[3], length = numYears),
                             beta4 = seq(betaValuesStart[4], 
                              betaValuesEnd[4], length = numYears)) 
  
  temp <- cascadeData * betaValuesDf
  propResults <- t(apply(temp, 1, function(x) x / sum(x)))
  
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
  
  # Apply proportion calculation to each year
  numResults <- t(apply(cascadeData, 1, 
                         function(x) x * betaValues))
  
  # Convert to data frame and return
  numResults <- as.data.frame(cbind(cascadeBest$year, numResults))
  colnames(numResults)[1] <- "year"
  
  return(numResults)
}

NumInfectionsTV <- function(cascadeData, betaValuesStart, betaValuesEnd) {
  # This function calculates the number of new infections due to each 
  # stage of the cascade for a given set of beta Values.
  
  # Apply proportion calculation to each year
  # numResults <- t(apply(cascadeData, 1, 
  #                       function(x) x * betaValues))
  
  numYears <- nrow(cascadeData)
  betaValuesDf <- data.frame(beta1 = seq(betaValuesStart[1], 
                              betaValuesEnd[1], length = numYears),
                             beta2 = seq(betaValuesStart[2], 
                              betaValuesEnd[2], length = numYears),
                             beta3 = seq(betaValuesStart[3], 
                              betaValuesEnd[3], length = numYears),
                             beta4 = seq(betaValuesStart[4], 
                              betaValuesEnd[4], length = numYears)) 
  
  numResults <- cascadeData * betaValuesDf
  
  # Convert to data frame and return
  numResults <- as.data.frame(cbind(cascadeBest$year, numResults))
  colnames(numResults)[1] <- "year"
  
  return(numResults)
}

# Plotting Functions -----------------------------------------------------

# PlotOptions.R needs to be sourced for these functions to work

ParameterPlot <- function(parameter, priorsSamples, posteriorSamples, 
                          savefolder = NULL, logCoords = FALSE, 
                          singleplot = FALSE, stats = FALSE,
                          distLabels = NULL) {
  # This function generates a plot of the prior and posterior for each
  # parameter. Specifically set up for this project. Parameters and inputs 
  # must correspond to the incidence model. Note can use this to compare
  # distributions by setting priorSamples and psoteriorSamples and labels
  # appropriately. Doesn't neccessarily have to be a prior and posterior. 
  #
  # Args:
  #   parameter: Model input paramter to plot. 
  #     Only beta1, beta2, beta3, beta4 allowed
  #   priorSamples: Data frame with columns corresponding to samples from 
  #     the prior parameter distributions
  #   posteriorSamples: Data frame with columns corresponding to samples 
  #     from the posterior parameter distributions
  #   savefolder: (Optional) Set to a folder to save the plot
  #   logCoords: (Optional) Set to TRUE tse log coordinates on x-axis 
  #   singleplot: (Optional) If TRUE only plots the posterior
  #   stats: (Optional) If true plots median lines for both plots and adds
  #     95% credible intervals for single plot
  #   distLabels: (Optional) By default is NUll and set to 
  #     c("Prior", "Posterior") but can be changed.
  # Returns:
  #   Plot handle corresponding to the created plot
  #
  
  # Check input paramter is appropriate
  if (!(parameter %in% c("beta", "beta1", "beta2", "beta3", "beta4", 
                         "f1", "f2", "f3", "f4", "startundiag"))) {
    stop("Unknown parameter entered")
  }
  
  # Set defualts
  if (is.null(distLabels)) {
    sampleLabels <- c("Prior (blue)", "Posterior (red)")
  } else {
    sampleLabels <- distLabels
  }
  
  # Specify parameter labels
  labels <- c("beta" = "Weighted transmission rate per 1000 PLHIV",
              "beta1" = "Transmission rate per 1000 undiagnosed PLHIV",
              "beta2" = "Transmission rate per 1000 diagnosed PLHIV",
              "beta3" = "Transmission rate per 1000 unsuppressed PLHIV",
              "beta4" = "Transmission rate per 1000 suppressed PLHIV",
              "f1" = "Undiagnosed factor",
              "f2" = "Diagnosed factor",
              "f3" = "Unsuppressed factor",
              "f4" = "Suppressed factor",
              "startundiag" = "Undiagnosed proportion")
  yLabel <- labels[parameter]
  
  if (parameter %in% c("beta", "beta1", "beta2", "beta3", "beta4")) {
    priorsSamples[, parameter] <- priorsSamples[, parameter] * 1000
    posteriorSamples[, parameter] <- posteriorSamples[, parameter] * 1000
  }
  
  priorDist <- priorsSamples[, parameter]
  postDist <- posteriorSamples[, parameter]
  
  if (singleplot) {
    plotRange <- range(postDist)
    plotStats <- paste("Mean = ", 
                       toString(signif(mean(postDist), digits = 2)), 
                       "   \n",
                       "95% CI = ",
                       toString(signif(range(postDist), digits = 2)),
                       "   \n",
                       sep = "")
  } else {
    plotRange <- range(c(priorDist, postDist))
    plotStats <- paste0("Mean, median:   \n", 
                     paste0(sampleLabels[1], " = "), 
                     toString(signif(mean(priorDist), digits = 2)), ", ",
                     toString(signif(median(priorDist), digits = 2)),
                     # toString(signif(GetMode(priorDist), digits = 2)), 
                     "   \n", 
                     paste0(sampleLabels[2], " = "), 
                     toString(signif(mean(postDist), digits = 2)), ", ",
                     toString(signif(median(postDist), digits = 2)),
                     # toString(signif(GetMode(postDist), digits = 2)), 
                     "   \t\n")
  }
  
  # Start plot
  postPlot <- ggplot(data = posteriorSamples, aes_string(x = parameter)) +
    # geom_density(colour = "red", fill = "red", 
    #           alpha = 0.1)
    geom_line(colour = "red3", size = 1.2, stat = "density")
  
  # Add prior if necessary
  if (!singleplot) {
    postPlot <- postPlot + 
      # geom_density(data = priorsSamples, fill = "black", 
      #            alpha = 0.2)
      geom_line(data = priorsSamples, colour = "blue", size = 1.2, 
                stat = "density")
  }
  
  # Transform coordinates if required
  if (logCoords) {
    postPlot <- postPlot + scale_x_log10()
    yLabel <- paste(yLabel, ", log10 scale") 
  }
  
  # Get the maximum density value for setting axes
  if (singleplot) {
    ymax <- max(ggplot_build(postPlot)$data[[1]]$density)
    yLimit <- ymax * 1.2
  } else {
    ymax <- max(max(ggplot_build(postPlot)$data[[1]]$density), 
                max(ggplot_build(postPlot)$data[[2]]$density))
    yLimit <- ymax * 1.3
  }
  
  if (stats) {
    
    if (!singleplot) {
      postPlot <- postPlot + 
        geom_segment(aes(xend = median(postDist), x = median(postDist),
                         y = -Inf, yend = ymax), size = 1.1, 
                         colour = "red3", linetype = "dashed") +
        geom_segment(aes(xend = median(priorDist), x = median(priorDist),
                         y = -Inf, yend = ymax), size = 1.1, 
                     colour = "blue", linetype = "dashed")
    } else {
      postPlot <- postPlot + 
        geom_segment(aes(xend = median(postDist), x = median(postDist),
                         y = -Inf, yend = ymax), size = 1.1, 
                     colour = "black", linetype = "solid") + 
        geom_segment(aes(xend = quantile(postDist, 0.025), 
                         x = quantile(postDist, 0.025),
                         y = -Inf, yend = ymax), size = 1.1, 
                     colour = "black", linetype = "dashed") +
        geom_segment(aes(xend = quantile(postDist, 0.975), 
                         x = quantile(postDist, 0.975),
                         y = -Inf, yend = ymax), size = 1.1, 
                     colour = "black", linetype = "dashed")
    }  
  }
  
  # Finish the plot
  postPlot <- postPlot +
    coord_cartesian(xlim = plotRange, ylim = c(0, yLimit)) + 
    ylab("Density") + 
    xlab(yLabel) + plotOpts +
    annotate("text", label = plotStats,
              x = Inf, y = Inf, hjust = 1, vjust = 1, alpha = 1)

  # Save plot if required
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
    xlab(paste("Percentage acquired from", cascadeStage)) +
    coord_cartesian(xlim = xlimits) +
    plotOpts + theme(legend.position = "right")
  } else {
    # Only a single year plotted so remove legend and fix colour
    distPlot <- ggplot(data = percentData, aes(x = percentage)) + 
      # geom_density(size = 1.2, colour = "blue", 
      #              fill = "blue", alpha = 0.2) +
      geom_line(size = 1.2, stat = "density", colour = "blue") +
      geom_vline(aes(xintercept = median(percentage)), size = 1.1) + 
      geom_vline(aes(xintercept = quantile(percentage, 0.025)), 
                 linetype = "dashed", size = 1.1) +
      geom_vline(aes(xintercept = quantile(percentage, 0.975)),
                 linetype = "dashed", size = 1.1) +
      ylab("Density") + 
      xlab(paste("Percentage acquired from", cascadeStage
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

