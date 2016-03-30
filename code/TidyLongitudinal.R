## R function to convert a longitidinal matrix to tidy data frame

# R. T. Gray

TidyLongitudinal <- function(matrix, startYear, firstCol = "sim") {
  # This function converts longitudinal output in the form of a matrix where 
  # each row is an individual model simulation, sample, or category (such
  # as age group or population) and each column is a year (timestep)
  # for the simulation. Requires dplyr.
  #
  # Can be used for any matrix where the rows are age groups or different 
  # samples for example
  #
  # Useful for creating tidy dataframes to plot using ggplot
  
  # Extract some information from the matrix
  nsims <- nrow(matrix)
  nyears <- ncol(matrix)
  
  # Reshape the matrix into the appropriate data frame
  matrix <- cbind(1:nsims, matrix)
  df <- as.data.frame(matrix)
  df <- gather(df,"year","value",2:(nyears+1))
  df$year <- rep(startYear:(startYear + nyears - 1), each = nsims)
  colnames(df)[1] <- firstCol
  
  # Return final data frame
  return(df)
  
}
