## FrontMatter.R

# This personal function is used to create a string of information regarding 
# the use version and package details for printing in documents.

# Richard T. Gray

FrontMatter <- function(packages = NULL, rstudio = NULL) {
  
  # String materials
  versionStr <- R.Version()$version
  time <- format(Sys.Date(), format="%Y-%m-%d")
  
  # Setup R/Rstudio string
  rstr <- paste("Version info: Code for this page was tested in", 
                versionStr)
  if (!is.null(rstudio)) {
    rstr <- paste(rstr, "\nUsing: Rstudio version", rstudio)
  }
  
  # Setup time string
  tstr <- paste("On:", time)
  
  # Setup package string
  if (!is.null(packages)) {
    # loop through 
  }
  
  # Put everything together
  finalStr <- paste(rstr, "\n", tstr, sep = "")
  
  return(finalStr)
}
