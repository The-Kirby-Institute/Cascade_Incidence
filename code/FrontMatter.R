## FrontMatter.R

# This personal function is used to create a string of information regarding 
# the use version and package details for printing in documents.

# Richard T. Gray

FrontMatter <- function(packages, rstudio = NULL) {
  
  # String materials
  versionStr <- R.Version()$version
  time <- format(Sys.Date(), format="%Y-%m-%d")
  
  # Merge if text
  rstr <- paste("Version info: Code for this page was tested in", 
                versionStr)
  if (!is.null(rstudio)) {
    rstr <- paste(rstr, "\nUsing: Rstudio version", rstudio)
  }
  
  # Setup time string
  tstr <- paste("On:", time)
  
  # Put everything together
  finalStr <- paste(rstr, "\n", tstr, sep = "")
  
  return(finalStr)
}
