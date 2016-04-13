## FrontMatter.R

# This personal function is used to create a string of information 
# regarding the use version and package details for printing in documents.

# Richard T. Gray

FrontMatter <- function(packages = NULL, rstudio = NULL) {
  # This function produces a string with useful information about the tools 
  # used for analysis. The returned string includes newline characters so 
  # requires cat() to display properly.
  
  # Args:
  #   packages: List of packages we want to display. Default is NULL saying
  #     we don't want to return any package information.
  #   rstudio: String specifying Rstudio version used. Default is NULL.
  #
  # Returns
  #   A string with the front matter information. By default returns R 
  #     version and time.
  #
  # -----------------------------------------------------------------------
  
  # String materials
  versionStr <- R.Version()$version
  time <- format(Sys.Date(), format="%Y-%m-%d")
  
  # Setup R/Rstudio string
  rstr <- paste("Version info: Code run in", 
                versionStr)
  if (!is.null(rstudio)) {
    rstr <- paste(rstr, "\nUsing: Rstudio version", rstudio)
  }
  
  # Setup time string
  tstr <- paste("On:", time)
  
  # Setup package string
  if (!is.null(packages)) {
    # loop through inputed list of packages
    pstr <- "With: "
    for (pkg in packages) {
      pstr <- paste(pstr, pkg, " ", packageVersion(pkg), "; ", sep = "")
    }
  }
  
  # Put everything together and return
  finalStr <- paste(rstr, "\n", tstr, "\n", pstr, sep = "")
  return(finalStr)
}
