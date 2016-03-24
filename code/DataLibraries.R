## DataLibraries.R

# This personal script just loads all the standard R libraries I use for data 
# analysis. It just needs to be sourced in a script for use. 

# The script relies on the load.library.R script being sourced

# R. T. Gray 

# Libraries for data manipulation
LoadLibrary(dplyr)
LoadLibrary(tidyr)
LoadLibrary(utils)
LoadLibrary(readxl)

# Libraries for output
LoadLibrary(ggplot2)
LoadLibrary(RColorBrewer)
LoadLibrary(Hmisc)
LoadLibrary(knitr)
LoadLibrary(gridExtra)
