## SaveFigure.R

# Wrapper function so we don't have to keep entering everything in
# ggsave. Requires ggplot2 to be loaded.

# R. T. Gray

library("ggplot2")

SaveFigure <- function(folder, filename, figure, 
                       format = ".png", width = 15, 
                       height = 10, units = "cm", ...) {
  
  ggsave(file.path(folder, paste(filename, format, sep = "")), 
         plot = figure, 
         width = width, height = height, units = units, ...)
}
