## R function to extract legend from a ggplot
# 
# Function to extract the legend from a ggplot for manipulation in a 
# publication plot
#
# Obtained from the following forum: http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization#change-legend-position

GetLegend<-function(myggplot){
  # Exctract ggplot legend
  #
  # Args:
  #   myggplot: ggplot plot handle
  #   
  #   Returns:
  #     legend object as a separate plot object

  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
