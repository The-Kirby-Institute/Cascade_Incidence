## PlotOptions.R

# This script stores all my favoured plotting options and colours
# It just needs to be sourced in a script for use. 

# R. T. Gray 

# Default plot specifications
graphics.off()
# Load ggplot2
require(ggplot2)

# Baseline theme for plot variables
plotOpts <- theme_bw() + theme(text = element_text(face = "bold",size=12,colour="black"),
                               axis.text.x = element_text(face = "plain",size=10,colour="black"),
                               axis.text.y = element_text(face = "plain",size=10,colour="black"),
                               axis.line = element_line(colour="black"),
                               axis.ticks = element_line(colour="black"),
                               legend.position = "top",
                               legend.background = element_rect(),
                               legend.key = element_blank(),
                               panel.grid.major = element_blank(), 
                               panel.grid.minor = element_blank(), 
                               panel.background = element_blank(), 
                               panel.border = element_rect(colour = "black"),
                               axis.line = element_line(colour = "black"),
                               plot.title=element_text(size=12, face="bold"),
                               strip.background = element_blank()
)

# Setup colours
# Palette with black and grey (colour blind friendly)
cbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# maincols <- brewer.pal(4,"Set1")

# Colour scheme used in ASR
asrcols <- c("#621C20", "#AB2322", "#A63603", "#D95728", "#E97164")

