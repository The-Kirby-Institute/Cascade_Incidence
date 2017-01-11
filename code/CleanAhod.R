## Function to clean AHOD data 

# R. T. Gray

# This script cleans the AHOD suppression data to extract the proportion of 
# MSM with a viral load < 200 copies at last test and caclulates the 95%
# confidence interval.

rm(list=ls()) 

# Source to current directory and set working directory
basePath <- getwd()

# Various directories
dataFolder <- file.path(basePath, "data")

# Load standard libraries, key functions and options
source(file.path(Rcode, "LoadLibrary.R"), echo = TRUE)
source(file.path(Rcode, "DataLibraries.R"), echo = TRUE)

# Specify year of data
analysisYear <- 2015

# Read in the data
ahodData <- read.csv(file.path(dataFolder, toString(analysisYear),
  paste0("ahod", toString(analysisYear), ".csv")))

# Extract the viral suppression data and calculate the 95% confidence
# interval
propSuppressed <- ahodData %>%
  select(one_of(c("year", "state", "population", "n_id")), 
         starts_with("n_rx"), -n_rx) %>% 
  group_by(year, state, population) %>%
  summarise(n = sum(n_id),
            n200 = sum(n_rx200)) %>%
  mutate(prop200 = n200/n) %>%
  # Added 95% confidence interval
  mutate(prop200lower = prop200 - qnorm(0.975) * 
           sqrt(prop200 * (1 - prop200) / n),
         prop200upper = prop200 + qnorm(0.975) * 
           sqrt(prop200 * (1 - prop200) / n)) %>%
  ungroup() 

# Extract results for MSM
propSuppressedMSM <- filter(propSuppressed, population == "MSM", state == "all")

# Save to file
write.csv(propSuppressedMSM, file.path(dataFolder, toString(analysisYear),
  paste0("ahod_suppressed", toString(analysisYear), ".csv")))
  
