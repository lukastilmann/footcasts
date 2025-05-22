#!/usr/bin/env Rscript

source("./config.R")

# Load necessary libraries
library(readr)
library(dplyr)

# Source necessary functions
source("./R/predict_strength_pre_season.R")
source("./R/forecast_season.R")
source("./R/load_data.R")
source("./R/create_forecasts.R")

# Define which leagues and years to process
league_ids <- CONFIG$leagues$enabled_ids  # league ids as set by config file
years <- c(CONFIG$seasons$available)       # seasons for which to generate forecasts

# Run the forecast creation
create_forecasts(league_ids, years)

# Print completion message
cat("Forecast creation script completed.\n")
