#!/usr/bin/env Rscript

# Load necessary libraries
library(readr)
library(dplyr)

# Source necessary functions
source("./R/predict_strength_pre_season.R")
source("./R/forecast_season.R")
source("./R/load_data.R")
source("./R/create_forecasts.R")

# Define which leagues and years to process
league_ids <- c(4, 9)  # Bundesliga and 2. Bundesliga
years <- c(2025)       # Season 2024/2025

# Run the forecast creation
create_forecasts(league_ids, years)

# Print completion message
cat("Forecast creation script completed.\n")
