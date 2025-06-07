#!/usr/bin/env Rscript

# Load necessary libraries

# Define which leagues and years to process
league_ids <- CONFIG$leagues$enabled_ids  # league ids as set by config file
years <- c(CONFIG$seasons$available)       # seasons for which to generate forecasts

# Run the forecast creation
create_forecasts(league_ids, years)

# Print completion message
cat("Forecast creation script completed.\n")
