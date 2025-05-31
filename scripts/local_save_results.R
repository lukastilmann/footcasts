#!/usr/bin/env Rscript

# Define which leagues and years to process
league_ids <- CONFIG$leagues$enabled_ids  # league ids as set by config file
years <- c(CONFIG$seasons$available)       # seasons for which to load results

# Run the forecast creation
for (year in years){
  for (id in league_ids){
    print(paste("Saving results for league", id))
    write_results_data(id, year)
  }
}

# Print completion message
cat("Results download script complete.\n")
