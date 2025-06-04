#!/usr/bin/env Rscript

# Define which leagues and years to process
league_ids <- CONFIG$leagues$enabled_ids  # league ids as set by config file
years <- unique(c(unlist(CONFIG$seasons))) # seasons for which to load results

# Save results locally
for (year in years){
  for (id in league_ids){
    print(paste("Saving results for league", id))
    write_results_data(id, year)
  }
}

# Print completion message
cat("Results download script complete.\n")
