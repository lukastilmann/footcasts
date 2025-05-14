# Function to format match results in a cleaner way
format_match_results <- function(results_df) {
  # Check if the input is a data frame with a message (error case)
  if ("Message" %in% names(results_df) && ncol(results_df) == 1) {
    return(results_df)  # Return the error message intact
  }

  # Check if we have the required columns
  required_cols <- c("Home", "Away", "HomeGoals", "AwayGoals")
  if (!all(required_cols %in% names(results_df))) {
    return(data.frame(Message = "Results data is missing required columns for formatting."))
  }

  # Only showing results of games that have been finished
  results_df <- results_df[!is.na(results_df$HomeGoals) & !is.na(results_df$AwayGoals), ]

  # Create a new data frame with formatted results
  formatted_results <- data.frame(
    Match = paste(results_df$Home, results_df$HomeGoals, ":", results_df$AwayGoals, results_df$Away),
    stringsAsFactors = FALSE
  )

  # Add xG columns if they exist
  if (all(c("Home_xG", "Away_xG") %in% names(results_df))) {
    formatted_results$xG <- paste(
      round(results_df$Home_xG, 2), ":", round(results_df$Away_xG, 2)
    )
  }

  return(formatted_results)
}
