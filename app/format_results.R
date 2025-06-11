format_match_results <- function(results_df) {
  # Check if the input is a data frame with a message (error case)
  if ("Message" %in% names(results_df) && ncol(results_df) == 1) {
    return(results_df)
  }

  # Check if we have the required columns
  required_cols <- c("Home", "Away", "HomeGoals", "AwayGoals")
  if (!all(required_cols %in% names(results_df))) {
    return(data.frame(Message = "Results data is missing required columns for formatting."))
  }

  # Only showing results of games that have been finished
  results_df <- results_df[!is.na(results_df$HomeGoals) & !is.na(results_df$AwayGoals), ]

  # If no finished games, return empty data frame
  if (nrow(results_df) == 0) {
    return(data.frame(Home = character(0), Score = character(0), Away = character(0)))
  }

  # Create separate columns for better alignment
  formatted_results <- data.frame(
    Home = results_df$Home,
    Score = paste0("<b>", results_df$HomeGoals, "</b> : <b>", results_df$AwayGoals, "</b>"),
    Away = results_df$Away,
    stringsAsFactors = FALSE
  )

  # Add xG columns if they exist
  if (all(c("Home_xG", "Away_xG") %in% names(results_df))) {
    formatted_results$xG <- paste(
      formatC(results_df$Home_xG, format = "f", digits = 1), ":",
      formatC(results_df$Away_xG, format = "f", digits = 1)
    )
  }

  DT::datatable(
    formatted_results,
    escape = FALSE,  # Allow HTML formatting
    rownames = FALSE,
    options = list(
      dom = 't',  # Show only table
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      info = FALSE,
      columnDefs = list(
        list(className = 'dt-right', targets = 0),  # Right align home team
        list(className = 'dt-center', targets = 1), # Center align score
        list(className = 'dt-left', targets = 2),   # Left align away team
        list(width = '40%', targets = 0),           # Home team column width
        list(width = '10%', targets = 1),           # Score column width (narrow)
        list(width = '40%', targets = 2),           # Away team column width
        list(width = '10%', targets = 3)            # xG column width
      )
    )
  )
}
