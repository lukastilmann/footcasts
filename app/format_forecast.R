source("../config.R")

# R/format_forecast_table.R
format_forecast_table <- function(raw_forecast_df, league_id, season = NULL) {

  # Loading path for league regions from config file
  regions_config_path <- CONFIG$paths$table_regions
  # Helper for %||% (provides a default if a value is NULL)
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # Handle cases where raw_forecast_df might be a message data.frame or empty
  if (is.null(raw_forecast_df) || nrow(raw_forecast_df) == 0 ||
      ("Message" %in% colnames(raw_forecast_df) && ncol(raw_forecast_df) == 1)) {
    return(raw_forecast_df %||% data.frame(Message = "No forecast data available to display."))
  }

  # Ensure essential columns are present in the raw data
  required_cols <- c("team", "avg_standing")
  if (!all(required_cols %in% names(raw_forecast_df))) {
    warning("Raw forecast data is missing essential columns (e.g., 'team', 'avg_standing').")
    return(data.frame(Message = "Forecast data structure is incorrect."))
  }

  # Load regions configuration
  if (!file.exists(regions_config_path)) {
    warning("Regions configuration file not found: ", regions_config_path)
    return(data.frame(Message = "League regions configuration not available."))
  }

  regions_config <- tryCatch({
    read.csv(regions_config_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Error reading regions configuration: ", e$message)
    return(NULL)
  })

  if (is.null(regions_config)) {
    return(data.frame(Message = "Could not read league regions configuration."))
  }

  # Filter regions for the specified league and season
  league_regions <- regions_config[regions_config$id == league_id &
                                     regions_config$year == season, ]

  if (nrow(league_regions) == 0) {
    warning("No region definitions found for league_id '", league_id,
            "' and season '", season, "'")
    return(data.frame(Message = paste0("No region definitions available for ",
                                       league_id, " (", season, ").") ))
  }

  # Identify all available probability columns (e.g., prob_1, prob_2, ..., prob_n)
  prob_cols_available <- grep("^prob_\\d+$", names(raw_forecast_df), value = TRUE)

  # Initialize the display dataframe
  display_df <- data.frame(
    Team = raw_forecast_df$team,
    `Average Standing` = round(raw_forecast_df$avg_standing, 1) # Rounded to 1 decimal place
  )

  rownames(display_df) <- 1:nrow(display_df)

  # Calculate probabilities for each region
  for (i in 1:nrow(league_regions)) {
    region <- league_regions[i, ]
    region_name <- region$region_name
    start_pos <- region$start_pos
    end_pos <- region$stop_pos

    # Construct column names for positions in this region
    region_prob_col_names <- paste0("prob_", start_pos:end_pos)

    # Filter for only those probability columns that actually exist in the raw data
    valid_region_cols <- intersect(region_prob_col_names, prob_cols_available)

    if (length(valid_region_cols) > 0) {
      # Ensure columns are numeric before summing
      region_data_for_sum <- sapply(
        raw_forecast_df[, valid_region_cols, drop = FALSE],
        function(col) suppressWarnings(as.numeric(col))
      )

      # Calculate sum of probabilities for this region
      if (is.matrix(region_data_for_sum)) {
        region_probs_sum <- rowSums(region_data_for_sum, na.rm = TRUE)
      } else {
        region_probs_sum <- region_data_for_sum
        region_probs_sum[is.na(region_probs_sum)] <- 0
      }

      # Add region probability to display dataframe with formatting
      column_name <- region_name
      display_df[[column_name]] <- ifelse(
        is.na(region_probs_sum),
        "-",
        paste0(as.character(round(region_probs_sum * 100)), "%")  # Integer percentage
      )
    } else {
      # No valid probability columns for this region
      column_name <- region_name
      display_df[[column_name]] <- "-"
    }
  }

  return(display_df)
}
