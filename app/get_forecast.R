# Function to retrieve forecast data for a specific matchday
# This function can be adapted later for database queries.
get_forecast <- function(season_end_year, league_id, matchday_value) {

  # file path from config
  file_path <- file.path("..", get_forecast_path(season_end_year, league_id, matchday_value))

  if (file.exists(file_path)) {
    tryCatch({
      data_table <- readRDS(file_path)
      return(data_table)
    }, error = function(e) {
      # Log error and return NULL or an error indicator
      message(paste("Error reading RDS file:", file_path, "-", e$message)) # Logs to console
      # You could also use shiny::showNotification here for immediate user feedback if needed
      return(NULL)
    })
  } else {
    # File not found, return NULL. The calling function can decide how to handle this.
    # message(paste("Data file not found:", file_path)) # Optional: log missing files
    return(NULL)
  }
}
