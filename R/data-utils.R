#' Write Match Results Data to Local Storage
#'
#' Fetches match results for a specific league and year using worldfootballR and
#' saves them locally as RDS files for faster future access.
#'
#' @param id Integer league ID corresponding to leagues data
#' @param year Integer season end year (e.g., 2024 for 2023-24 season)
#'
#' @return Invisible path to the saved file
#' @export
#'
#' @details
#' The function retrieves league information from the configured leagues data,
#' fetches match results via worldfootballR::fb_match_results(), and saves
#' the data using the standardized file path convention. This enables the
#' load_results() function to use cached local data instead of making API calls.
#'
#' @seealso \code{\link{load_results}} for loading the cached results data
write_results_data <- function(id, year) {
  # Read leagues data using CONFIG path
  leagues <- readr::read_csv(CONFIG$paths$leagues_data, show_col_types = FALSE)
  league <- leagues[leagues$id == id, ]

  # Check if league exists
  if (nrow(league) == 0) {
    stop("League ID ", id, " not found in leagues data")
  }

  # Define tier levels
  levels <- c("1st", "2nd", "3rd")

  # Fetch match results
  data <- worldfootballR::fb_match_results(
    country = league$country,
    gender = "M",
    season_end_year = year,
    tier = levels[league$level]
  )

  # Generate output path using helper function
  output_path <- get_results_path(league$country, league$level, year)

  # Create directory if it doesn't exist
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  # Save data
  readr::write_rds(data, output_path)

  message("Saved results data to: ", output_path)
  invisible(output_path)
}
