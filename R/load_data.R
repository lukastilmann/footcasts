#' Load Football Match Results Data
#'
#' Loads match results for a specific country, league level, and season.
#' Attempts to load from local cache first, falls back to fetching from worldfootballR.
#'
#' @param end_year Integer or vector of season end years to load
#' @param country Character string of country code (e.g., "GER", "ENG")
#' @param level Integer between 1-4 indicating league tier (1 = top division)
#' @param league_id Integer indicating leage_id in leagues file.
#' @param only_regular_season Logical whether to filter to regular season games only (default: TRUE)
#' @param from_app Logical indicating whether function is called from shiny app,
#'   in which case the working directory is different, and ".." needs to be
#'   appended to path.
#'
#' @return Data frame with match results containing columns: Season_End_Year, Home, Away,
#'   HomeGoals, AwayGoals, Round, Wk, and optionally Home_xG, Away_xG
#' @export
load_results <- function(end_year, country = NULL, level = NULL, league_id = NULL,
                         only_regular_season = TRUE, from_app = FALSE) {

  if (is.null(level) & (is.null(country) || is.null(level))){
    stop("Both league_id and country or level are null.")
  }

  if (!is.null(league_id)){
    leagues <- load_leagues(from_app = from_app)
    league <- leagues[leagues$id == league_id]
    level <- as.numeric(league$level)
    country <- league$country
  }

  levels <- c("1st", "2nd", "3rd", "4th")

  # Input checks for level
  if (!level %in% 1:4) {
    stop("level must be an integer between 1 and 4")
  }

  load_local = FALSE

  if (length(end_year) == 1){
    # If file exists, load it from local storage
    # First, check if the data exists locally
    file_path <- get_results_path(country, level, end_year)

    if (from_app){
      file_path <- file.path("..", file_path)
    }

    if (file.exists(file_path)) {
      load_local = TRUE
    }
  }

  if (load_local){
    data <- readRDS(file_path)
  } else {
    data <- worldfootballR::fb_match_results(
      country = country,
      gender = "M",
      season_end_year = end_year,
      tier = levels[level]
    )
  }

  # Check if any data is actually returned
  if (is.null(data) || nrow(data) == 0) {
    warning("No data returned for the specified parameters")
    return(NULL)
  }

  # Select columns
  required_cols <- c("Season_End_Year", "Home", "Away", "HomeGoals", "AwayGoals", "Round", "Wk")
  optional_cols <- c("Home_xG", "Away_xG")

  # Check which columns exist in the data
  available_cols <- required_cols[required_cols %in% names(data)]
  available_optional <- optional_cols[optional_cols %in% names(data)]

  # Combine all columns to select
  cols_to_select <- c(available_cols, available_optional)

  # Select the columns
  data <- data[, cols_to_select]

  # correct types
  data$Wk <- as.numeric(data$Wk)

  # Select regular season
  if (only_regular_season){
    data <- data[!is.na(data$Wk), ]
  }

  return(data)
}

#' Load Market Value Data
#'
#' Loads consolidated market value data from RDS file, filtered by league and year.
#' Data includes player market values, ages, and team information.
#'
#' @param year Integer or vector of season end years (required)
#' @param league_id Integer or vector of league IDs to filter (optional).
#'   If NULL, returns data for all leagues.
#' @param from_app Logical indicating whether function is called from shiny app,
#'   in which case the working directory is different, and ".." needs to be
#'   appended to path.
#'
#' @return Data frame with market value data containing player information,
#'   market values, ages, team assignments, league_id, and season_end_year
#' @importFrom readr read_csv
#'
#' @export
load_mv <- function(year, league_id = NULL, from_app = FALSE) {
  # Define the path to the consolidated RDS file
  mv_file <- CONFIG$paths$mv_consolidated

  if (from_app){
    mv_file <- file.path("..", mv_file)
  }

  # Check if the RDS file exists
  if (!file.exists(mv_file)) {
    stop(paste("Consolidated MV data file not found:", mv_file))
  }

  # Load the consolidated data
  mv_data <- read_csv(mv_file, show_col_types = FALSE)

  # Filter by year
  if (missing(year) || is.null(year)) {
    stop("year parameter is required")
  }

  # Ensure year is numeric
  year <- as.numeric(year)

  # Filter for the specified year(s)
  mv_data <- mv_data %>%
    filter(season_end_year %in% year)

  # Filter by league_id if provided
  if (!is.null(league_id)) {
    # IMPORTANT: Ensure league_id is character to match the data type in the dataframe
    league_id <- as.character(league_id)

    # Also ensure the league_id column in the data is character
    mv_data <- mv_data %>%
      mutate(league_id = as.character(league_id)) %>%
      filter(league_id %in% !!league_id)
  }

  # Check if any data remains after filtering
  if (nrow(mv_data) == 0) {
    warning("No data found for the specified league_id(s) and year(s)")
  }

  return(mv_data)
}

#' Load Leagues
#'
#' Loads file with info on leagues.
#'
#' @param from_app Logical indicating whether function is called from shiny app,
#'   in which case the working directory is different, and ".." needs to be
#'   appended to path.
#'
#' @return Data frame leagues data.
#'
#' @export
load_leagues <- function(from_app = FALSE){
  leagues_path <- CONFIG$paths$leagues_data

  if (from_app){
    leagues_path <- file.path("..", leagues_path)
  }

  read_csv(leagues_path, show_col_types = FALSE)
}
