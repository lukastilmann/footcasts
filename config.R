# config.R (at project root)

# Helper to handle relative paths from wherever the script is run
get_project_root <- function() {
  # If called from app/ or scripts/ directory
  if (file.exists("../config.R")) {
    return("..")
  }
  # If called from project root
  else if (file.exists("config.R")) {
    return(".")
  }
  else {
    stop("Cannot determine project root directory")
  }
}

root <- get_project_root()

CONFIG <- list(
  # League configuration
  leagues = list(
    enabled_ids = c(1, 2, 3, 4, 5, 9) # Enabled league ids
  ),

  seasons = list(
    current = 2025, # current season
    available = c(2025) # all available seasons
  ),

  forecast_params = list(
    n_sims = 1000, # Number of simulations to sample
    update_rate = 0.05, # Update rate on realized results
    xG_weight = 1 # Weight for xG as opposed to actual goals for updating
  ),

  # Paths configuration (relative to project root)
  paths = list(
    leagues_data = file.path(root, "data", "leagues.csv"),
    forecasts = file.path(root, "outputs", "forecasts"),  # Note: no trailing slash
    results_data = file.path(root, "data", "results"),    # Note: no trailing slash
    strength_preds = file.path(root, "data", "processed", "strength_preds"),
    mv_data = file.path(root, "data", "processed", "squads_mv_adj"),
    table_regions = file.path(root, "data", "table_regions.csv")
  )
)

# Path helper functions - all using file.path() for proper path construction

#' Get path to forecast file
#' @param year Season end year
#' @param league_id League ID
#' @param matchday Matchday number
#' @return Full path to forecast file
get_forecast_path <- function(year, league_id, matchday) {
  filename <- paste0("forecast_year_", year, "_league_", league_id, "_matchday_", matchday, ".rds")
  file.path(CONFIG$paths$forecasts, filename)
}

#' Get path to results file
#' @param country Country code (e.g., "FRA", "GER")
#' @param level League level (1, 2, 3, etc.)
#' @param year Season end year
#' @return Full path to results file
get_results_path <- function(country, level, year) {
  filename <- paste0(country, "_", level, "_", year, ".rds")
  file.path(CONFIG$paths$results_data, filename)
}

#' Get path to strength prediction file
#' @param year Season end year
#' @param league_id League ID
#' @return Full path to strength prediction file
get_strength_pred_path <- function(year, league_id) {
  filename <- paste0("strength_pred_year_", year, "_league_", league_id, ".rds")
  file.path(CONFIG$paths$strength_preds, filename)
}

#' Get path to market value data file
#' @param league_id League ID
#' @param year Season end year
#' @return Full path to market value file
get_mv_path <- function(league_id, year) {
  filename <- paste0("tm_mv_", league_id, "_", year, "_adj.csv")
  file.path(CONFIG$paths$mv_data, filename)
}

#' Create directory if it doesn't exist
#' @param path Directory path to create
create_dir_if_missing <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    message("Created directory: ", path)
  }
}

#' Initialize all required directories
#' @return Invisible TRUE if successful
initialize_directories <- function() {
  dirs_to_create <- c(
    CONFIG$paths$forecasts,
    CONFIG$paths$results_data,
    CONFIG$paths$strength_preds,
    dirname(CONFIG$paths$leagues_data),  # data directory
    dirname(CONFIG$paths$table_regions)  # data directory
  )

  sapply(dirs_to_create, create_dir_if_missing)
  invisible(TRUE)
}
