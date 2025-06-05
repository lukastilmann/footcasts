# Add this instead:
.onLoad <- function(libname, pkgname) {
  # Export CONFIG to package namespace when package loads
  assign("CONFIG", CONFIG, envir = parent.env(environment()))
}

#' Package Configuration Settings
#'
#' Configuration object containing league settings, forecast parameters, and file paths.
#'
#' @export
CONFIG <- NULL  # Will be populated by .onLoad()


#' Get path to forecast file
#' @param year Season end year
#' @param league_id League ID
#' @param matchday Matchday number
#' @return Full path to forecast file
#' @export
get_forecast_path <- function(year, league_id, matchday) {
  filename <- paste0("forecast_year_", year, "_league_", league_id, "_matchday_", matchday, ".rds")
  file.path(CONFIG$paths$forecasts, filename)
}

#' Get path to results file
#' @param country Country code (e.g., "FRA", "GER")
#' @param level League level (1, 2, 3, etc.)
#' @param year Season end year
#' @return Full path to results file
#' @export
get_results_path <- function(country, level, year) {
  filename <- paste0(country, "_", level, "_", year, ".rds")
  file.path(CONFIG$paths$results_data, filename)
}

#' Get path to strength prediction file
#' @param year Season end year
#' @param league_id League ID
#' @return Full path to strength prediction file
#' @export
get_strength_pred_path <- function(year, league_id) {
  filename <- paste0("strength_pred_year_", year, "_league_", league_id, ".rds")
  file.path(CONFIG$paths$strength_preds, filename)
}

#' Get path to market value data file
#' @param league_id League ID
#' @param year Season end year
#' @return Full path to market value file
#' @export
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


