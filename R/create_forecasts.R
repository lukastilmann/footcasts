#' Create Season Forecasts for Multiple Leagues and Years
#'
#' Generates pre-season strength predictions and incremental forecasts for each
#' completed matchday across specified leagues and years. Saves all outputs to disk
#' and skips existing files to enable resumable batch processing.
#'
#' @param league_ids Integer vector of league IDs to process
#' @param years Integer vector of years (season end years) to process
#'
#' @return NULL (invisible). Function called for side effects of creating forecast files.
#' @export
#'
#' @details
#' For each league-year combination:
#' \itemize{
#'   \item Creates or loads pre-season strength predictions
#'   \item Identifies completed matchdays (where all fixtures are finished)
#'   \item Generates incremental forecasts from matchday 0 (pre-season) through last completed matchday
#'   \item Saves forecasts as RDS files using standardized file paths
#' }
#'
#' Uses fixed simulation parameters: 1000 simulations, 0.05 update rate, xG weight of 1.
#' Creates necessary output directories automatically.
create_forecasts <- function(league_ids, years) {
  # Read leagues data
  leagues <- read_csv(CONFIG$paths$leagues_data, show_col_types = FALSE)
  leagues <- leagues[leagues$id %in% league_ids, ]

  # Create directories if they don't exist
  dir.create("./data/processed/strength_preds", recursive = TRUE, showWarnings = FALSE)
  dir.create("./outputs/forecasts", recursive = TRUE, showWarnings = FALSE)

  # Process each year
  for (year in years) {
    # Create strength predictions for each league
    for (league_id in leagues$id) {
      print(paste("Processing league", league_id, "for year", year))

      # Check if strength prediction exists
      strength_pred_file <- get_strength_pred_path(year, league_id)

      if (!file.exists(strength_pred_file)) {
        print(paste("Creating strength prediction for league", league_id, "year", year))
        strength_pred <- predict_strength_pre_season(leagues, league_id, year, 5)
        saveRDS(strength_pred, strength_pred_file)
      } else {
        print(paste("Strength prediction already exists for league", league_id, "year", year))
      }

      # Load strength prediction
      strength_pred <- readRDS(strength_pred_file)

      # Get league info and results
      league <- leagues[leagues$id == league_id, ]
      results <- load_results(league$country, league$level, year)

      # Find last completed matchday
      if (nrow(results) > 0) {
        # Filter to include only rows where both HomeGoals and AwayGoals are not NA
        completed_games <- results %>%
          filter(!is.na(HomeGoals) & !is.na(AwayGoals))

        uncompleted_games <- results %>%
          filter(is.na(HomeGoals) & is.na(AwayGoals))

        if (nrow(completed_games) > 0) {
          # Only matchdays where all games are completed
          # TODO: At some point, updating and simulating shouldn't be dependant
          # on fully completed matchdays
          completed_weeks <- completed_games$Wk[!completed_games$Wk %in% uncompleted_games$Wk]
          max_matchday <- max(completed_weeks, na.rm = TRUE)
          matchdays <- 0:max_matchday  # Include pre-season (0) up to last completed matchday

          print(paste("Creating forecasts for matchdays 0 to", max_matchday))

          # Create forecasts for each matchday
          for (matchday in matchdays) {
            forecast_file <- get_forecast_path(year, league_id, matchday)

            if (!file.exists(forecast_file)) {
              print(paste("Creating forecast for matchday", matchday))
              forecast <- forecast_season(results, strength_pred, matchday, 1000, 0.05, 1)
              saveRDS(forecast, forecast_file)
            } else {
              print(paste("Forecast already exists for matchday", matchday))
            }
          }
        } else {
          print("No completed games found. Creating pre-season forecast only.")
          forecast_file <- get_forecast_path(year, league_id, 0)

          if (!file.exists(forecast_file)) {
            forecast <- forecast_season(results, strength_pred, 0, 1000, 0.05, 1)
            saveRDS(forecast, forecast_file)
          } else {
            print("Pre-season forecast already exists.")
          }
        }
      } else {
        print("No results data found.")
      }
    }
  }

  print("Forecast creation complete.")
}
