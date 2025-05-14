source("./R/predict_strength_pre_season.R")
source("./R/forecast_season.R")

# Read leagues data
leagues <- read_csv("./data/leagues.csv", show_col_types = FALSE)
subset <- c(4, 9)
leagues <- leagues[subset, ]

# matchdays to simulate
matchdays <- seq(0,33)

for (league_id in leagues$id){
  print(paste("Pre-season pred for league", league_id))
  strength_pred <- predict_strength_pre_season(leagues, league_id, 2025, 5, "./Footcasts/data/squads_mv_adj")
  filename <- paste0("./Footcasts/data/strength_preds/strength_pred_year_", 2025, "_league_", league_id, ".rds")
  saveRDS(strength_pred, filename)
}


for (league_id in leagues$id){
  strength_pred <- readRDS(paste0("./Footcasts/data/strength_preds/strength_pred_year_2025_league_", league_id, ".rds"))

  league <- leagues[leagues$id == league_id, ]
  results <- load_results(league$country, league$level, 2025)

  for (matchday in matchdays){
    forecast <- forecast_season(results, strength_pred, matchday, 1000, 0.05, 1)
    filename <- paste0("./Footcasts/data/forecasts/forecast_year_", 2025, "_league_", league_id, "_matchday_", matchday, ".rds")
    saveRDS(forecast, filename)
  }
}

