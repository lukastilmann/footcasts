CONFIG <- list(
  leagues = list(enabled_ids = c(1, 2, 3, 4, 5, 9)),
  seasons = list(current = 2025, available = c(2025)),
  forecast_params = list(n_sims = 1000, update_rate = 0.05, xG_weight = 1),
  paths = list(
    leagues_data = "data/leagues.csv",
    forecasts = "outputs/forecasts",
    results_data = "data/results",
    strength_preds = "data/processed/strength_preds",
    mv_data = "data/processed/squads_mv_adj",
    mv_consolidated = "data/processed/squads_mv_adj/consolidated_mv_data.rds",
    table_regions = "data/table_regions.csv"
  )
)

# Save as internal data (not exported to users)
usethis::use_data(CONFIG, internal = TRUE)
