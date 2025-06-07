CONFIG <- list(
  leagues = list(enabled_ids = c(1, 2, 3, 4, 5, 9)),
  seasons = list(current = 2025,
                 available = c(2025),
                 eval = c(2023, 2024),
                 model = c(2018, 2019, 2020, 2021, 2022)),
  forecast_params = list(n_sims = 1000,
                         update_rate = 0.0296,
                         var_factor = 0.5277,
                         variance_decay_param = 0.3098,
                         mixture_weight_param = 0.0573,
                         mixture_start_matchday = 4,
                         xG_weight = 1),
  paths = list(
    leagues_data = "data/leagues.csv",
    forecasts = "outputs/forecasts",
    results_data = "data/results",
    strength_preds = "data/processed/strength_preds",
    mv_data = "data/processed/squads_mv_adj",
    mv_consolidated = "data/mv/consolidated_mv_data.csv",
    table_regions = "data/table_regions.csv"
  )
)

# Save as internal data (not exported to users)
usethis::use_data(CONFIG, internal = TRUE, overwrite = TRUE)
