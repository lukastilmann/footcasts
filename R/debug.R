source("./R/predict_strength_pre_season.R")
source("./R/forecast_season.R")
source("./R/load_data.R")

# Read leagues data
leagues <- read_csv("./data/leagues.csv", show_col_types = FALSE)

strength_pred_bl <- predict_strength_pre_season(leagues, 4, 2023, 5, "./data/squads_mv_adj")

#evaluate_and_plot_update_rates(leagues, 9, 2024, strength_pred_bl2, c(0.03, 0.04, 0.05, 0.06, 0.07),
#                                1, 100)
data <- load_results("GER", 1, 2023, only_regular_season = TRUE)
forecast_bl <- forecast_season(data, strength_pred_bl, 1, 1000, 0.05, 1)
#
# summary(strength_pred$att_model)
as_tibble(load_match_results("GER", "M", 2024, "2nd"))

df_res <- load_match_results("SWE", "M", 2025, "1st")
df_res <- fb_match_results("GER", "M", 2022, "1st")
df_res$Wk <- as.numeric(df_res$Wk)



mv_test <- load_mv(9, 2025)
