score_updating <- function(games_current_season, strength_pred, update_rate = 0.05,
                           xG_weight = 1, n_sims){

  # load model for pre-season
  model <- model_from_params(games_current_season, strength_pred)

  mse_vector <- numeric(max(games_current_season$Wk))

  for (week in seq(1, max(games_current_season$Wk))){
    results <- games_current_season %>% filter(Wk == week)

    model <- update_parameters(results$Home, results$Away, results$HomeGoals,
                               results$AwayGoals, results$Home_xG, results$Away_xG,
                               model, update_rate, xG_weight)

    matches_unfinished <- games_current_season %>% filter(Wk > week)
    games_up_to_now <- games_current_season %>% filter(Wk <= week)
    preds <- predict_expg(model, team1 = matches_unfinished$Home,
                          team2 = matches_unfinished$Away,
                          return_df = TRUE)

    simulation_results <- replicate(n_sims,
                                    simulate_season(preds, games_up_to_now),
                                    simplify = FALSE)

    # Combine results from all simulations
    all_results <- bind_rows(simulation_results, .id = "simulation")

    # Calculate average points for each team
    average_points <- all_results %>%
      group_by(team) %>%
      summarise(avg_points = mean(points),
                sd_points = sd(points)) %>%
      arrange(desc(avg_points))

    final_table <- calculate_final_standing(games_current_season$Home,
                                            games_current_season$Away,
                                            games_current_season$HomeGoals,
                                            games_current_season$AwayGoals)

    final_table_preds <- merge(final_table, average_points, by = "team")
    mse <- mean((final_table_preds$points - final_table_preds$avg_points)^2)

    mse_vector[week] <- mean((final_table_preds$points - final_table_preds$avg_points)^2)

  }

  return(mse_vector)
}


evaluate_and_plot_update_rates <- function(leagues, league_id, year, strength_pred,
                                           update_rates, xG_weights, n_sims) {

  league <- leagues[leagues$id == league_id, ]
  country <- league$country
  level <- levels[league$level]

  # load season games
  games_current_season <- as_tibble(load_match_results(country, "M", year, level))%>%
    filter(Round == "Regular season")
  games_current_season$Wk <- as.numeric(games_current_season$Wk)

  # Check which parameter is a vector
  if (length(update_rates) > 1 && length(xG_weights) > 1) {
    stop("Only one of update_rates or xG_weights should be a vector")
  }

  if (length(update_rates) > 1) {
    param <- "Update Rate"
    # Iterate over update_rates
    results <- list()
    for (rate in update_rates) {
      mse_vector <- score_updating(games_current_season, strength_pred, rate, xG_weights, n_sims)
      results[[as.character(rate)]] <- mse_vector
    }
  } else if (length(xG_weights) > 1) {
    param <- "xG Weight"
    # Iterate over xG_weights
    results <- list()
    for (weight in xG_weights) {
      mse_vector <- score_updating(games_current_season, strength_pred, update_rates, weight, n_sims)
      results[[as.character(weight)]] <- mse_vector
    }
  }

  # Convert the list to a data frame
  df <- data.frame(
    week = rep(1:length(results[[1]]), times = length(results)),
    mse = unlist(results),
    update_rate = rep(names(results), each = length(results[[1]]))
  )

  # Calculate average MSE for each update rate
  avg_mse <- df %>%
    group_by(update_rate) %>%
    summarise(avg_mse = mean(mse))

  # Create the plot
  plot <- ggplot(df, aes(x = week, y = mse, color = update_rate)) +
    geom_line() +
    scale_color_viridis(discrete = TRUE) +  # Use viridis color palette
    theme_minimal() +
    labs(title = "MSE by Week for Different Update Rates",
         x = "Week",
         y = "Mean Squared Error",
         color = param)

  # Return the results, plot, and average MSE
  return(list(results = results, plot = plot, avg_mse = avg_mse))
}
