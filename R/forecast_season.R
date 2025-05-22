
forecast_season <- function(games_current_season, strength_pred,
                            matchday, n_sims = 1000, update_rate = 0.05,
                            xG_weight = 1,
                            var_factor = 1.0,
                            variance_decay_param = 0.1,
                            mixture_weight_param = 0.05,
                            mixture_start_matchday = 3,
                            decimal_places = 4) {

  # Check if xG data is available
  has_xG <- all(c("Home_xG", "Away_xG") %in% names(games_current_season)) &&
    !all(is.na(games_current_season$Home_xG)) &&
    !all(is.na(games_current_season$Away_xG))

  # Set xG_weight to 0 if no xG data available
  if (!has_xG) {
    xG_weight <- 0
    message("xG data not available, setting xG_weight to 0")
  }

  # Load model for pre-season
  model <- model_from_params(games_current_season, strength_pred)

  # Store initial model for mixture modeling
  initial_model <- model

  # Updating model on games up to matchday
  if (matchday > 0) {
    for (week in seq(1, matchday)) {
      print(paste("Updating on results of week", week))
      results <- games_current_season %>% filter(Wk == week)

      # Only pass xG if available
      if (has_xG) {
        model <- update_parameters(results$Home, results$Away, results$HomeGoals,
                                   results$AwayGoals, results$Home_xG,
                                   results$Away_xG, model, update_rate, xG_weight)
      } else {
        model <- update_parameters(results$Home, results$Away, results$HomeGoals,
                                   results$AwayGoals, NULL, NULL,
                                   model, update_rate, 0)
      }
    }
  }

  # Calculate mixture model if conditions are met
  final_model <- model
  if (matchday >= mixture_start_matchday) {
    print(paste("Creating mixture model for matchday", matchday))

    # Fit Cole-Dixon model on games played so far
    games_played <- games_current_season %>%
      filter(Wk <= matchday, !is.na(HomeGoals), !is.na(AwayGoals))

    if (nrow(games_played) >= 10) {  # Minimum games to fit a reliable model
      cd_model <- goalmodel(goals1 = games_played$HomeGoals,
                            goals2 = games_played$AwayGoals,
                            team1 = games_played$Home,
                            team2 = games_played$Away,
                            rs = TRUE)

      # Calculate mixture weight: w_x = 1 - exp(-a_w^2) * x^2
      mixture_weight <- 1 - exp(-(mixture_weight_param^2) * matchday^2)
      print(paste("Mixture weight for CD model:", round(mixture_weight, 3)))

      # Create mixed model parameters
      final_model <- create_mixture_model(model, cd_model, mixture_weight)
    } else {
      print("Not enough games to fit Cole-Dixon model, using updating model only")
    }
  }

  # Calculate variance scaling factors
  # Variance decay: 1 / x^a_vd (for x > 0, use 1 for x = 0)
  variance_decay_factor <- if (matchday == 0) 1 else 1 / (matchday^variance_decay_param)

  # Apply variance scaling
  scaled_att_var <- strength_pred$att_var * var_factor * variance_decay_factor
  scaled_def_var <- strength_pred$def_var * var_factor * variance_decay_factor

  print(paste("Variance decay factor:", round(variance_decay_factor, 3)))
  print(paste("Scaled attack variance:", round(scaled_att_var, 4)))
  print(paste("Scaled defense variance:", round(scaled_def_var, 4)))

  matches_unfinished <- games_current_season %>% filter(Wk > matchday)
  games_up_to_now <- games_current_season %>% filter(Wk <= matchday)

  # Store base model for resampling
  base_model <- final_model

  # Simulate n seasons with resampled team strengths
  simulation_results <- vector("list", n_sims)
  for (i in 1:n_sims) {
    # Resample model parameters for each simulation with scaled variances
    resampled_model <- resample_model(base_model, scaled_att_var, scaled_def_var)

    # Make predictions with resampled model
    preds <- predict_expg(resampled_model, team1 = matches_unfinished$Home,
                          team2 = matches_unfinished$Away,
                          return_df = TRUE)

    # Simulate season with resampled model
    simulation_results[[i]] <- simulate_season(preds, games_up_to_now)
  }

  # Combine results from all simulations
  all_results <- bind_rows(simulation_results, .id = "simulation")

  # Calculate average standing and probabilities for each position
  n_teams <- length(unique(all_results$team))
  standing_probabilities <- all_results %>%
    group_by(team) %>%
    summarise(
      avg_standing = mean(standing),
      sd_standing = sd(standing)
    ) %>%
    arrange(avg_standing)

  # Calculate probabilities for each standing position
  prob_matrix <- matrix(0, nrow = n_teams, ncol = n_teams)
  for (i in 1:n_teams) {
    prob_matrix[, i] <- all_results %>%
      group_by(team) %>%
      summarise(prob = mean(standing == i)) %>%
      arrange(match(team, standing_probabilities$team)) %>%  # Ensure correct order
      pull(prob)
  }

  # Round probabilities to specified decimal places
  prob_matrix <- round(prob_matrix, decimal_places)

  # Add probability columns to the standing_probabilities dataframe
  standing_probabilities <- cbind(
    standing_probabilities,
    setNames(as.data.frame(prob_matrix), paste0("prob_", 1:n_teams))
  )

  # Round avg_standing and sd_standing to the specified decimal places
  standing_probabilities$avg_standing <- round(standing_probabilities$avg_standing, decimal_places)
  standing_probabilities$sd_standing <- round(standing_probabilities$sd_standing, decimal_places)

  return(standing_probabilities)
}


# Helper function to create mixture model
create_mixture_model <- function(updating_model, cd_model, mixture_weight) {
  # mixture_weight is the weight for the Cole-Dixon model
  # (1 - mixture_weight) is the weight for the updating model
  mixed_model <- updating_model  # Start with updating model structure

  # Get team names that exist in both models
  updating_teams <- names(updating_model$parameters$attack)
  cd_teams <- names(cd_model$parameters$attack)
  common_teams <- intersect(updating_teams, cd_teams)

  if (length(common_teams) == 0) {
    warning("No common teams between updating and CD models, returning updating model")
    return(updating_model)
  }

  # Mix attack parameters
  mixed_attack <- updating_model$parameters$attack
  mixed_attack[common_teams] <- (1 - mixture_weight) * updating_model$parameters$attack[common_teams] +
    mixture_weight * cd_model$parameters$attack[common_teams]

  # Mix defense parameters
  mixed_defense <- updating_model$parameters$defense
  mixed_defense[common_teams] <- (1 - mixture_weight) * updating_model$parameters$defense[common_teams] +
    mixture_weight * cd_model$parameters$defense[common_teams]

  # Mix other parameters (home advantage, intercept)
  mixed_hfa <- (1 - mixture_weight) * updating_model$parameters$hfa +
    mixture_weight * cd_model$parameters$hfa

  mixed_intercept <- (1 - mixture_weight) * updating_model$parameters$intercept +
    mixture_weight * cd_model$parameters$intercept

  # Update mixed model parameters
  mixed_model$parameters$attack <- mixed_attack
  mixed_model$parameters$defense <- mixed_defense
  mixed_model$parameters$hfa <- mixed_hfa
  mixed_model$parameters$intercept <- mixed_intercept

  return(mixed_model)
}


# Function to calculate points
calculate_points <- function(home_goals, away_goals) {
  if (home_goals > away_goals) {
    return(c(3, 0))  # Home win
  } else if (home_goals < away_goals) {
    return(c(0, 3))  # Away win
  } else {
    return(c(1, 1))  # Draw
  }
}


calculate_final_standing <- function(home, away, home_goals, away_goals) {
  # Determine the result of each match
  points <- t(mapply(calculate_points, home_goals, away_goals))
  home_points <- points[,1]
  away_points <- points[,2]

  season <- data.frame(Home = home, Away = away, HomeGoals = home_goals,
                       AwayGoals = away_goals, HomePoints = home_points,
                       AwayPoints = away_points)

  # Calculate points and goal difference for home games
  home_stats <- season %>%
    group_by(Home) %>%
    summarise(
      points = sum(HomePoints),
      goals_for = sum(HomeGoals),
      goals_against = sum(AwayGoals)
    )

  # Calculate points and goal difference for away games
  away_stats <- season %>%
    group_by(Away) %>%
    summarise(
      points = sum(AwayPoints),
      goals_for = sum(AwayGoals),
      goals_against = sum(HomeGoals)
    )

  # Combine home and away stats
  season_stats <- full_join(home_stats, away_stats, by = c("Home" = "Away")) %>%
    mutate(
      team = Home,
      points = points.x + points.y,
      goals_for = goals_for.x + goals_for.y,
      goals_against = goals_against.x + goals_against.y,
      goal_difference = goals_for - goals_against
    ) %>%
    select(team, points, goal_difference)

  # Sort by points (descending) and then by goal difference (descending)
  final_standings <- season_stats %>%
    arrange(desc(points), desc(goal_difference)) %>%
    mutate(standing = row_number()) %>%
    select(standing, team, points, goal_difference)

  return(final_standings)
}


simulate_season <- function(preds, season_finished){
  home <- c(season_finished$Home, preds$team1)
  away <- c(season_finished$Away, preds$team2)
  home_goals <- c(season_finished$HomeGoals, rpois(nrow(preds), lambda = preds$expg1))
  away_goals <- c(season_finished$AwayGoals, rpois(nrow(preds), lambda = preds$expg2))
  calculate_final_standing(home, away, home_goals, away_goals)
}


model_from_params <- function(games, parameters){
  week_1_dummy <- games %>% filter(Wk < 10)
  week_1_dummy$HomeGoals <- 1
  week_1_dummy$AwayGoals <- 0
  model <- goalmodel(goals1 = week_1_dummy$HomeGoals,
                     goals2 = week_1_dummy$AwayGoals,
                     team1 = week_1_dummy$Home,
                     team2 = week_1_dummy$Away,
                     rs = TRUE)

  # load model based on pre-season prediction
  model$parameters$defense <- parameters[["def_pred"]]
  model$parameters$attack <- parameters[["att_pred"]]
  model$parameters$gamma <- 0.05
  model$parameters$hfa <- parameters[["ha"]]
  model$parameters$intercept <- parameters[["intercept"]]
  model
}


# Modified resample_model function to use scaled variances
resample_model <- function(model, scaled_att_var, scaled_def_var){
  # Create a deep copy of the model to avoid modifying the original
  resampled_model <- model

  # Get current attack and defense parameters
  att_mean <- model$parameters$attack
  def_mean <- model$parameters$defense

  # Sample new attack parameters from normal distribution with scaled variance
  resampled_attack <- rnorm(n = length(att_mean),
                            mean = att_mean,
                            sd = sqrt(scaled_att_var))
  names(resampled_attack) <- names(att_mean)

  # Sample new defense parameters from normal distribution with scaled variance
  resampled_defense <- rnorm(n = length(def_mean),
                             mean = def_mean,
                             sd = sqrt(scaled_def_var))
  names(resampled_defense) <- names(def_mean)

  # Update the model with resampled parameters
  resampled_model$parameters$attack <- resampled_attack
  resampled_model$parameters$defense <- resampled_defense

  return(resampled_model)
}


update_parameters <- function(home, away, home_goals, away_goals,
                              home_xG = NULL, away_xG = NULL,
                              model, update_size = 0.05, xG_weight = 1){
  # ignore xG if values are not passed or if they contain NA
  if (is.null(home_xG) || is.null(away_xG) ||
      any(is.na(home_xG)) || any(is.na(away_xG))) {
    xG_weight <- 0
  }

  preds <- predict_expg(model, home, away, return_df = TRUE)

  if (xG_weight > 0){
    home_goals <- (1 - xG_weight) * home_goals + xG_weight * home_xG
    away_goals <- (1 - xG_weight) * away_goals + xG_weight * away_xG
  }

  home_error <- preds$expg1 - home_goals
  away_error <- preds$expg2 - away_goals

  errors <- c(home_error, away_error)
  # which attack it belongs to
  names(errors) <- c(home, away)
  attack_updated <- model$parameters$attack - log(1 + (update_size * errors[names(model$parameters$attack)])) / 2

  names(errors) <- c(away, home)
  defense_updated <- model$parameters$defense + log(1 + (update_size * errors[names(model$parameters$defense)])) / 2

  # Update model parameters
  model$parameters$attack <- attack_updated
  model$parameters$defense <- defense_updated

  model
}
