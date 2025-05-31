#' Score a single forecast using Ranked Probability Score
#'
#' @param forecast_df Data frame with columns: team, prob_1, prob_2, ..., prob_n
#' @param final_standings Data frame with columns: team, standing (final position)
#' @return Numeric RPS score (lower is better)
#' @export
score_forecast_rps <- function(forecast_df, final_standings) {
  # Validate inputs
  if (!all(c("team") %in% names(forecast_df))) {
    stop("forecast_df must contain 'team' column")
  }

  if (!all(c("team", "standing") %in% names(final_standings))) {
    stop("final_standings must contain 'team' and 'standing' columns")
  }

  # Get probability columns
  prob_cols <- grep("^prob_\\d+$", names(forecast_df), value = TRUE)
  if (length(prob_cols) == 0) {
    stop("No probability columns (prob_1, prob_2, ...) found in forecast_df")
  }

  # Extract position numbers and sort
  positions <- as.numeric(sub("prob_", "", prob_cols))
  positions <- sort(positions)
  n_positions <- length(positions)

  # Check all teams in final standings are in forecast
  if (!all(final_standings$team %in% forecast_df$team)) {
    missing_teams <- setdiff(final_standings$team, forecast_df$team)
    stop(paste("Teams in final_standings not found in forecast:",
               paste(missing_teams, collapse = ", ")))
  }

  # Calculate RPS for each team
  team_rps_scores <- numeric(nrow(final_standings))

  for (i in 1:nrow(final_standings)) {
    team_name <- final_standings$team[i]
    actual_position <- final_standings$standing[i]

    # Get forecast probabilities for this team
    team_forecast <- forecast_df[forecast_df$team == team_name, ]

    if (nrow(team_forecast) == 0) {
      stop(paste("Team", team_name, "not found in forecast"))
    }

    # Extract probabilities in order
    forecast_probs <- numeric(n_positions)
    for (j in 1:n_positions) {
      col_name <- paste0("prob_", positions[j])
      forecast_probs[j] <- team_forecast[[col_name]]
    }

    # Handle any NA values (shouldn't happen but defensive)
    if (any(is.na(forecast_probs))) {
      warning(paste("NA values in forecast probabilities for team", team_name))
      forecast_probs[is.na(forecast_probs)] <- 0
    }

    # Normalize probabilities if they don't sum to 1 (due to rounding)
    prob_sum <- sum(forecast_probs)
    if (abs(prob_sum - 1) > 1e-6) {
      if (prob_sum == 0) {
        warning(paste("All probabilities are 0 for team", team_name))
        # Assign uniform distribution as fallback
        forecast_probs <- rep(1/n_positions, n_positions)
      } else {
        forecast_probs <- forecast_probs / prob_sum
      }
    }

    # Calculate cumulative forecast probabilities
    cum_forecast <- cumsum(forecast_probs)

    # Create cumulative actual (0,0,...,1,1,1)
    cum_actual <- rep(0, n_positions)
    if (actual_position <= n_positions) {
      cum_actual[actual_position:n_positions] <- 1
    } else {
      warning(paste("Actual position", actual_position, "for team", team_name,
                    "exceeds number of positions", n_positions))
      # Team finished worse than last position in forecast (shouldn't happen)
      cum_actual <- rep(0, n_positions)
    }

    # Calculate RPS for this team
    team_rps <- sum((cum_forecast - cum_actual)^2)
    team_rps_scores[i] <- team_rps
  }

  # Return average RPS across all teams
  mean_rps <- mean(team_rps_scores)

  # Add attributes for debugging/analysis
  attr(mean_rps, "team_scores") <- data.frame(
    team = final_standings$team,
    rps = team_rps_scores
  )

  return(mean_rps)
}
