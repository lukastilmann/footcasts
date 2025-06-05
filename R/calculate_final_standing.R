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

#'Calculates league table based on result
#'
#' @param home vector of home teams
#' @param away vector of away teams
#' @param home_goals vector of home team goals
#' @param away_goals vector of away team goals
#' @importFrom magrittr %>%
#' @import dplyr
#' @return final table
#' @export
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
