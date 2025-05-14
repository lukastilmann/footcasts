library(readr)
library(dplyr)
library(purrr)
library(fs)

# Function to find the best match
find_best_match <- function(player_name, team_fbref, current_market_value, prev_season_data) {
  matches <- prev_season_data %>%
    filter(player_name == !!player_name)

  if (nrow(matches) == 0) {
    return(current_market_value)
  } else if (nrow(matches) == 1) {
    return(matches$market_value)
  } else {
    same_team <- matches %>% filter(team_fbref == !!team_fbref)
    if (nrow(same_team) > 0) {
      return(same_team$market_value[1])
    } else {
      return(matches$market_value[which.min(abs(matches$market_value - current_market_value))])
    }
  }
}

# Read leagues data
leagues <- read_csv("./data/leagues.csv")

# Create output directory if it doesn't exist
dir_create("./data/squads_mv_adj", recurse = TRUE)

# Loop through years
for (year in 2019:2025) {
  cat("Processing year:", year, "\n")

  # Combine market value datasets from the previous season
  previous_season_data <- map_df(seq(1,25), function(id) {
    tryCatch(
      read_csv(paste0("./data/squads_mv/tm_mv_", id, "_", year - 1, ".csv"), show_col_types = FALSE),
      error = function(e) NULL
    )
  })

  # Process each league
  for (league_id in 1:nrow(leagues)) {
    cat("  Processing league:", league_id, "\n")
    if (leagues[league_id, ]$end_of_year_end == 1 & year == 2025){
      next()
    }

    # Read the current season's data
    current_file <- paste0("./data/squads_mv/tm_mv_", league_id, "_", year, ".csv")
    if (!file_exists(current_file)) {
      cat("    File not found:", current_file, "\n")
      next
    }

    df <- read_csv(current_file, show_col_types = FALSE)

    # Add the new column to the dataframe
    df <- df %>%
      rowwise() %>%
      mutate(market_value_prev = find_best_match(player_name, team_fbref, market_value, previous_season_data)) %>%
      ungroup()

    # Save the adjusted dataset
    output_file <- paste0("./data/squads_mv_adj/tm_mv_", league_id, "_", year, "_adj.csv")
    write_csv(df, output_file)
    cat("    Saved:", output_file, "\n")
  }
}
