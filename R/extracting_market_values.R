install.packages('stringdist')

library(worldfootballR)
library(dplyr)
library(stringdist)
library(clue)
library(readr)


years <- c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
#years <- c(2018)
levels = c("1st", "2nd", "3rd")

# Function to convert market value strings to numeric
convert_market_value <- function(value) {
  if (is.na(value) || !grepl("€", value)) return(0)
  value <- gsub("€|\\s", "", value)
  multiplier <- 1
  if (grepl("m$", value)) {
    multiplier <- 1e6
    value <- gsub("m$", "", value)
  } else if (grepl("k$", value)) {
    multiplier <- 1e3
    value <- gsub("k$", "", value)
  }
  as.numeric(value) * multiplier
}

# Function to clean data for a single squad
clean_squad_data <- function(squad_data) {
  result <- data.frame(
    player_name = character(),
    market_value = numeric(),
    player_age = numeric(),
    stringsAsFactors = FALSE
  )

  i <- 1
  while (i <= nrow(squad_data)) {
    player_name <- squad_data$player_name[i]

    if (i + 1 <= nrow(squad_data) && grepl("€", squad_data$player_name[i + 1])) {
      market_value <- convert_market_value(squad_data$player_name[i + 1])
      i <- i + 2
    } else {
      market_value <- 0
      i <- i + 1
    }

    player_age <- squad_data$player_dob[length(result$player_name) + 1]

    result <- rbind(result, data.frame(
      player_name = player_name,
      market_value = market_value,
      player_age = player_age,
      stringsAsFactors = FALSE
    ))
  }

  result
}


# Function to match team names
match_team_names <- function(teams1, teams2) {
  # Calculate similarity matrix
  dist_matrix <- stringdistmatrix(teams1, teams2, method = "cosine")
  sim_matrix <- 1 - dist_matrix  # Convert distance to similarity

  # Find optimal matching using Hungarian algorithm
  assignment <- solve_LSAP(sim_matrix, maximum = TRUE)

  # Create result dataframe
  result <- data.frame(
    Team1 = teams1,
    Team2 = teams2[assignment],
    Similarity = sapply(1:length(teams1), function(i) sim_matrix[i, assignment[i]])
  )

  # Sort by similarity
  result <- result[order(result$Similarity, decreasing = TRUE), ]

  # Calculate and print total similarity score
  total_similarity <- sum(result$Similarity)
  cat("Total similarity score:", total_similarity, "\n")

  return(result)
}


get_mvs <- function(year, url, country, level){

  level_code <- levels[as.numeric(level)]
  start_year = year - 1

  mv <- as_tibble(tm_player_market_values(country_name = "",
                                          start_year = start_year,
                                          league_url = url))
  #TODO: only regular season games
  games <- as_tibble(load_match_results(country, "M", year, level_code))
  if ("Regular season" %in% games$Round){
    games <- games %>% filter(Round == "Regular season")
  }

  # Assuming your dataset is called 'original_data'
  # and the columns are 'squad', 'player_name', and 'player_dob'
  # If these are different in your actual data, please adjust accordingly

  # Group by squad and apply the cleaning function
  new_data <- mv %>%
    group_by(squad) %>%
    do(clean_squad_data(.)) %>%
    ungroup()

  # Format market_value to avoid exponential notation
  new_data$market_value <- format(new_data$market_value, scientific = FALSE)

  # mapping team names from both datasets
  teams_tm <- unique(new_data$squad)
  teams_fbref <- unique(games$Home)
  mapping <- match_team_names(teams_tm, teams_fbref)
  print(mapping)

  # using mapping
  mv_mapped <- new_data %>% left_join(mapping %>% select(Team1, Team2), by = c("squad" = "Team1")) %>%
    rename(team_fbref = Team2)

  return(mv_mapped)
}


leagues <- read_csv("./data/leagues.csv")

ids <- c(9, 23, 24, 25)
for (id in ids){
  row <- leagues[id, ]
  for (year in years){
    if (row$end_of_year_end == 1 & year == 2025){
      next()
    }
    print(year)
    print(row$name)
    df <- get_mvs(year = year, url = row$tm_url, country = row$country, level = row$level)
    write_csv(df, paste0("./data/squads_mv/tm_mv_", id, "_", year, ".csv"))
  }
}

test <- tm_player_market_values(start_year = 2018,
                                league_url = "https://www.transfermarkt.com/2-bundesliga/startseite/wettbewerb/L2")

