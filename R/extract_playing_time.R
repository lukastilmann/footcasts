library(worldfootballR)
library(dplyr)
library(readr)

data <- fb_team_player_stats(country = "GER", gender = "M",
                        season_end_year = 2024,
                        tier = "2nd", stat_type = "playing_time")
