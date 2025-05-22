source("./config.R")

library(readr)

write_results_data <- function(id, year){

  leagues <- read_csv(CONFIG$paths$leagues_data, show_col_types = FALSE)
  league <- leagues[leagues$id == id, ]
  levels <- c("1st", "2nd", "3rd")

  data <- data <- worldfootballR::fb_match_results(
    country = league$country,
    gender = "M",
    season_end_year = year,
    tier = levels[league$level]
  )
  write_rds(data, get_results_path(league$country, league$level, year))
}
