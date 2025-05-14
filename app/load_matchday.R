load_matchday_results <- function(country, level, end_year, matchday, only_regular_season = TRUE) {
  levels <- c("1st", "2nd", "3rd", "4th")

  # Input checks for level
  if (!level %in% 1:4) {
    stop("level must be an integer between 1 and 4")
  }

  load_local = FALSE

  if (length(end_year) == 1){
    # If file exists, load it from local storage
    # First, check if the data exists locally
    file_path <- paste0("./data/results/", country, "_", level, "_", end_year, ".rds")
    if (file.exists(file_path)) {
      load_local = TRUE
    }
  }

  if (load_local){
    data <- readRDS(file_path)
  } else {
    data <- worldfootballR::fb_match_results(
      country = country,
      gender = "M",
      season_end_year = end_year,
      tier = levels[level]
    )
  }

  # Check if any data is actually returned
  if (is.null(data) || nrow(data) == 0) {
    warning("No data returned for the specified parameters")
    return(NULL)
  }

  # Select columns
  required_cols <- c("Season_End_Year", "Home", "Away", "HomeGoals", "AwayGoals", "Round", "Wk")
  optional_cols <- c("Home_xG", "Away_xG")

  # Check which columns exist in the data
  available_cols <- required_cols[required_cols %in% names(data)]
  available_optional <- optional_cols[optional_cols %in% names(data)]

  # Combine all columns to select
  cols_to_select <- c(available_cols, available_optional)

  # Select the columns
  data <- data[, cols_to_select]

  # correct types
  data$Wk <- as.numeric(data$Wk)

  # Select regular season
  if (only_regular_season){
    data <- data[!is.na(data$Wk), ]
  }

  # select correct matchday
  data <- data[data$Wk == matchday, ]

  return(data)
}
