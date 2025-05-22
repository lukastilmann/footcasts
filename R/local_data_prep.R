library(tidyverse)
library(fs)
library(readr)

source("./config.R")

# Function to extract league_id and year from filename
extract_info_from_filename <- function(filename) {
  # Pattern: tm_mv_{league_id}_{year}_adj.csv
  pattern <- "tm_mv_(.+)_([0-9]{4})_adj\\.csv$"
  matches <- str_match(filename, pattern)

  if (is.na(matches[1])) {
    warning(paste("Filename doesn't match expected pattern:", filename))
    return(list(league_id = NA, year = NA))
  }

  list(
    league_id = matches[2],
    year = as.integer(matches[3])
  )
}

# Function to read and process a single file
read_mv_file <- function(file_path) {
  # Extract info from filename
  filename <- basename(file_path)
  info <- extract_info_from_filename(filename)

  if (is.na(info$league_id) || is.na(info$year)) {
    warning(paste("Skipping file due to invalid filename:", file_path))
    return(NULL)
  }

  # Read the CSV file
  tryCatch({
    df <- read_csv(file_path, show_col_types = FALSE)

    # Add league_id and season_end_year columns
    df <- df %>%
      mutate(
        league_id = info$league_id,
        season_end_year = info$year
      )

    # Rename player_age to player_dob if it exists
    if ("player_age" %in% names(df)) {
      df <- df %>%
        rename(player_dob = player_age)
    }

    return(df)
  }, error = function(e) {
    warning(paste("Error reading file:", file_path, "\nError:", e$message))
    return(NULL)
  })
}

# Main function to consolidate all files
consolidate_mv_files <- function(mv_dir) {
  # Get all CSV files matching the pattern
  file_pattern <- "tm_mv_.*_[0-9]{4}_adj\\.csv$"
  csv_files <- dir_ls(mv_dir, regexp = file_pattern)

  if (length(csv_files) == 0) {
    stop("No matching CSV files found in directory: ", mv_dir)
  }

  message(sprintf("Found %d CSV files to process", length(csv_files)))

  # Read all files and combine them
  all_data <- map(csv_files, read_mv_file) %>%
    discard(is.null) %>%
    bind_rows()

  if (nrow(all_data) == 0) {
    stop("No data could be read from the CSV files")
  }

  message(sprintf("Total rows: %d", nrow(all_data)))

  return(all_data)
}


mv_data <- consolidate_mv_files("./data/processed/squads_mv_adj")
mv_data <- mv_data %>%
  mutate(team_fbref = recode(team_fbref,
                             "Braunschweig" = "BTSV",
                             "M'Gladbach" = "Gladbach"
  ))

write_csv(mv_data, CONFIG$paths$mv_consolidated)


