library(dplyr)
library(ggplot2)
library(goalmodel)
library(readr)
library(lubridate)
library(tidyr)

source("./R/load_data.R")

predict_strength_pre_season <- function(leagues, league_id, end_year, years_back){

  league <- leagues[leagues$id == league_id, ]
  country <- league$country
  level <- league$level
  level_below <- if (level < 4) level + 1 else NA

  # ids_surrounding should ideally include the current league_id itself if its MVs are needed for turnover calc
  # For now, assuming it's for leagues other than the primary one for previous season comparisons if needed
  # Or, if it's for current, below, and above:
  ids_to_consider <- c(level -1, level, level + 1)
  ids_to_consider <- ids_to_consider[ids_to_consider > 0 & ids_to_consider <= 4]
  ids_surrounding <- leagues[leagues$country == country & leagues$level %in% ids_to_consider, ]$id


  # Set years based on years_back
  # These are the seasons for which we have FULL data to model and create historical features
  start_year <- max(2019, end_year - years_back)
  years <- seq(from = start_year, to = end_year - 1, by = 1)
  strength_model_years <- seq(from = start_year -1, to = end_year - 1, by = 1)
  print(paste("Historical years for modeling:", paste(years, collapse=", ")))

  # data_league <- as_tibble(fb_match_results(country = country, gender = "M", season_end_year = strength_model_years, tier = level)) %>%
  #   select(Season_End_Year, Home, Away, HomeGoals, AwayGoals, Round, Wk, Home_xG, Away_xG) %>%
  #   filter(Round == "Regular season" | is.na(Round) | Round == "") # Handle cases where Round might be NA or empty for some leagues
  #
  # if (!is.na(level_below)) {
  #   data_below <- as_tibble(fb_match_results(country = country, gender = "M", season_end_year = strength_model_years, tier = level_below)) %>%
  #     select(Season_End_Year, Home, Away, HomeGoals, AwayGoals, Round, Wk, Home_xG, Away_xG) %>%
  #     filter(Round == "Regular season" | is.na(Round) | Round == "")
  # } else {
  #   data_below <- tibble() # Empty tibble if no league below
  # }

  data_league <- as_tibble(load_results(country = country, end_year = strength_model_years, level = level))

  if (!is.na(level_below)) {
    data_below <- as_tibble(load_results(country = country, end_year = strength_model_years, level = level_below))
  } else {
    data_below <- tibble() # Empty tibble if no league below
  }

  # Check xG data quality
  ignore_xg <- FALSE
  if (("Home_xG" %in% names(data_league)) && ("Away_xG" %in% names(data_league))) {
    na_home_xg <- sum(is.na(data_league$Home_xG)) / nrow(data_league)
    na_away_xg <- sum(is.na(data_league$Away_xG)) / nrow(data_league)
    if (na_home_xg > 0.05 || na_away_xg > 0.05) {
      ignore_xg <- TRUE
      print("Ignoring xG data due to >5% NAs in historical data.")
    } else {
      print("Using xG data.")
    }
  } else {
    ignore_xg <- TRUE
    print("Home_xG or Away_xG columns not found in data_league. Ignoring xG data.")
  }

  # Historical strength models
  # The goalmodel requires data for each year, so ensure 'years' for get_strength_models aligns with available data_league years
  # We need models for years up to end_year-1 to predict for end_year
  # This means we also need strength models for year-1 (i.e. end_year-2) to create features for year (i.e. end_year-1)
  # So models should be from (end_year - years_back -1) up to (end_year - 1)

  m <- get_strength_models(data_league %>% filter(Season_End_Year %in% strength_model_years),
                           strength_model_years) # Pass only relevant years of data
  models_fh <- m[["models_fh"]]
  models_sh <- m[["models_sh"]]
  models_full <- m[["models_full"]]

  # Creating historical data based on team strengths
  # We are creating features for 'years'. For a given 'y' in 'years', we need model from 'y-1'.
  # The first year in 'years' is 'end_year - years_back'. Its features require model from 'end_year - years_back - 1'.

  dfs_features <- lapply(years, function(y){
    print(paste("Creating features for season:", y))
    # Get teams for year 'y'
    current_season_games <- data_league %>% filter(Season_End_Year == y)
    teams_for_year_y <- unique(c(current_season_games$Home, current_season_games$Away))

    create_season_data(year = y,
                       teams = teams_for_year_y,
                       league_id = league_id,
                       data_league = data_league, # Full historical data_league
                       data_league_below = data_below, # Full historical data_below
                       models_fh = models_fh, # List of models
                       models_sh = models_sh, # List of models
                       ids_surrounding = ids_surrounding,
                       ignore_xg = ignore_xg)
  })
  df_hist_features <- bind_rows(dfs_features)

  # Add current attack and defense from models_full to df_hist_features
  att_curr_list <- list()
  def_curr_list <- list()
  for(y_str in names(models_full)){
    y <- as.numeric(y_str)
    if (y %in% years) { # Only for years we are creating historical rows for
      model_y <- models_full[[y_str]]
      # Ensure model_y and its parameters are not NULL before proceeding
      if (!is.null(model_y) && !is.null(model_y$parameters) && !is.null(model_y$parameters$attack) && !is.null(model_y$parameters$defense)) {
        teams_in_model_y <- names(model_y$parameters$attack)

        current_year_teams_in_df <- gsub(paste0(y, "_"), "", rownames(df_hist_features)[startsWith(rownames(df_hist_features), paste0(y, "_"))])

        common_teams <- intersect(teams_in_model_y, current_year_teams_in_df)

        if (length(common_teams) > 0) { # Ensure common_teams is not empty
          att_curr_list[[y_str]] <- setNames(model_y$parameters$attack[common_teams], paste0(y, "_", common_teams))
          def_curr_list[[y_str]] <- setNames(model_y$parameters$defense[common_teams], paste0(y, "_", common_teams))
        }
      } else {
        warning(paste("Model or parameters missing for year", y_str, "when extracting att/def_curr."))
      }
    }
  }

  # Remove names from the list before concatenating
  all_att_curr <- do.call(c, unname(att_curr_list))
  all_def_curr <- do.call(c, unname(def_curr_list))

  # Ensure that the rownames of df_hist_features are present in the names of all_att_curr and all_def_curr
  # This step helps catch any discrepancies before assignment.
  # It's possible some rows in df_hist_features don't have corresponding model data if common_teams was empty.

  # Initialize columns with NA
  df_hist_features$att_curr <- NA_real_
  df_hist_features$def_curr <- NA_real_

  # Match and assign
  # Get the rownames from df_hist_features that exist in our combined strength vectors
  valid_rownames_att <- intersect(rownames(df_hist_features), names(all_att_curr))
  valid_rownames_def <- intersect(rownames(df_hist_features), names(all_def_curr))

  df_hist_features[valid_rownames_att, "att_curr"] <- all_att_curr[valid_rownames_att]
  df_hist_features[valid_rownames_def, "def_curr"] <- all_def_curr[valid_rownames_def]

  df_hist_features <- na.omit(df_hist_features) # Omit rows with any NAs after all processing

  att_curr <- df_hist_features$att_curr
  def_curr <- df_hist_features$def_curr
  # Ensure team and year info isn't accidentally scaled if it becomes a column
  df_hist_to_scale <- df_hist_features[, !(names(df_hist_features) %in% c("att_curr", "def_curr"))]

  scaled <- scale(df_hist_to_scale)
  df_hist_scaled <- as.data.frame(scaled)
  df_hist_scaled$att_curr <- att_curr
  df_hist_scaled$def_curr <- def_curr

  # Check if prom and rel columns exist and if they are all zeros, remove them
  columns_to_check <- c("prom", "rel")
  for (col in columns_to_check) {
    if (col %in% names(df_hist_scaled) && all(is.na(df_hist_scaled[[col]]))) {
      df_hist_scaled[[col]] <- NULL
    }
  }

  # Dynamic model formula based on ignore_xg and available columns
  base_vars <- c("att_prev_fh", "def_prev_fh", "def_prev_sh", "att_prev_sh",
                 "avg_mv_log", "avg_age", "turnover")

  # Add prom and rel only if they exist in the dataframe
  if ("prom" %in% names(df_hist_scaled)) {
    base_vars <- c(base_vars, "prom")
  }
  if ("rel" %in% names(df_hist_scaled)) {
    base_vars <- c(base_vars, "rel")
  }

  base_formula_vars <- paste(base_vars, collapse = " + ")

  if (!ignore_xg) {
    att_model_formula_str <- paste("att_curr ~", base_formula_vars, "+ xG_pm")
    def_model_formula_str <- paste("def_curr ~", base_formula_vars, "+ xGa_pm")
  } else {
    att_model_formula_str <- paste("att_curr ~", base_formula_vars)
    def_model_formula_str <- paste("def_curr ~", base_formula_vars)
  }


  att_model <- lm(as.formula(att_model_formula_str), data = df_hist_scaled)
  def_model <- lm(as.formula(def_model_formula_str), data = df_hist_scaled)

  # Getting data for current season (end_year)
  prev_year_for_current = end_year - 1

  # Load games for the *predicting* season (end_year) to get the list of teams
  # fb_match_results can return 0-row tibble if season hasn't started or no data
  #games_current_season_check <-fb_match_results(country = country, gender = "M", season_end_year = end_year, tier = level)
  games_current_season_check <- load_results(country = country, level = level, end_year = end_year)
  if(nrow(games_current_season_check) == 0) {
    warning(paste("No match data found for the current prediction season", end_year, "for", level, "tier. Cannot determine teams. Attempting to use teams from previous season's model."))
    if(as.character(prev_year_for_current) %in% names(models_full)) {
      teams_current <- names(models_full[[as.character(prev_year_for_current)]]$parameters$attack)
    } else {
      stop("Cannot determine current teams and no model for previous year to infer.")
    }
  } else {
    teams_current <- unique(c(games_current_season_check$Home, games_current_season_check$Away))
  }

  print(paste("Teams for current prediction season (", end_year, "):", paste(teams_current, collapse=", ")))


  df_current_features <- create_season_data(year = end_year, # 'year' is the season we are predicting FOR
                                            teams = teams_current,
                                            league_id = league_id,
                                            data_league = data_league, # historical league data
                                            data_league_below = data_below, # historical below data
                                            models_fh = models_fh,
                                            models_sh = models_sh,
                                            ids_surrounding = ids_surrounding,
                                            ignore_xg = ignore_xg)

  # Scale df_current_features using scaling parameters from df_hist_to_scale
  # Ensure columns are in the same order as 'scaled' object's  center/scale attributes
  cols_for_scaling <- names(attr(scaled, "scaled:center"))
  df_current_features_ordered <- df_current_features[, cols_for_scaling, drop = FALSE]

  df_current_scaled <- as.data.frame(scale(df_current_features_ordered,
                                           center=attr(scaled, "scaled:center"),
                                           scale=attr(scaled, "scaled:scale")))
  # If top league and 'rel' was created as 0 in create_season_data, it will be scaled to NaN if variance was 0.
  # Re-set to 0 after scaling if that's the case.
  if (league$level == 1 && "rel" %in% names(df_current_scaled) && all(is.nan(df_current_scaled$rel))) {
    df_current_scaled$rel <- 0
  }
  if (ignore_xg) { # Ensure these columns exist for predict, even if 0 and not in formula
    if(!"xG_pm" %in% names(df_current_scaled)) df_current_scaled$xG_pm <- 0
    if(!"xGa_pm" %in% names(df_current_scaled)) df_current_scaled$xGa_pm <- 0
  }


  att_pred <- predict(att_model, newdata=df_current_scaled)
  def_pred <- predict(def_model, newdata=df_current_scaled)

  # Average ha and intercept over all historical years used for modeling
  # models_full are for 'model_years' which go one year further back than 'years'
  # We want averages from models related to the seasons used in df_hist (i.e. 'years')
  relevant_model_years_for_avg <- as.character(years) # years for which df_hist was created

  avg_ha <- mean(sapply(models_full[relevant_model_years_for_avg], function(m) {
    if (!is.null(m) && !is.null(m$parameters$hfa)) m$parameters$hfa else NA
  }), na.rm = TRUE)

  avg_intercept <- mean(sapply(models_full[relevant_model_years_for_avg], function(m) {
    if (!is.null(m) && !is.null(m$parameters$intercept)) m$parameters$intercept else NA
  }), na.rm = TRUE)

  # Variances of the models
  att_var <- summary(att_model)$sigma^2
  def_var <- summary(def_model)$sigma^2

  # Make sure predictions have team names
  names(att_pred) <- teams_current
  names(def_pred) <- teams_current

  return(list(att_pred = att_pred, def_pred = def_pred,
              ha = avg_ha, intercept = avg_intercept,
              att_model = att_model, def_model = def_model,
              att_var = att_var, def_var = def_var,
              df_hist_scaled = df_hist_scaled, # For inspection
              df_current_scaled = df_current_scaled, # For inspection
              ignore_xg = ignore_xg))
}


get_strength_models <- function(data_league_for_models, model_years){
  # data_league_for_models should be pre-filtered for relevant years

  # Check if Wk column has enough non-NA values to determine season_length reliably
  if (all(is.na(data_league_for_models$Wk)) || length(unique(na.omit(data_league_for_models$Wk))) < 2 ) {
    warning("Week 'Wk' column is mostly NA or has insufficient unique values. Cannot reliably split season for FH/SH models. Using all games for FH/SH if fallback needed.")
    # Fallback: use all games for both, or handle error. For now, a warning.
    # A more robust solution would be to estimate based on number of teams if Wk is unreliable.
    season_length <- NA
  } else {
    # Attempt to determine season length per year, then take max or common value
    # This handles cases where season length might vary slightly if data is imperfect
    season_lengths_by_year <- data_league_for_models %>%
      filter(!is.na(Wk), Wk != "") %>% # Filter out NA or empty Wk values
      group_by(Season_End_Year) %>%
      summarise(max_wk = max(as.numeric(Wk), na.rm = TRUE)) %>%
      pull(max_wk)

    if (length(season_lengths_by_year) > 0 && all(is.finite(season_lengths_by_year))) {
      season_length <- max(season_lengths_by_year, na.rm = TRUE)
      print(paste("Determined season length for FH/SH models:", season_length))
    } else {
      warning("Could not determine a valid season_length from Wk column. FH/SH split might be unreliable.")
      season_length <- NA # or a default like 34 for Bundesliga, 38 for EPL etc.
    }
  }

  models_fh <- lapply(model_years, function(year){
    season_games <- data_league_for_models %>% filter(Season_End_Year == year)
    if (!is.na(season_length) && "Wk" %in% names(season_games) && sum(!is.na(as.numeric(season_games$Wk))) > 0) {
      season_games_fh <- season_games %>% filter(as.numeric(Wk) <= (season_length / 2))
    } else {
      season_games_fh <- season_games # Fallback or if Wk is not usable
    }
    if(nrow(season_games_fh) < 10) { # Minimum games to fit a model
      warning(paste("Insufficient games for FH model in year", year))
      return(NULL)
    }
    goalmodel(goals1 = season_games_fh$HomeGoals, goals2 = season_games_fh$AwayGoals,
              team1 = season_games_fh$Home, team2 = season_games_fh$Away, rs = TRUE)
  })

  models_sh <- lapply(model_years, function(year){
    season_games <- data_league_for_models %>% filter(Season_End_Year == year)
    if (!is.na(season_length) && "Wk" %in% names(season_games) && sum(!is.na(as.numeric(season_games$Wk))) > 0) {
      season_games_sh <- season_games %>% filter(as.numeric(Wk) > (season_length / 2))
    } else {
      season_games_sh <- season_games # Fallback
    }
    if(nrow(season_games_sh) < 10) {
      warning(paste("Insufficient games for SH model in year", year))
      return(NULL)
    }
    goalmodel(goals1 = season_games_sh$HomeGoals, goals2 = season_games_sh$AwayGoals,
              team1 = season_games_sh$Home, team2 = season_games_sh$Away, rs = TRUE)
  })

  models_full <- lapply(model_years, function(year){
    season_games <- data_league_for_models %>% filter(Season_End_Year == year)
    if(nrow(season_games) < 10) {
      warning(paste("Insufficient games for Full model in year", year))
      return(NULL)
    }
    goalmodel(goals1 = season_games$HomeGoals, goals2 = season_games$AwayGoals,
              team1 = season_games$Home, team2 = season_games$Away, rs = TRUE)
  })

  names(models_fh) <- model_years
  names(models_sh) <- model_years
  names(models_full) <- model_years

  return(list(models_fh = models_fh, models_sh = models_sh, models_full = models_full))
}

get_mv_data <- function(data, teams, season_start_year){
  # Calculate season start date (e.g., Aug 1st of the year the season begins)
  # If season_start_year is 2022, it means the 2022-2023 season.
  # Player ages are often as of a specific date in the season. Transfermarkt usually provides age at time of record.
  # For consistency, let's use a fixed date like start of calendar year or standard season start.
  # The 'player_age' in tm_player_bio is date of birth.
  # Market value is usually 'market_value_prev' for values at the start of the season.

  # Use player_dob from data, if it's actual DOB, otherwise player_age might be age at time of data scrape.
  # Assuming 'player_age' is DOB string "YYYY-MM-DD" or similar parseable by lubridate::ymd

  # If data is empty or teams list is empty, return empty results
  if (nrow(data) == 0 || length(teams) == 0) {
    avg_market_values <- setNames(rep(NA_real_, length(teams)), teams)
    avg_ages <- setNames(rep(NA_real_, length(teams)), teams)
    return(list(avg_market_values = avg_market_values, avg_ages = avg_ages))
  }

  # Ensure player_age is a date object
  if (!inherits(data$player_dob, "Date")) {
    data$player_age <- ymd(data$player_dob, quiet = TRUE) # quiet to avoid excessive warnings if some fail
  }

  # Define a reference date for age calculation, e.g., start of the season
  # If season_start_year is 2022 (for 22/23 season), reference date is Aug 1, 2022
  reference_date <- ymd(paste0(season_start_year, "-08-01"))


  top_18_stats <- t(sapply(teams, function(team_name){
    data_team <- data %>%
      filter(team_fbref == team_name) %>%
      arrange(desc(market_value_prev)) %>% # Using market_value_prev for start-of-season value
      slice_head(n = 18)

    if(nrow(data_team) == 0) return(c(avg_market_value = NA_real_, avg_age = NA_real_))

    avg_market_value <- mean(data_team$market_value_prev, na.rm = TRUE)

    # Calculate age if player_age (DOB) and reference_date are valid
    valid_ages <- !is.na(data_team$player_dob)
    if (any(valid_ages) & !is.na(reference_date)) {
      ages_in_years <- as.numeric(interval(data_team$player_dob[valid_ages], reference_date) / years(1))
      avg_age <- mean(ages_in_years, na.rm = TRUE)
    } else {
      avg_age <- NA_real_
    }

    return(c(avg_market_value = avg_market_value, avg_age = avg_age))
  }))

  avg_market_values <- top_18_stats[, "avg_market_value"]
  avg_ages <- top_18_stats[, "avg_age"]

  # Ensure names are preserved if some teams had no data
  names(avg_market_values) <- teams
  names(avg_ages) <- teams

  return(list(avg_market_values = avg_market_values, avg_ages = avg_ages))
}


get_turnover <- function(data_past_season_all_leagues, data_current_season_team, teams){
  # data_past_season_all_leagues: combined MV data from previous season for relevant leagues
  # data_current_season_team: MV data for the current team for the current season

  if (nrow(data_current_season_team) == 0 || nrow(data_past_season_all_leagues) == 0 || length(teams) == 0) {
    return(setNames(rep(NA_integer_, length(teams)), teams))
  }

  roster_turnover_team <- function(team_name) {
    current_team_data <- data_current_season_team %>% filter(team_fbref == team_name)
    past_team_data <- data_past_season_all_leagues %>% filter(team_fbref == team_name)

    if(nrow(current_team_data) == 0) return(NA_integer_) # Or 18 if we assume a full new squad

    # Get top 18 most valuable players for the current season for this team
    # 'market_value' column for current season, 'market_value_prev' for past
    current_top_players <- current_team_data %>%
      arrange(desc(market_value_prev)) %>% # 'market_value' for the current snapshot
      slice_head(n = 18) %>%
      pull(player_name) # Make sure player_name is unique identifier

    if(length(current_top_players) == 0) return(NA_integer_)
    if(nrow(past_team_data) == 0) return(length(current_top_players)) # All are new if no past data for team

    # Check how many of these players were in the previous season's roster for that team
    # This assumes player_name is a consistent identifier across seasons/data files
    previous_players_count <- past_team_data %>%
      filter(player_name %in% current_top_players) %>%
      distinct(player_name) %>% # Ensure unique players if duplicates exist
      nrow()

    # Turnover is number of new players in top 18 of current season
    turnover <- length(current_top_players) - previous_players_count
    # If current_top_players < 18, turnover is (actual_count - previous_players_count)
    # The prompt implied turnover is based on 18 players.
    # If a team has <18 players in current_top_players, interpretation might differ.
    # Let's stick to definition: how many of the current top N are new.
    # If current top N is less than 18, then turnover is from that N.
    # The original was `18 - previous_players`, which implies `previous_players` were counted from the fixed N=18 set.

    # Re-evaluating based on `18 - previous_players`
    # This means "how many of the top 18 of the current season were NOT in the previous season's roster for the team"
    # which is essentially what (length(current_top_players) - previous_players_count) does, assuming length(...) is 18.
    # If length(current_top_players) < 18, then it's how many of those X players are new.
    # Let's assume we are always trying to get 18 players.

    # A simpler definition of turnover: number of players in current top 18 not in *any* roster of that team last year.
    turnover_val <- length(current_top_players) - previous_players_count
    return(turnover_val)
  }

  turnovers <- sapply(teams, roster_turnover_team)
  # names(turnovers) <- teams # sapply should preserve names if teams is named, or use setNames
  return(setNames(turnovers, teams))
}


calculate_xg_difference <- function(data, teams = NULL, ignore_xg_flag = FALSE) {
  # If ignoring xG, return 0s and skip calculation
  if (ignore_xg_flag) {
    if (!is.null(teams)) {
      xG_pm_vec <- setNames(rep(0, length(teams)), teams)
      xGa_pm_vec <- setNames(rep(0, length(teams)), teams)
    } else {
      # This case (teams is NULL) might need a list of all unique teams from data if used.
      # For now, if teams is NULL and ignore_xg_flag is true, behavior is undefined, return empty.
      # Or, more safely, if data has teams, use those.
      if (nrow(data) > 0) {
        all_teams_in_data <- unique(c(data$Home, data$Away))
        xG_pm_vec <- setNames(rep(0, length(all_teams_in_data)), all_teams_in_data)
        xGa_pm_vec <- setNames(rep(0, length(all_teams_in_data)), all_teams_in_data)
      } else {
        xG_pm_vec <- numeric()
        xGa_pm_vec <- numeric()
      }
    }
    return(list(xG_pm = xG_pm_vec, xGa_pm = xGa_pm_vec))
  }

  # Calculate statistics for all teams
  # Ensure Home_xG and Away_xG exist, otherwise this function will error or produce all NAs for xG parts
  if (!"Home_xG" %in% names(data) || !"Away_xG" %in% names(data)) {
    warning("xG columns not found in `calculate_xg_difference`. Returning 0s for xG metrics.")
    # Effectively same as ignore_xg_flag = TRUE for this call
    if (!is.null(teams)) {
      xG_pm_vec <- setNames(rep(0, length(teams)), teams)
      xGa_pm_vec <- setNames(rep(0, length(teams)), teams)
    } else {
      if (nrow(data) > 0) {
        all_teams_in_data <- unique(c(data$Home, data$Away))
        xG_pm_vec <- setNames(rep(0, length(all_teams_in_data)), all_teams_in_data)
        xGa_pm_vec <- setNames(rep(0, length(all_teams_in_data)), all_teams_in_data)
      } else {
        xG_pm_vec <- numeric()
        xGa_pm_vec <- numeric()
      }
    }
    return(list(xG_pm = xG_pm_vec, xGa_pm = xGa_pm_vec))
  }

  team_stats <- data %>%
    mutate(
      Home_xG = ifelse(is.na(Home_xG), HomeGoals, Home_xG), # Impute with actual goals if xG is NA
      Away_xG = ifelse(is.na(Away_xG), AwayGoals, Away_xG)
    ) %>%
    pivot_longer(cols = c(Home, Away),
                 names_to = "HomeAway",
                 values_to = "Team") %>%
    group_by(Team) %>%
    summarise(
      xG = mean(ifelse(HomeAway == "Home", Home_xG, Away_xG), na.rm = TRUE),
      xG_against = mean(ifelse(HomeAway == "Home", Away_xG, Home_xG), na.rm = TRUE),
      Goals_For = mean(ifelse(HomeAway == "Home", HomeGoals, AwayGoals), na.rm = TRUE),
      Goals_Against = mean(ifelse(HomeAway == "Home", AwayGoals, HomeGoals), na.rm = TRUE),
      .groups = 'drop' # Recommended to avoid messages
    ) %>%
    mutate(
      xG_pm = xG - Goals_For,
      xGa_pm = xG_against - Goals_Against
    )

  # If teams are specified
  if (!is.null(teams)) {
    # Create vectors with ordered teams and appropriate values (default to 0 or NA)
    # Defaulting to NA is safer to catch issues if a team isn't in team_stats
    xG_pm <- setNames(rep(NA_real_, length(teams)), teams)
    xGa_pm <- setNames(rep(NA_real_, length(teams)), teams)

    # Fill in values for teams that exist in team_stats
    # Ensure team names match (case, spacing, etc.)
    common_teams <- intersect(teams, team_stats$Team)
    if (length(common_teams) > 0) {
      xG_pm[common_teams] <- team_stats$xG_pm[match(common_teams, team_stats$Team)]
      xGa_pm[common_teams] <- team_stats$xGa_pm[match(common_teams, team_stats$Team)]
    }
    # For teams requested but not found in team_stats (e.g. newly promoted, no games yet in 'data')
    # their xG_pm/xGa_pm will remain NA. These should be handled (e.g. imputed) later if necessary.
    # For this function, NA is the correct representation of "no data".
    # However, if the model expects 0s for missing xG (e.g. when ignore_xg is true), this might need adjustment
    # or be handled by the caller. For now, NA is fine. The main function will set to 0 if ignore_xg.

  } else {
    # If no teams are specified, use all teams from team_stats
    xG_pm <- setNames(team_stats$xG_pm, team_stats$Team)
    xGa_pm <- setNames(team_stats$xGa_pm, team_stats$Team)
  }

  return(list(
    xG_pm = xG_pm,
    xGa_pm = xGa_pm
  ))
}

create_season_data <- function(year, teams, league_id,
                               data_league, data_league_below,
                               models_fh, models_sh,
                               ids_surrounding, ignore_xg){
  # year: The season for which to create data (e.g., if predicting for 2023-24, year is 2024)
  # teams: list of teams in this 'year' for this league

  prev_season_year_num <- year - 1
  prev_season_year_char <- as.character(prev_season_year_num)

  # Initialize vectors with NAs, names will be set to 'teams'
  prev_def_fh_vec <- setNames(rep(0, length(teams)), teams) # Default to 0
  prev_att_fh_vec <- setNames(rep(0, length(teams)), teams)
  prev_def_sh_vec <- setNames(rep(0, length(teams)), teams)
  prev_att_sh_vec <- setNames(rep(0, length(teams)), teams)

  if (prev_season_year_char %in% names(models_fh) && !is.null(models_fh[[prev_season_year_char]])) {
    model_prev_fh_obj <- models_fh[[prev_season_year_char]]
    # Intersect with current teams to only get relevant ones
    common_teams_fh <- intersect(teams, names(model_prev_fh_obj$parameters$defense))
    prev_def_fh_vec[common_teams_fh] <- model_prev_fh_obj$parameters$defense[common_teams_fh]
    prev_att_fh_vec[common_teams_fh] <- model_prev_fh_obj$parameters$attack[common_teams_fh]
  } else {
    warning(paste("No FH model found for previous season:", prev_season_year_char, "when processing year:", year))
  }

  if (prev_season_year_char %in% names(models_sh) && !is.null(models_sh[[prev_season_year_char]])) {
    model_prev_sh_obj <- models_sh[[prev_season_year_char]]
    common_teams_sh <- intersect(teams, names(model_prev_sh_obj$parameters$defense))
    prev_def_sh_vec[common_teams_sh] <- model_prev_sh_obj$parameters$defense[common_teams_sh]
    prev_att_sh_vec[common_teams_sh] <- model_prev_sh_obj$parameters$attack[common_teams_sh]
  } else {
    warning(paste("No SH model found for previous season:", prev_season_year_char, "when processing year:", year))
  }

  # Promoted/Relegated teams
  # teams_prom: were in data_league_below last season (prev_season_year_num) AND are in 'teams' this season
  # teams_rel: were NOT in this league's model last season (model_prev_fh_obj) AND are NOT promoted from below this season
  # This implies they were relegated from a league above (if one exists).

  teams_prom_logical <- rep(FALSE, length(teams))
  if (nrow(data_league_below) > 0) {
    season_prev_below <- data_league_below %>% filter(Season_End_Year == prev_season_year_num)
    if (nrow(season_prev_below) > 0) {
      teams_in_league_below_last_year <- unique(c(season_prev_below$Home, season_prev_below$Away))
      teams_prom_logical <- teams %in% teams_in_league_below_last_year
    }
  }

  teams_in_current_league_last_year <- character(0)
  if (prev_season_year_char %in% names(models_fh) && !is.null(models_fh[[prev_season_year_char]])) {
    teams_in_current_league_last_year <- names(models_fh[[prev_season_year_char]]$parameters$defense)
  }

  teams_rel_logical <- !(teams %in% teams_in_current_league_last_year) & !teams_prom_logical

  prom_int <- as.integer(teams_prom_logical)
  rel_int <- as.integer(teams_rel_logical)
  names(prom_int) <- teams
  names(rel_int) <- teams

  # xG difference from previous season
  prev_season_games <- data_league %>% filter(Season_End_Year == prev_season_year_num)
  xG_diff_result <- calculate_xg_difference(prev_season_games, teams, ignore_xg_flag = ignore_xg)
  xG_pm_vec <- xG_diff_result$xG_pm
  xGa_pm_vec <- xG_diff_result$xGa_pm

  # Replace NA xG values with 0 if ignore_xg is false (i.e., we want to use xG, but some teams might be new)
  # If ignore_xg is true, calculate_xg_difference already returns 0s.
  if(!ignore_xg) {
    xG_pm_vec[is.na(xG_pm_vec)] <- 0
    xGa_pm_vec[is.na(xGa_pm_vec)] <- 0
  }


  # # Market Value data for the *start* of 'year'
  # # MV file naming: assuming "tm_mv_leagueid_SEASONENDYEAR_adj.csv"
  # # So for season 'year' (e.g. 2024 meaning 23-24 season), we need MV data as of start of that season.
  # # The file would be tm_mv_leagueid_2024_adj.csv. get_mv_data uses season_start_year which is year-1.
  # mv_df_current_squad <- tibble() # Ensure it exists
  # if (file.exists(mv_current_squad_file)) {
  #   mv_df_current_squad <- read_csv(mv_current_squad_file, show_col_types = FALSE)
  # } else {
  #   warning(paste("MV file not found for current squad:", mv_current_squad_file))
  # }
  #
  # TODO: Market Value NA Handling - this is a placeholder
  # If mv_df_current_squad is empty or has issues, get_mv_data might return NAs or errors.
  # Consider imputation strategies here or in the main function:
  # - League average for new teams
  # - Average of similar teams (e.g. other promoted teams)
  # - Carry-forward if team existed but data is missing for current year (less ideal)
  mv_current <- load_mv(league_id = league_id, year = year)

  if (nrow(mv_current) == 0) {
    warning(paste("No MV data loaded for year", year, "league", league_id, ". MV, Age, Turnover will be NA."))
    mv_vec <- setNames(rep(NA_real_, length(teams)), teams)
    avg_age_vec <- setNames(rep(NA_real_, length(teams)), teams)
  } else {
    mv_res <- get_mv_data(mv_current, teams, season_start_year = prev_season_year_num) # season_start_year is correct here
    mv_vec <- mv_res$avg_market_values
    avg_age_vec <- mv_res$avg_ages
  }

  # Turnover: compare current squad (mv_df_current_squad for 'year') with past season squad ('prev_season_year_num')
  mv_previous_surrounding <- load_mv(league_id = ids_surrounding, year = prev_season_year_num)

  turnover_vec <- get_turnover(data_past_season_all_leagues = mv_previous_surrounding,
                               data_current_season_team = mv_current, # MV data for the teams in 'year'
                               teams = teams)

  df_season <- data.frame(
    def_prev_fh = prev_def_fh_vec[teams], # Ensure correct order
    att_prev_fh = prev_att_fh_vec[teams],
    def_prev_sh = prev_def_sh_vec[teams],
    att_prev_sh = prev_att_sh_vec[teams],
    xG_pm = xG_pm_vec[teams],
    xGa_pm = xGa_pm_vec[teams],
    prom = prom_int[teams],
    rel = rel_int[teams],
    avg_mv = mv_vec[teams],
    avg_age = avg_age_vec[teams],
    turnover = turnover_vec[teams],
    row.names = paste0(year, "_", teams)
  )

  # Market values are log scaled to model decreasing returns
  df_season$avg_mv_log <- log(df_season$avg_mv)
  df_season$avg_mv_log[is.infinite(df_season$avg_mv_log)] <- NA

  # Handle NAs that might have arisen from missing MV data or other issues before returning
  # For example, if avg_mv is NA, avg_mv_log will be NA. This will be handled by na.omit in main.
  # If any core metric like prom/rel is NA, it's an issue. They should be 0/1.
  # The 0-replacement for strengths is done. MV related NAs are complex (TODO).
  # xG NAs should be 0 if ignore_xg or imputed to 0 for new teams if use_xg.

  return(df_season)
}
