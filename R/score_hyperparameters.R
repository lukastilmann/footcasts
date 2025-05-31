#' Score hyperparameter settings across multiple leagues and seasons
#'
#' @param hyperparams Named list of hyperparameters for forecast_season function
#' @param n_sims Number of simulations to run for each forecast
#' @param years Vector of season end years to evaluate
#' @param league_ids Vector of league IDs to evaluate
#' @param parallel Logical, whether to use parallel processing (default FALSE)
#' @param max_cores Maximum number of cores to use if parallel=TRUE (NULL for auto-detect)
#' @return Numeric score (lower is better) with detailed results as attributes
#' @export
score_hyperparameters <- function(hyperparams, n_sims, years, league_ids,
                                  parallel = FALSE, max_cores = NULL) {

  # Validate hyperparameters
  required_params <- c("update_rate", "xG_weight", "var_factor",
                       "variance_decay_param", "mixture_weight_param",
                       "mixture_start_matchday")
  missing_params <- setdiff(required_params, names(hyperparams))
  if (length(missing_params) > 0) {
    stop(paste("Missing hyperparameters:", paste(missing_params, collapse = ", ")))
  }

  # Load leagues data once
  leagues <- readr::read_csv(CONFIG$paths$leagues_data, show_col_types = FALSE)

  # Create all league-year combinations
  league_year_combos <- expand.grid(
    league_id = league_ids,
    year = years,
    stringsAsFactors = FALSE
  )

  # Filter out end-of-year leagues in 2025
  league_year_combos <- league_year_combos %>%
    left_join(leagues %>% select(id, end_of_year_end),
              by = c("league_id" = "id")) %>%
    filter(!(end_of_year_end == 1 & year == 2025)) %>%
    select(-end_of_year_end)

  if (nrow(league_year_combos) == 0) {
    stop("No valid league-year combinations to evaluate")
  }

  # Define function to score a single league-year
  score_league_year <- function(league_id, year, leagues_df) {
    print(paste("Evaluating league", league_id, "year", year))

    # Get league info
    league_info <- leagues_df[leagues_df$id == league_id, ]

    tryCatch({
      # Load or create strength prediction
      strength_pred_file <- get_strength_pred_path(year, league_id)
      if (!file.exists(strength_pred_file)) {
        print(paste("Creating strength prediction for league", league_id, "year", year))
        strength_pred <- predict_strength_pre_season(leagues_df, league_id, year, 5)
      } else {
        strength_pred <- readRDS(strength_pred_file)
      }

      # Load results data
      results <- load_results(league_info$country, league_info$level, year)

      if (is.null(results) || nrow(results) == 0) {
        return(list(
          error = paste("No results found for league", league_id, "year", year),
          scores = list()
        ))
      }

      if (any(is.na(results$HomeGoals)) || any(is.na(results$AwayGoals))){
        return(list(
          error = paste("NAs in results, seasons seems not to be finished."),
          scores = list()
        ))
      }

      # Get final standings
      final_standings <- calculate_final_standing(
        results$Home, results$Away,
        results$HomeGoals, results$AwayGoals
      )

      # Get completed matchdays
      completed_games <- results %>%
        filter(!is.na(HomeGoals) & !is.na(AwayGoals))

      uncompleted_games <- results %>%
        filter(is.na(HomeGoals) | is.na(AwayGoals))

      completed_weeks <- completed_games$Wk[!completed_games$Wk %in% uncompleted_games$Wk]
      max_matchday <- max(completed_weeks, na.rm = TRUE)
      total_matchdays <- max(results$Wk, na.rm = TRUE)

      # Score each matchday
      matchday_scores <- list()

      for (matchday in 0:(max_matchday - 1)) {
        #print(paste("Scoring matchday", matchday))

        # Create forecast
        forecast <- tryCatch({
          forecast_season(
            games_current_season = results,
            strength_pred = strength_pred,
            matchday = matchday,
            n_sims = n_sims,
            update_rate = hyperparams$update_rate,
            xG_weight = hyperparams$xG_weight,
            var_factor = hyperparams$var_factor,
            variance_decay_param = hyperparams$variance_decay_param,
            mixture_weight_param = hyperparams$mixture_weight_param,
            mixture_start_matchday = hyperparams$mixture_start_matchday,
            verbose = FALSE
          )
        }, error = function(e) {
          warning(paste("Error in forecast_season for league", league_id,
                        "year", year, "matchday", matchday, ":", e$message))
          return(NULL)
        })

        if (is.null(forecast)) next

        # Score the forecast
        rps_score <- tryCatch({
          score_forecast_rps(forecast, final_standings)
        }, error = function(e) {
          warning(paste("Error scoring forecast for league", league_id,
                        "year", year, "matchday", matchday, ":", e$message))
          return(NA)
        })

        if (is.na(rps_score)) next

        # Calculate weight
        matchdays_left <- total_matchdays - matchday
        weight <- if (matchdays_left > 0) 1 / sqrt(matchdays_left) else 1
        weighted_score <- rps_score * weight

        # Store results
        key <- paste(league_id, year, matchday, sep = "_")
        matchday_scores[[key]] <- list(
          league_id = league_id,
          year = year,
          matchday = matchday,
          raw_rps = rps_score,
          weight = weight,
          weighted_score = weighted_score,
          matchdays_left = matchdays_left,
          total_matchdays = total_matchdays
        )
      }

      return(list(
        error = NULL,
        scores = matchday_scores
      ))

    }, error = function(e) {
      return(list(
        error = paste("Error processing league", league_id, "year", year, ":", e$message),
        scores = list()
      ))
    })
  }

  # Process league-years either in parallel or sequentially
  if (parallel) {
    require(future)
    require(furrr)

    # Set up parallel backend
    n_workers <- if (is.null(max_cores)) {
      min(availableCores() - 1, nrow(league_year_combos))
    } else {
      min(max_cores, nrow(league_year_combos))
    }

    print(paste("Running in parallel with", n_workers, "workers"))

    oplan <- plan(multisession, workers = n_workers)
    on.exit(plan(oplan), add = TRUE)

    # Run parallel computation
    results_list <- future_map(
      1:nrow(league_year_combos),
      function(i) {
        score_league_year(
          league_id = league_year_combos$league_id[i],
          year = league_year_combos$year[i],
          leagues_df = leagues
        )
      },
      .options = furrr_options(
        seed = TRUE,
        globals = c("hyperparams", "n_sims", "CONFIG",
                    "get_strength_pred_path", "predict_strength_pre_season",
                    "load_results", "calculate_final_standing",
                    "forecast_season", "score_forecast_rps",
                    "model_from_params", "update_parameters",
                    "resample_model", "simulate_season",
                    "create_mixture_model", "calculate_points",
                    "goalmodel", "predict_expg"),
        packages = c("dplyr", "readr", "goalmodel")
      ),
      .env_globals = asNamespace("footcasts")
    )
  } else {
    print("Running sequentially")

    # Run sequential computation
    results_list <- lapply(
      1:nrow(league_year_combos),
      function(i) {
        score_league_year(
          league_id = league_year_combos$league_id[i],
          year = league_year_combos$year[i],
          leagues_df = leagues
        )
      }
    )
  }

  # Process results
  all_scores <- list()
  errors <- character()

  for (result in results_list) {
    if (!is.null(result$error)) {
      errors <- c(errors, result$error)
    }
    if (length(result$scores) > 0) {
      all_scores <- c(all_scores, result$scores)
    }
  }

  # Print any errors
  if (length(errors) > 0) {
    warning("Errors during processing:\n", paste(errors, collapse = "\n"))
  }

  # Calculate total score
  if (length(all_scores) == 0) {
    stop("No valid scores calculated. Check data availability.")
  }

  weighted_scores <- sapply(all_scores, function(x) x$weighted_score)
  total_score <- sum(weighted_scores)

  # Add detailed information as attributes
  attr(total_score, "n_evaluations") <- length(all_scores)
  attr(total_score, "score_details") <- all_scores
  attr(total_score, "hyperparams") <- hyperparams
  attr(total_score, "errors") <- errors

  # Summary statistics
  raw_scores <- sapply(all_scores, function(x) x$raw_rps)

  attr(total_score, "summary") <- list(
    mean_raw_rps = mean(raw_scores),
    sd_raw_rps = sd(raw_scores),
    mean_weighted_score = mean(weighted_scores),
    sd_weighted_score = sd(weighted_scores),
    leagues_evaluated = unique(sapply(all_scores, function(x) x$league_id)),
    years_evaluated = unique(sapply(all_scores, function(x) x$year)),
    parallel = parallel,
    n_workers = if (parallel) n_workers else 1,
    n_errors = length(errors)
  )

  print(paste("Total score:", round(total_score, 4),
              "from", length(all_scores), "evaluations"))

  return(total_score)
}
