#' Hyperparameter Optimization Using TPE with Successive Halving
#'
#' Performs hyperparameter optimization using Tree Parzen Estimators (TPE)
#' as a surrogate model combined with successive halving for efficient
#' resource allocation. Optimizes hyperparameters for models evaluated
#' across specified leagues and years.
#'
#' @param league_ids A vector of league identifiers to use for model evaluation.
#' @param years A vector of years to include in the evaluation.
#' @param param_bounds A named list where each element specifies the bounds
#'   for a parameter. For continuous and integer parameters, provide a
#'   numeric vector of length 2 with (min, max). For factor parameters,
#'   provide a character vector of possible values.
#' @param param_types A named list specifying the type of each parameter.
#'   Valid types are "continuous", "integer", or "factor". Names must
#'   match those in param_bounds.
#' @param checkpoint_dir Directory path for saving checkpoints and logs.
#'   If NULL (default), no checkpointing is performed. If TRUE, creates
#'   a timestamped directory under runs/hpo/. If a character string,
#'   uses that as the specific checkpoint directory.
#' @param n_initial Number of initial random configurations. Default is 64.
#' @param initial_budget Initial number of simulations for each configuration.
#'   Default is 100.
#' @param halving_proportion Proportion of configurations to keep in each
#'   successive round. Default is 0.5.
#' @param n_rounds Number of halving rounds to perform. Default is 5.
#' @param gamma Quantile threshold for TPE good/bad split. Default is 0.25.
#' @param bw_factor Numeric value that is multiplied with KDE-bandwidths. Values
#'   above 1 allow for more exploration of the space. Default is 1.
#' @param verbose Logical indicating whether to print progress messages.
#'   Default is TRUE.
#' @param parallel Logical indicating whether to use parallel processing
#'   for model evaluation. Default is TRUE.
#'
#' @return A list containing:
#'   \item{configs}{Data frame of all evaluated configurations}
#'   \item{scores}{Numeric vector of all scores}
#'   \item{budgets}{Numeric vector of simulation budgets used for each evaluation}
#'   \item{best_config}{Data frame with the best configuration found}
#'   \item{best_score}{The best (lowest) score achieved}
#'   \item{n_evaluations}{Total number of function evaluations}
#'
#' @details
#' The function uses successive halving to efficiently allocate computational
#' resources. In each round, the budget (number of simulations) is doubled
#' while the number of configurations is halved. The TPE surrogate model
#' learns from all previous evaluations to propose promising new configurations.
#'
#' The objective function calls \code{score_hyperparameters} internally with
#' the specified hyperparameters, number of simulations, years, league IDs,
#' and parallel setting.
#'
#' When checkpoint_dir is provided, the function saves progress after each
#' evaluation and can resume from the last checkpoint if interrupted.
#'
#' @export
#' @importFrom np npudensbw npudens
#'
#' @examples
#' \dontrun{
#' # Define parameter space for a sports prediction model
#' param_bounds <- list(
#'   decay_rate = c(0.01, 0.5),
#'   home_advantage = c(0.0, 0.3),
#'   regularization = c(0.001, 1.0),
#'   model_type = c("linear", "poisson", "negbin")
#' )
#'
#' param_types <- list(
#'   decay_rate = "continuous",
#'   home_advantage = "continuous",
#'   regularization = "continuous",
#'   model_type = "factor"
#' )
#'
#' # Run optimization with automatic run directory creation
#' results <- hpoptimize(
#'   league_ids = c(1, 2, 3),
#'   years = 2018:2022,
#'   param_bounds = param_bounds,
#'   param_types = param_types,
#'   checkpoint_dir = TRUE,  # Auto-create timestamped directory
#'   n_initial = 64,
#'   initial_budget = 100,
#'   n_rounds = 4,
#'   parallel = TRUE
#' )
#'
#' # Or specify a custom checkpoint directory
#' results <- hpoptimize(
#'   league_ids = c(1, 2, 3),
#'   years = 2018:2022,
#'   param_bounds = param_bounds,
#'   param_types = param_types,
#'   checkpoint_dir = "runs/hpo/my_experiment",
#'   n_initial = 64,
#'   initial_budget = 100,
#'   n_rounds = 4,
#'   parallel = TRUE
#' )
#'
#' print(results$best_config)
#' print(results$best_score)
#' }
#'
#' @seealso \code{\link{fit_tpe_surrogate}}, \code{\link{sample_tpe}},
#'   \code{\link{score_hyperparameters}}
hpoptimize <- function(league_ids, years, param_bounds, param_types,
                       checkpoint_dir = NULL,
                       n_initial = 64, initial_budget = 100,
                       halving_proportion = 0.5, n_rounds = 5,
                       gamma = 0.25, bw_factor = 1,
                       verbose = TRUE, parallel = TRUE) {

  # Initialize checkpoint and logging infrastructure
  checkpoint_state <- NULL
  log_file <- NULL

  # Handle checkpoint directory creation
  if (!is.null(checkpoint_dir)) {
    # Always ensure runs/hpo base directory exists
    runs_base_dir <- "runs/hpo"
    if (!dir.exists("runs")) {
      dir.create("runs")
      if (verbose) cat("Created 'runs' directory\n")
    }
    if (!dir.exists(runs_base_dir)) {
      dir.create(runs_base_dir, recursive = TRUE)
      if (verbose) cat("Created 'runs/hpo' directory\n")
    }

    if (isTRUE(checkpoint_dir)) {
      # Create timestamped directory for this run
      run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      run_tag <- sprintf("leagues%s_n%d",
                         paste(league_ids, collapse = ""),
                         n_initial)
      checkpoint_dir <- file.path(runs_base_dir, paste(run_timestamp, run_tag, sep = "_"))
    } else if (is.character(checkpoint_dir)) {
      # Use provided name under runs/hpo
      checkpoint_dir <- file.path(runs_base_dir, checkpoint_dir)
    } else {
      stop("checkpoint_dir must be NULL, TRUE, or a character string")
    }

    # Create the specific run directory
    if (!dir.exists(checkpoint_dir)) {
      dir.create(checkpoint_dir, recursive = TRUE)
      if (verbose) cat(sprintf("Created run directory: %s\n", checkpoint_dir))
    } else {
      if (verbose) cat(sprintf("Using existing run directory: %s\n", checkpoint_dir))
    }

    # Save run metadata
    run_metadata <- list(
      start_time = Sys.time(),
      league_ids = league_ids,
      years = years,
      param_bounds = param_bounds,
      param_types = param_types,
      n_initial = n_initial,
      initial_budget = initial_budget,
      halving_proportion = halving_proportion,
      n_rounds = n_rounds,
      gamma = gamma,
      bw_factor = bw_factor
    )
    saveRDS(run_metadata, file.path(checkpoint_dir, "run_metadata.rds"))

    # Set up log file
    log_file <- file.path(checkpoint_dir, "optimization_log.txt")

    # Check for existing checkpoint
    checkpoint_file <- file.path(checkpoint_dir, "checkpoint.rds")
    if (file.exists(checkpoint_file)) {
      if (verbose) cat("Loading from checkpoint...\n")
      checkpoint_state <- readRDS(checkpoint_file)

      # Validate checkpoint matches current parameters
      if (!identical(checkpoint_state$param_bounds, param_bounds) ||
          !identical(checkpoint_state$param_types, param_types) ||
          !identical(checkpoint_state$league_ids, league_ids) ||
          !identical(checkpoint_state$years, years)) {
        warning("Checkpoint parameters don't match current call. Starting fresh.")
        checkpoint_state <- NULL
      } else {
        if (verbose) {
          cat(sprintf("Resuming from round %d, config %d\n",
                      checkpoint_state$current_round,
                      checkpoint_state$current_config_idx))
        }
      }
    }

    # Initialize or append to log file
    write_log(log_file, sprintf("\n=== HPO Run Started: %s ===", Sys.time()), append = file.exists(log_file))
    write_log(log_file, sprintf("Leagues: %s", paste(league_ids, collapse = ", ")))
    write_log(log_file, sprintf("Years: %s", paste(years, collapse = ", ")))
    write_log(log_file, sprintf("Parameters: %s", paste(names(param_bounds), collapse = ", ")))
  }

  # Initialize from checkpoint or start fresh
  if (!is.null(checkpoint_state)) {
    all_configs <- checkpoint_state$all_configs
    all_scores <- checkpoint_state$all_scores
    all_budgets <- checkpoint_state$all_budgets
    start_round <- checkpoint_state$current_round
    start_config_idx <- checkpoint_state$current_config_idx
    current_configs <- checkpoint_state$current_configs
    current_budget <- checkpoint_state$current_budget
    total_evaluations <- checkpoint_state$total_evaluations
  } else {
    # Storage for all evaluations
    all_configs <- list()
    all_scores <- list()
    all_budgets <- list()
    # Generate initial configurations
    current_configs <- generate_uniform_samples(n_initial, param_bounds, param_types)
    current_budget <- initial_budget
    start_round <- 1
    start_config_idx <- 1
    total_evaluations <- 0

    # Save initial state if checkpointing
    if (!is.null(checkpoint_dir)) {
      save_round_configs(checkpoint_dir, 1, current_configs)
    }
  }

  # Main optimization loop
  for(round in start_round:n_rounds) {
    if(verbose) {
      cat(sprintf("\n--- Round %d ---\n", round))
      cat(sprintf("Evaluating %d configs with budget %d\n",
                  nrow(current_configs), current_budget))
    }

    # Initialize scores for this round if starting fresh
    if (round > start_round || is.null(checkpoint_state)) {
      scores <- numeric(nrow(current_configs))
      config_start_idx <- 1
    } else {
      # Resume from checkpoint
      scores <- checkpoint_state$current_scores
      config_start_idx <- start_config_idx
    }

    # Evaluate configurations
    for(i in config_start_idx:nrow(current_configs)) {
      config <- as.list(current_configs[i, ])
      budget <- current_budget

      total_evaluations <- total_evaluations + 1

      if(verbose) {
        cat(sprintf("\nEvaluation %d (Round %d, Config %d/%d):\n",
                    total_evaluations, round, i, nrow(current_configs)))
        cat(paste(sprintf("%s=%s", names(config), config), collapse = ", "), "\n")
      }

      # Time the evaluation
      eval_start_time <- Sys.time()

      scores[i] <- objective_fn(hp = config,
                                n_sims = budget,
                                years = years,
                                league_ids = league_ids,
                                parallel = parallel)

      eval_time <- difftime(Sys.time(), eval_start_time, units = "secs")

      if(verbose) {
        cat(sprintf("Score: %.4f (Time: %.1f sec)\n", scores[i], eval_time))
      }

      # Log the evaluation
      if (!is.null(log_file)) {
        log_entry <- sprintf("[%s] Eval %d | Round %d | Config %d/%d | Budget %d | Score: %.4f | Time: %.1f sec | Params: %s",
                             Sys.time(), total_evaluations, round, i, nrow(current_configs),
                             budget, scores[i], eval_time,
                             paste(sprintf("%s=%s", names(config), config), collapse = ", "))
        write_log(log_file, log_entry)
      }

      # Save checkpoint after each evaluation
      if (!is.null(checkpoint_dir)) {
        checkpoint_state <- list(
          all_configs = all_configs,
          all_scores = all_scores,
          all_budgets = all_budgets,
          current_round = round,
          current_config_idx = i + 1,  # Next config to evaluate
          current_configs = current_configs,
          current_scores = scores,
          current_budget = current_budget,
          total_evaluations = total_evaluations,
          # Save parameters for validation
          param_bounds = param_bounds,
          param_types = param_types,
          league_ids = league_ids,
          years = years
        )
        saveRDS(checkpoint_state, file.path(checkpoint_dir, "checkpoint.rds"))

        # Also save scores for this round
        save_round_scores(checkpoint_dir, round, scores[1:i])
      }
    }

    # Store results
    all_configs[[round]] <- current_configs
    all_scores[[round]] <- scores
    all_budgets[[round]] <- rep(current_budget, length(scores))

    # Report best so far
    best_idx <- which.min(scores)
    if(verbose) {
      cat(sprintf("\nBest score this round: %.4f\n", scores[best_idx]))
      cat("Best config: ")
      print(current_configs[best_idx, ])
    }

    # Log round summary
    if (!is.null(log_file)) {
      write_log(log_file, sprintf("Round %d completed. Best score: %.4f", round, scores[best_idx]))
    }

    # Prepare for next round (if not last)
    if(round < n_rounds) {
      # Combine all historical data
      historical_configs <- do.call(rbind, all_configs)
      historical_scores <- unlist(all_scores)

      # Fit new surrogate of enough datapoints collected
      # TODO: sampling could be done with EA if too little data
      if (nrow(historical_configs) >= 2 * (ncol(historical_configs) + 1)){
        # Fit surrogate model
        surrogate <- fit_tpe_surrogate(historical_configs, historical_scores,
                                       gamma, bw_factor)
      }

      # New configurations
      # Rules: At least 20% top configs of previous iteration, at least 20%
      # randomly sampled, rest sampled from tpe
      n_next <- ceiling(nrow(current_configs) * halving_proportion)
      n_quintile <- ceiling(n_next * 0.2)

      # Top from previous run
      scores_ordered <- order(historical_scores)
      top_idx <- scores_ordered[1:n_quintile]
      top <- logical(nrow(historical_configs))
      top[top_idx] <- TRUE
      top_configs <- historical_configs[top, , drop = FALSE]

      # Random sample from space
      random_configs <- generate_uniform_samples(n_quintile, param_bounds, param_types)

      # Sample from TPE
      n_tpe <- n_next - (2 * n_quintile)
      tpe_configs <- sample_tpe(surrogate, n_tpe, param_bounds, param_types)

      # Add all new configs together
      current_configs <- rbind(top_configs, random_configs, tpe_configs)

      if(verbose) {
        cat("\nConfigurations for next round:\n")
        cat(sprintf("- Top %d from previous rounds\n", n_quintile))
        cat(sprintf("- %d random samples\n", n_quintile))
        cat(sprintf("- %d TPE samples\n", n_tpe))
      }

      current_budget <- current_budget * 2

      # Save configurations for next round
      if (!is.null(checkpoint_dir)) {
        save_round_configs(checkpoint_dir, round + 1, current_configs)
      }
    }

    # Reset for next round
    start_config_idx <- 1
  }

  # Compile final results
  final_configs <- do.call(rbind, all_configs)
  final_scores <- unlist(all_scores)
  final_budgets <- unlist(all_budgets)

  best_overall_idx <- which.min(final_scores)

  # Log final results
  if (!is.null(log_file)) {
    write_log(log_file, sprintf("\n=== Optimization Complete ==="))
    write_log(log_file, sprintf("Total evaluations: %d", total_evaluations))
    write_log(log_file, sprintf("Best score: %.4f", final_scores[best_overall_idx]))
    write_log(log_file, sprintf("Best config: %s",
                                paste(sprintf("%s=%s",
                                              names(final_configs[best_overall_idx,]),
                                              final_configs[best_overall_idx,]),
                                      collapse = ", ")))
  }

  # Clean up checkpoint file if completed successfully
  if (!is.null(checkpoint_dir)) {
    checkpoint_file <- file.path(checkpoint_dir, "checkpoint.rds")
    if (file.exists(checkpoint_file)) {
      file.remove(checkpoint_file)
      if (verbose) cat("\nCheckpoint file removed after successful completion.\n")
    }
  }

  return(list(
    configs = final_configs,
    scores = final_scores,
    budgets = final_budgets,
    best_config = final_configs[best_overall_idx, , drop = FALSE],
    best_score = final_scores[best_overall_idx],
    n_evaluations = total_evaluations
  ))
}

#' Write to log file
#'
#' Internal helper function to write messages to the log file
#'
#' @param log_file Path to the log file
#' @param message Message to write
#' @param append Whether to append to existing file
#'
#' @keywords internal
write_log <- function(log_file, message, append = TRUE) {
  cat(message, "\n", file = log_file, append = append)
}

#' Save round configurations
#'
#' Internal helper function to save configurations for a round
#'
#' @param checkpoint_dir Directory for checkpoints
#' @param round Round number
#' @param configs Configurations to save
#'
#' @keywords internal
save_round_configs <- function(checkpoint_dir, round, configs) {
  filename <- file.path(checkpoint_dir, sprintf("round_%02d_configs.csv", round))
  write.csv(configs, filename, row.names = FALSE)
}

#' Save round scores
#'
#' Internal helper function to save scores for a round
#'
#' @param checkpoint_dir Directory for checkpoints
#' @param round Round number
#' @param scores Scores to save
#'
#' @keywords internal
save_round_scores <- function(checkpoint_dir, round, scores) {
  filename <- file.path(checkpoint_dir, sprintf("round_%02d_scores.csv", round))
  write.csv(data.frame(score = scores), filename, row.names = FALSE)
}

#' Objective Function Wrapper for Hyperparameter Scoring
#'
#' Internal function that wraps the score_hyperparameters function call
#' with the appropriate arguments structure for use within hpoptimize.
#'
#' @param hp A list of hyperparameter values to evaluate.
#' @param n_sims Number of simulations to run for evaluation.
#' @param years Vector of years to include in the evaluation.
#' @param league_ids Vector of league identifiers to use for evaluation.
#' @param parallel Logical indicating whether to use parallel processing.
#'
#' @return Numeric score from score_hyperparameters (lower is better).
#'
#' @details
#' This function serves as an adapter between the optimization framework
#' and the score_hyperparameters function. It constructs the appropriate
#' argument list and handles the function call.
#'
#' @note This is an internal function used by hpoptimize and typically
#'   should not be called directly by users.
#'
#' @keywords internal
objective_fn <- function(hp, n_sims, years, league_ids, parallel){
  #TODO: input checks
  args <- list(hyperparams = hp,
               n_sims = n_sims,
               years = years,
               league_ids = league_ids,
               parallel = parallel)
  do.call(score_hyperparameters, args)
}

#' List HPO Runs
#'
#' Lists all hyperparameter optimization runs in the runs/hpo directory
#'
#' @param runs_dir Base directory containing runs (default: "runs/hpo")
#' @param full_info Logical, whether to load and display full run information
#'
#' @return A data frame with run information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # List all runs
#' list_hpo_runs()
#'
#' # Get detailed information
#' list_hpo_runs(full_info = TRUE)
#' }
list_hpo_runs <- function(runs_dir = "runs/hpo", full_info = FALSE) {
  if (!dir.exists(runs_dir)) {
    message("No runs directory found")
    return(NULL)
  }

  run_dirs <- list.dirs(runs_dir, full.names = TRUE, recursive = FALSE)
  if (length(run_dirs) == 0) {
    message("No runs found")
    return(NULL)
  }

  run_info <- data.frame(
    run_name = basename(run_dirs),
    path = run_dirs,
    stringsAsFactors = FALSE
  )

  # Extract basic info from directory names
  run_info$timestamp <- sub("_.*", "", run_info$run_name)

  # Try to get more info from each run
  for (i in seq_len(nrow(run_info))) {
    # Check if run completed
    checkpoint_file <- file.path(run_info$path[i], "checkpoint.rds")
    run_info$completed[i] <- !file.exists(checkpoint_file)

    # Get number of evaluations from log
    log_file <- file.path(run_info$path[i], "optimization_log.txt")
    if (file.exists(log_file)) {
      log_content <- readLines(log_file)
      eval_lines <- grep("^\\[.*\\] Eval [0-9]+", log_content, value = TRUE)
      run_info$n_evaluations[i] <- length(eval_lines)

      # Get best score if completed
      if (run_info$completed[i]) {
        best_score_line <- grep("Best score:", log_content, value = TRUE)
        if (length(best_score_line) > 0) {
          run_info$best_score[i] <- as.numeric(sub(".*Best score: ([0-9.]+).*", "\\1",
                                                   tail(best_score_line, 1)))
        }
      }
    }

    # Get metadata if requested
    if (full_info) {
      metadata_file <- file.path(run_info$path[i], "run_metadata.rds")
      if (file.exists(metadata_file)) {
        metadata <- readRDS(metadata_file)
        run_info$leagues[i] <- paste(metadata$league_ids, collapse = ",")
        run_info$years[i] <- paste(range(metadata$years), collapse = "-")
        run_info$n_initial[i] <- metadata$n_initial
        run_info$n_rounds[i] <- metadata$n_rounds
      }
    }
  }

  # Sort by timestamp (most recent first)
  run_info <- run_info[order(run_info$timestamp, decreasing = TRUE), ]

  return(run_info)
}

#' Clean Up Old HPO Runs
#'
#' Removes old or incomplete HPO runs, keeping only the most recent or best ones
#'
#' @param runs_dir Base directory containing runs (default: "runs/hpo")
#' @param keep_n Number of most recent completed runs to keep (default: 10)
#' @param keep_best Number of best-scoring runs to keep (default: 5)
#' @param remove_incomplete Whether to remove incomplete runs (default: TRUE)
#' @param dry_run If TRUE, only shows what would be deleted without actually deleting
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # See what would be cleaned up
#' cleanup_hpo_runs(dry_run = TRUE)
#'
#' # Actually clean up
#' cleanup_hpo_runs(keep_n = 5, keep_best = 3)
#' }
cleanup_hpo_runs <- function(runs_dir = "runs/hpo",
                             keep_n = 10,
                             keep_best = 5,
                             remove_incomplete = TRUE,
                             dry_run = FALSE) {

  runs <- list_hpo_runs(runs_dir, full_info = FALSE)
  if (is.null(runs)) return(invisible(NULL))

  to_keep <- character()

  # Keep the most recent completed runs
  completed_runs <- runs[runs$completed, ]
  if (nrow(completed_runs) > 0) {
    recent_to_keep <- head(completed_runs$path, keep_n)
    to_keep <- c(to_keep, recent_to_keep)

    # Keep the best scoring runs
    if (!is.null(completed_runs$best_score) &&
        any(!is.na(completed_runs$best_score))) {
      best_runs <- completed_runs[order(completed_runs$best_score), ]
      best_to_keep <- head(best_runs$path, keep_best)
      to_keep <- c(to_keep, best_to_keep)
    }
  }

  # Keep incomplete runs unless specified otherwise
  if (!remove_incomplete) {
    incomplete_runs <- runs[!runs$completed, ]
    to_keep <- c(to_keep, incomplete_runs$path)
  }

  to_keep <- unique(to_keep)
  to_remove <- setdiff(runs$path, to_keep)

  if (length(to_remove) == 0) {
    message("No runs to clean up")
    return(invisible(NULL))
  }

  cat(sprintf("Runs to remove (%d):\n", length(to_remove)))
  for (path in to_remove) {
    cat(sprintf("  - %s\n", basename(path)))
  }

  if (!dry_run) {
    cat("\nRemoving runs...\n")
    for (path in to_remove) {
      unlink(path, recursive = TRUE)
      cat(sprintf("  Removed: %s\n", basename(path)))
    }
    message(sprintf("Cleaned up %d runs", length(to_remove)))
  } else {
    message("\nDry run - no files were deleted")
  }

  return(invisible(to_remove))
}
