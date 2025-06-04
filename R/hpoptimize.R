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
#' # Run optimization for specific leagues and years
#' results <- hpoptimize(
#'   league_ids = c(1, 2, 3),
#'   years = 2018:2022,
#'   param_bounds = param_bounds,
#'   param_types = param_types,
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
                       n_initial = 64, initial_budget = 100,
                       halving_proportion = 0.5, n_rounds = 5,
                       gamma = 0.25, bw_factor = 1,
                       verbose = TRUE, parallel = TRUE) {

  # Storage for all evaluations
  all_configs <- list()
  all_scores <- list()
  all_budgets <- list()

  # Generate initial configurations
  current_configs <- generate_uniform_samples(n_initial, param_bounds, param_types)
  current_budget <- initial_budget

  # Main optimization loop
  for(round in 1:n_rounds) {
    if(verbose) {
      cat(sprintf("\n--- Round %d ---\n", round))
      cat(sprintf("Evaluating %d configs with budget %d\n",
                  nrow(current_configs), current_budget))
    }

    # Evaluate configurations
    scores <- as.numeric(nrow(current_configs))
    for(i in 1:nrow(current_configs)) {
      config <- as.list(current_configs[i, ])
      budget <- current_budget
      print(config)
      scores[i] <- objective_fn(hp = config,
                                n_sims = budget,
                                years = years,
                                league_ids = league_ids,
                                parallel = parallel)

      if(verbose && i %% 10 == 0) cat(".")
    }
    if(verbose) cat("\n")

    # Store results
    all_configs[[round]] <- current_configs
    all_scores[[round]] <- scores
    all_budgets[[round]] <- rep(current_budget, length(scores))

    # Report best so far
    best_idx <- which.min(scores)
    if(verbose) {
      cat(sprintf("Best score this round: %.4f\n", scores[best_idx]))
      cat("Best config: ")
      print(current_configs[best_idx, ])
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
      print("Sampled configs for this round:")
      print(current_configs)
      current_budget <- current_budget * 2
    }
  }

  # Compile final results
  final_configs <- do.call(rbind, all_configs)
  final_scores <- unlist(all_scores)
  final_budgets <- unlist(all_budgets)
  best_overall_idx <- which.min(final_scores)

  return(list(
    configs = final_configs,
    scores = final_scores,
    budgets = final_budgets,
    best_config = final_configs[best_overall_idx, , drop = FALSE],
    best_score = final_scores[best_overall_idx],
    n_evaluations = length(final_scores)
  ))
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
