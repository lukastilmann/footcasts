#' Fit a Tree Parzen Estimator (TPE) Surrogate Model
#'
#' Fits kernel density estimates to good and bad configurations based on
#' their scores, using the TPE approach for Bayesian optimization.
#'
#' @param configs A data frame containing the hyperparameter configurations.
#'   Each column represents a hyperparameter.
#' @param scores A numeric vector of scores/losses for each configuration.
#'   Lower scores are considered better.
#' @param gamma Numeric value between 0 and 1 specifying the quantile threshold
#'   for splitting configurations into "good" and "bad" groups. Default is 0.25,
#'   meaning the best 25% of configurations are considered "good".
#' @param bw_factor Numeric value that is multiplied with KDE-bandwidths. Values
#'   above 1 allow for more exploration of the space. Default is 1.
#'
#' @return A list containing:
#'   \item{dens_good}{np density object for good configurations}
#'   \item{dens_bad}{np density object for bad configurations}
#'   \item{good_data}{Data frame of configurations classified as good}
#'   \item{bad_data}{Data frame of configurations classified as bad}
#'
#' @export
#' @importFrom np npudensbw npudens
#'
#' @examples
#' \dontrun{
#' # Create example data
#' configs <- data.frame(
#'   x1 = runif(100, -5, 5),
#'   x2 = sample(1:10, 100, replace = TRUE),
#'   x3 = factor(sample(c("a", "b", "c"), 100, replace = TRUE))
#' )
#' scores <- apply(configs, 1, function(x) sum(as.numeric(x[1:2])^2))
#'
#' # Fit surrogate model
#' surrogate <- fit_tpe_surrogate(configs, scores, gamma = 0.25)
#' }
fit_tpe_surrogate <- function(configs, scores, gamma = 0.25, bw_factor = 1) {
  # Ensure configs is a data frame
  configs <- as.data.frame(configs)

  # Check if enough configs to fit at least with good and bad scores split at median
  n_params <- ncol(configs)
  n_data <- nrow(configs)

  if (n_data < 2 * (n_params + 1)){
    stop("Attempted to fit surrogate model with too few data points.")
  }

  # Ensure we have enough points in each group for KDE
  n_good_target <- ceiling(max(n_data * gamma, n_params + 1))
  n_bad_target <- n_params + 1

  # Sort scores and get indices for splitting
  sorted_indices <- order(scores)
  good_idx <- sorted_indices[1:n_good_target]
  bad_idx <- sorted_indices[(n_good_target + 1):n_data]

  # Create good/bad splits using logical indexing
  is_good <- logical(n_data)
  is_good[good_idx] <- TRUE

  good_data <- configs[is_good, , drop = FALSE]
  bad_data <- configs[!is_good, , drop = FALSE]

  # Fit kernel density estimates with bounded kernels
  bw_good <- np::npudensbw(
    dat = good_data,
    bwmethod = "cv.ml",
    ckertype = "epanechnikov"
  )

  bw_bad <- np::npudensbw(
    dat = bad_data,
    bwmethod = "cv.ml",
    ckertype = "epanechnikov"
  )

  bw_good$bw <- bw_good$bw * bw_factor
  bw_bad$bw <- bw_bad$bw * bw_factor

  # Multiplying bandwidths by factor increases exploration of space
  dens_good <- np::npudens(bws = bw_good)
  dens_bad <- np::npudens(bws = bw_bad)

  # Return all components needed for sampling
  list(
    dens_good = dens_good,
    dens_bad = dens_bad,
    good_data = good_data,
    bad_data = bad_data
  )
}


#' Generate Uniform Random Samples Within Parameter Bounds
#'
#' Generates random samples uniformly distributed within the specified
#' parameter bounds, respecting the parameter types.
#'
#' @param n Integer specifying the number of samples to generate.
#' @param param_bounds A named list where each element specifies the bounds
#'   for a parameter. For continuous and integer parameters, provide a
#'   numeric vector of length 2 with (min, max). For factor parameters,
#'   provide a character vector of possible values.
#' @param param_types A named list specifying the type of each parameter.
#'   Valid types are "continuous", "integer", or "factor". Names must
#'   match those in param_bounds.
#'
#' @return A data frame with n rows and one column per parameter, containing
#'   the generated samples with appropriate types.
#'
#' @export
#'
#' @examples
#' # Define parameter space
#' param_bounds <- list(
#'   learning_rate = c(0.001, 0.1),
#'   n_trees = c(50, 500),
#'   loss = c("mse", "mae", "huber")
#' )
#'
#' param_types <- list(
#'   learning_rate = "continuous",
#'   n_trees = "integer",
#'   loss = "factor"
#' )
#'
#' # Generate 10 random configurations
#' samples <- generate_uniform_samples(10, param_bounds, param_types)
# Generate uniform samples within bounds
generate_uniform_samples <- function(n, param_bounds, param_types) {

  samples <- data.frame(matrix(ncol = length(param_bounds), nrow = n))
  names(samples) <- names(param_bounds)

  for(param in names(param_bounds)) {
    bounds <- param_bounds[[param]]
    type <- param_types[[param]]

    if(type == "continuous") {
      samples[[param]] <- runif(n, bounds[1], bounds[2])
    } else if(type == "integer") {
      samples[[param]] <- as.integer(round(runif(n, bounds[1], bounds[2])))
    } else if(type == "factor") {
      samples[[param]] <- factor(sample(bounds, n, replace = TRUE), levels = bounds)
    }
  }

  return(samples)
}

#' Rejection Sampling from TPE Good Distribution
#'
#' Performs rejection sampling to draw samples from the "good" distribution
#' of a fitted TPE surrogate model.
#'
#' @param surrogate A fitted TPE surrogate model object returned by
#'   \code{\link{fit_tpe_surrogate}}.
#' @param n_samples_target Integer specifying the target number of samples
#'   to generate.
#' @param param_bounds A named list of parameter bounds (see
#'   \code{\link{generate_uniform_samples}}).
#' @param param_types A named list of parameter types (see
#'   \code{\link{generate_uniform_samples}}).
#' @param max_attempts Maximum number of rejection sampling attempts.
#'   Default is 10000.
#'
#' @return A data frame containing the accepted samples. If fewer than
#'   n_samples_target samples are accepted within max_attempts, a warning
#'   is issued and the available samples are returned.
#'
#' @details
#' This function uses rejection sampling with a uniform proposal distribution.
#' The maximum density is estimated from a set of test points with a 20%
#' buffer to improve acceptance rates.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming surrogate is already fitted
#' samples <- rejection_sample_tpe(
#'   surrogate,
#'   n_samples_target = 100,
#'   param_bounds,
#'   param_types
#' )
#' }
rejection_sample_tpe <- function(surrogate, n_samples_target, param_bounds,
                                 param_types, max_attempts = 10000) {
  samples <- list()
  n_accepted <- 0
  attempts <- 0

  # Estimate maximum density
  test_points <- generate_uniform_samples(
    min(1000, max_attempts / 10),
    param_bounds,
    param_types
  )
  test_densities <- predict(surrogate$dens_good, edat = test_points)
  max_dens <- max(test_densities) * 1.1  # 10% buffer

  while(n_accepted < n_samples_target && attempts < max_attempts) {
    attempts <- attempts + 1

    # Generate uniform proposal
    proposal <- generate_uniform_samples(1, param_bounds, param_types)

    # Evaluate density at proposal
    prop_dens <- predict(surrogate$dens_good, edat = proposal)

    # Accept/reject
    if(runif(1) < (prop_dens / max_dens)) {
      n_accepted <- n_accepted + 1
      samples[[n_accepted]] <- proposal
    }
  }

  if(n_accepted < n_samples_target) {
    warning(sprintf("Only accepted %d samples out of %d requested after %d attempts",
                    n_accepted, n_samples_target, max_attempts))
  }

  return(do.call(rbind, samples))
}

#' Sample New Configurations Using TPE Acquisition Function
#'
#' Samples new configurations from a fitted TPE surrogate model by
#' maximizing the expected improvement (EI) acquisition function,
#' defined as the ratio of good to bad density.
#'
#' @param surrogate A fitted TPE surrogate model object returned by
#'   \code{\link{fit_tpe_surrogate}}.
#' @param n_samples Integer specifying the number of configurations to return.
#' @param param_bounds A named list of parameter bounds (see
#'   \code{\link{generate_uniform_samples}}).
#' @param param_types A named list of parameter types (see
#'   \code{\link{generate_uniform_samples}}).
#'
#' @return A data frame containing n_samples configurations selected based
#'   on the highest expected improvement scores.
#'
#' @details
#' The function oversamples candidates (20x the requested number) from the
#' good distribution using rejection sampling, then selects the top n_samples
#' based on the acquisition function p(x|good)/p(x|bad).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming surrogate is already fitted
#' new_configs <- sample_tpe(
#'   surrogate,
#'   n_samples = 32,
#'   param_bounds,
#'   param_types
#' )
#' }
# Sample new configurations using TPE acquisition
sample_tpe <- function(surrogate, n_samples, param_bounds, param_types) {
  # Sample many candidates from good distribution
  n_candidates <- n_samples * 10
  candidates <- rejection_sample_tpe(
    surrogate, n_candidates, param_bounds, param_types
  )

  # Evaluate acquisition function
  p_good <- predict(surrogate$dens_good, edat = candidates)
  p_bad <- predict(surrogate$dens_bad, edat = candidates)

  # Avoid division by zero
  p_bad[p_bad < 1e-10] <- 1e-10

  # Compute EI scores
  ei_scores <- p_good / p_bad

  # Return top n_samples by EI score
  top_indices <- order(ei_scores, decreasing = TRUE)[1:min(n_samples, nrow(candidates))]
  selected <- candidates[top_indices, , drop = FALSE]

  # Ensure correct types
  for(param in names(param_types)) {
    if(param_types[[param]] == "integer") {
      selected[[param]] <- as.integer(round(selected[[param]]))
    }
  }

  return(selected)
}
