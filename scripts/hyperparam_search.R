devtools::load_all(".")
devtools::install(quick = TRUE, quiet = FALSE)
library(footcasts)

param_bounds <- list(
  update_rate = c(0.01, 0.3),
  # Any tests indicated that only using xG is ideal, so omitted for now
  #xG_weight = c(0, 1),
  var_factor = c(0.1, 3),
  variance_decay_param = c(0.01, 1),
  mixture_weight_param = c(0.01, 0.5),
  mixture_start_matchday = c(3, 20)
)

param_types <- list(
  update_rate = "continuous",
  #xG_weight = "continuous",
  var_factor = "continuous",
  variance_decay_param = "continuous",
  mixture_weight_param = "continuous",
  mixture_start_matchday = "integer"
)

# Run optimization
results <- hpoptimize(
  years = CONFIG$seasons$eval,
  league_ids = CONFIG$leagues$enabled_ids,
  param_bounds = param_bounds,
  param_types = param_types,
  n_initial = 64,
  initial_budget = 25,
  halving_proportion = 0.5,
  gamma = 0.2,
  n_rounds = 4,
  parallel = FALSE,
  bw_factor = 1.5
)
