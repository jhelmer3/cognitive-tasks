
define_tuning_specs <- function(penalized_regression_type) {
  if (penalized_regression_type == "lasso") {
    linear_reg(penalty = tune(), mixture = 1) |>
      set_engine("glmnet")
  } else if (penalized_regression_type == "elasticnet") {
    linear_reg(penalty = tune(), mixture = tune()) |>
      set_engine("glmnet")
  } else if (penalized_regression_type == "ridge") {
    linear_reg(penalty = tune(), mixture = 0) |>
      set_engine("glmnet")
  } else if (penalized_regression_type == "xgboost") {
    boost_tree(
      trees = 1000,
      stop_iter = 20,
      tree_depth = tune(),
      min_n = tune(),
      mtry = tune(),
      sample_size = tune(),
      learn_rate = tune()
    ) |>
      set_engine("xgboost") |>
      set_mode("regression")
  } else {
    stop("Invalid `penalized_regression_type`. Must be one of 'lasso', 'elasticnet', 'ridge', or 'xgboost'.")
  }
}
  