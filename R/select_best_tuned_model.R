
select_best_tuned_model <- function(penalized_regression_model_tuning_grid) {
  penalized_regression_model_tuning_grid |>
    select_best(metric = "rmse")
}

# tar_read(penalized_regression_model_tuning_grid_lasso) |>
#   select_best(metric = 'rmse')
