
tune_penalized_regression_model <- function(penalized_regression_type,
                                            item_data_training_bootstraps,
                                            penalized_regression_workflow, 
                                            tuning_specs) {
  lambda_grid <- grid_regular(penalty(), levels = 50)
  
  if (penalized_regression_type == "elasticnet") {
    lambda_grid <- lambda_grid |>
      expand_grid(mixture = seq(0.05, 0.95, 0.15))
  }
  
  tune_grid(
    penalized_regression_workflow |> add_model(tuning_specs),
    resamples = item_data_training_bootstraps,
    grid = lambda_grid,
    metrics = metric_set(rmse)
  )
}

# tar_read(penalized_regression_model_tuning_grid_lasso) |>
#   pull(.notes)
