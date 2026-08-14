
get_penalized_regression_final_recipe <- function(penalized_regression_workflow,
                                                  penalized_regression_best_model,
                                                  penalized_regression_tuning_specs) {
  finalize_workflow(
    penalized_regression_workflow |> add_model(penalized_regression_tuning_specs),
    penalized_regression_best_model
  )
}

