
## penalized regressions
penalized_regression_mapped <- list(
  tar_map(
    list(penalized_regression_type = c("lasso", "elasticnet", "ridge")),
    
    tar_target(penalized_regression_tuning_specs, 
               define_tuning_specs(penalized_regression_type)),
    tar_target(
      penalized_regression_model_tuning_grid, 
      tune_penalized_regression_model(
        penalized_regression_type, 
        item_data_training_bootstraps,
        penalized_regression_workflow, 
        penalized_regression_tuning_specs
      )
    ),
    tar_target(penalized_regression_best_model, 
               select_best_tuned_model(penalized_regression_model_tuning_grid)),
    tar_target(
      penalized_regression_final_recipe, 
      get_penalized_regression_final_recipe(
        penalized_regression_workflow,
        penalized_regression_best_model,
        penalized_regression_tuning_specs
      )
    ),
    tar_target(
      penalized_regression_importance, 
      get_penalized_regression_importance(
        item_data_training_wide,
        penalized_regression_final_recipe,
        penalized_regression_best_model
      )
    ),
    tar_target(
      penalized_regression_fit_statistics,
      get_penalized_regression_fit_statistics(
        item_data_validation_set_wide,
        penalized_regression_final_recipe
      )
    )
  )
)

penalized_regression_combined <- list(
  tar_combine(penalized_regression_fit_statistics_combined,
              # index based on `penalized_regression_fit_statistics`'s 
              # position within `tar_map()`
              penalized_regression_mapped[[1]][[6]],
              command = dplyr::bind_rows(!!!.x, .id = "method"))
)

penalized_regression_targets <- c(
  tar_target(penalized_regression_workflow, define_penalized_regression_workflow(regression_model_recipe)),
  tar_target(item_data_training_bootstraps, rsample::bootstraps(item_data_training_wide)),
  penalized_regression_mapped,
  penalized_regression_combined
)
