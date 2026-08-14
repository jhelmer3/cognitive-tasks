
get_penalized_regression_fit_statistics <- function(item_data_validation_set_wide,
                                                    penalized_regression_final_recipe) {
  penalized_regression_final_recipe |>
    fit_resamples(item_data_validation_set_wide, 
                  metrics = metric_set(rmse, rsq, mae)) |>
    collect_metrics(summarize = F)
}


