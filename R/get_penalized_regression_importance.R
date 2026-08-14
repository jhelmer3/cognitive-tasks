
get_penalized_regression_importance <- function(item_data_training_wide,
                                                penalized_regression_final_recipe,
                                                penalized_regression_best_model) {
  penalized_regression_final_recipe |>
    fit(item_data_training_wide) |>
    extract_fit_parsnip() |>
    vi(lambda = penalized_regression_best_model$penalty) |>
    rename(rank = Importance, item = Variable)
}
