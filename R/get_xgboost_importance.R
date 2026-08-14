
get_xgboost_importance <- function(item_data_training_wide, xgboost_final_recipe) {
  xgboost_final_recipe |>
    fit(item_data_training_wide) |>
    extract_fit_parsnip() |>
    vi() |>
    rename(rank = Importance, item = Variable)
}
