
get_xgboost_final_recipe <- function(xgboost_workflow, xgboost_tuning_race) {
  xgboost_workflow |>
    finalize_workflow(select_best(xgboost_tuning_race, metric = "rmse"))
}
