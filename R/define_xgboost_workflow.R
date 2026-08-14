
define_xgboost_workflow <- function(regression_model_recipe, xgboost_tuning_specs) {
  workflow(regression_model_recipe, xgboost_tuning_specs)
}