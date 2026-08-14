
define_penalized_regression_workflow <- function(regression_model_recipe) {
  workflow() |>
    add_recipe(regression_model_recipe)
}