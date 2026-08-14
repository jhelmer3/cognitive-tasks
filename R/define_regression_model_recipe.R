
define_regression_model_recipe <- function(item_data_training_wide) {
  # building "recipe" of all items regressed on sscore
  recipe <- recipe(sscore ~ ., data = item_data_training_wide) |>
    # not including subject_id
    update_role(subject_id, new_role = "ID") |>
    # and z-scoring first
    step_normalize(all_numeric(), -all_outcomes())
}