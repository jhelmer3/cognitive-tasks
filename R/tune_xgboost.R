
tune_xgboost <- function(item_data_training_fivefold,
                         xgboost_workflow) {
  n_vars <- ncol(item_data_training_fivefold |> pluck("splits", 1, "data")) - 2
  
  xgboost_grid <- grid_space_filling(
    tree_depth(c(5L, 10L)),
    min_n(c(1L, 40L)),
    mtry(c(1L, floor(n_vars / 3))),
    sample_prop(c(0.5, 1.0)),
    learn_rate(c(-3, -1)),
    size = 100)
  
  tune_race_anova(
    xgboost_workflow,
    item_data_training_fivefold,
    grid = xgboost_grid,
    control = control_race(verbose_elim = TRUE))
}

# item_data_training_fivefold <- tar_read(item_data_training_fivefold)
# xgboost_workflow <- tar_read(xgboost_workflow)
# 
# tune_xgboost(item_data_training_fivefold, xgboost_workflow)