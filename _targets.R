
library(targets)
library(tarchetypes) 
library(stantargets)
library(crew)

tar_option_set(
  packages = c("tidyverse", "tidymodels", "vip", "finetune"),
  format = "qs",
  controller = crew_controller_local(workers = 4),
  error = "trim"
)

tar_source()

zip_filename <- "jatos_results_20260609223410"
scores_quantile_url <- "https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_forecasting/processed_data/scores_quantile.csv"

# Replace the target list below with your own:
list(
  c(
    tar_target(j_gt_hash, j_gt),
    #tar_target(unzipped_files, unzip_zip(zip_filename))
    tar_target(scores_quantile, rio::import(scores_quantile_url)),
    tar_target(quantile_pers, quantile_agg_pers(scores_quantile)),
    tar_target(dn_s_data, readRDS(here::here("Data", "Denominator Neglect", "dn_dat_c.rds"))),
    # tar_target(dn_s_stan_data, make_2pl_stan_data(dn_s_data, quantile_pers)),
    # tar_stan_mcmc(dn_s_2pl, "Models/irt_2pl_code.stan", dn_s_stan_data),
    tar_target(startup_costs, readRDS(here::here("Data", "startup_costs.rds"))),
    tar_target(v5_dat, readRDS(here::here("Data", "v5_dat.rds"))),
    tar_target(item_data, combine_dn_items(v5_dat)),
    tar_target(item_timing_data, make_item_timing_data(item_data)),
    
    tar_target(item_data_split, rsample::group_initial_validation_split(item_data, group = subject_id)),
    tar_target(item_data_training, rsample::training(item_data_split)),
    tar_target(item_data_validation, rsample::validation(item_data_split)),
    tar_target(item_data_testing, rsample::testing(item_data_split)),
    
    tar_target(item_data_training_wide, item_data_pivot(item_data_training)),
    tar_target(item_data_validation_wide, item_data_pivot(item_data_validation)),
    
    tar_target(item_data_validation_set_wide,
               rsample::make_splits(item_data_training_wide, item_data_validation_wide) |>
                 list() |> rsample::manual_rset(ids = "validation")),
    
    tar_target(regression_model_recipe, define_regression_model_recipe(item_data_training_wide)),
    
    ## search algorithm
    tar_target(search_algorithm_test, conduct_search_algorithm(item_data_training, startup_costs)),
    tar_target(search_algorithm_plt, plt_search_algorithm(search_algorithm_test)),
    tar_target(search_algorithm_short_test, 
               extract_shorter_test(search_algorithm_test, item_timing_data, startup_costs)),
    tar_target(search_algorithm_short_test_r2_train, get_test_r2(item_data_testing, search_algorithm_short_test)),
    tar_target(search_algorithm_short_test_r2, get_test_r2(item_data_training, search_algorithm_short_test)),
    
    ## penalized regressions
    penalized_regression_targets,
    tar_target(elasticnet_short_test, 
               extract_shorter_test(penalized_regression_importance_elasticnet, item_timing_data, startup_costs)),
    tar_target(elasticnet_short_test_r2_train, get_test_r2(item_data_training, elasticnet_short_test)),
    tar_target(elasticnet_short_test_r2, get_test_r2(item_data_testing, elasticnet_short_test)),
    
    ## XGBoost
    tar_target(item_data_training_fivefold, rsample::vfold_cv(item_data_training_wide, v = 5)),
    tar_target(xgboost_tuning_specs, define_tuning_specs("xgboost")),
    tar_target(xgboost_workflow, define_xgboost_workflow(regression_model_recipe, xgboost_tuning_specs)),
    tar_target(xgboost_tuning_race, tune_xgboost(item_data_training_fivefold, xgboost_workflow)),
    tar_target(xgboost_final_recipe, get_xgboost_final_recipe(xgboost_workflow, xgboost_tuning_race)),
    tar_target(xgboost_importance, get_xgboost_importance(item_data_training_wide, xgboost_final_recipe)),
    tar_target(xgboost_short_test, extract_shorter_test(xgboost_importance, item_timing_data, startup_costs)),
    tar_target(xgboost_short_test_r2_train, get_test_r2(item_data_training, xgboost_short_test)),
    tar_target(xgboost_short_test_r2, get_test_r2(item_data_testing, xgboost_short_test)),
    
    ## reports
    tar_quarto(index, "index.qmd"),
    tar_quarto(penalized_regressions_report, "Reports/penalized_regressions.qmd"),
    tar_quarto(post_summer_report, "Reports/post-summer.qmd")
  )
)



















