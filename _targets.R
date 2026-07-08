
library(targets)
library(tarchetypes) 
library(stantargets)
library(crew)

tar_option_set(
  packages = c("tidyverse"),
  format = "qs",
  controller = crew_controller_local(workers = 4)
)

tar_source()

zip_filename <- "jatos_results_20260609223410"
scores_quantile_url <- "https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_forecasting/processed_data/scores_quantile.csv"

# Replace the target list below with your own:
list(
  #tar_target(unzipped_files, unzip_zip(zip_filename))
  tar_target(scores_quantile, rio::import(scores_quantile_url)),
  tar_target(quantile_pers, quantile_agg_pers(scores_quantile)),
  tar_target(dn_s_data, readRDS(here::here("Data", "Denominator Neglect", "dn_dat_c.rds"))),
  tar_target(dn_s_stan_data, make_2pl_stan_data(dn_s_data, quantile_pers)),
  tar_stan_mcmc(dn_s_2pl, "Models/irt_2pl_code.stan", dn_s_stan_data)
)
