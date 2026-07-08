
make_2pl_stan_data <- function(data, quantile_pers) {
  stan_data <- data |>
    select(subject_id, item, correct) |>
    left_join(quantile_pers, by = "subject_id") |>
    pivot_wider(names_from = item, values_from = correct) |>
    drop_na() |>
    select(-subject_id)
  
  list(
    J = nrow(stan_data),
    K = ncol(stan_data),
    y = as.matrix(stan_data),
    quantile_pers = stan_data$quantile_pers
  )
}

# make_2pl_stan_data(tar_read(dn_dat_s), tar_read(scores_quantile))
