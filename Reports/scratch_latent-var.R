
library(tidyverse)
library(cmdstanr)
library(bayesplot)

n_people <- 1000
n_items <- 9

lambdas <- rep(c(.4, .6, .8), each = 3)

d <- tibble(n_people = n_people,
       n_items = n_items) |>
  uncount(n_people, .id = "person_id") |>
  mutate(theta_g = rnorm(n())) |>
  uncount(n_items, .id = "item_id") |>
  mutate(
    lambda = lambdas[item_id],
    item_y = rnorm(n(), lambda * theta_g) # intercept = 0
    ) 

stan_d <- list(
  N_j = length(unique(d$person_id)),
  N_k = length(unique(d$item_id)),
  N = nrow(d),
  j = d$person_id,
  k = d$item_id,
  y = d$item_y
)

init_fn <- function() {
  list(
    lambdas_rest = rep(0.5, n_items - 1),
    lambdas_last = array(0.8, dim = 1),
    theta_j = rnorm(n_people),
    sigma_k = rep(0.5, n_items)
  )
}

mod <- cmdstan_model(here::here("Models", "one_factor.stan"))
stan_fit <- mod$sample(stan_d, parallel_chains = 4)
draws <- stan_fit$draws()

map_vec(1:9, \(item_id) glue::glue("sigma_k[{item_id}]")) |>
  bayesplot::mcmc_areas_ridges(draws, pars = _)

map_vec(1:9, \(item_id) glue::glue("lambdas[{item_id}]")) |>
  bayesplot::mcmc_areas_ridges(draws, pars = _)

map_vec(1:9, \(item_id) glue::glue("lambda_std[{item_id}]")) |>
  bayesplot::mcmc_areas_ridges(draws, pars = _)

map_vec(1:9, \(item_id) glue::glue("lambda_std[{item_id}]")) |>
  bayesplot::mcmc_rank_overlay(draws, pars = _)





d_wide <- d |>
  pivot_wider(id_cols = -lambda,
              names_from = item_id, values_from = item_y, names_glue = "item_{item_id}")

lav_frm <- map_chr(seq.int(1, n_items), \(item_id) paste0("item_", item_id)) |>
  paste(collapse = " + ") |>
  paste("y =~", ... = _)

fit <- lavaan::cfa(model = lav_frm,
                   data = d_wide)

lavaan::standardizedsolution(fit)

d |>
  mutate(y_pred = rep(lavaan::predict(fit, newdata = d_wide), each = n_items)) |>
  select(theta_g, y_pred) |>
  unique() |>
  ggplot(aes(x = theta_g, y = y_pred)) +
  geom_point()

