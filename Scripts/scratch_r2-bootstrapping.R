
library(tidyverse)
library(patchwork)

n_people <- seq.int(50, 250, by = 50)
n_bootstraps <- seq.int(50, 250, by = 50)
n_reps <- 10

r2s <- c(.43, .44)
n_tasks <- 5

conditions_dat <- tibble(n_reps = n_reps,
                         n_tasks = n_tasks,
                         n_bootstraps = n_bootstraps) |>
  expand_grid(n_people = n_people) |>
  expand_grid(r2 = r2s) |>
  mutate(sigma_error = sqrt(n_tasks * ((1 - r2s) / r2s)))

bootstrapped_dat <- conditions_dat |>
  uncount(n_reps, .id = "rep") |>
  mutate(
    data = pmap(
      list(n_people, n_tasks, sigma_error),
      \(n_people, n_tasks, sigma_error) 
      tibble(id = 1:n_people) |>
        expand_grid(task = 1:n_tasks) |>
        mutate(task_score = rnorm(n_people * n_tasks)) |>
        summarize(.by = id,
                  mu = sum(task_score)) |>
        mutate(y = rnorm(n_people, mu, sigma_error))
    )
  ) |>
  uncount(n_bootstraps, .id = "bootstrap_id", .remove = F) |>
  mutate(
    data_boot = map2(data, n_people, \(data, n_people) data |>
                       slice_sample(n = nrow(data), replace = T)),
    model = map(data_boot,
      \(data_boot) lm(y ~ mu, data = data_boot)),
    r2_obs = map_dbl(model,
      \(model) model |>
        summary() |>
        pluck("r.squared")),
  ) |>
  select(-model, -data_boot) |>
  nest(r2_obs_dat = c(bootstrap_id, r2, r2_obs), .by = c(rep, n_people, n_bootstraps)) |>
  mutate(
    r2_model = map(r2_obs_dat, \(r2_obs_dat) r2_obs_dat |>
                     mutate(r2 = factor(r2)) |>
                     lm(r2_obs ~ r2, data = _)),
    p = map_dbl(r2_model, \(r2_model) r2_model |>
                  anova() |>
                  pluck("Pr(>F)", 1)),
    eta_squared = map_dbl(r2_model,
      \(r2_model) r2_model |>
        effectsize::eta_squared(verbose = F) |>
        purrr::pluck("Eta2")),
    cohens_d = map_dbl(r2_obs_dat,
                       \(r2_obs_dat) r2_obs_dat |>
                         mutate(r2 = factor(r2)) |>
                         effectsize::cohens_d(r2_obs ~ r2, data = _) |>
                         pluck("Cohens_d"))
  ) |>
  select(-r2_model) |>
  unnest(r2_obs_dat) |>
  nest(r2_obs_dat = c(bootstrap_id, r2_obs))
 
# saveRDS(bootstrapped_dat, here::here("Data", "bootstrapped_dat.rds"))

bootstrapped_dat |>
  ggplot(aes(x = cohens_d,
             y = factor(n_people, ordered = T),
             fill = factor(n_people, ordered = T))) +
  ggridges::geom_density_ridges(quantile_lines = T, quantiles = 2, rel_min_height = 0.1) +
  geom_point(data = bootstrapped_dat |>
               summarize(.by = c(n_people, n_bootstraps),
                         cohens_d = mean(cohens_d))) +
  facet_wrap(~ n_bootstraps, ncol = 1) +
  theme_classic() +
  theme(legend.position = "none")

bootstrapped_dat |>
  ggplot(aes(x = eta_squared,
             y = factor(n_people, ordered = T),
             fill = factor(n_people, ordered = T))) +
  ggridges::geom_density_ridges() +
  geom_point(data = bootstrapped_dat |>
               summarize(.by = c(n_people, n_bootstraps),
                         eta_squared = mean(eta_squared))) +
  facet_wrap(~n_bootstraps) +
  theme_classic()

bootstrapped_dat |>
  mutate(sig = p < .05) |>
  summarize(.by = c(n_people, n_bootstraps),
            prop_sig = mean(sig)) |>
  ggplot(aes(y = prop_sig, x = n_people)) +
  geom_point() +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~n_bootstraps)

bootstrapped_dat |>
  select(rep, r2, n_people, r2_obs_dat) |>
  unnest(r2_obs_dat) |>
  summarize(.by = c(rep, n_people, bootstrap_id),
            contrast = reduce(r2_obs, `-`)) |>
  ggplot(aes(x = contrast, color = n_people, group = n_people)) +
  geom_density()

bootstrapped_dat |>
  select(rep, r2, n_people, r2_obs_dat) |>
  mutate(rep_id = row_number()) |>
  unnest(r2_obs_dat) |>
  mutate(r2_obs_z = (r2_obs - mean(r2_obs)) / sd(r2_obs)) |>
  ggplot(aes(x = r2_obs_z, color = r2, group = rep_id)) +
  geom_density() +
  facet_wrap(~ n_people)

bootstrapped_dat |>
  select(rep, r2, n_people, r2_obs_dat) |>
  unnest(r2_obs_dat) |>
  mutate(r2_obs_z = (r2_obs - mean(r2_obs)) / sd(r2_obs)) |>
  summarize(.by = c(rep, n_people, bootstrap_id),
            contrast = reduce(r2_obs, `-`)) |>
  ggplot(aes(x = contrast, group = rep)) +
  geom_vline(aes(xintercept = 0)) +
  geom_line(stat = "density", alpha = 0.4) +
  facet_wrap(~ n_people, ncol = 1) +
  theme_classic()

# tibble(x1 = rnorm(1000, 5, 1),
#        x2 = rnorm(1000, 4, 1),
#        diff = x1 - x2) |>
#   pivot_longer(everything(),
#                names_to = "cond", values_to = "draw") |>
#   ggplot(aes(x = draw, color = cond, group = cond)) +
#   geom_density()

n_bootstraps <- 200

model_strings <- c("Petal.Length ~ Sepal.Length", 
                   "Petal.Length ~ Sepal.Length + Petal.Width", 
                   "Petal.Length ~ Sepal.Length + Petal.Width + Sepal.Width",
                   "Petal.Length ~ Sepal.Length + Petal.Width + Sepal.Width + Species")

iris_bootstrapping <- tibble(
  model_string = model_strings,
  model_frm = model_strings |> map(\(frm) paste(frm, collapse = " ") |> formula()),
  full_data = list(iris |> mutate(id = row_number()) |> tibble()),
  n_bootstraps = n_bootstraps
)

iris_bootstrapping |>
  uncount(n_bootstraps, .id = "bootstrap_id") |>
  mutate(
    data_boot = map(full_data, \(full_data) full_data |>
                      slice_sample(n = nrow(full_data), replace = T)),
    model = map2(data_boot, model_frm, 
                 \(data_boot, model_frm) lm(model_frm, data = data_boot)),
    r2_obs = map_dbl(model, \(model) summary(model)$r.squared)
  ) |>
  ggplot(aes(x = r2_obs, color = model_string, group = model_string)) +
  geom_density() +
  theme(legend.position = "bottom")

iris_bootstrapping |>
  uncount(n_bootstraps, .id = "bootstrap_id") |>
  mutate(
    data_boot = map(full_data, \(full_data) full_data |>
                      slice_sample(n = nrow(full_data), replace = T)),
    model = map2(data_boot, model_frm, 
                 \(data_boot, model_frm) lm(model_frm, data = data_boot)),
    r2_obs = map_dbl(model, \(model) summary(model)$r.squared)
  ) |>
  filter(str_detect(model_string, "Sepal.Width")) |>
  lm(r2_obs ~ model_string, data = _) |>
  summary()

