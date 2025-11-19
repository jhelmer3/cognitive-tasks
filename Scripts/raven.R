
# setup ----

options(scipen=999)

library(tidyverse)
library(mirt)
library(rstan)

here::here()

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = T)

# reading ----

raven <- rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/task_datasets/data_raven.csv") |>
  left_join(rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/metadata_tables/session.csv"),
            by = "session_id") |>
  left_join(rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_forecasting/processed_data/scores_quantile.csv") |>
              select(subject_id, sscore_standardized) |>
              summarize(.by = subject_id,
                        sscore = mean(sscore_standardized)),
            by = "subject_id") |>
  select(subject_id, list_id, stimulus, correct, sscore) 


# ravdat <- pmap_df(expand_grid(n_items = round(seq(10, 42, length.out = 5)),
#                               listid = raven |> pluck("list_id") |> unique(),
#                               rep = 1:10), 
#                   \(n_items, listid, rep) {
# 
#                     raven |>
#                       filter(list_id == listid) |>
#                       filter(stimulus %in% sample(unique(stimulus), n_items)) |>
#                       pivot_wider(names_from = stimulus, values_from = correct) |>
#                       mutate(meanscore = rowMeans(across(!c(subject_id, sscore)), na.rm = T)) |> # alert alert na.rm = T
#                       select(sscore, meanscore) |>
#                       summarize(listid = listid,
#                                 n_items = n_items,
#                                 rep = rep,
#                                 cor = cor(sscore, meanscore) |> abs())
#                   })

rav <- function(n_items, listid, rep = 1) {
  pmap_df(expand_grid(n_items = n_items,
                      listid = listid,
                      rep = rep), 
          \(n_items, listid, rep) {
            
            raven |>
              filter(list_id == listid) |>
              filter(stimulus %in% sample(unique(stimulus), n_items)) |>
              pivot_wider(names_from = stimulus, values_from = correct) |>
              mutate(meanscore = rowMeans(across(!c(subject_id, sscore)), na.rm = T)) |> # alert alert na.rm = T
              select(sscore, meanscore) |>
              summarize(listid = listid,
                        n_items = n_items,
                        rep = rep,
                        cor = cor(sscore, meanscore) |> abs())
          })
}

rav_1_12_18 <- rav(n_items = 10:42,
    listid = c(1, 12, 18),
    rep = 1:100)

rav_all <- rav(n_items = 10:42,
                   listid = raven |> pluck("list_id") |> unique(),
                   rep = 1:10) # needs running and saving

saveRDS(rav_1_12_18, here::here("Data", "Raven", "rav_1_12_18.rds"))
saveRDS(rav_all, here::here("Data", "Raven", "rav_all.rds"))

## archive ----

raven <- rio::import("https://raw.githubusercontent.com/forecastingresearch/fpt/refs/heads/main/data_cognitive_tasks/task_datasets/data_raven.csv") 

raven |>
  select(stimulus) |>
  unique() |>
  nrow()
  
raven |>
  select(session_id, list_id, stimulus, correct) |>
  summarize(.by = c(list_id, stimulus),
            n = n())

raven |>
  summarize(.by = stimulus,
            n_listids = length(unique(list_id))) |>
  filter(n_listids > 1)

raven |>
  filter(stimulus == "A4_1") |>
  select(stimulus, list_id) |>
  unique()



