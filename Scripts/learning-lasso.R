
library(tidyverse)

ratings_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv")

remove_regex <- "[:punct:]|[:digit:]|parts |part |the |and"

office_ratings <- ratings_raw %>%
  mutate(
    episode_name = str_to_lower(title),
    episode_name = str_remove_all(episode_name, remove_regex),
    episode_name = str_trim(episode_name),
    imdb_rating,
    .keep = "none"
  )

office_info <- schrute::theoffice %>%
  mutate(
    season = as.numeric(season),
    episode = as.numeric(episode),
    episode_name = str_to_lower(episode_name),
    episode_name = str_remove_all(episode_name, remove_regex),
    episode_name = str_trim(episode_name)
  ) %>%
  select(season, episode, episode_name, director, writer, character)

office_info

characters <- office_info %>%
  count(episode_name, character) %>%
  add_count(character, wt = n, name = "character_count") %>%
  filter(character_count > 800) %>%
  select(-character_count) %>%
  pivot_wider(
    names_from = character,
    values_from = n,
    values_fill = list(n = 0)
  )

characters

creators <- office_info %>%
  distinct(episode_name, director, writer) %>%
  pivot_longer(director:writer, names_to = "role", values_to = "person") %>%
  separate_rows(person, sep = ";") %>%
  add_count(person) %>%
  filter(n > 10) %>%
  distinct(episode_name, person) %>%
  mutate(person_value = 1) %>%
  pivot_wider(
    names_from = person,
    values_from = person_value,
    values_fill = list(person_value = 0)
  )

creators

office <- office_info %>%
  distinct(season, episode, episode_name) %>%
  inner_join(characters) %>%
  inner_join(creators) %>%
  inner_join(office_ratings %>%
               select(episode_name, imdb_rating)) %>%
  janitor::clean_names()

office

office %>%
  ggplot(aes(episode, imdb_rating, fill = as.factor(episode))) +
  geom_boxplot(show.legend = FALSE)

library(tidymodels)
office_split <- initial_split(office, strata = season)
office_train <- training(office_split)
office_test <- testing(office_split)

office_rec <- recipe(imdb_rating ~ ., data = office_train) %>%
  update_role(episode_name, new_role = "ID") %>%
  step_zv(all_numeric(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

office_prep <- office_rec %>%
  prep(strings_as_factors = FALSE)

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet")

wf <- workflow() %>%
  add_recipe(office_rec)

lasso_fit <- wf %>%
  add_model(lasso_spec) %>%
  fit(data = office_train)

lasso_fit %>%
  pull_workflow_fit() %>%
  tidy()








