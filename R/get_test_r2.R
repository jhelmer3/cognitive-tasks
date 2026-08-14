
get_test_r2 <- function(item_data_testing, test) {
  item_data_testing |>
    select(subject_id, sscore, item, score) |>
    inner_join(test, by = "item") |>
    summarize(.by = c(subject_id, task),
              score = mean(score),
              sscore = first(sscore),
              task = first(task)) |>
    # create lm formula from all tasks within current iteration's candidate test
    mutate(formula = paste("sscore ~", task |> unique() |> paste(collapse = "+"))) |>
    # pivot as prep for model
    pivot_wider(names_from = task, values_from = score, id_cols = c("subject_id", "sscore", "formula")) |>
    # collect information
    summarize(test_r2 = summary(lm(as.formula(first(formula)),
                                   data = pick(everything())))$r.squared) |>
    pull(test_r2)
}


# get_test_r2(
#   tar_read(item_data_testing), tar_read(elasticnet_short_test)
# )
