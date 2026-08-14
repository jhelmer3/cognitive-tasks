
conduct_search_algorithm <- function(training, startup_costs) {
  v5_dat <- training
  
  test <- data.frame(rep = 0,
                     task = "INIT",
                     item = "INIT",
                     test_r2 = 0,
                     time = 0,
                     added_r2 = 0,
                     added_time = 0,
                     r2_rate = 0)
  
  # map over number of items in the data
  walk(seq(1, v5_dat |> pull(item) |> unique() |> length()), 
       \(i) {test <<- v5_dat |> 
         # within that, map over all items that are not already in the test
         filter(!(item %in% pull(test, item))) |>
         pull(item) |>
         unique() |> 
         # combine the data from the current iteration item and the existing test items
         map(~ rbind(v5_dat |>
                       filter(item == .x),
                     v5_dat |>
                       filter(item %in% pull(test, item))) |> 
               # get sum scores for each task
               summarize(.by = c(task, subject_id),
                         score = mean(score),
                         sscore = first(sscore),
                         time = sum(time),
                         task = first(task)) |>
               # calculate time for each task
               mutate(time = time + map_dbl(task, \(task) get_startup(startup_costs, task))) |>
               # aggregate time in test
               mutate(.by = subject_id,
                      time = sum(time)) |> 
               # create lm formula from all tasks within current iteration's candidate test
               mutate(formula = paste("sscore ~", task |> unique() |> paste(collapse = "+"))) |>
               # pivot as prep for model
               pivot_wider(names_from = task, values_from = score) |>
               # collect information
               summarize(rep = i,
                         task = str_split_i(.x, "_", 1),
                         item = .x,
                         test_r2 = summary(lm(as.formula(first(formula)),
                                              data = pick(everything())))$r.squared,
                         # calculate increase in r2
                         added_r2 = first(test_r2) - test |>
                           head(1) |>
                           pull(test_r2),
                         # calculate increase in testing time
                         added_time = first(time) - test |>
                           head(1) |>
                           pull(time),
                         time = first(time),
                         # calculate rate of added r2 to added testing time
                         r2_rate = first(added_r2 / added_time))) |>
         list_rbind() |>
         # select item that has the max rate of added information per 
         # added testing time
         filter(r2_rate == max(r2_rate)) |>
         # and add it to the test
         rbind(test) |>
         filter(item != "INIT")}, 
       .progress = T)
  
  test |>
    arrange(test_r2) |>
    mutate(rank = row_number())
}
