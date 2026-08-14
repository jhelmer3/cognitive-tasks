
extract_shorter_test <- function(items_with_ranking, item_timing_data, startup_costs, time_in_minutes = 15) {
  items_with_ranking |>
    select(item, rank) |>
    inner_join(item_timing_data, by = "item") |>
    mutate(task = str_split_i(item, "_", 1)) |>
    mutate(.by = task,
           time_w_startup = time + ifelse(item == first(item), get_startup(startup_costs, first(task)), 0)) |>
    mutate(cum_time = cumsum(time_w_startup)) |>
    filter(cum_time < time_in_minutes)
}

# extract_shorter_test(
#   tar_read(search_algorithm_test),
#   tar_read(item_timing_data),
#   tar_read(startup_costs)
# )
# 
# tar_read(search_algorithm_test) |>
#   select(item, rank)
#   inner_join(tar_read(item_timing_data), by = "item") |>
#   mutate(task = str_split_i(item, "_", 1)) |>
#   mutate(.by = task,
#          time_w_startup = time + ifelse(item == first(item), get_startup(startup_costs, first(task)), 0)) |>
#   mutate(cum_time = cumsum(time_w_startup)) |>
#   filter(cum_time < time_in_minutes * 60)

# startup_costs <- tar_read(startup_costs)
# 
# tar_read(search_algorithm_test) |>
#   arrange(test_r2) |>
#   mutate(rank = row_number()) |>
#   extract_shorter_test(startup_costs)
# 
# tar_read(penalized_regression_importance_elasticnet) |>
#   extract_shorter_test(tar_read(item_timing_data), startup_costs)

