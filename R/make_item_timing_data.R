
make_item_timing_data <- function(item_data) {
  item_data |>
    select(task, item, time) |>
    unique()
}

