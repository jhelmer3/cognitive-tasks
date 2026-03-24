get_startup <- function(input_task) {
  startup_costs |>
    filter(task == input_task) |>
    pull(startup)
}