get_startup <- function(startup_costs, input_task) {
  startup_costs |>
    filter(task == input_task) |>
    pull(startup)
}