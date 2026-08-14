
item_data_pivot <- function(item_data) {
  item_data |>
    arrange(subject_id, item) |>
    pivot_wider(names_from = "item", values_from = "score",
                id_cols = c(subject_id, sscore))
}


