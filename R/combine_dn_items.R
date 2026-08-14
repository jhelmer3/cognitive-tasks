
combine_dn_items <- function(v5_dat) {
  item_data <- v5_dat |>
    as_tibble() |>
    # condensing denominator neglect scores down to one score per item pair
    mutate(.by = c(subject_id, item),
           score = ifelse(task == "dn.c" | task == "dn.s",
                          mean(score),
                          score)) |>
    unique()
  
  has_missingness <- item_data |>
    pivot_wider(names_from = "item", values_from = "score",
                id_cols = c(subject_id, sscore)) |>
    mutate(across(!subject_id, is.na)) |>
    pivot_longer(!subject_id, names_to = "item", values_to = "na") |>
    summarize(.by = subject_id,
              na_count = sum(na)) |>
    filter(na_count > 0) |>
    pull(subject_id)
  
  item_data |>
    filter_out(subject_id %in% has_missingness)
}


