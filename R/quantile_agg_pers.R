
quantile_agg_pers <- function(scores_quantile) {
  scores_quantile |>
    summarize(.by = c(subject_id, item),
              sscore = sum(sscore_standardized)) |>
    summarize(.by = subject_id,
              sscore = mean(sscore))
}
