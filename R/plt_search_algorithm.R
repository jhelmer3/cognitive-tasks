
plt_search_algorithm <- function(test) {
  test |>
    ggplot(aes(x = time, y = test_r2)) +
    geomtextpath::geom_textsegment(data = test |> filter(time < 10.49) |> filter(time == max(time)),
                                   aes(x = -Inf, xend = time, y = test_r2, yend = test_r2, label = "10 MIN"),
                                   color = scales::col_lighter("#202020", amount = 35), linewidth = .3, linetype = "longdash") +
    geom_segment(data = test |> filter(time < 10.49) |> filter(time == max(time)),
                 aes(y = -Inf, yend = test_r2, x = time),
                 color = scales::col_lighter("#202020", amount = 35), linewidth = .3, linetype = "longdash") +
    
    geomtextpath::geom_textsegment(data = filter(test, test_r2 == max(test_r2)),
                                   aes(x = -Inf, xend = time, y = test_r2, yend = test_r2, label = "PEAK"),
                                   color = scales::col_lighter("#202020", amount = 35), linewidth = .3, linetype = "longdash") +
    geom_segment(data = filter(test, test_r2 == max(test_r2)),
                 aes(y = -Inf, yend = test_r2, x = time),
                 color = scales::col_lighter("#202020", amount = 35), linewidth = .3, linetype = "longdash") +
    geom_line() +
    geom_point(aes(color = task), size = 2) +
    
    annotate("text", label = paste0(test |> 
                                      filter(time < 10.49) |> filter(test_r2 == max(test_r2)) |> 
                                      pull(time) |> round(1),
                                    " minutes:\n• ",
                                    test |> filter(time < 10.49) |>
                                      count(task) |>
                                      pmap_chr(\(task, n) paste(n, task)) |> 
                                      paste(collapse = "\n• ")),
             x = 8.2, y = 0.2, size = 3.4, hjust = "left", color = "#202020") +
    annotate("text", label = paste0(filter(test, test_r2 == max(test_r2)) |>
                                      pull(time) |> round(1), " minutes:\n• ",
                                    test |>
                                      filter(time <= filter(test, test_r2 == max(test_r2)) |>
                                               pull(time)) |>
                                      count(task) |>
                                      pmap_chr(\(task, n) paste(n, task)) |> 
                                      paste(collapse = "\n• ")),
             x = 26.6, y = 0.25, size = 3.4, hjust = "left", color = "#202020") +
    
    coord_cartesian(xlim = c(0, 45), ylim = c(0, 0.8), expand = c(0, 1)) +
    guides(x = guide_axis(cap = "both"),
           y = guide_axis(cap = "both")) +
    labs(x = "Time (minutes)", y = "Test R^2", color = "Task") +
    theme_classic()  +
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_blank())
}

# plt_search_algorithm(test)
