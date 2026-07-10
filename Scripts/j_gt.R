
site_bg_color <- "#eceadf"

ggplot2::set_theme(ggplot2::theme_classic(base_size = 16, paper = site_bg_color))

## clearly AI-written theming function
j_gt <- function(data, in_tabset = FALSE, ...) {

    bg <- if (in_tabset) "#e2e0d4" else site_bg_color
  
  data |>
    gt(...) |>
    
    fmt_number(
      columns = where(is.numeric),
      decimals = 2
    ) |>
    
    tab_options(
      table.background.color            = bg,
      table.border.top.style            = "none",
      table.border.bottom.style         = "none",
      
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.width = px(1.25),
      column_labels.border.bottom.color = "#a89f8c",
      column_labels.border.top.color    = "#a89f8c", 
      column_labels.font.weight         = "bold",
      
      row.striping.include_table_body   = FALSE,
      
      data_row.padding                  = px(6),
      
      table_body.hlines.color           = "#d6d3c6",
      table_body.hlines.width           = px(1),
      table_body.border.bottom.color    = "#a89f8c",
      table_body.border.bottom.width    = px(1),
      table_body.border.top.color       = "#a89f8c",
      table_body.border.top.width       = px(1),
      
      # Vertical column dividers
      column_labels.vlines.style = "none",
      
      # Row group divider (the vertical line in image 1)
      row_group.border.top.color    = "#d6d3c6",
      row_group.border.bottom.color = "#d6d3c6",
      
      summary_row.border.color = "#d6d3c6",  # if you have summary rows
      stub.border.color        = "#d6d3c6",  # the stub divider line
      stub.border.width        = px(1)
    ) |>  # closing tab_options()
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style     = cell_borders(
        sides  = c("top", "bottom"),
        color  = "#a89f8c",
        weight = px(1)
      ),
      locations = cells_column_spanners()
    ) |>
    tab_style(
      style     = cell_borders(
        sides  = "bottom",
        color  = "#a89f8c",
        weight = px(1)
      ),
      locations = cells_title()
    )
}