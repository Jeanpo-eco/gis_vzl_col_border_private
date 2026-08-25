#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| warning: false

library(gt)

headline_tbl <- tibble::tribble(
  ~Metric, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
  "Treatment intensity × Post", "-0.017**", "-0.010", "-0.006", "-0.006",
  "", "(0.008)", "(0.007)", "(0.010)", "(0.011)",
  "Municipality FE", "Yes", "Yes", "Yes", "Yes",
  "Quarter FE", "Yes", "No", "No", "No",
  "Country × Quarter FE", "No", "Yes", "Yes", "No",
  "Crossing corridor × Quarter FE", "No", "No", "Yes", "No",
  "State/Department × Quarter FE", "No", "No", "No", "Yes",
  "Observations", "4,032", "4,032", "4,032", "4,032"
)

headline_tbl |>
  gt(rowname_col = "Metric") |>
  cols_align(align = "left", columns = "Metric") |>
  cols_align(align = "right", columns = c(`(1)`, `(2)`, `(3)`, `(4)`)) |>
  cols_label(
    `Metric` = "",
    `(1)` = "(1)",
    `(2)` = "(2)",
    `(3)` = "(3)",
    `(4)` = "(4)"
  ) |>
  tab_stubhead(label = "") |>
  tab_style(
    style = cell_text(font = "serif"),
    locations = cells_body(columns = everything())
  ) |>
  fmt_markdown(columns = c(`(1)`, `(2)`, `(3)`, `(4)`)) |>
  tab_options(
    table.align = "left",
    table.font.size = px(18),
    data_row.padding = px(8),
    column_labels.padding = px(8),
    table.width = pct(100),
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.top.color = "#2d3e50",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#2d3e50",
    table_body.hlines.style = "solid",
    table_body.hlines.color = "#d8dee6",
    table_body.border.top.style = "solid",
    table_body.border.top.width = px(1),
    table_body.border.top.color = "#d8dee6",
    column_labels.background.color = "#f4f7fb",
    column_labels.font.weight = "bold",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "#b9c5d2",
    stub.border.style = "solid",
    stub.border.width = px(1),
    stub.border.color = "#d8dee6",
    heading.align = "left",
    source_notes.font.size = px(12)
  )
#
#
#
#
#
#
