library(gt)

pizza_gtable <- 
  pizzaplace |>
  dplyr::filter(type %in% c("chicken", "supreme")) |>
  dplyr::group_by(type, size) |>
  dplyr::summarize(
    sold = dplyr::n(),
    income = sum(price),
    .groups = "drop"
  ) |>
  gt(
    rowname_col = "size",
    groupname_col = "type",
    row_group_as_column = TRUE
  ) |>
  tab_header(title = "Продажи пиццы в 2015 году") |>
  fmt_integer(columns = sold) |>
  fmt_currency(columns = income) |>
  summary_rows(
    fns = list(label = "Все размеры", fn = "sum"),
    side = c("top"),
    fmt = list(
      ~ fmt_integer(., columns = sold),
      ~ fmt_currency(., columns = income)
    )
  ) |>
  grand_summary_rows(
    columns = c("sold", "income"),
    fns = Sum ~ sum(.),
    fmt = list(
      ~ fmt_integer(., columns = sold),
      ~ fmt_currency(., columns = income)
    )
  ) |>
  tab_options(summary_row.background.color = "gray98") |>
  tab_stub_indent(
    rows = everything(),
    indent = 2
  ) |>
  as_gtable()

pizza_plot <-
  pizzaplace |>
  dplyr::mutate(date = as.Date(date)) |>
  dplyr::filter(type %in% c("chicken", "supreme")) |>
  dplyr::group_by(date, type) |>
  dplyr::summarize(
    sold = dplyr::n(),
    .groups = "drop"
  ) |>
  ggplot() +
  geom_line(aes(x = date, y = sold, color = type, group = type)) +
  facet_wrap(~type, nrow = 2) +
  scale_x_date(date_labels = "%b", breaks = "1 month") +
  theme_minimal() +
  silgelib::theme_roboto() +
  theme(legend.position = "none") +
  labs(x = "", y = "Количество продаж пиццы",
       color = "вид пиццы")

library(patchwork)
pizza_plot + pizza_gtable
