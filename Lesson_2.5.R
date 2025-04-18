# Дополнительные вопросы визуализации данных в R --------------------------
# 17 апреля 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 2.5
# цель занятия - отработать навыки работы с библиотекой {ggplot2},
# а также расширениями {ggplot2}
# https://www.youtube.com/watch?v=aMtEgUAWYjw&ab_channel=AlbertRapp

# загрузка библиотек ------------------------------------------------------
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter,
                             dplyr::select)
library(magrittr)

# темы графиков
# https://github.com/hrbrmstr/hrbrthemes
# https://github.com/eddelbuettel/tinythemes
library(hrbrthemes) # либо library(tinythemes)
# шкалирование
library(scales)
# цветовые палитры
library(ggsci)
library(viridis)

# данные
library(palmerpenguins)

# вариации легенды в legendry ---------------------------------------------

#####################
# пузырьковая легенда
#####################

library(legendry)
# https://teunbrand.github.io/legendry/

gapminder::gapminder |>
  # dplyr::filter(year == max(year)) |>
  filter(year == max(year)) |>
  ggplot(aes(gdpPercap, lifeExp, 
             size = pop, 
             fill = continent)) +
  geom_point(pch = 21, alpha = 0.8) +
  # новый тип легенды!
  scale_size_area(
    limits = c(0, NA), max_size = 20,
    breaks = c(0, 100, 500, 1000)*1e6,
    labels = c(0, "100 млн", "500 млн", "1 млрд"),
    guide  = guide_circles(vjust = 1)
  ) +
  scale_fill_discrete(guide = 
                        guide_legend(override.aes = 
                                       list(size = 4, alpha = 0.8))) +
  scale_x_log10() +
  labs(
    x = "ВВП на душу населения",
    y = "Ожидаемая продолжительность жизни",
    fill = "Континент",
    size = "Население"
  )

##########################
# вложенные подписи к осям
##########################

df_for_legend <- data.frame(
  item = c("Кофе", "Чай", "Яблоко", "Груша"),
  type = c("Напиток", "Напиток", "Фрукт", "Фрукт"),
  amount = c(5, 1, 2, 3)
)
df_for_legend

gg_plain <- df_for_legend |>
  ggplot(aes(interaction(item, type), amount)) +
  geom_col() + labs(x = "", y = "количество")

gg_plain
gg_plain + guides(x = "axis_nested")

##########################
# вложенные подписи к осям
##########################

presidential
presidents <- key_range_map(presidential, 
                            start = start, 
                            end   = end, 
                            name  = name)

economics
gg_eco <- economics |>
  ggplot(aes(date, unemploy)) +
  geom_line() +
  labs(x = "", y = "Количество безработных")

gg_eco + guides(x = guide_axis_nested(key = presidents)) +
  # поля
  theme(plot.margin = ggplot2::margin(t = 0.2,
                                      r = 0.5, 
                                      b = 0.2, 
                                      l = 0.3, "cm"))

##################
# вариации легенды
##################

gg_base <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
  geom_point() +
  labs(
    x = "Рабочий объем двигателя",
    y = "Расход в милях по шоссе на галлон",
    col = "Расход по городу \nв милях на галлон"
  ) +
  theme(axis.line = element_line())
gg_base

# отображение для скобки
efficient_bracket <- primitive_bracket(
  # ключи определяют, что отображается на экране
  key = key_range_manual(start = 25, end = Inf, name = "Эффективно"),
  bracket = "square",
  # вертикальный текст
  theme = theme(
    legend.text = element_text(angle = 90, hjust = 0.5),
    axis.text.y.left = element_text(angle = 90, hjust = 0.5)
  )
)

gg_base + guides(y = guide_axis_stack("axis", efficient_bracket))

gg_base + 
  scale_colour_viridis_c(
    guide = compose_sandwich(
      middle = gizmo_density(), 
      text = "axis_base",
      opposite = efficient_bracket
    )
  )

# как график превратить в интерактивный? ----------------------------------

library(ggiraph)
# https://davidgohel.github.io/ggiraph/
# https://www.ardata.fr/ggiraph-book/ <-- книга

# данные
mtcars_db <- rownames_to_column(mtcars, var = "carname")
mtcars_db |> as_tibble()

gg_ggiraph <- 
  ggplot(data = mtcars_db,
         mapping = aes(x = disp, y = qsec, 
                       # для всплывающих подсказок
                       tooltip = carname, data_id = carname)) + 
  # вместо geom_point()
  geom_point_interactive(size = 3, hover_nearest = TRUE) +
  theme_minimal()
gg_ggiraph

# интерактивный график
girafe(ggobj = gg_ggiraph)

###############################
# пример интерактивного графика
###############################

gg_ggiraph_barplot <-
ggplot(data = diamonds, 
       mapping = aes(x = color, fill = cut, 
                     data_id = cut)) +
  geom_bar_interactive(
    aes(tooltip = sprintf("%s: %.0f", fill, after_stat(count))),
    size = 3
  )

girafe(ggobj = gg_ggiraph_barplot)

# анимации в gganimate ----------------------------------------------------

library(gganimate)

################
# базовый график
################

library(gapminder)
library(scales)

base_gg <- 
  gapminder |>
  ggplot(aes(x = gdpPercap, y = lifeExp, 
             size = pop,
             fill = continent)) +
  geom_point(shape = 21, colour = "white", stroke = 0.2, alpha = 0.8) +
  scale_x_log10() + 
  scale_size(range = c(2, 12)) +
  scale_fill_manual(values = c("#A053A1", "#DB778F", "#E69F52", "#09A39A", "#5869C7")) +
  silgelib::theme_roboto() +
  labs(x = "ВВП на душу населения", y = "Ожидаемая продолжительность жизни\n") +
  facet_wrap(~continent, scale = "free") +
  theme(legend.position = "none")

base_gg

# анимация

library(gganimate)

base_gg + 
  transition_time(year) +
  labs(title = "Год: {frame_time}")

# текстовые аннотации вдоль осей ------------------------------------------

library(geomtextpath)

gg_textpath <- 
tibble(x = 1:20, y = -2 * x^2 + 1) |>
  ggplot(aes(x, y)) +
  geom_line(linewidth = 1) 
gg_textpath

gg_textpath +
  geom_labelpath(size = 5, 
                 label = "пример текста",
                 fill = "#F6F6FF")

gg_textpath +
  geom_textline(size = 5, 
                label = "пример текста",
                size = 10,
                hjust = 0.75,
                vjust = 0)


# надграфики --------------------------------------------------------------

gg_side_plot <- 
  penguins |> 
  na.omit() |> 
  ggplot(aes(x = bill_length_mm,
             y = bill_depth_mm,
             fill = species)) + 
  geom_point(pch = 21,
             size = 4,
             color = "white", 
             alpha = 0.8) +
  theme_grey(base_size = 13) +
  scale_fill_manual(
    values = c("#0072B2", 
               "#D55E00", 
               "#018571")
  )

gg_side_plot

library(ggside)
# https://github.com/jtlandis/ggside

gg_side_plot +
  geom_xsideboxplot(aes(y = species), 
                    orientation = "y",
                    alpha = 0.8) +
  geom_ysidedensity(aes(x = after_stat(density)), 
                    position = "identity",
                    alpha = 0.8) +
  scale_ysidex_continuous(guide = guide_axis(angle = 90), 
                          minor_breaks = NULL) +
  theme(ggside.panel.scale = 0.3,
        legend.position = "none")



# увеличения части графика ------------------------------------------------

library(ggforce)

# исходный график
gg_for_scale <- penguins |> 
  na.omit() |> 
  ggplot(aes(x = bill_length_mm,
             y = bill_depth_mm,
             fill = species,
             size = body_mass_g)) + 
  geom_point(pch = 21,
             color = "white", 
             alpha = 0.7) +
  theme_bw(base_size = 13) +
  scale_fill_manual(
    values = c(Adelie = "#0072B2", 
               Chinstrap = "#D55E00", 
               Gentoo = "#018571"),
    name = NULL,
    guide = guide_legend(
      direction = "horizontal",
      override.aes = list(size = 4,
                          alpha = 1)
    ) 
  ) +
  guides(size = "none") +
  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.box.spacing = unit(0.1, "cm"),
    legend.text = element_text(size = 13)
  ) +
  labs(x = "Длина клюва",
       y = "Высота клюва") +
  scale_x_continuous(name = "Длина клюва",
                     labels = function(x) str_c(x, " мм")) +
  scale_y_continuous(name = "Высота клюва",
                     labels = function(x) str_c(x, " мм"))

gg_for_scale + facet_zoom(xlim = c(40, 45), 
                          show.area = TRUE)

gg_for_scale + facet_zoom(xy = species == "Chinstrap", 
                          split = TRUE)

# таблицы в gt ------------------------------------------------------------

# почему gt?
# лучшая библиотека для отображения статических таблиц
# идеология сходна с ggplot2

library(gt)

gtcars |>
  select(-bdy_style, -mfr, -trim, -drivetrain, -trsmn, - ctry_origin) |>
  slice(1:8) |>
  gt(rowname_col = "model") |>
  tab_spanner(
    label = "производительность", 
    columns = c(hp, hp_rpm, trq, trq_rpm, mpg_c, mpg_h)
  )

# совмещение gt-таблиц и ggplot-графиков ----------------------------------

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
