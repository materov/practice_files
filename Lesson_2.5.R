# Дополнительные вопросы визуализации данных в R --------------------------
# 17 апреля 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 2.5
# цель занятия - отработать навыки работы с библиотекой {ggplot2},
# а также расширениями {ggplot2}
# https://www.youtube.com/watch?v=aMtEgUAWYjw&ab_channel=AlbertRapp

# загрузка библиотек ------------------------------------------------------
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
library(magrittr)

# темы графиков
library(hrbrthemes) # либо library(tinythemes)
# шкалирование
library(scales)
# цветовые палитры
library(ggsci)
library(viridis)


# вариации легенды в legendry ---------------------------------------------

#####################
# пузырьковая легенда
#####################

library(legendry)
# https://teunbrand.github.io/legendry/

gapminder::gapminder |>
  dplyr::filter(year == max(year)) |>
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

plain <- df_for_legend |>
  ggplot(aes(interaction(item, type), amount)) +
  geom_col() + labs(x = "", y = "количество")

plain
plain + guides(x = "axis_nested")

##########################
# вложенные подписи к осям
##########################

presidents <- key_range_map(presidential, 
                            start = start, 
                            end   = end, 
                            name  = name)

eco <- economics |>
  ggplot(aes(date, unemploy)) +
  geom_line() +
  labs(x = "", y = "Количество безработных")

eco + guides(x = guide_axis_nested(key = presidents)) +
  # поля
  theme(plot.margin = ggplot2::margin(t = 0.2,
                                      r = 0.5, 
                                      b = 0.2, 
                                      l = 0.3, "cm"))

##################
# вариации легенды
##################

base <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
  geom_point() +
  labs(
    x = "Рабочий объем двигателя",
    y = "Расход в милях по шоссе на галлон",
    col = "Расход в милях \nпо городу \nна галлон"
  ) +
  theme(axis.line = element_line())
base

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

base + guides(y = guide_axis_stack("axis", efficient_bracket))

base + 
  scale_colour_viridis_c(
    guide = compose_sandwich(
      middle = gizmo_density(), 
      text = "axis_base",
      opposite = efficient_bracket
    )
  )

# таблицы в gt ------------------------------------------------------------

# почему gt?
# лучшая библиотека для отображения статических таблиц
# идеология сходна с ggplot2



# как график превратить в интерактивный? ----------------------------------



# анимации в gganimate ----------------------------------------------------



library(gganimate)

library(ggiraph)

library(magick)

library(geomtextpath)
library(ggtext)

# ggtext
# ggforce - увеличение части графика
# geomtextpath
# gganimate