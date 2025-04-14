# Работа с {ggplot2} и расширениями {ggplot2} -----------------------------
# 17 апреля 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 2.4
# цель занятия - отработать навыки работы с библиотекой {ggplot2}
# а также расширениями {ggplot2}

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



# legendry ----------------------------------------------------------------

library(legendry)

# bubble-легенда
gapminder::gapminder |>
  dplyr::filter(year == max(year)) |>
  ggplot(aes(gdpPercap, lifeExp, 
             size = pop, 
             fill = continent)) +
  geom_point(pch = 21, alpha = 0.8) +
  # новинка для легенды!
  scale_size_area(
    limits = c(0, NA), max_size = 20,
    breaks = c(0, 100, 500, 1000)*1e6,
    labels = c(0, "100M", "500M", "1B"),
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

# вариации легенды
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

# gganimate ---------------------------------------------------------------

library(gganimate)





# ggtext
# ggforce - увеличение части графика
# geomtextpath
# gganimate