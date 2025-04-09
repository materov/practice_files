# Начало работы с графикой в R, библиотека {ggplot2} ----------------------
# 17 апреля 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 2.2
# цель занятия - отработать базовые навыки работы с библиотекой {ggplot2}


# загрузка библиотек ------------------------------------------------------
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
library(magrittr)

# данные для исследования
library(gapminder)
data("gapminder")

# темы графиков
library(hrbrthemes) # либо library(tinythemes)
# шкалирование
library(scales)
# выделение цветом
library(gghighlight)
# цветовые палитры
library(ggsci)
library(viridis)

# данные gapminder --------------------------------------------------------

gapminder

# наш "холст"
gapminder |>
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp))

# первый график
gapminder |>
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

# сохраним основу для графика
gg <- gapminder |>
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp))

# точки
gg + geom_point() +
  labs(title    = "Данные Gapminder",
       subtitle = "экономические и социальные показатели: продолжительность жизни \nи ВВП на душу населения стран с течением времени",
       x        = "ВВП на душу населения",
       y        = "продолжительность жизни, лет",
       caption  = "данные с 1952 по 2007 год")

# улучшение графика
gapminder |>
  ggplot(mapping = aes(x = gdpPercap, 
                       y = lifeExp,
                       color = continent)) + 
  geom_point(alpha = 0.2) +
  scale_x_log10(labels = scales::label_log()) + # labels = scales::dollar
  geom_smooth(method = "gam") + # method = "loess" / "lm"
  theme(legend.position = "none") +
  facet_wrap(vars(continent)) +
  labs(title    = "Данные Gapminder",
       subtitle = "экономические и социальные показатели: продолжительность жизни \nи ВВП на душу населения стран с течением времени",
       x        = "\nВВП на душу населения (log-шкала)",
       y        = "продолжительность жизни, лет\n",
       color    = "континент:",
       caption  = "данные с 1952 по 2007 год") +
  ggsci::scale_color_d3() +
  silgelib::theme_roboto()
  #hrbrthemes::theme_ipsum()

# пример боксплота
gapminder |>
  ggplot(aes(x = factor(year), y = pop)) + geom_boxplot() +
  scale_y_log10(labels = scales::label_log()) +
  labs(x = "", y = "население (log-шкала)")
  
# линейный график
gapminder |>
  ggplot(aes(x = year, y = pop, 
             color = continent,
             group = country)) + 
  geom_line(linewidth = 0.7) +
  scale_y_log10(labels = scales::label_log()) +
  facet_wrap(vars(continent),
             scales = "free_y") +
  gghighlight::gghighlight(max(lifeExp) > 75, 
              use_direct_label = FALSE) +
  #theme_light()
  silgelib::theme_roboto()

# данные diamonds ---------------------------------------------------------

# пример facet_grid + scales = "free"
# точки
diamonds |>
  mutate(cut = fct_rev(cut)) |>
  ggplot(aes(x = carat, y = price)) + 
  geom_point(alpha = 0.1, 
             size = 0.1,
             show.legend = FALSE) +
  scale_y_continuous(transform = "sqrt",
                     labels = scales::dollar) +
  #geom_smooth(color = "red", linewidth = 0.7, method = "lm") +
  facet_grid(cols = vars(clarity),
             rows = vars(cut),
             scales = "free")

# hexplot
diamonds |>
  ggplot(aes(x = carat, y = price)) + 
  scale_y_log10(labels = scales::dollar) +
  #geom_point() +
  geom_hex(bins = 60) +
  viridis::scale_fill_viridis(option = "turbo") +
  labs(x = "вес, карат",
       y = "цена, log-формат")

# боксплоты
diamonds |>
  mutate(cut = fct_reorder(cut, carat)) |>
  ggplot(aes(x = cut, y = carat)) + 
  geom_boxplot() +
  coord_flip()

diamonds |>
  ggplot(aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.2)))

# пропорции
diamonds |>
  ggplot(aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "процент", 
                     labels = label_percent())

# немног лучше - видим динамику
diamonds |>
  ggplot(aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")


# данные mpg --------------------------------------------------------------
# зачем нужны vars в facet_grid
ggplot(mpg, aes(displ, hwy, size = cty)) +
  geom_point(alpha = 0.3, 
             fill = "grey30",
             color = "white",
             shape = 21) + 
  facet_grid(cols = vars(year, drv),
             rows = vars(cyl), 
             # все значения на оси y
             axes = "all_y")


# данные economics --------------------------------------------------------

# работа со временем
# https://bookdown.org/Maxine/ggplot2-maps/posts/2019-11-27-using-scales-package-to-modify-ggplot2-scale/
economics |>
  ggplot(aes(x = date, 
             y = unemploy, 
             color = "значение")) + 
  geom_line(key_glyph = "timeseries") +
  scale_x_date(breaks = scales::breaks_width("5 years"),
               labels = date_format("%Y")) +
  # scale_y_continuous(labels = function(x) format(x, big.mark = " ", 
  #                                                scientific = FALSE)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = " ")) +
  scale_color_manual(values = "#3B6895") +
  labs(title = "Динамика безработицы в США",
       x = "",
       y = "количество безработных\n",
       color = "",
       caption = "по данным economics") +
  silgelib::theme_roboto() +
  theme(legend.position = "right")


