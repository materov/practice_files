# Начало работы с графикой в R, библиотека {ggplot2} ----------------------
# 17 апреля 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 2.2
# цель занятия - отработать базовые навыки работы с библиотекой {ggplot2}
# шпаргалка по ggplot2:
# https://rstudio.github.io/cheatsheets/data-visualization.pdf

# загрузка библиотек ------------------------------------------------------
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
# в R > 4.4 можно сделать так:
# use("dplyr", c("filter", "select"))
library(magrittr)

# данные для исследования
library(gapminder)
data("gapminder")

# темы графиков
library(hrbrthemes) # либо library(tinythemes)
# pak::pak(juliasilge/silgelib) + устанавливаем шрифты Roboto
# шкалирование
library(scales)
# https://bookdown.org/Maxine/ggplot2-maps/posts/2019-11-27-using-scales-package-to-modify-ggplot2-scale/
# выделение цветом
library(gghighlight)
# цветовые палитры
library(ggsci)
library(viridis)

# данные gapminder --------------------------------------------------------

gapminder

# элементы графика --------------------------------------------------------

# наш "холст"
gapminder |>
  # aes() "эстетика" - что отображается
  ggplot(mapping = aes(x = gdpPercap, y = lifeExp))

# посмотрите спецификации эстетики
# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html

# первый график
gapminder |>
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() # геометрия - как отображается

# сохраним основу для графика
gg <- gapminder |>
  ggplot(aes(x = gdpPercap, y = lifeExp))
gg + geom_point()

# инспектируем график
gg$data
gg$mapping
gg$labels

# улучшение графика
# добавим цвет и прозрачность
gg <- 
  gapminder |>
  # удобство подхода с оператором |>
  # можно данные отфильтровать или сделать иные действия
  filter(year <= 1990) |>
  ggplot(aes(x = gdpPercap, 
             y = lifeExp,
             fill = continent)) + 
  geom_point(alpha = 0.7, # прозрачность
             size = 2.5,
             color = "white",
             shape = 21) 
gg

# rugged plot
gg <- gg +
  geom_rug(aes(x = gdpPercap, y = lifeExp,
               color = continent), 
           alpha = 0.1)
gg

# логарифмическое преобразование
gg <- gg + scale_x_log10(labels = scales::dollar)
gg

# панелирование
gg <- gg + facet_wrap(~continent)
gg

# можно убрать легенду, поскольку цвет соответствует панелям
gg <- gg + theme(legend.position = "none") 
gg
  
# подписи к графику
gg <- gg + 
  labs(title    = "Данные Gapminder",
       subtitle = "экономические и социальные показатели: продолжительность жизни \nи ВВП на душу населения стран с течением времени",
       x        = "\nВВП на душу населения (log-шкала)",
       y        = "продолжительность жизни, лет\n",
       fill     = "континент:",
       caption  = "данные с 1952 по 1990 год",
       tag = "Рис. 1")
gg

# цвет значений
gg <- gg + 
  # для geom_point()
  ggsci::scale_fill_d3() +
  # для geom_rug()
  ggsci::scale_color_d3() 
gg

# тема
gg <- gg + silgelib::theme_roboto() +
  theme(legend.position = "none",                      # убираем легенду
        panel.grid.minor = element_blank(),            # и часть сетки
        plot.subtitle = element_text(face = "plain"))  # шрифт подзаголовка
# gg + hrbrthemes::theme_ipsum()
# gg + theme_classic()
# gg + theme_light()
# gg + theme_minimal()
gg

# моделирование
gg + geom_smooth(method = "lm",   # method = "loess" / "gam"
                 linewidth = 0.7,
                 aes(color = continent))

# как сохранить график? ---------------------------------------------------

# ggsave(gg, filename = "my_plot.png")
# 
# ggsave("my_plot.png")
# 
# ggsave("my_plot.png", width = 8, height = 5, dpi = 600)
# 
# ggsave("my_plot.pdf", width = 20, height = 12, unit = "cm", 
#        device = cairo_pdf)
# 
# grDevices::cairo_pdf("my_plot.pdf", width = 10, height = 7)
# gg
# dev.off()

# пример боксплота и преобразования шкалы ---------------------------------

# оценим прирост населения
gg_boxplot <-
gapminder |>
  # dplyr::filter(continent == "Asia") |>
  ggplot(aes(x = factor(year), y = pop)) + 
  geom_boxplot()
# нужно преобразование шкалы
gg_boxplot
  
gg_boxplot + scale_y_log10(labels = scales::label_log()) +
  labs(x = "", y = "население (log-шкала)")


# линейный график ---------------------------------------------------------

# рост населения
gg_linear <- 
gapminder |>
  ggplot(aes(x = year, y = pop, 
             color = continent,
             group = country)) + 
  geom_line(linewidth = 0.7)
gg_linear
  
# панелирование  
gg_linear <-
gg_linear + facet_wrap(~continent,
             scales = "free_y") 
gg_linear

# новая шкала
gg_linear <-
  gg_linear + scale_y_log10(labels = scales::label_log())
gg_linear

# выделим значения:
# пусть продолжительность жизни > 75 лет
gg_linear <-
gg_linear +
  gghighlight::gghighlight(max(lifeExp) > 75, 
              use_direct_label = FALSE) # можно сделать метки
gg_linear

# тема
gg_linear <-
  gg_linear + silgelib::theme_roboto() +
  theme(legend.position = "none")
gg_linear

# подписи к графику
final_gg_linear <- gg_linear +
  labs(title    = "Данные Gapminder",
       subtitle = "рост населения стран с течением времени \nвыделены страны с продолжительностью жизни > 75 лет",
       x        = "",
       y        = "численность населения (log-шкала)\n",
       caption  = "данные с 1952 по 2007 год")
final_gg_linear


# разберемся с панелированием ---------------------------------------------

# mpg |> View()
# 
# mpg |>
#   count(year)
# 
# mpg |>
#   count(cyl)

# пример панелирования ----------------------------------------------------

gg_mpg <- 
mpg |>
  ggplot(aes(x = cty, y = hwy)) +
  geom_point() +
  labs(title = "Расход топлива",
       x = "в городе",
       y  = "на трассе")
gg_mpg

# матричное отображение графиков
gg_mpg + facet_wrap(~year)

# тоже, что и выше
gg_mpg + facet_wrap(vars(year))

# добавим еще категорию
gg_mpg + facet_wrap(vars(year, cyl))

# укажем количество строк в матричном графике
gg_mpg + facet_wrap(vars(year, cyl),
                    nrow = 2)

# заполним пустое пространство графика
gg_mpg + facet_wrap(vars(year, cyl),
                    scales = "free_y")

gg_mpg + facet_wrap(vars(year, cyl),
                    scales = "free")

# еще один тип матричного графика / панелирования
gg_mpg + facet_grid(vars(year),
                    vars(cyl))

gg_mpg + facet_grid(cols = vars(year),
                    rows = vars(cyl),
                    scales = "free")


# работа со временем ------------------------------------------------------

# исходные данные
gg_economics <-
economics |>
  ggplot(aes(x = date, 
             y = unemploy, 
             color = "значение")) 
gg_economics

# линейный график
gg_economics + geom_line()

gg_economics <-
gg_economics + geom_line(key_glyph = "timeseries")
gg_economics

# временная шкала по x
# интервалы - каждые 5 лет
gg_economics <-
gg_economics + 
  scale_x_date(breaks = scales::breaks_width("5 years"),
               labels = date_format("%Y"))

gg_economics

# разряды тысяч для оси y
gg_economics <- gg_economics +
  scale_y_continuous(labels = scales::comma_format(big.mark = " "))
gg_economics

# альтернативный вариант
# gg_economics +
#   scale_y_continuous(labels = function(x) format(x, big.mark = " ",
#                                                  scientific = FALSE))

# цвет графика
gg_economics <- gg_economics + 
  scale_color_manual(values = "#3B6895")
gg_economics

# подписи к графику
gg_economics <- gg_economics + 
  labs(title = "Динамика безработицы в США",
       x = "",
       y = "количество безработных\n",
       color = "",
       caption = "по данным economics")
gg_economics  

# тема и положение легенды
gg_economics <- gg_economics + 
silgelib::theme_roboto() +
  theme(legend.position = "right")
gg_economics


# пример преобразования в значениях ---------------------------------------

# исходный график

gg_diamonds <-
diamonds |>
  mutate(cut = fct_rev(cut)) |>
  ggplot(aes(x = carat, y = price)) + 
  geom_point(alpha = 0.1, 
             size = 0.1,
             show.legend = FALSE)
gg_diamonds

# преобразованный график
gg_diamonds + scale_y_continuous(transform = "sqrt",
                                 labels = scales::dollar)


# пример с пропорциями ----------------------------------------------------

diamonds |>
  ggplot(aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "процент", 
                     labels = label_percent())

# вариант отображения пропорций -------------------------------------------

diamonds |>
  ggplot(aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")


# задание -----------------------------------------------------------------

# попробуйте сделайть аналогичные графики для набора данных mtcars и starwars
# (предварительно преобразуйте категориальные переменные в факторные!)
# рассмотрите всевозможные типы графиков


