# Работа с {ggplot2} и расширениями {ggplot2} -----------------------------
# 17 апреля 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 2.4
# цель занятия - отработать навыки работы с библиотекой {ggplot2}

# загрузка библиотек ------------------------------------------------------
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
library(magrittr)


# 1. комбинирование графиков ----------------------------------------------
# patchwork + cowplot

library(ggplot2)
library(patchwork)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + 
  ggtitle("График 1")
p2 <- ggplot(mtcars) + geom_boxplot(aes(factor(gear), disp, group = gear)) + 
  ggtitle("График 2")
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec), se = TRUE) + 
  labs(tag = "A")
p4 <- ggplot(mtcars) + geom_bar(aes(factor(carb))) + 
  labs(tag = "B")

# комбинирование графиков в patchwork
p1 + p2
p3 + p4

# комбинация графиков
(p1 | p2) /
  p3

# применение темы
(p1 | p2) /
   p3 & theme(legend.position = "none", 
              plot.background = element_rect(color = "black", 
                                             # раньше это был size!
                                             linewidth = 1))

# размеры объектов
(p1 | p2) /
  p3 + plot_layout(heights = c(2, 1), widths = c(3, 1))

library(cowplot)

# комбинирование графиков в cowplot
plot_grid(p1, p2, 
          labels = c("A", "B"), 
          label_size = 12)


# 2. длинные подписи ------------------------------------------------------

library(tidyverse)

long_boxplot <- 
starwars |>
  select(homeworld, height) |>
  na.omit() |>
  mutate(homeworld = fct_lump_n(homeworld, n = 9, 
                              other_level = "остальные"),
         homeworld = fct_reorder(homeworld, height)) |>
  ggplot(aes(x = homeworld, y = height)) +
  geom_boxplot(fill = "grey90") +
  labs(x = "планета", y = "рост") +
  bbplot::bbc_style()

long_boxplot

long_boxplot + 
  coord_flip()

long_boxplot +  
    scale_x_discrete(
    guide = guide_axis(
      n.dodge = 2
    )
  )

# 3. шкалирование радиуса -------------------------------------------------
# scale_size

library(palmerpenguins)
df_penguins <- 
  penguins |>
  select(-island, -year) |>
  na.omit() |>
  mutate(body_mass = body_mass_g / 1000)

df_penguins

# базовый график
gg_penguins <-
df_penguins |>
ggplot(aes(x = bill_length_mm, 
           y = bill_depth_mm)) +
  geom_point(aes(color = body_mass,
                  size = body_mass), 
             alpha = 0.5) 

gg_penguins

# изменение масштаба точек, сравните:
gg_penguins + 
  scale_size(range = c(1, 2))

gg_penguins + 
  scale_size(range = c(2, 7))


# 4. Markdown в подписях к графику ----------------------------------------

library(marquee)

markdown_text <- "Это {.red **текст**}, написанный в _Markdown_"

tibble::tibble(x = 1, y = 1, label = markdown_text) |>
  ggplot(aes(x, y)) +
  geom_marquee(
    aes(label = label),
    size = 9,
    family = "Times New Roman"
  ) +
  theme_void()

library(ggtext)

gg_penguins_text <-
  gg_penguins +
  labs(title = "Размеры клюва кистехвостых пингвинов 
    <i style='color:#28A87D;font-size:18pt;font-family:serif;'>Pygoscelis</i><br><span style = 'font-size:10pt; color:grey'>
    Набор данных <span style = 'color:black;'>palmerpenguins</span> для исследования 
    и визуализации данных <br>был собран <span style = 'color:black;'>Dr. Kristen Gorman</span> со станции 
    **Palmer Station, Antarctica LTER**.<br>
    Данные включают в себя три вида пингвинов: 
    <span style = 'color:#0072B2;'>Adelie</span>, 
    <span style = 'color:#D55E00;'>Chinstrap</span> и 
    <span style = 'color:#018571;'>Gentoo</span>.</span>",
       caption = "Данные: Gorman, Williams и Fraser (2014), журнал *PLoS ONE*",
       x = "**Длина клюва** (в мм)",
       y = "**Глубина клюва** (в мм)") +
  theme_minimal() +
  theme(
    plot.title = element_markdown(face = "bold"),
    plot.caption = element_markdown(margin = margin(t = 15)),
    plot.caption.position = "plot",
    axis.title.x = element_markdown(size = 13),
    axis.title.y = element_markdown(size = 13),
    panel.grid.major = element_line("grey90", 
                                    linewidth = 0.3),
    axis.line = element_line()
  )
gg_penguins_text


# 5. работа с цветом ------------------------------------------------------

# пример непрерывной палитры viridis
gg_penguins_text +
  scale_color_viridis_c()

# больше возможностей в библиотеке viridis
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
gg_penguins_text +
  viridis::scale_colour_viridis(option = "plasma", direction = 1)

# палитры:
# https://r-charts.com/color-palettes/
gg_penguins_text +
  paletteer::scale_color_paletteer_c("grDevices::Purple-Yellow", 
                                     direction = -1,
                                     guide = "colorsteps")

# выделим цветом пингвинов, масса которых > 5 кг
library(scales)
gg_color <- 
gg_penguins_text +
  paletteer::scale_color_paletteer_c("ggthemes::Classic Orange-Blue", 
                                     direction = -1,
                                     rescaler = ~ rescale_mid(.x, mid = 5),
                                     name = "масса, кг:"
                                     )
gg_color


# 6. аннотирование --------------------------------------------------------

# простой пример
ggplot(mpg, aes(displ, hwy)) + 
  # geom_point() +
  geom_text(aes(label = model), 
            check_overlap = TRUE) + 
  xlim(1, 8)

# продолжение примера
set.seed(2025)
mini_mpg <- mpg[sample(nrow(mpg), 20), ]
gg_mpg_annotate <- 
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(color = "grey70") +
  geom_point(data = mini_mpg, 
             aes(displ, hwy),
             colour = "red") 
gg_mpg_annotate

# текстовые метки
gg_mpg_annotate +  
  ggrepel::geom_text_repel(data = mini_mpg, 
                           aes(label = class),
                           max.overlaps = 15,
                           hjust = 1,
                           vjust = 1)

# метки
gg_mpg_annotate +  
  ggrepel::geom_label_repel(data = mini_mpg, 
                           aes(label = class),
                           alpha = 0.6)

# выделение области
gg_mpg_annotate +
  annotate(
    geom = "rect",
    xmin = 1.7,
    xmax = 2.1,
    ymin = 40,
    ymax = 45,
    color = "firebrick", 
    fill = NA
  ) +
  annotate(
    geom = "text",
    x = 2.7,
    y = 43,
    label = "Особый случай",
    family = "Roboto",
    size = 4,
    vjust = 1.3,
    color = "firebrick"
  )

gg_mpg_annotate +
  ggforce::geom_mark_rect(
    aes(label = "Особый случай",
        description = c("описание"),
        filter = hwy > 40)
  )

# выделение области в ggforce как эллипс
df_penguins <- df_penguins |>
  mutate(
    type = 
      case_when(
        body_mass > 5 ~ "масса > 5 кг",
        .default = "масса < 5 кг"
      ),
    type = factor(type)
  )

library(ggforce)
gg_annotated <-
gg_color + 
  geom_mark_ellipse(data = df_penguins |>
                      filter(type == "масса > 5 кг"), 
                    aes(label = type),
                    linewidth = 0.5,
                    show.legend = FALSE)
gg_annotated


# 7. работа с легендой ----------------------------------------------------

# объединим легенды и сделаем прозрачность = 1
gg_final_1 <-
gg_annotated +
  scale_size_continuous(name = "масса, кг:") +
  guides(color = guide_legend(),
         size = guide_legend(override.aes = list(alpha = 1)))
gg_final_1

# другой вариант легенды

gg_final_2 <-
gg_annotated + 
  # убираем точки
  guides(size = "none") +
  # легенда вверху слева
  theme(legend.position = "top",
        legend.justification = "left") +
  # полоска легенды
  guides(color = guide_colorbar(title.position = "top", 
                                title.hjust = 0.5,
                                barwidth = unit(15, "lines"), 
                                barheight = unit(0.5, "lines")))
gg_final_2

# -------------------------------------------------------------------------

# сторонние темы

# ggpubr
# hrbrthemes
# silgelib
# bbplot
# ggthemes
# theme_tufte
