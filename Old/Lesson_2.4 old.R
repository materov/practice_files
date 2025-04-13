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
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear)) + 
  ggtitle("График 2")
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec)) + 
  labs(tag = "A")
p4 <- ggplot(mtcars) + geom_bar(aes(carb)) + 
  labs(tag = "B")

# комбинирование графиков в patchwork
p1 + p2
p3 + p4

(p1 | p2) /
   p3

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
  silgelib::theme_roboto()
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
  viridis::scale_colour_viridis(option = "plasma", direction = -1)

# палитры:
# https://r-charts.com/color-palettes/
gg_penguins_text +
  paletteer::scale_color_paletteer_c("grDevices::Purple-Yellow", 
                                     direction = -1)

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

# выделение области в ggforce как эллипс
df_penguins <- df_penguins |>
  mutate(
    type = 
      case_when(
        body_mass_g > 5000 ~ "масса > 5 кг",
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


# 8. guide_legend для проработки легенды ----------------------------------




# как переписать легенду?
# размер точек - пропорции
# gt() + ggplot2

# аннотирование
library(ggrepel)



# labels - аннотирование --------------------------------------------------

# ggforce

# 1.работа с цветом -------------------------------------------------------

library(gapminder)

small_gapminder <-
gapminder |>
  summarise(S = max(pop) / 10^6,
            .by = c(country, continent)) |>
  arrange(desc(S)) |>
  head(20) |>
  mutate(country = fct_reorder(country, S))

# выделение цветом
small_gapminder |>
  ggplot(aes(x = country, y = S, fill = continent)) + 
  geom_col(show.legend = TRUE, fill = "grey80") +
  geom_col(data = small_gapminder |>
             filter(continent %in% c("Europe",
                                     "Africa"))) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format(big.mark = " "),
                     breaks = seq(from = 0, to = 2000, by = 200)) +
  labs(title = "Максимум численности населения, млн",
       x = "",
       y = "по данным Gapminder",
       
       fill = "континент") +
  # viridis::scale_fill_viridis(option = "plasma",
  #                             discrete = TRUE) +
  ggokabeito::scale_fill_okabe_ito() +
  hrbrthemes::theme_ipsum(grid = "X")

library(palmerpenguins)

# выделение цветом
penguins |> 
  na.omit() |>
  ggplot(aes(x = flipper_length_mm, 
             fill = sex)) + 
  geom_bar() +
  gghighlight::gghighlight() +
  facet_wrap(vars(sex)) +
  silgelib::theme_roboto() +
  labs(x = "размах крыла, мм")

# 2. "облегченный" график лучше воспринимается ----------------------------

gapminder |>
  summarise(min_val = min(pop) / 10^6,
            mean_val = mean(pop) / 10^6,
            max_val = max(pop) / 10^6,
            .by = country) |>
  slice_max(n = 20, order_by = mean_val) |>
  mutate(country = fct_reorder(country, mean_val)) |>
  ggplot(aes(ymin = min_val,
             ymax = max_val,
             x = country,
             y = mean_val)) +
  geom_pointrange() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format(big.mark = " "),
                     breaks = seq(from = 0, to = 2000, by = 200)) +
  labs(y = "диапазон численности населения, млн",
       x = "",
       fill = "континент") +
  hrbrthemes::theme_ipsum()


# выделение части данных --------------------------------------------------



# еще раз про highlight ---------------------------------------------------

# можно с labels


# 3. показ неопределенностей ----------------------------------------------

library(ggdist)

penguins |>
  na.omit() |>
  ggplot(aes(x = body_mass_g / 1000, y = species)) +
  ggdist::stat_halfeye(
    position = "dodgejust",
    aes(fill = species)
    ) + 
  ggdist::stat_interval(alpha = 0.6) +
  labs(x = "масса в кг",
       y = "") +
  viridis::scale_color_viridis(option = "plasma", 
                               direction = -1,
                               discrete = T,
                               name = "уровень")

# 

# множественные распределния
# ggside

# ggridges
# paletteer

# выделение части графика, аннотирование



# -------------------------------------------------------------------------
