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


# 2. шкалирование радиуса -------------------------------------------------
# scale_size

library(palmerpenguins)
df_penguins <- 
  penguins |>
  select(-island, -year) |>
  na.omit()

df_penguins

# базовый график
gg_penguins <-
df_penguins |>
ggplot(aes(x = bill_length_mm, 
           y = bill_depth_mm)) +
  geom_point(aes(color = body_mass_g,
                 size = body_mass_g), 
             alpha = 0.3) 

# изменение масштаба точек, сравните:
gg_penguins + 
  scale_size(range = c(1, 2))

gg_penguins + 
  scale_size(range = c(2, 7))


# 3. Markdown в подписях к графику ----------------------------------------

library(ggtext)

gg_penguins_text <-
gg_penguins +
  labs(title = "Размеры клюва кистехвостых пингвинов 
       <i style='color:#28A87D;font-size:20pt;font-family:serif;'>Pygoscelis</i><br><span style = 'font-size:10pt; color:grey'>
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
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

gg_penguins_text

# 2. guide_legend для проработки легенды ----------------------------------



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
