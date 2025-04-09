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

# данные starwars ---------------------------------------------------------

# аннотирование в ggrepel
library(ggrepel)
set.seed(2025)
starwars |>
  select(height, mass, name, gender) |>
  na.omit() |>
  filter(mass < 1000) |>
  ggplot(aes(x = height, y = mass, color = gender,
             label = name)) + 
  geom_point() +
  geom_label_repel(alpha = 0.9,
                  max.overlaps = 15) +
  silgelib::theme_roboto(base_size = 12) +
  ggsci::scale_color_aaas()
  

# комбинирование графиков с patchwork -------------------------------------
library(patchwork)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + ggtitle("График 1")
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear)) + ggtitle("График 2")
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec)) + ggtitle("График 3")
p4 <- ggplot(mtcars) + geom_bar(aes(carb)) + ggtitle("График 4")

# комбинирование графиков
p1 + p2
p3 + p4

(p1 | p2 | p3) /
  p4

# данные mpg --------------------------------------------------------------
library(ggrepel)
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_text_repel(hjust = 1, vjust = 1) +
  geom_point(color = 'red') +
  theme_classic(base_size = 16)

# ggtext
# ggforce - увеличение части графика
# geomtextpath
# gganimate