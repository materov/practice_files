# Преобразования табличных данных в tidyverse -----------------------------
# 10 апреля 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 1.4
# см. также:
# шпаргалка по dplyr
# https://rstudio.github.io/cheatsheets/html/data-transformation.html
# А. Селезнев
# https://selesnow.github.io/dplyr_1_0_0_course/index.html
# https://habr.com/ru/articles/444622/

# альтернатива для dplyr - data.table:
# https://rdatatable.gitlab.io/data.table/articles/ru/datatable-intro.html
# https://bookdown.org/statist_/DataTableManual/

# многое можно делать непосредственно и в base R,
# однако tidyverse дает больше возможностей
# Base R equivalents of tidyverse verbs
# https://mansthulin.se/posts/basetidyverseverbs/
mtcars |> 
  transform(avg = mpg / wt) |>
  subset(avg > 5,
         c(wt, mpg, avg)) |>
  head()

# библиотеки --------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(janitor)

# загрузим данные ---------------------------------------------------------

# данные могут быть самые различные, поэтому 
# мы должны вычистить названия переменных
# это можно сделать в библиотеке janitor

# всегда стараемся хранить необработанные данные отдельно
raw_data <- starwars

raw_data
View(raw_data)

# имена переменных --------------------------------------------------------

# имена в наборе данных
names(starwars)

# переименуем несколько колонок
starwars_bad_names <-
starwars |>
  rename(Hair_Color = hair_color,
         HEIGHT = height,
         `mass$$` = mass) |> head(2) %T>% print()

starwars_bad_names |>
  janitor::clean_names() |>
  names()

# дубликаты ---------------------------------------------------------------

# можно найти дублированные строки
# список повторяющихся строк
starwars |>
  janitor::get_dupes(eye_color, 
                     hair_color, 
                     skin_color)

# различные значения в строках
starwars |> distinct(eye_color, 
                     hair_color, 
                     skin_color)

# выбор переменных --------------------------------------------------------

starwars |>
  select(name, height, sex, species)

# подряд идущие колонки
starwars |> select(name:mass)

# можно как выбирать, так и удалять переменные
# не выбираем столбцы name, height, sex, species
starwars |>
  select(c(-name, -height, -sex, -species))

# можно выбрать переменные по окончанию / началу в названиях имен
starwars |>
  select(ends_with("color")) |>
  na.omit(hair_color)

# обратный выбор происходит через !
starwars |>
  select(!ends_with("_color")) |>
  head()

# выбираем все числовые столбцы
starwars |>
  select(where(is.numeric))

# столбцы, имеющие тип character
starwars |>
  select(where(is.character))

# поместим все числовые переменные в начало
starwars |>
  relocate(where(is.numeric))

# определенные переменные
vars <- c("name", "height", "mass")
starwars |> select(all_of(vars))
# обратный выбор
starwars |> select(!all_of(vars))

# набор данных для дальнейшей работы
starwars_data <- raw_data |>
  select(-films, -vehicles, -starships, -sex) |>
  na.omit(c(hair_color, birth_year))

starwars_data |> View()
View(starwars_data)

# создание новых переменных -----------------------------------------------

# с помощью команды mutate()
starwars_data |>
  mutate(id = row_number(),
         # указывем место, где будет новая переменная
         .before = name)

# последовательное id
starwars_data |>
  mutate(id = consecutive_id(skin_color),
         # указывем место, где будет новая переменная
         .before = name)

# в tidyverse можно создавать переменные на основе вычислений
starwars_data |>
  mutate(ratio = height / mass,
         ratio2 = ratio^2) |>
  select(name, height, mass, ratio, ratio2)

# создание переменной с помощью команды case_when(),
# которая создает переменную при соблюдении условий
starwars |>
  mutate(type = 
    case_when(
    # условия
    height > 200 | mass > 200 ~ "large",
    species == "Droid" ~ "robot",
    # значение по умолчанию
    .default = "other"
  )) |>
  select(name, height, mass, species, type)

# не всегда так удобно, есть и другая функция
lands <- c("USA", "Canada", "Wales", "UK", "China", NA, "Mexico", "Russia")

# проверять утомительно
case_when(
  lands %in% c("USA", "Canada", "Mexico") ~ "North America",
  lands %in% c("Wales", "UK") ~ "Europe",
  lands %in% "China" ~ "Asia"
)

# проще
case_match(
  lands,
  c("USA", "Canada", "Mexico") ~ "North America",
  c("France", "UK") ~ "Europe",
  "China" ~ "Asia"
)

# фильтрация --------------------------------------------------------------

# conflicted::conflicts_prefer(dplyr::filter)
# команда filter()
starwars_data |> 
  # или
  filter(height > 200 | mass > 100) |> head(3)

starwars_data |> 
  # и
  filter(height > 200 & mass > 100) |> head(3)

# выбор строк по номеру позиции
starwars_data |> slice(1:5)

# выбирает строки с наименьшим / наибольшим значениями.
starwars_data |> slice_min(height, prop = 0.25)

# тоже, что и head()
starwars_data |> slice_head(n = 5)

# случайным образом выбираемые строки
starwars_data |> slice_sample(n = 5)

# группировка и сводные характеристики ------------------------------------

# упорядочивание по возрастанию
starwars_data |>
  arrange(desc(height))

# упорядочивание по убыванию
starwars_data |>
  arrange(desc(height)) |> head(3) # или arrange(-height)

# подсчет числа элементов
starwars_data |>
  count(eye_color)
count(starwars_data, eye_color, sort = TRUE)

# добавление колонки с количеством повторений
starwars_data |>
  select(name, hair_color) |>
  add_count(hair_color)


# группировка -------------------------------------------------------------

# группировка
starwars_data |>
  group_by(gender) |> head(3)

# подсчет числа элементов в группах
starwars_data |>
  group_by(gender) |>
  summarise(num_by_gender = n())

# тоже, но чуть проще
starwars_data |>
  summarise(num_by_gender = n(),
            .by = gender)

starwars_data |>
  summarise(mean_height = mean(height),
            .by = c(gender, homeworld))

# группировка по двум переменным
starwars_data |>
  summarise(min_height = min(height),
            .by = c(gender, homeworld))

starwars_data %<>% na.omit(c(hair_color, birth_year))
# starwars_data <- starwars_data |>
#   na.omit(c(hair_color, birth_year))

# сводные характеристики
starwars_data |>
  group_by(gender) |>
  reframe(qs = quantile(height, c(0.25, 0.75)), 
                         prob = c(0.25, 0.75))

# функция across() --------------------------------------------------------

# манипуляции с несколькими переменными одновременно
starwars_data |>
  select(where(is.numeric)) |> 
  summarize(across(everything(), mean))

# функция rowwise() -------------------------------------------------------

# вычисления по строкам
starwars_data |>
  select(where(is.numeric)) |> 
  rowwise() |>
  mutate(x_total = sum(c_across(height:mass)))

# попробуйте https://pivotteer.netlify.app/ для следующих функций
# pivot_longer() ----------------------------------------------------------

# таблица c результатами опроса, в котором спрашивали людей об их религии и годовом доходе
relig_income |> View()

relig_income |>
  pivot_longer(cols = !religion,       # столбцы, которые остаются после преобразования
               names_to = "income",    # новая переменная для объединенных столбцов
               values_to = "count") |> # значения ячеек объединенных столбцов
  arrange(desc(count))

# рейтинг по неделям
billboard |> View()

billboard_long <- billboard |>
  pivot_longer(
    cols           = starts_with("wk"),
    names_to       = "week",
    names_prefix   = "wk",             # удаляем wk из каждого имени
    values_to      = "rank",
    values_drop_na = TRUE
  ) |>
  mutate(week = as.numeric(week)) %T>% print()

# кто дольше всех был в топе
billboard_long |> 
  # arrange(rank) |>
  summarise(weeks_top = n(),
            .by = artist) |>
  arrange(desc(weeks_top))

# общий рейтинг - кто дольше всех был в топе на более высоком месте   
billboard_long |>
  summarise(pseudo_rank = sum(n() / rank),
            .by = artist) |>
  arrange(desc(pseudo_rank))

# pivot_wider() -----------------------------------------------------------

us_rent <- 
us_rent_income |>
  janitor::clean_names() 

count(us_rent, variable)

us_rent |> 
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) |> View()

