# Разведочный анализ данных -----------------------------------------------
# 10 апреля 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 1.5
# цель занятия - отработать навыки работы с данными в tidyverse
# см. также:
# David Robinson - Ten Tremendous Tricks in the Tidyverse
# https://www.youtube.com/watch?v=NDHSBUN_rVU&ab_channel=LanderAnalytics

# общий подход:
# загружаем данные (CSV, MS Excel, Google sheets, parquet)
# проверяем заголовки (названия переменных)
# проверяем типы данных
# ищем пропущенные значения na.omit(), na.rm = TRUE
# категориальные переменные: проводим count()-анализ
# непрерывные переменные: строим гистограммы и попарные корреляции
# категорные / непрерывные: боксплоты
# непрерывные / непрерывные: диаграммы рассеяния
# производим анализ, используя dplyr и графики

# не используйте rds для сохранения данных вместе с типами
# используйте qs
# https://cran.r-project.org/web/packages/qs/vignettes/vignette.html
# library(qs)
# qsave(df, "myfile.qs")
# df <- qread("myfile.qs")

# пример 1 ----------------------------------------------------------------

data(deliveries, package = "modeldata")
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
library(magrittr)

# посмотрим имена ---------------------------------------------------------

deliveries |> names()

# рассмотрим данные -------------------------------------------------------
View(deliveries)

glimpse(deliveries)


# столбиковая диаграмма ---------------------------------------------------
deliveries |>
  count(day, sort = TRUE) |>
  ggplot(aes(x = day, y = n)) + 
  geom_col()

# в какой день недели было больше всего доставок? -------------------------
# почему мы не рассматриваем круговые диаграммы ---------------------------
deliveries |>
  count(day, sort = TRUE) |>
  mutate(day = fct_reorder(day, n)) |>
  ggplot(aes(x = day, y = n)) + 
  geom_col() +
  coord_flip()

# диапазоны переменных ----------------------------------------------------
range(deliveries$hour)             # час доставки
range(deliveries$time_to_delivery) # время доставки (в мин)

# данные в длинном формате ------------------------------------------------
deliveries_base <- 
  deliveries |> 
  pivot_longer(cols      = starts_with("item"), 
               names_to  = "item", 
               values_to = "value") |>
  mutate(
    velocity = 60*distance / time_to_delivery,
    day_of_week = ifelse(day %in% c("Sat", "Sun"), 
                         "weekend", # если условие истинно
                         "weekday") # если ложно
  )

deliveries_base

# гистограмма = расстояния ------------------------------------------------
deliveries_base |>
  ggplot(aes(x = velocity)) + 
  geom_histogram(bins = 60) +
  scale_x_log10()

# плотности = время доставки / день недели --------------------------------
deliveries_base |>
  ggplot(aes(x = time_to_delivery, fill = day)) + 
  geom_density(alpha = 0.6) +
  #viridis::scale_fill_viridis(discrete = T) +
  facet_wrap(vars(day_of_week)) +
  labs(x = "время доставки (в мин)")

# количество доставок по часам --------------------------------------------
deliveries_base |>
  mutate(delivery_hour = as_factor(floor(hour)),
         day = fct_rev(day)) |>
  summarise(total_sum = sum(value),
            .by = c(day, delivery_hour)) |>
  ggplot(aes(delivery_hour, y = total_sum,
             group = day, colour = day)) +
  geom_line() +
  labs(x = "час доставки",
       y = "количество товаров",
       color = "день недели")

# расстояние доставки по часам --------------------------------------------
deliveries_base |>
  mutate(delivery_hour = as_factor(floor(hour)),
         day = fct_rev(day)) |>
  summarise(total_sum = sum(distance),
            .by = c(day, delivery_hour)) |>
  ggplot(aes(x = delivery_hour, y = total_sum, 
             group = day, color = day)) + 
  geom_line() +
  labs(x = "час доставки",
       y = "суммарное расстояние доставки (в км)",
       color = "день недели")


# расстояние / время доставки ---------------------------------------------
deliveries_base |>
  ggplot(aes(x = distance, y = time_to_delivery)) + 
  geom_hex(bins = 60) +
  viridis::scale_fill_viridis(option = "turbo") +
  labs(x = "расстояние (в км)",
       y = "время доставки (в мин)")

# диаграма рассеяния = расстояние / скорость доставки ---------------------
deliveries_base |>
  ggplot(aes(x = distance, y = velocity)) + 
  geom_hex(bins = 90) +
  #geom_smooth(color = "red") +
  viridis::scale_fill_viridis(option = "turbo") +
  labs(x = "расстояние (в км)",
       y = "скорость доставки (в км/ч)")

# быстро доставляют или медленно / час доставки ---------------------------
deliveries_base |>
  ggplot(aes(x = as_factor(floor(hour)),
             y = velocity)) + 
  geom_violin(fill = "grey80") + 
  geom_hline(yintercept = mean(deliveries_base$velocity)) +
  #geom_jitter(alpha = 0.01) +
  labs(x = "час доставки",
       y = "скорость доставки (в км/ч)")

# наиболее доставляемые товары за один день -------------------------------
deliveries_base |>
  summarise(total_value = sum(value),
            .by = item) |>
  arrange(desc(total_value))

# график ------------------------------------------------------------------
deliveries_base |>
  summarise(total_value = sum(value),
            .by = c(item, day_of_week)) |>
  mutate(item = fct_reorder(item, total_value)) |> 
  ggplot(aes(x = item, y = total_value, fill = day_of_week)) +
  geom_col() +
  coord_flip()


  
# пример 2 ----------------------------------------------------------------
# основные библиотеки -----------------------------------------------------

library(tidyverse)
library(magrittr)

# данные ------------------------------------------------------------------

# пример данных
library(RKaggle)
# https://github.com/benyamindsmith/RKaggle
# pak::pak("benyamindsmith/RKaggle)
# https://www.kaggle.com/datasets?sort=votes&fileType=csv

data_raw <- RKaggle::get_dataset("spscientist/students-performance-in-exams")
# https://www.kaggle.com/datasets/spscientist/students-performance-in-exams

View(data_raw)

# предварительный анализ данных -------------------------------------------

# размер данных
nrow(data_raw)
dim(data_raw)

# взглянем на данные
glimpse(data_raw)

# переведем переменные в факторные
data_students <- data_raw |>
  mutate(across(where(is.character), as.factor)) |>
  select(-lunch)

# вычищаем имена переменных
data_students <- data_students |>
  janitor::clean_names()

glimpse(data_students)
skimr::skim(data_students)

# узнаем, как разделена группа студентов по полу
data_students |>
  count(gender, sort = TRUE)

# уровень образования родителей
data_students |>
  count(parental_level_of_education) |>
  mutate(percentage = 100*n / sum(n))

# подготовительные курсы
data_students |>
  count(test_preparation_course) |>
  mutate(percentage = 100*n / sum(n))

# попробуем рассмотреть результаты тестов по группам
# были / не были подготовительные курсы
data_students %>% 
  count(., test_preparation_course, math_score, 
        name = "count") |>
  arrange(desc(math_score)) 

# как высоки оценки у студентов, родители которых
# имеют степень
high_parental_level_of_education <- 
data_students |>
  filter(grepl("degree", parental_level_of_education, ignore.case = TRUE))
  # filter(str_detect(parental_level_of_education, 
  #                   stringr::fixed("degree")))

# не закончили ВУЗ
low_parental_level_of_education <- 
  data_students |>
  dplyr::filter(!str_detect(parental_level_of_education, 
                            stringr::fixed("degree")))

View(high_parental_level_of_education)
View(low_parental_level_of_education)

# как образование родителей влияет на средний балл?
high_parental_level_of_education |> 
  # mutate(total_score = math_score + reading_score + writing_score) |>
  rowwise() |> 
  mutate(total_score = mean(c_across(ends_with("_score")))) |> 
  ungroup() |>
  summarise(mean_score = mean(total_score))

# low_parental_level_of_education

# корреляции --------------------------------------------------------------

cor_matrix <-
data_students |>
  select(where(is.numeric)) |>
  cor()

library(GGally)
ggpairs(data_students |>
          select(where(is.numeric)))

# графики -----------------------------------------------------------------

# гистограмма -------------------------------------------------------------
data_students |>
  ggplot(aes(x = math_score / 100)) + geom_histogram() +
  scale_x_continuous(labels = percent)

library(scales)
# reading_score -----------------------------------------------------------
data_students |>
  ggplot(aes(x = reading_score / 100, y = gender)) + geom_boxplot() +
  scale_x_continuous(labels = percent)

# writing_score -----------------------------------------------------------
data_students |>
  ggplot(aes(x = writing_score / 100, y = gender)) + geom_boxplot() +
  scale_x_continuous(labels = percent)

# math_score --------------------------------------------------------------
data_students |>
  ggplot(aes(x = math_score / 100, y = gender)) + geom_boxplot() +
  scale_x_continuous(labels = percent)


# влияние образования родителей на оценки по математике -------------------
data_students |>
  mutate(parental_level_of_education = 
           fct_reorder(parental_level_of_education, math_score)) |>
  ggplot(aes(x = math_score / 100, y = parental_level_of_education)) + geom_boxplot() +
  scale_x_continuous(labels = percent)

# длинный формат данных ---------------------------------------------------
data_students_longer <- 
  data_students |>
  pivot_longer(cols = ends_with("_score"),
               names_to = "score")

# гистограмма с подсветкой ------------------------------------------------
data_students_longer |> 
  ggplot(aes(x = value / 100, fill = score)) + 
  geom_histogram(bins = 30, alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  facet_wrap(vars(score)) +
  gghighlight::gghighlight() +
  ggsci::scale_fill_aaas() +
  theme_minimal() +
  labs(x = "", y = "")

# нормальность распределения ----------------------------------------------
# ggpubr::ggqqplot(data_students$math_score, 
#                  xlab = "теоретические квантили", 
#                  ylab = "эмпирические квантили", 
#                  title = "Нормальный QQ график")

# функция для построения диаграммы рассеяния ------------------------------
data_education <- function(var_1, var_2) {
data_students |>
  mutate(
    `образование родителей` = case_when(
      parental_level_of_education %in% c("associate's degree", 
                                         "master's degree") ~ "закончили ВУЗ",
      .default = "не закончили ВУЗ"
    )
  ) |> 
  count(`образование родителей`, {{ var_1 }}, {{ var_2 }}) |>
    ggplot(aes(x = {{ var_1 }} / 100, y = {{ var_2 }} / 100, 
               color = `образование родителей`, size = n)) + 
    geom_point(alpha = 0.5) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    facet_wrap(vars(`образование родителей`)) +
    hrbrthemes::theme_ipsum() +
    coord_fixed()
}

data_education(reading_score, writing_score)
data_education(math_score, writing_score)  
data_education(math_score, reading_score) 
