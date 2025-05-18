# Моделирование с помощью библиотеки tidymodels --------------------------
# 15 мая 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 4.2
# цель занятия - отработать навыки работы с библиотекой {tidymodels}

# некоторые нетривиальные применения {tidymodels}
# в работе с временными рядами:
# https://naukaidannye.netlify.app/blog/posts/2021-02-19-modeltime/
# в работе с географическими данными:
# https://geocompx.org/post/2025/sml-bp1/
# анализ текстов:
# Supervised Machine Learning for Text Analysis in R
# https://smltar.com/

# мы рассмотрим достаточно классические задачи машинного обучения:
# 1. задачи классификации
# 2. задачи регерессии
# ? задачи кластеризации

# упростим пример из:
# https://www.youtube.com/watch?v=J32pRt1nuoY&t=1262s&ab_channel=JamesWade
# https://jameshwade.quarto.pub/mlops-in-r-the-whole-game/
# https://jameshwade.com/posts/2022-12-27_mlops-the-whole-game.html

library(tidyverse)
library(tidymodels)
tidymodels_prefer()
library(magrittr)

library(palmerpenguins)
library(conflicted)
conflict_prefer("penguins", "palmerpenguins")

# 1. Подготовка данных ----------------------------------------------------

penguins_df <- penguins |>
  filter(!is.na(sex)) |>
  select(-year, -island)

penguins_df |>
  ggplot(aes(x = flipper_length_mm, 
             y = body_mass_g, 
             color = sex, size = bill_length_mm)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~species) +
  theme_bw()

# 2. Разбиение на выборки -------------------------------------------------

# задает начальное число для генератора случайных чисел, 
# что помогает улучшить воспроизводимость кода
set.seed(2025)

# разбиение
penguin_split <- initial_split(penguins_df, 
                               strata = sex,
                               prop = 0.8)

# обучающая выборка
penguin_train <- training(penguin_split) %T>% print()

# тестовая выборка
penguin_test <- testing(penguin_split) %T>% print()

# <Обучающая/Тестовая/Вся>
penguin_split

# кросс-валидация
penguin_folds <- vfold_cv(penguin_train) %T>% print()

# 3. Формула --------------------------------------------------------------

# пропускаем, см. далее

# 4. Конструирование признаков --------------------------------------------

penguin_rec <-
  recipe(sex ~ ., data = penguin_train) |>
  step_YeoJohnson(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(species)

penguin_rec

summary(penguin_rec)

# 5. Указание модели ------------------------------------------------------

glm_spec <-
  logistic_reg() |>
  set_engine("glm")

library(ranger)

tree_spec <-
  rand_forest(min_n = tune()) |>
  set_engine("ranger") |>
  set_mode("classification")

# 5. Рабочий процесс (workflow) -------------------------------------------

# future! 🚀 - параллельные вычисления
library(future)
plan(sequential)

workflow_set <-
  workflow_set(
    preproc = list(penguin_rec),
    models = list(
      glm = glm_spec,
      tree = tree_spec)
  ) |>
  workflow_map(resamples = penguin_folds)

# 6. Эффективность моделей ------------------------------------------------

rank_results(workflow_set,
             select_best = TRUE)

workflow_set |> autoplot() +
  theme_bw()

workflow_set |> autoplot(select_best = TRUE) +
  theme_bw()

# выбор наилучшей модели
best_model_id <- "recipe_tree" # из workflow_set

best_fit <-
  workflow_set |>
  extract_workflow_set_result(best_model_id) |>
  select_best(metric = "accuracy")

# рабочий процесс наилучшей модели
final_workflow <-
  workflow_set |>
  extract_workflow(best_model_id) |>
  finalize_workflow(best_fit)

# 7. Дообучение модели ----------------------------------------------------

# дообучение на всех данных
final_fit <-
  final_workflow |>
  last_fit(penguin_split)

# финальные метрики
collect_metrics(final_fit)

# Прогнозирование
# распространим модель на исходное множество
# модель для исходных данных
final_model <- fit(final_workflow, penguins_df)

# искусственно созданные данные
new_penguin <- tribble(~species, 
                       ~bill_length_mm, 
                       ~bill_depth_mm, 
                       ~flipper_length_mm, 
                       ~body_mass_g,
                       "Adelie", 38.5, 19.4, 185, 3700)

# прогноз на искусственно созданных данных
predict(final_model, new_data = new_penguin)

# 8. Диагностика модели ---------------------------------------------------

final_fit |>
  collect_predictions() |>
  roc_curve(truth = sex, 
            .pred_female) |>
  autoplot()

# матрица ошибок

conf_matrix <- 
  collect_predictions(final_fit) |>
  conf_mat(sex, .pred_class)

# тепловая карта
model_heatmap <- 
  conf_matrix |>
  autoplot(type = "heatmap")

# мозаичный график
model_mosaic <-
  conf_matrix |>
  autoplot(type = "mosaic")

library(patchwork)

model_heatmap + model_mosaic

###############################
# рассмотрим числовой предиктор
# задача регрессии
###############################

# библиотеки
library(tidymodels)
library(tidyverse)
tidymodels_prefer()

# исходные данные ---------------------------------------------------------

our_data <- diamonds |>
  na.omit() |>
  select(price, carat, depth, table, x, y, z, color, cut)

our_data |>
  ggplot(aes(x = carat, y = price)) + geom_point(alpha = 0.2)

# разбиение на выборки ----------------------------------------------------

set.seed(2025)

splits <- initial_split(our_data, prop = 0.8)

diamonds_train <- training(splits)
diamonds_test <- testing(splits)

diamonds_folds <- vfold_cv(diamonds_train)

# модель ------------------------------------------------------------------

# 1-я модель: random forest
rf_reg_spec <- 
  rand_forest(trees = 200, 
              min_n = 5) |>
  set_mode("regression") |>
  set_engine("ranger", 
             # для того, чтобы потом можно было выявить важность признаков
             importance = "impurity")
rf_reg_spec

# 2-я модель: линейная регрессия

lin_reg_spec <- linear_reg()

# предскажем цены на алмазы -----------------------------------------------

rf_reg_fit <- rf_reg_spec |> 
  fit(price ~ ., data = diamonds_train)
rf_reg_fit

lr_reg_fit <- lin_reg_spec |> 
  fit(price ~ ., data = diamonds_train)
lr_reg_fit |> tidy()

# проверим качество прогнозирования

predict(rf_reg_fit, new_data = diamonds_test)

augment(rf_reg_fit, new_data = diamonds_test) |>
  select(price, .pred) |>
  ggplot(aes(x = price, y = .pred)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE, method = "lm")

augment(lr_reg_fit, new_data = diamonds_test) |>
  select(price, .pred) |>
  ggplot(aes(x = price, y = .pred)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE, method = "lm")

# рассмотрим подход {tidymodels} ------------------------------------------

# формула + "рецепт"
our_recipe <- recipe(price ~ .,
                     data = our_data) |>
  # потенциально удалит переменные, которые являются 
  # крайне разреженными и несбалансированными
  step_nzv(all_predictors()) |> 
  # удалит переменные, имеющие большую абсолютную корреляцию с другими переменными
  step_corr(all_numeric_predictors()) |>
  # нормализует числовые данные так, чтобы стандартное отклонение 
  # было равно единице, а среднее значение — нулю
  step_normalize(all_numeric_predictors()) 

workflow_set <-
  workflow_set(
    preproc = list(our_recipe),
    models = list(
      rand_for = rf_reg_spec,
      lin_reg = lin_reg_spec)
  ) |>
  workflow_map(resamples = diamonds_folds,
               metrics = metric_set(rmse, mae, rsq))

# выбираем модель

rank_results(workflow_set,
             select_best = TRUE)

best_model_id <- "recipe_rand_for"

workflow_set |> autoplot() +
  theme_bw()

# дообучаем модель

best_fit <-
  workflow_set |>
  extract_workflow_set_result(best_model_id) |>
  select_best(metric = "rmse")

final_workflow <-
  workflow_set |>
  extract_workflow(best_model_id) |>
  finalize_workflow(best_fit)

final_fit <-
  final_workflow |>
  last_fit(splits)

collect_metrics(final_fit)

final_model <- fit(final_workflow, our_data)

# summary(final_model)

# визуально сравним качество новой модели

augment(final_model, new_data = diamonds_test) |>
  select(price, .pred) |>
  ggplot(aes(x = price, y = .pred)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(se = FALSE, method = "lm")

# важность предикторов в модели -------------------------------------------

library(vip)

final_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 5)
