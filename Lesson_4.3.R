# Моделирование с помощью библиотеки tidymodels --------------------------
# 22 мая 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 4.3
# цель занятия - отработать навыки работы с библиотекой {tidymodels}

# использование предварительной обработки 
# данных в {tidymodels} с помощью библиотеки {recipes}
# https://r-posts.com/mastering-data-preprocessing-in-r-with-the-recipes-package/

# общая идея работы с "рецептом"
# "рецептом" позволяет определять последовательность этапов 
# предварительной обработки 
# (таких как центрирование, масштабирование и кодирование) 
# простым и воспроизводимым способом

# центрирование и масштабирование обеспечивают сопоставимость всех функций, 
# повышая производительность модели и ее сходимость

preprocess_recipe <- recipe(target_variable ~ ., 
                            data = training_data) |>
  step_center(all_numeric(), -all_outcomes()) |>
  step_scale(all_numeric(), -all_outcomes()) |>
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)

# Как только "рецепт" будет определен, 
# вы сможете применить его к своим данным:

# подготовьте "рецепт" на обучающей выборке
prepared_recipe <- prep(preprocess_recipe, 
                        training = training_data, 
                        verbose = TRUE)

# примените "рецепт" к обучающей выборке
train_data_preprocessed <- juice(prepared_recipe)

# примените "рецепт" к тестовой выборке
test_data_preprocessed <- bake(prepared_recipe, 
                               new_data = testing_data)

# библиотека, которая "подсказывает" пути решения задач с ML
# это полезный способ быстрого создания фрагментов кода, 
# соответствующих моделям, с использованием {tidymodels}
library(usemodels)

library(palmerpenguins)
library(magrittr)
library(tidyverse)
library(tidymodels)

penguins_df <- penguins |>
  filter(!is.na(sex)) |>
  select(-year, -island)

use_glmnet(sex ~ ., data = penguins_df, verbose = TRUE, tune = FALSE)
use_kknn(sex ~ ., data = penguins_df, verbose = TRUE, tune = FALSE)

# -------------------------------------------------------------------------
# пример автоматизации на основе use_kknn()

kknn_recipe <- 
  recipe(formula = sex ~ ., data = penguins_df) |> 
  step_novel(all_nominal_predictors()) |> 
  ## This model requires the predictors to be numeric. The most common 
  ## method to convert qualitative predictors to numeric is to create 
  ## binary indicator variables (aka dummy variables) from these 
  ## predictors. 
  step_dummy(all_nominal_predictors()) |> 
  ## Since distance calculations are used, the predictor variables should 
  ## be on the same scale. Before centering and scaling the numeric 
  ## predictors, any predictors with a single unique value are filtered 
  ## out. 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) 

kknn_spec <- 
  nearest_neighbor() |> 
  set_mode("classification") |> 
  set_engine("kknn") 

kknn_workflow <- 
  workflow() |> 
  add_recipe(kknn_recipe) |> 
  add_model(kknn_spec) 

# -------------------------------------------------------------------------

