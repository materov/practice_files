# Моделирование с помощью библиотеки tidymodels --------------------------
# 22 мая 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 4.3
# цель занятия - отработать навыки работы с библиотекой {tidymodels}

# два руководства:
# https://juliasilge.github.io/tidymodels-tutorial/
# https://workshops.tidymodels.org/

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

# пример - классификация спама --------------------------------------------
# источник:
# https://juliasilge.com/blog/spam-email/


# загрузка данных ---------------------------------------------------------
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2023/2023-08-15/readme.md

library(tidyverse)
spam <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv')
glimpse(spam)

# crl.tot -  Общая длина непрерывных последовательностей заглавных букв
# dollar  -  Occurrences of the dollar sign, as percent of total number of characters
# bang  -  Occurrences of ‘!’, as percent of total number of characters
# money -  Occurrences of ‘money’, as percent of total number of characters
# n000  -  Occurrences of the string ‘000’, as percent of total number of words
# make  -  Occurrences of ‘make’, as a percent of total number of words
# yesno - Outcome variable, a factor with levels 'n' not spam, 'y' spam

# плотность в отношении crl.tot
spam |> 
  ggplot(aes(crl.tot, fill = yesno, 
             color = yesno)) +
  geom_density(linewidth = 1.2, 
               alpha = 0.2) +
  scale_x_log10() +
  labs(fill = "Spam?", color = "Spam?") +
  hrbrthemes::theme_ipsum()

# ML в {tidymodels} -------------------------------------------------------

library(tidymodels)

# разбиение на выборки
set.seed(123)
spam_split <-
  spam |> 
  mutate(yesno = as.factor(yesno)) |> 
  initial_split(strata = yesno)

spam_train <- training(spam_split)
spam_test <- testing(spam_split)
set.seed(234)
spam_folds <- vfold_cv(spam_train, strata = yesno)
spam_folds

# мы не будем использовать конструирование признаков!

# наши модели -------------------------------------------------------------
# у каждой из моделей есть гиперпараметры, 
# поэтому включим как спецификацию модели tune()

library(discrim)

# "наивный" байесовский классификатор
nb_spec <- naive_Bayes()
nb_spec_tune <- naive_Bayes(smoothness = tune())

# Hands-On Machine Learning with R
# https://bradleyboehmke.github.io/HOML/
# https://bradleyboehmke.github.io/HOML/mars.html
mars_spec <- mars() |> 
  set_mode("classification")
mars_spec_tune <- mars(num_terms = tune()) |> 
  set_mode("classification")

# mtry - количество предикторов, выбранных случайным образом 
# при каждом разбиении [1, ncol(x)]
# trees - количество (обычно тысяч) деревьев
# min_n - количество выборок, необходимое для дальнейшего разделения
rf_spec <- rand_forest(trees = 1e3) |> 
  set_mode("classification")
rf_spec_tune <- rand_forest(trees = 1e3, 
                            mtry = tune(), 
                            min_n = tune()) |> 
  set_mode("classification")

# объединяем все это в набор рабочих процессов ----------------------------

spam_models <-
  workflow_set(
    preproc = list(formula = yesno ~ .),
    models = list(
      nb = nb_spec, 
      mars = mars_spec, 
      rf = rf_spec,
      nb_tune = nb_spec_tune, 
      mars_tune = mars_spec_tune, 
      rf_tune = rf_spec_tune
    )
  )

spam_models

# обучение моделей --------------------------------------------------------

set.seed(123)
library(future)
plan(sequential)

spam_res <-
  spam_models |> 
  workflow_map(
    "tune_grid",
    resamples = spam_folds,
    # добавляем конкретные метрики
    metrics = metric_set(accuracy, sensitivity, specificity)
  )

# метрики
# # https://workshops.tidymodels.org/slides/intro-04-evaluating-models.html#/confusion-matrix

# ранжирование моделей ----------------------------------------------------

autoplot(spam_res) +
  silgelib::theme_roboto()

rank_results(spam_res, rank_metric = "accuracy")

# наивный байесовский классификатор справляется хуже
# случайный лес выглядит довольно хорошо

# обучение и оценка окончательной модели ----------------------------------
# значение метрик могут быть хуже, чем для обучающей выборки!

# обновим спецификацию модели, чтобы она вычисляла 
# важность признаков во время обучения

spam_wf <- workflow(
  yesno ~ ., 
  rf_spec |> set_engine("ranger", 
                        # параметр для важности признаков
                        importance = "impurity")
)
spam_fit <- last_fit(spam_wf, spam_split)
spam_fit

# confusion matrix

collect_predictions(spam_fit) |> 
  conf_mat(yesno, .pred_class)

# легче определить, что электронное письмо не является спамом, чем спам

# ROC кривая
collect_predictions(spam_fit) |> 
  roc_curve(yesno, .pred_n) |> 
  autoplot()

# вычисление важности переменных
library(vip)

extract_workflow(spam_fit) |>
  extract_fit_parsnip() |>
  vip()
