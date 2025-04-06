# Описательные статистики в R ---------------------------------------------
# 3 апреля 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 1.2

# комментирование в RStudio:
# Shift + Ctrl/Cmd + R


# используйте reprex!
# https://ubogoeva.github.io/R4Analytics/posts/R_question_how_to_ask.html
library(reprex)
reprex(
  {
    head(mtcars)
  }
)

# подключение библиотек ---------------------------------------------------
library(tidyverse)
library(magrittr)

# текущая директория ------------------------------------------------------
# сначала setwd("/my/path")
library(here)

# чтение / запись файлов --------------------------------------------------
# Excel
library(readxl)

# отметим что является относительным
df <- read_excel(
  here("files/data", "example.xlsx")
)

df

# пример факторов ---------------------------------------------------------
df$gender %<>% as.factor()
df$gender <- as.factor(df$gender)

df

class(df$gender)

# чтение / запись CSV -----------------------------------------------------
# проверяем класс - фрейм данных
class(mtcars)

# таблица как tibble
mtcars |> as_tibble()

# HTML-версия
mtcars |>
  gt::gt()

write_csv(
  mtcars, here("files/data/mtcars.csv")
)

# быстрое чтение таблицы
# (часто спасает, когда файл плохо читается)
data.table::fread(here("files/data/mtcars.csv"))

# что делать, если размер файла большой? ----------------------------------
# в таком случае, волшебное слово - Parquet!

nanoparquet::write_parquet(mtcars, here("files/data/mtcars.parquet"))

big_data <- nanoparquet::read_parquet(here("files/data/mtcars.parquet"))

# чтение данных из Google Sheets ------------------------------------------

library(googlesheets4)
# doc.new
# создание: перейти по адресу - sheet.new
# не забудьте выбрать "чекбокс" при авторизации!

googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1bxURH9GQx-sH4P2LLbe5ywXvAFSthF2i-r3mTE2c7bA/edit?gid=0#gid=0")

# оператор pipe -----------------------------------------------------------
# из библиотеки magrittr

# сравните
pi |> sin() |> round()
# либо так:
pi %>% sin() %>% round()
# Shift + Ctrl/Cmd + M
round(sin(pi))

# когда это удобно?
mtcars |>
  as_tibble() |>
  filter(gear == 4) |>
  mutate(new_column = "q") |>
  select(where(is.numeric))

# сравните
df$gender %<>% as.factor()
df$gender <- df$gender |> as.factor()
# df$gender <- as.factor(df$gender)

# когда нужно сделать действие и вывести его результат
(x <- head(mtcars))
x <- head(mtcars) %T>% print()

# как работают индексы в таблице ------------------------------------------
# это имена колонок
names(mtcars)
colnames(mtcars)
rownames(mtcars)

mtcars[1, 1]

mtcars[1, ]

mtcars[, 1]

mtcars[1:3, 4:6]

# задание последовательности ----------------------------------------------
seq(from = 1, to = 100, by = 10)

# все есть ветор в R!
c(1:18) + 1

# пример данных -----------------------------------------------------------

# пример вектора
x <- c(16.86, 16.4, 15.64, 10.03, 20.95, 9.3, 2.89, 12.46,
       17.31, 16.03, 20.24, 8.07, 12.53, 6.79, 7.01, 8.12, 6.54,
       7.55, 4.4, 7.19, 14.92, 3.99, 8.3, 21.87, 15.19, 8.39, 3.96,
       9.02, 5.14, 4.56, 4.69, 6.24, 4.83, 4.8, 5.8, 6.02, 3.95,
       10.76, 4.65, 6.07, 3.55, 5.73, 7.85, 11.87, 3.76, 7.02,
       11.01, 5.88, 8.87, 4.87, 23.02, 7.68, 9.58, 6.25, 10.09,
       6.92, 5.59, 8.55, 6.99, 6.62, 6.61, 3.5, 8.25, 12.13, 6.79,
       6.4, 6.26, 7.23, 6.22, 7.27, 7.42, 4.53, 5.83, 7.28, 10.52,
       10, 7.7, 9.17, 16.52, 6.3, 10.16, 22.98, 12.91, 8.9)

# вычисляем среднее значение
mean(x)

# вычисляем медиану
median(x)

# размах
range(x)
c(min(x), max(x))

# СКВ
sd(x)

# квантили
quantile(x, 0.25)
quantile(x, 0.75)

# интерквантильный размах
IQR(x)

# асимметрия
moments::skewness(x)

# эксцесс
moments::kurtosis(x)

# описание выборки
summary(x)
fivenum(x)
psych::describe(x)
# здесь trimmed усеченное среднее, среднее по цензурированной выборке
# mad: медианное значение абсолютного отклонения от медианы

# описательные статистики по группам
psych::describeBy(diamonds$price, diamonds$cut)

# Hmisc::describe(x)
# pastecs::stat.desc(x)

# гистограмма
hist(x, freq = FALSE, 
     breaks = 15, 
     col = "grey88", 
     main = "Гистограмма и ядерная плотность")

# ядерная плотность
lines(density(x), lwd = 2)

# библиотека tinyplot для быстрого отображения
# https://grantmcdermott.com/tinyplot/
library("tinyplot")
tinyplot(
  ~Petal.Width | Species,
  type = "histogram",
  data = iris, 
  main = "Гистограмма"
)

# квантиль-квантиль график
ggpubr::ggqqplot(x, 
                 xlab = "теоретические квантили", 
                 ylab = "эмпирические квантили", 
                 title = "Нормальный QQ график")

# критерий Шапиро-Уилка
shapiro.test(x)

# доверительная полоса
sm::sm.density(x, model = "Normal",
               xlab = "Имитированная выборка",
               ylab = "Функция плотности распределения")

# просмотр данных
glimpse(mtcars)

# одна из самых полезных функций "скрининга" данных
skimr::skim(mtcars)

View(mtcars)

library(GGally)

# корреляции --------------------------------------------------------------

# библиотека ggcorrplot
library(ggcorrplot)
library(tidyverse)

num_cars <- mtcars |>
  # as_tibble() |>
  select(where(is.numeric))

# матрица корреляций
corr <- round(cor(num_cars), 1)

# диаграмма корреляций
ggcorrplot(corr)
# использование иерархической кластеризации
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")
# отображение значений корреляций
ggcorrplot(corr, type = "lower",
           lab = TRUE)
# p-значения матрицы корреляций (статистическая значимость)
ggcorrplot(corr, type = "lower", p.mat = p.mat <- cor_pmat(num_cars))

# библиотека corrplot
library(corrplot)
# эллипсы
corrplot(corr, method = "ellipse")
# значения коэффциентов корреляции
corrplot(corr, method = "number") 
# смешанный вариант
corrplot.mixed(corr, lower.col = "black", number.cex = .7)
# три кластера
corrplot(corr, order = "hclust", addrect = 3)

# библиотека GGally
library(GGally)
ggpairs(num_cars)

# пример "конвеера" в base R
num_mtcars <- mtcars |>
  as_tibble() |> 
  select(-vs, -am, -carb, -drat, -qsec) |>
  mutate(across(all_of(c("cyl","gear")), as.factor))

ggpairs(num_mtcars)


# что еще? ----------------------------------------------------------------
# попробуйте самостоятельно! ----------------------------------------------

# esquisse
# https://cran.r-project.org/web/packages/esquisse/vignettes/get-started.html
# library(esquisse)
# esquisser()

# gtsummary
# https://www.danieldsjoberg.com/gtsummary/

# easystats
# весь {easystats} ставит большое количество библиотек!
# https://easystats.github.io/easystats/

# информация о сессии и установленных библиотеках -------------------------

sessioninfo::session_info()
