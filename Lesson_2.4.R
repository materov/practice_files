

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
  labs(y = "максимум численности населения, млн",
       x = "",
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
  gghighlight() +
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


  
