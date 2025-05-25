# Моделирование с помощью библиотеки tidymodels --------------------------
# 29 мая 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 5.3
# цель занятия - отработать навыки работы с географическими данными

# источники:
# https://naukaidannye.netlify.app/blog/posts/2021-11-22-reachability/

# общие библиотеки
library(tidyverse)
library(magrittr)

# работа с географическими данными
library(osmdata)
library(osrm)
library(sf)

# шкала масштаба на карте
library(ggspatial)

# здесь можно указать любой другой город
my_place <- "Saint Petersburg, Russia"


# границы -----------------------------------------------------------------

# загрузим все границы, связанные с объектом
all_boundaries <- opq(my_place, timeout = 300) |>
  add_osm_feature(key = "boundary",
                  value = "administrative") |>
  osmdata_sf() |>
  unname_osmdata_sf() %>%
  .$osm_multipolygons

all_boundaries |> names()

all_boundaries |>
  as_tibble() |>
  dplyr::select(osm_id, name, geometry)

# 337422  Санкт-Петербург

boundary_SPb <- all_boundaries |> 
  dplyr::filter(osm_id == 337422) |>
  dplyr::select(osm_id, name, geometry)

boundary_SPb

ggplot() +
  geom_sf(data = boundary_SPb, alpha = 0.001)

# получение данных по районам города из OSM
# https://wiki.openstreetmap.org/wiki/RU:Tag:boundary%3Dadministrative
districts_osm <- opq(my_place, timeout = 300) |>
  add_osm_feature(key = "admin_level", value = 8) |>
  osmdata_sf() |>
  unname_osmdata_sf()

# непустые подмножества - мультиполигоны
districts_osm

# границы районов с площадями
districts <- districts_osm %>% 
  .$osm_multipolygons |>
  dplyr::select(osm_id, name) %>% 
  mutate(area = st_area(.),
         region_area = as.numeric(area))

# центроиды районов
regions <- cbind(districts, 
                 st_coordinates(st_centroid(districts))) |>
  rename(c_lon = X, c_lat = Y)

# районы города
regions

# районы города
ggplot() +
  geom_sf(data = regions, alpha = 0.001) +
  geom_sf(data = boundary_SPb, alpha = 0.001, color = "red")

regions |> View()

our_region <- regions |> 
  dplyr::filter(osm_id == 359179) |>
  dplyr::select(osm_id, name, geometry)
  
ggplot() +
  geom_sf(data = our_region, alpha = 0.001, color = "red") +
  geom_sf(data = boundary_SPb, alpha = 0.001)


# дороги и водные объекты -------------------------------------------------

# типы загружаемых дорог и толщина линий на карте
highway_sizes <- tibble::tribble(
  ~highway, ~highway_group,
  "motorway",      "large",
  "trunk",         "large",
  "motorway_link", "large",
  "primary",       "large",
  "primary_link",  "large",
  "secondary",     "medium",
  "secondary_link","medium",
  "tertiary",      "medium",
  "tertiary_link", "medium",
  "trunk_link",    "medium",
  "residential",   "small",
  "living_street", "small",
  "unclassified",  "small",
  "service",       "small",
  "road",          "small",
)

# streets (улицы)
streets_osm <- opq(bbox = sf::st_bbox(our_region),
                   timeout = 300) |>
  add_osm_feature(key = "highway", 
                  value = highway_sizes$highway) |>
  osmdata_sf() |> 
  unname_osmdata_sf()

streets_osm

streets <- streets_osm$osm_lines %>%
  mutate(length = as.numeric(st_length(.))) |>
  left_join(highway_sizes, by = "highway")

streets |> as_tibble() |> 
  dplyr::select(name)

# railways (железные дороги)
railways_osm <- opq(bbox = sf::st_bbox(our_region), timeout = 300) |>
  add_osm_feature(key = "railway", 
                  value = "rail") |>
  osmdata_sf() |> 
  unname_osmdata_sf()

railways <- railways_osm$osm_lines |> 
  dplyr::select()

# water (водные объекты)
water_osm <- opq(bbox = sf::st_bbox(our_region), timeout = 300) |>
  add_osm_feature(key = "natural", 
                  value = "water") |>
  osmdata_sf() |> 
  unname_osmdata_sf()

river_osm <- opq(bbox = sf::st_bbox(our_region), timeout = 300) |>
  add_osm_feature(key = "waterway", 
                  value = c("river", 
                            "riverbank")) |>
  osmdata_sf() |> 
  unname_osmdata_sf()

water <- 
  c(water_osm, river_osm) %>%
  .$osm_multipolygons |>
  dplyr::select(osm_id, name)

water


# карта -------------------------------------------------------------------

base_map <- ggplot() +
  # дорожная сеть
  geom_sf(data = streets |>
            dplyr::filter(highway_group == "large"),
          linewidth = 0.5,
          color = "grey30") +
  geom_sf(data = streets |>
            dplyr::filter(highway_group == "medium"),
          linewidth = 0.3,
          color = "grey35") +
  geom_sf(data = streets |>
            dplyr::filter(highway_group == "small"),
          linewidth = 0.1,
          color = "grey40") +
  # железные дороги
  geom_sf(data = railways,
          color = "grey30",
          linewidth = 0.3,
          linetype = "dotdash",
          alpha = 0.6) +
  # водные объекты
  geom_sf(data = water,
          fill = "steelblue",
          lwd = 0,
          alpha = 0.3) +
  hrbrthemes::theme_ipsum()

base_map +
  geom_sf(data = our_region, alpha = 0.001, 
          color = "red",
          linewidth = 0.5) +
  # ограничения
  coord_sf(xlim = c(30.205, 30.460), 
           ylim = c(59.625, 59.765),
           expand = FALSE)

base_map +
  # ограничения
  coord_sf(xlim = c(30.35, 30.45), 
           ylim = c(59.70, 59.74),
           expand = FALSE)


# скорости ----------------------------------------------------------------

# максимальные разрешенные скорости движения из OpenStreetMap
streets_speed <- streets |> 
  # рассмотрим только данные без пропусков
  dplyr::filter(!is.na(maxspeed)) |> 
  # заменим коды значениями из OSM
  mutate(maxspeed_new = 
           case_when(
             maxspeed == "RU:living_street" ~ "20",
             maxspeed == "RU:rural" ~ "90",
             maxspeed == "RU:urban" ~ "60",
             .default = maxspeed
           )
  ) 

# преобразуем переменные
streets_speed$maxspeed_new <- 
  as.numeric(streets_speed$maxspeed_new)

# создадим факторную переменную для скорости
streets_speed$max_speed_factor <-
  factor(streets_speed$maxspeed_new,
         levels = c(5, 8, 10, 20, 30, 40, 49, 50, 60, 70, 90),
         labels = c("5 км/ч", "8 км/ч", 
                    "10 км/ч", "20 км/ч", 
                    "30 км/ч", "40 км/ч", 
                    "49 км/ч", "50 км/ч", 
                    "60 км/ч", "70 км/ч", 
                    "90 км/ч"))

streets_speed |> 
  drop_na(max_speed_factor) |>
  st_drop_geometry() %>% 
  count(., max_speed_factor, sort = TRUE)

# карта: максимальная разрешенная скорость движения
base_map +
  # дорожная сеть
  geom_sf(data = streets_speed |> 
            drop_na(max_speed_factor),
          linewidth = 0.5,
          aes(color = max_speed_factor)) +
  viridis::scale_color_viridis(discrete = TRUE,
                               option = "turbo") +
  # географические границы части города
  coord_sf(xlim = c(30.30, 30.460), 
           ylim = c(59.68, 59.765),
           expand = FALSE) +
  labs(color = "максимальная \nразрешенная \nскорость") +
  #theme_void() +
  theme(text = element_text(size = 14))


# здания ------------------------------------------------------------------

# buildings (здания)
buildings_osm <- opq(bbox = sf::st_bbox(our_region), timeout = 300) |>
  add_osm_feature(key = "building") |>
  osmdata_sf()

buildings_polygons <- buildings_osm %>% 
  .$osm_polygons

buildings_multipolygons <- buildings_osm %>% 
  .$osm_multipolygons

buildings <- bind_rows(buildings_polygons, 
                       buildings_multipolygons)

# названия зданий
buildings |>
  drop_na(name) |>
  as_tibble() |>
  dplyr::select(name)

# количество улиц
buildings |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  dplyr::filter(`addr:street` != "") |>
  count(`addr:street`, sort = TRUE)

base_map +
  geom_sf(data = buildings, linewidth = 0.15, 
          aes(fill = `addr:street`, 
              color = `addr:street`)) +
  # географические границы части города
  coord_sf(xlim = c(30.35, 30.45), 
           ylim = c(59.70, 59.74),
           expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

