# Моделирование с помощью библиотеки tidymodels --------------------------
# 22 мая 2025 года
# Е.Н. Матеров для курса ITMO Большие данные и аналитика
# ПЗ 5.2
# цель занятия - отработать навыки работы с географическими данными

# источники:
# https://tsamsonov.github.io/r-spatstat-course/
# https://www.paulamoraga.com/book-spatial/
# https://r.geocompx.org/
# https://ecodynizw.github.io/posts/r-spatial-data/

library(tidyverse)
library(magrittr)
# установка timeout для долгой загрузки
options(timeout = max(2000, getOption("timeout")))

# основная библиотека для работы с векторными данными
library(sf)

# работа с растровыми данными
library(terra)
# высоты над уровнем моря
library(elevatr)

# пример чтения данных
pathshp <- system.file("shape/nc.shp", package = "sf")
nc <- st_read(pathshp, quiet = TRUE)
as_tibble(nc) |>
  dplyr::select(AREA, geometry)

# отображение площади регионов
nc_ggplot <-
ggplot(nc) +
  geom_sf(aes(fill = AREA),
          color = "black",
          linewidth = 0.2)
nc_ggplot

nc_ggplot +
  labs(fill = "площадь") +
  hrbrthemes::theme_ipsum() +
  viridis::scale_fill_viridis() + 
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust = 0.5,
                               barwidth = unit(20, "lines"), 
                               barheight = unit(0.7, "lines"))) +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))


# интерактивный график ----------------------------------------------------

# https://r-spatial.github.io/mapview/
library(mapview)
mapview(nc, zcol = "AREA")

# leaflet -----------------------------------------------------------------

# https://rstudio.github.io/leaflet/index.html
library(leaflet)

leaflet(data = nc) |>
  addPolygons(fill = TRUE, weight = 1) |>
  addProviderTiles("Esri.WorldTopoMap")

# NaturalEarth — это общедоступный набор картографических данных, 
# содержащий векторные и растровые данные физических и культурных объектов. 
# он доступен в масштабах 1:10 м, 1:50 м и 1:110 миллионов.
library(rnaturalearth)

world <- ne_countries(returnclass = "sf")
world |>
  #as_tibble() |>
  dplyr::select(name_long, geometry) |>
  head()

# пример карты
ggplot(world) + 
  geom_sf(aes(fill = economy)) + 
  coord_sf(crs = "+proj=eqearth")

# для корректных и официальных границ используйте {rgeoboundaries}

# NaturalEarth также предоставляет несколько наборов данных, 
# таких как аэропорты, дороги, спорные территории 
# давайте посмотрим на городские районы по всему миру

urban <- ne_download(type = "urban_areas", 
                     category = "cultural", 
                     scale = "medium", 
                     returnclass = "sf")

ggplot() + 
  geom_sf(data = world, 
          color = "grey30", 
          fill = "grey80") +
  geom_sf(data = urban, 
          color = "firebrick", 
          fill = "firebrick") + 
  coord_sf(crs = "+proj=moll") +
  hrbrthemes::theme_ipsum()

# растровые данные --------------------------------------------------------

library(rnaturalearth)

# relief <- ne_download(type = "MSR_50M", category = "raster",
#                       scale = 50, returnclass = "sf")
# terra::plot(relief)
# class(relief)

# примеры данных:
# климат, высота, землепользование, почва, урожай, распространение видов

# климатические данные
# https://www.worldclim.org/
library(geodata)
Jamaica_temp <- worldclim_country(country = "Jamaica", 
                                  var = "tmax",
                                  path = tempdir())
terra::plot(mean(Jamaica_temp), 
            plg = list(title = "Минимальная температура (C)"))

# высота над уровнем моря
library(rnaturalearth)
library(elevatr)
library(terra)
map <- ne_countries(type = "countries", 
                    country = "Switzerland",
                    scale = "medium", 
                    returnclass = "sf")
Switzerland_elev <- get_elev_raster(locations = map, 
                                    z = 9, 
                                    clip = "locations")

plot(Switzerland_elev)

library(marmap)
library(tidyterra)

ggplot() +
  geom_spatraster(data = rast(Switzerland_elev)) +
  scale_fill_whitebox_c(palette = "deep", trans = "log", 
                        direction = 1) +
  theme_void() + 
  theme(legend.position = "none")


# пример растровых данных - высоты США ------------------------------------

bbox_usa <- data.frame(x = c(-125.0011, -66.9326), 
                       y = c(24.9493, 49.5904))

sf_bbox_usa <- sf::st_as_sf(bbox_usa, coords = c("x", "y"), crs = 4326)
sf_bbox_usa

elev_usa <- get_elev_raster(locations = sf_bbox_usa, z = 5)
terra::plot(elev_usa)

names(elev_usa) <- "z"

library(tmap)
tm_shape(elev_usa) +
  tm_raster("z")

# пример растровых данных -------------------------------------------------

# Питер
# 60.00352 с.ш. 30.45646 в.д
# 59.87626 с.ш. 30.17905 в.д
coners <-
  tribble(
    ~name, ~lon, ~lat,
    "left_top",     30.45000, 60.00352,
    "right_bottom", 30.35000, 59.87626
  )

# географическая проекция
projection <- "EPSG:4326"

# получение данных по высотам
elevation_df <- get_elev_raster(coners |> 
                                  dplyr::select(lon, lat) |>
                                  rename(x = lon, y = lat) |>
                                  as.data.frame(), 
                                prj = projection, 
                                # z = разрешение
                                z = 12)

elevation_rast <- rast(elevation_df)
elevation_rast

plot(elevation_rast)

# растр в tidyterra
ggplot() +
  geom_spatraster(data = elevation_rast) +
  scale_fill_whitebox_c(palette = "deep", 
                        direction = -1) +
  theme_void()

# coners как dataframe
coners_as_df <- 
  as.data.frame(coners |>
                  dplyr::select(lon, lat) |>
                  rename(x = lon, y = lat) |>
                  na.omit())
coners_as_df %<>% as_tibble()

# пример извлечения данных из растров
# извлекаем значения высот
coners_as_df$z <- 
  terra::extract(elevation_df, coners_as_df)
coners_as_df

# присоединяем извлеченные значения
coners_true_elev <- 
  full_join(coners, 
            coners_as_df %>% 
              rename(lon = x, lat = y, height = z)) 
coners_true_elev

coners_only_elev <- coners_true_elev |>
  dplyr::select(lon, lat, height)
coners_only_elev

coners_only_elev_sf <- coners_only_elev |>
  na.omit() |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
class(elevation_df)
coners_only_elev_sf


# mapboxgl ----------------------------------------------------------------

library(mapgl)

mapboxgl(
  center = c(55.2744, 25.1972), 
  zoom = 15.3,
  bearing = -20.8,
  pitch = 82,
  config = list(
    basemap = list(
      lightPreset = "dusk"
    )
  )
) |> 
  set_snow()


# mapsf -------------------------------------------------------------------

library(mapsf)

# импорт набора данных
mtq <- mf_get_mtq()
# базовая карта
mf_map(x = mtq)
# символы
mf_map(x = mtq, var = "POP", type = "prop", leg_pos = "topright")
# слой
mf_layout(
  title = "Population in Martinique",
  credits = "T. Giraud; Sources: INSEE & IGN, 2018"
)


