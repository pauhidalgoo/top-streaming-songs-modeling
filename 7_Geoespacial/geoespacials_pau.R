# DESCRIPTIVA ------------------------------------------------------------------

load("./7_Geoespacial/2024_top5_countries.RData")

names(data2024)
#install.packages(c("ggplot2", "sf", "leaflet", "tidygeocoder"))
library(ggplot2)
library(sf)
library(leaflet)
library(tidygeocoder)
library(sf)
library(tmap)
library(leaflet)
library(gifski)
library(tidyverse)

# Create a leaflet map
popularity_by_country <- data2024 %>%
  group_by(country) %>%
  summarise(avg_popularity = mean(popularity, na.rm = TRUE))

# Load world spatial dataset
#world <- st_read(system.file("./7_Geoespacial/countries_map.shp", package = "maptools"))

world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")
ggplot() +
  geom_sf(data = world_cities, fill = "white", color = "black") +
  theme_minimal() +
  labs(title = "Mapa del món")

popularity_by_country

# Merge popularity data with world spatial dataset
world <- left_join(world_cities, popularity_by_country, by = c("iso_3166_1_" = "country"))
library(tmap)

# Plot choropleth map of average Spotify popularity by country
tm_shape(world) +
  tm_borders() +
  tm_fill("avg_popularity", palette = "Blues", style = "quantile") +
  tm_layout(title = "Average Spotify Popularity by Country")

library(tmap)

# For interactive map using leaflet, you can do something like this:
# Assuming your data2024 has latitude and longitude columns named "lat" and "lon"
data2024$snapshot_date <- as.Date(data2024$snapshot_date)

# Create tmap object

noglobal <- na.omit(data2024)

data2024_sf <- st_as_sf(noglobal, coords = c("lon", "lat"), crs = 4326)

# Create frames for animation
frames <- tm_shape(data2024_sf) +
  tm_dots(col = "popularity", size = 0.5) +
  tm_facets(along = "snapshot_date")

# Create animation
tmap_animation(frames, filename = "./7_Geoespacial/paugifpopularity.gif", delay = 2)

# Create leaflet map

top_songs <- data2024 %>%
  filter(snapshot_date == as.Date("2024-05-22")) %>%
  group_by(country) %>%
  filter(daily_rank == 1) %>%
  ungroup()

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = top_songs, ~lon, ~lat, popup = ~name)


top_songs <- na.omit(top_songs)

# Plotting with ggplot2
ggplot(data = top_songs) +
  geom_sf(aes(geometry = geometry, color = name), size = 3) +
  labs(title = "Top Songs for Each Country on 2024-05-22", color = "Song Name") +
  theme_minimal()


world <- st_as_sf(rworldmap::getMap(resolution = "low"))

# Join data with world map
popularity_data <- data2024 %>%
  group_by(country) %>%
  summarize(avg_popularity = mean(popularity))

world <- world %>%
  left_join(popularity_data, by = c("ISO_A3" = "country"))

# Plotting
ggplot(data = world) +
  geom_sf(aes(fill = avg_popularity), color = NA) +
  scale_fill_viridis_c(na.value = "white") +
  labs(title = "Heatmap of Song Popularity by Region", fill = "Average Popularity") +
  theme_minimal()


last_day <- data2024[data2024$snapshot_date=="2024-05-22",]


leaflet(last_day) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = ~paste("Song:", name, "<br>",
                                              "Artist:", artists, "<br>",
                                              "Popularity:", popularity),
                   color = ~ifelse(is_explicit, "red", "blue"), radius = 5,
                   fillOpacity = 0.7) %>%
  addLegend("bottomright", colors = c("red", "blue"), labels = c("Explicit", "Non-Explicit"),
            title = "Song Type")

############### DATA 2024

# is_explicit:

filtered_data <- data2024 %>%
  select(country, is_explicit)

filtered_data <- filter(filtered_data, country != '')
filtered_data$is_explicit <- as.logical(filtered_data$is_explicit)

# Agrupar por país y calcular el porcentaje de canciones explícitas
explicit_percentage_by_country <- filtered_data %>%
  group_by(country) %>%
  summarise(percent_explicit = mean(is_explicit, na.rm = TRUE) * 100)  # Multiplica por 100 para obtener el porcentaje

world <- left_join(world_cities, explicit_percentage_by_country, by = c("iso_3166_1_" = "country"))

tm_map <- tm_shape(world) +
  tm_borders() +
  tm_fill(percent_explicit, palette = "Blues", style = "quantile") +
  tm_layout(title = paste("Average Spotify", "names[i]", "by Country"))
# Asegúrate que 'world_cities' y 'explicit_percentage_by_country' estén correctamente definidos y unidos
world <- left_join(world_cities, explicit_percentage_by_country, by = c("iso_3166_1_" = "country"))

# Crear el mapa usando tmap
tm_map <- tm_shape(world) +
  tm_borders() +
  tm_fill("percent_explicit", palette = "Blues", style = "quantile") +  # Corregido: nombre de columna entre comillas
  tm_layout(title = "Average Spotify Explicit Content by Country")  # Corregido: título simplificado sin 'names[i]'

# Imprimir el mapa
print(tm_map)


# Preparar los datos
filtered_data <- data2024 %>%
  select(country, is_explicit) %>%
  filter(country != '') %>%
  mutate(is_explicit = as.logical(is_explicit))

# Agrupar por país y determinar si la mayoría son explícitas
majority_explicit_by_country <- filtered_data %>%
  group_by(country) %>%
  summarise(majority_explicit = mean(is_explicit, na.rm = TRUE) > 0.5)  # TRUE si más del 50% son explícitas

# Unir con datos espaciales
world <- left_join(world_cities, majority_explicit_by_country, by = c("iso_3166_1_" = "country"))

# AQUEST ÉS EL BO DE EXPLICIT
# Crear el mapa con tmap
tm_map <- tm_shape(world) +
  tm_borders() +
  tm_fill("majority_explicit", palette = c("red", "green"), style = "cat", 
          labels = c("No", "Yes"), title = "Explicit Content") +
  tm_layout(title = "Countries with Majority Explicit Songs")

print(tm_map)
# altres numèriques
library(dplyr)
library(sf)
library(tmap)

# popularity, duration_ms

# Leer los datos espaciales
world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")

# Calcular la media de las variables de interés por país
averages_by_country <- data2024 %>%
  group_by(country) %>%
  summarise(
    avg_energy = mean(energy, na.rm = TRUE),
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_speechiness = mean(speechiness, na.rm = TRUE),
    avg_loudness = mean(loudness, na.rm = TRUE)
  )

# Unir los datos de promedios con los datos espaciales
world <- left_join(world_cities, averages_by_country, by = c("iso_3166_1_" = "country"))

# Lista de las variables para iterar
variables <- c("avg_energy", "avg_danceability", "avg_speechiness", "avg_loudness", "avg_duration", "avg_popularity")
names <- c("energy", "danceability", "speechiness", "loudness", "duration", "popularity")  # Nombres para los títulos

# Ciclo para crear un mapa para cada variable
for (i in seq_along(variables)) {
  tm_map <- tm_shape(world) +
    tm_borders() +
    tm_fill(variables[i], palette = "Blues", style = "quantile") +
    tm_layout(title = paste("Average Spotify", names[i], "by Country"))
  
  # Imprimir el mapa
  print(tm_map)
  
  # Opcional: Guardar el mapa como PNG
  #tmap_save(tm_map, filename = paste("map_", names[i], ".png"))
}

############### MEAN NEW DATA

# Load world spatial dataset
#world <- st_read(system.file("./7_Geoespacial/countries_map.shp", package = "maptools"))


### amb is_explicit:

# Seleccionar solo las columnas 'country' e 'is_explicit'
filtered_data <- mean_new_data %>%
  select(country, is_explicit)
world <- left_join(world_cities, filtered_data, by = c("iso_3166_1_" = "country"))

# Plot choropleth map of average Spotify popularity by country
tm_shape(world) +
  tm_borders() +
  tm_fill("is_explicit", palette = c("red", "green"),
          labels = c("Explicit", "Not Explicit"), style = "cat") +
  tm_layout(title = "Countries with Majority Explicit Songs")

library(dplyr)
library(sf)
library(tmap)

# Leer los datos espaciales
world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")

# Calcular la media de las variables de interés por país
averages_by_country <- mean_new_data %>%
  group_by(country) %>%
  summarise(
    avg_energy = mean(energy, na.rm = TRUE),
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_speechiness = mean(speechiness, na.rm = TRUE),
    avg_loudness = mean(loudness, na.rm = TRUE)
  )

# Unir los datos de promedios con los datos espaciales
world <- left_join(world_cities, averages_by_country, by = c("iso_3166_1_" = "country"))

# Lista de las variables para iterar
variables <- c("avg_energy", "avg_danceability", "avg_speechiness", "avg_loudness", "avg_duration", "avg_popularity")
names <- c("energy", "danceability", "speechiness", "loudness", "duration", "popularity")  # Nombres para los títulos

# Ciclo para crear un mapa para cada variable
for (i in seq_along(variables)) {
  tm_map <- tm_shape(world) +
    tm_borders() +
    tm_fill(variables[i], palette = "Blues", style = "quantile") +
    tm_layout(title = paste("Average Spotify", names[i], "by Country"))
  
  # Imprimir el mapa
  print(tm_map)
  
  # Opcional: Guardar el mapa como PNG
  #tmap_save(tm_map, filename = paste("map_", names[i], ".png"))
}


