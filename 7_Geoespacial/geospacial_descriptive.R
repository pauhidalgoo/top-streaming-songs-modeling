load('./7_Geoespacial/data_coordenades.RData')

install.packages("viridis")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("leaflet")

library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)

# Obrim el shapefile
world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")

# Visualitzem el mapa del món
ggplot() +
  geom_sf(data = world_cities, fill = "white", color = "black") +
  theme_minimal() +
  labs(title = "Mapa del món")

data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Visualitzem els llocs on tenim dades (ubicació de cada artista)
ggplot() +
  geom_sf(data = world_cities, fill = "white", color = "black") +  # Dibuixem el shapefile
  geom_sf(data = data_sf, color = "darkgreen", size = 0.5) +  # Afegim els punts
  theme_minimal() +
  labs(title = "Mapa amb els punts de dades")

st_crs(world_cities) <- 4326

############# ARTIST_FOLLOWERS #############

# Agrupem pel nom de l'artista, ja que les coordenades estan segons l'artista 
data_grouped <- data %>%
  group_by(artist_name) %>% 
  summarize(
    artist_followers = mean(artist_followers, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),  
    latitude = mean(latitude, na.rm = TRUE),
    .groups = "drop"
  )

data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

data_joined <- st_join(world_cities, data_sf, join = st_intersects)

data_summarized <- data_joined %>%
  group_by(name) %>%
  summarize(avg_followers = mean(artist_followers), .groups = "drop")

# Creem un mapa de calor segons els followers dels artistes
ggplot(data = data_summarized) +
  geom_sf(aes(fill = avg_followers), color = "grey") +
  scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar") +
  labs(title = "Mapa de Calor de Artist_followers per País",
       fill = "Avg Followers") +
  theme_minimal()

#########################################
# VISUALITZAR ARTIST_FOLLOWERS PER ANYS #
#########################################

data_grouped <- data %>%
  group_by(artist_name, year_week) %>% 
  summarize(
    artist_followers = mean(artist_followers, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),  
    latitude = mean(latitude, na.rm = TRUE),
    .groups = "drop"
  )

data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

data_joined <- st_join(world_cities, data_sf, join = st_intersects)

data_summarized <- data_joined %>%
  group_by(name) %>%
  summarize(songs_per_genre = n(), .groups = "drop")
data_summarized <- data_joined %>%
  group_by(name, year_week) %>%
  summarize(avg_followers = mean(artist_followers), .groups = "drop")

ggplot(data = data_summarized) +
  geom_sf(aes(fill = avg_followers)) +
  facet_wrap(~year_week) +
  theme_bw() +
  scale_fill_viridis(name = "Followers") +
  labs(title = "Mapa de Seguidores de Artistas por Año", fill = "Artist Followers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

View(data_summarized)

# Com es veu molt petit fem els plots per separat:

data_per_anys <- data %>%
  group_by(nationality, year_week, latitude, longitude) %>%
  summarize(mean_followers = mean(artist_followers, na.rm = TRUE), .groups = 'drop')

data_sf <- st_as_sf(data_per_anys, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
data_joined <- st_join(world_cities, data_sf, join = st_intersects)

data_summarized <- data_joined %>%
  group_by(name, year_week) %>%
  summarize(avg_followers = mean(mean_followers), .groups = "drop")

plot_heatmap_per_year <- function(year) {
  ggplot(data = filter(data_summarized, year == year)) +
    geom_sf(aes(fill = avg_followers), color = "grey") +
    scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar") +
    labs(title = paste("Mapa de Calor de Artist_followers por País en el Año", year),
         fill = "Avg Followers") +
    theme_minimal()
}

unique_years <- unique(data_summarized$year_week)
plots <- lapply(unique_years, plot_heatmap_per_year)

for (i in seq_along(plots)) {
  print(plots[[i]])
  
  # Para guardar cada plot como archivo de imagen
  #ggsave(filename = paste("heatmap_", unique_years[i], ".png", sep = ""),
  #plot = plots[[i]], width = 10, height = 8)
}

#########################################################
######################## GÈNERES ########################
#########################################################

#### NOMBRE DE CANÇONS QUE HI HA DE CADA GÈNERE A CADA PAÍS

# Recordar que això s'ha de filtrar per cancons, sinó les contarà repetides

generos <- c("pop", "hip_hop", "rock", "christmas", "cinema", "latino", "electro")

plot_genre_heatmap <- function(genre) {
  # Agrupem i sumem la variable per nacionalitat
  data_grouped <- data %>%
    group_by(nationality) %>%
    summarize(
      count = sum(!!sym(genre), na.rm = TRUE),
      longitude = mean(longitude, na.rm = TRUE),
      latitude = mean(latitude, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Convertim a sf object
  data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  
  # Unió geospacial
  data_joined <- st_join(world_cities, data_sf, join = st_intersects)
  
  # Per país (name)
  data_summarized <- data_joined %>%
    group_by(name) %>%
    summarize(sum_genre = sum(count), .groups = "drop")
  
  ggplot(data = data_summarized) +
    geom_sf(aes(fill = sum_genre), color = "grey") +
    scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar") +
    labs(title = paste("Mapa de Calor del número de canciones de", genre),
         fill = paste("Sum", genre)) +
    theme_minimal()
}

plots <- lapply(generos, plot_genre_heatmap)

for (i in seq_along(plots)) {
  print(plots[[i]])  # Mostrar el plot
  
  # Guardar el plot
  #ggsave(filename = paste("heatmap_", generos[i], ".png", sep = ""),
         #plot = plots[[i]], width = 10, height = 8)
}

#### VISUALITZACIÓ DE QUIN GÈNERE PREDOMINA A CADA PAÍS

genre_colors <- c("Pop" = "gold", "Rock" = "blue", "Hip Hop" = "lightgreen", "Christmas" = "red",
                  "Cinema" = "purple", "Latino" = "orange", "Electro" = "pink", "Other" = "grey")
st_crs(world_cities) <- 4326

data_grouped <- data %>%
  group_by(nationality) %>%
  summarize(
    pop = sum(pop, na.rm = TRUE),
    rock = sum(rock, na.rm = TRUE),
    hip_hop = sum(hip_hop, na.rm = TRUE),
    christmas = sum(christmas, na.rm = TRUE),
    cinema = sum(cinema, na.rm = TRUE),
    latino = sum(latino, na.rm = TRUE),
    electro = sum(electro, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),
    latitude = mean(latitude, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    max_value = pmax(pop, rock, hip_hop, christmas, cinema, latino, electro, na.rm = TRUE),
    most_popular_genre = case_when(
      pop == max_value ~ "Pop",
      rock == max_value ~ "Rock",
      hip_hop == max_value ~ "Hip Hop",
      christmas == max_value ~ "Christmas",
      cinema == max_value ~ "Cinema",
      latino == max_value ~ "Latino",
      electro == max_value ~ "Electro",
      TRUE ~ "Other"  # En caso de empate o valores inexistentes
    )
  )

# Unir los datos agrupados al shapefile de países
data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
data_joined <- st_join(world_cities, data_sf, join = st_intersects)

# Crear el mapa
ggplot(data = data_joined) +
  geom_sf(aes(fill = most_popular_genre), color = "black") +  # Pinta cada país con el color del género más popular
  scale_fill_manual(values = genre_colors, na.value = "grey") +  # Usa la paleta de colores definida
  labs(title = "Género Musical Más Popular por País", fill = "Género") +
  theme_minimal()

##############################

# 1. Porcentaje de Canciones de un Género por País

generos <- c("pop", "hip_hop", "rock", "christmas", "cinema", "latino", "electro", "collab")

for (genre in generos) {
  data_grouped <- data %>%
    group_by(nationality) %>%
    summarize(
      total_songs = n(),
      genre_count = sum(!!sym(genre), na.rm = TRUE),
      genre_percentage = 100 * genre_count / total_songs,
      longitude = mean(longitude, na.rm = TRUE),
      latitude = mean(latitude, na.rm = TRUE),
      .groups = "drop"
    )
  
  data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  data_joined <- st_join(world_cities, data_sf, join = st_intersects)
  
  p <- ggplot(data = data_joined) +
    geom_sf(aes(fill = genre_percentage), color = "black") +
    scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar", name = "Percentage of Songs") +
    labs(title = paste("Percentage of", genre, "Music by Country"), fill = "Percentage of Songs") +
    theme_minimal()
  
  print(p)
}

# 2. Porcentaje del Total Mundial de Canciones de un Género por País

for (genre in generos) {
  total_genre_songs_worldwide <- sum(data[[genre]], na.rm = TRUE)
  
  data_grouped_world <- data %>%
    group_by(nationality) %>%
    summarize(
      genre_count = sum(!!sym(genre), na.rm = TRUE),
      genre_world_percentage = 100 * genre_count / total_genre_songs_worldwide,
      longitude = mean(longitude, na.rm = TRUE),
      latitude = mean(latitude, na.rm = TRUE),
      .groups = "drop"
    )
  
  data_sf_world <- st_as_sf(data_grouped_world, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  data_joined_world <- st_join(world_cities, data_sf_world, join = st_intersects)
  
  p <- ggplot(data = data_joined_world) +
    geom_sf(aes(fill = genre_world_percentage), color = "black") +
    scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar", name = "Global Percentage of Songs") +
    labs(title = paste("Global Share of", genre, "Music by Country"), fill = "Percentage of World's Songs") +
    theme_minimal()
  
  print(p)
}

################
# Visualitzar el percentatge de cancons de cada genere que estan a cada país

# Suponiendo que 'world_cities' y 'data' ya están cargados
genres <- c("pop", "rock", "hip_hop", "christmas", "cinema", "latino", "electro")

# Calcular el total de canciones por país
data_grouped <- data %>%
  group_by(nationality) %>%
  summarise(
    total_songs = sum(pop + rock + hip_hop + christmas + cinema + latino + electro, na.rm = TRUE),
    across(all_of(genres), sum, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),
    latitude = mean(latitude, na.rm = TRUE),
    .groups = "drop"
  )

# Calcular los porcentajes de cada género en relación al total de canciones por país
data_grouped <- data_grouped %>%
  mutate(across(all_of(genres), ~ . / total_songs * 100, .names = "pct_{.col}"))

# Convertir a simple features
data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
data_joined <- st_join(world_cities, data_sf, join = st_intersects)

# Crear y mostrar los mapas de calor para cada género usando un bucle
for (genre in genres) {
  # Añadir sufijo para encontrar la columna de porcentaje correcta
  genre_pct_col <- paste("pct", genre, sep = "_")
  
  # Crear y mostrar el mapa de calor para el género actual
  ggplot(data = data_joined) +
    geom_sf(aes(fill = .data[[genre_pct_col]]), color = "black") +
    scale_fill_viridis_c(
      option = "C", 
      na.value = "grey", 
      guide = "colorbar",
      name = paste("Percentage of", genre)
    ) +
    labs(title = paste("Percentage of", genre, "Music by Country"),
         fill = paste("Percentage of", genre, "Songs")) +
    theme_minimal()
}

####################################################
## MAPA INTERACTIU QUE ET DIU LA INTENSITAT DE ARTIST_FOLLOWERS
####################################################

spotify_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")

# MAPA ESTÀTIC
ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +  # Dibuja el mapa de fondo
  geom_sf(data = spotify_sf, aes(size = artist_popularity, color = artist_popularity), alpha = 0.6) + 
  scale_color_viridis_c() + 
  coord_sf() + 
  theme_minimal() +
  labs(title = "Mapa de Popularidad de Artistas", x = "Longitude", y = "Latitude") +
  theme(legend.position = 'right')

# MAPA INTERACTIU
leaflet(data = spotify_sf) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(
    radius = ~artist_popularity / 10,
    color = ~viridis::viridis_pal(option = "C")(100)[artist_popularity],
    popup = ~paste("Artist:", artist_name, "<br>Popularity:", artist_popularity),
    fillOpacity = 0.7
  )

# Al mapa interactiu podem posar la popularitat de l'artista i el gènere més fet per aquest artista

########## NOMÉS FALTA EL MAPA DE DENSITAT PER CONTINENTS


