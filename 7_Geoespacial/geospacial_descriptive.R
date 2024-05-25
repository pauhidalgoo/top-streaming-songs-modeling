load('./7_Geoespacial/data_coordenades.RData')
library(sf)
library(ggplot2)
library(dplyr)
library(viridis) 

library(dplyr)
library(sf)
library(ggplot2)
library(viridis)

install.packages("viridis")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("leaflet")
library(ggplot2)
library(sf)
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

ggplot() +
  geom_sf(data = world_cities, fill = "white", color = "black") +  # Dibuixem el shapefile
  geom_sf(data = data_sf, color = "darkgreen", size = 0.5) +  # Afegim els punts
  theme_minimal() +
  labs(title = "Mapa amb els punts de dades")

st_crs(world_cities) <- 4326
View(data)

# Les coordenades estan segons artistes, així agrupem pel nom d'artista. 
data_grouped <- data %>%
  group_by(artist_name) %>% 
  summarize(
    artist_followers = mean(artist_followers, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),  
    latitude = mean(latitude, na.rm = TRUE),
    .groups = "drop"
  )

View(data_grouped)

data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

data_joined <- st_join(world_cities, data_sf, join = st_intersects)

data_summarized <- data_joined %>%
  group_by(name) %>%
  summarize(avg_followers = mean(artist_followers), .groups = "drop")

print(head(data_summarized))

# Creem un mapa de calor segons els followers dels artistes
ggplot(data = data_summarized) +
  geom_sf(aes(fill = avg_followers), color = "grey") +
  scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar") +
  labs(title = "Mapa de Calor de Artist_followers per País",
       fill = "Avg Followers") +
  theme_minimal()

#######################################
###### PARA HACER EL DE GÈNERES #######
#######################################

# Cargar los datos de los países
world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")

data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
View(data_sf)

data_joined <- st_join(world_cities, data_sf, join = st_intersects)
View(data_joined)

data_summarized <- data_joined %>%
  group_by(name, hip_hop) %>%
  summarize(songs_per_genre = n(), .groups = "drop")

print(head(data_summarized))

create_heatmap <- function(data, genre_column) {
  ggplot(data = data %>% filter(!!sym(genre_column) == 1)) +
    geom_sf(aes(fill = songs_per_genre), color = "grey") +
    scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar") +
    labs(title = paste("Mapa de Calor de Canciones por Género:", genre_column),
         fill = "Número de Canciones") +
    theme_minimal()
}

map_hip_hop <- create_heatmap(data_summarized, "hip_hop")

print(map_hip_hop)

########################
# VISUALITZAR PER ANYS #
########################

data_summarized <- data_joined %>%
  group_by(name) %>%
  summarize(songs_per_genre = n(), .groups = "drop")

ggplot(data = data_summarized) +
  geom_sf(aes(fill = artist_followers)) +
  facet_wrap(~year_weak) +
  theme_bw() +
  scale_fill_viridis(name = "Followers") +
  labs(title = "Mapa de Seguidores de Artistas por Año", fill = "Artist Followers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

View(data_summarized)

#########################################################
# VISUALITZAR QUIN GÈNERE ÉS EL PREDOMINANT A CADA PAIS #
#########################################################

genre_colors <- c("Pop" = "red", "Rock" = "blue", "Hip Hop" = "lightgreen", "Christmas" = "gold",
                  "Cinema" = "purple", "Latino" = "orange", "Electro" = "pink", "Other" = "grey")
st_crs(world_cities) <- 4326

library(tidyr)

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


###################POP

data_grouped <- data %>%
  group_by(nationality) %>%
  summarize(
    latino = sum(latino, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),
    latitude = mean(latitude, na.rm = TRUE),
    .groups = "drop"
  )

# Convertir a simple features para unir con el shapefile de países
data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
data_joined <- st_join(world_cities, data_sf, join = st_intersects)

# Definir los colores para el mapa de calor
scale_color <- scale_fill_viridis_c(
  option = "C", 
  na.value = "grey", 
  guide = "colorbar",
  name = "Popularity of Pop"
)

# Crear el mapa de calor para música pop
ggplot(data = data_joined) +
  geom_sf(aes(fill = latino), color = "black") +  # Pintar cada país según la cantidad de música pop
  scale_color +
  labs(title = "Popularity of Pop Music by Country", fill = "Number of Pop Songs") +
  theme_minimal()


############# PER TOTS ELS GENERES

library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# Suponiendo que 'world_cities' ya está cargado
genres <- c("pop", "rock", "hip_hop", "christmas", "cinema", "latino", "electro")

# Asumiendo que 'data' ya está cargado
for (genre in genres) {
  # Agrupar y sumar por género
  data_grouped <- data %>%
    group_by(nationality) %>%
    summarize(
      total = sum(.data[[genre]], na.rm = TRUE),
      longitude = mean(longitude, na.rm = TRUE),
      latitude = mean(latitude, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Convertir a simple features
  data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  data_joined <- st_join(world_cities, data_sf, join = st_intersects)
  
  # Crear y mostrar el mapa de calor para el género actual
  ggplot(data = data_joined) +
    geom_sf(aes(fill = total), color = "black") +
    scale_fill_viridis_c(
      option = "C", 
      na.value = "grey", 
      guide = "colorbar",
      name = "Popularity of " %>% paste(genre)
    ) +
    labs(title = paste("Popularity of", genre, "Music by Country"),
         fill = paste("Number of", genre, "Songs")) +
    theme_minimal()
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

# Podem afegir el gènere més fet de cada artista



########## NOMÉS FALTA EL MAPA DE DENSITAT

########## PER ANYS:

library(dplyr)

data_per_anys <- data %>%
  group_by(nationality, year_week, latitude, longitude) %>%
  summarize(mean_followers = mean(artist_followers, na.rm = TRUE), .groups = 'drop')

# Visualizar los resultados
View(data_per_anys)

data_sf <- st_as_sf(data_per_anys, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Suponiendo que 'world_cities' es un sf object con ciudades y países, y que 'name' es el país
data_joined <- st_join(world_cities, data_sf, join = st_intersects)

# Agrupar por país y calcular la media de seguidores
data_summarized <- data_joined %>%
  group_by(name) %>%
  summarize(avg_followers = mean(mean_followers), .groups = "drop")

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

# Aplicar la función a cada año y guardar o mostrar los mapas
unique_years <- unique(data_summarized$year_week)
plots <- lapply(unique_years, plot_heatmap_per_year)

# Si deseas visualizar los plots uno por uno o guardarlos
for (i in seq_along(plots)) {
  print(plots[[i]])  # Mostrar en RStudio o en una interfaz gráfica
  
  # Para guardar cada plot como archivo de imagen
  #ggsave(filename = paste("heatmap_", unique_years[i], ".png", sep = ""),
         #plot = plots[[i]], width = 10, height = 8)
}



# Imprimir los primeros registros para verificar
print(head(data_summarized))

# Crear un mapa de calor usando ggplot2
library(ggplot2)
library(viridis)  # para la escala de colores viridis
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)

ggplot(data = data_summarized) +
  geom_sf(aes(fill = avg_followers), color = "grey") +
  scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar") +
  labs(title = "Mapa de Calor de Artist_followers por País",
       fill = "Avg Followers") +
  theme_minimal()

