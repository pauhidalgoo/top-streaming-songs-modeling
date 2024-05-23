load('./7_Geoespacial/data_coordenades.RData')
library(sf)
library(ggplot2)
library(dplyr)

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

# Crear el mapa de calor
ggplot(data = data_summarized) +
  geom_sf(aes(fill = avg_followers), color = "grey") +
  scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar") +
  labs(title = "Mapa de Calor de Followers de Artistas por País",
       fill = "Avg Followers") +
  theme_minimal()

#######################################

library(sf)
library(dplyr)
library(ggplot2)

# Cargar los datos
load('./7_Geoespacial/data_coordenades.RData')

# Define una paleta de colores para cada género
genre_colors <- c("Pop" = "red", "Rock" = "blue", "Hip Hop" = "lightgreen", "Christmas" = "gold",
                  "Cinema" = "purple", "Latino" = "orange", "Electro" = "pink", "Other" = "grey")

# Suponemos que 'data' es tu dataframe y ya se ha cargado
# Asegurarse de que el CRS coincide
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

# Convertir a sf para unir con el shapefile
data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = st_crs(world_cities), agr = "constant")
data_joined <- st_join(world_cities, data_sf, join = st_intersects)

# Crear el mapa
ggplot(data = data_joined) +
  geom_sf(aes(fill = most_popular_genre), color = "black") +  # Pinta cada país con el color del género más popular
  scale_fill_manual(values = genre_colors, na.value = "grey") +  # Usa la paleta de colores definida
  labs(title = "Género Musical Más Popular por País", fill = "Género") +
  theme_minimal()

########################################

# Calcular la media de artist_followers por artista
artist_avg_followers <- data %>%
  group_by(artist_name) %>%  # Asumiendo que existe un identificador único para cada artista
  summarize(artist_followers = mean(artist_followers, na.rm = TRUE), .groups = "drop")

# Crear un objeto sf a partir de los datos sumarizados, usando longitud y latitud
data_sf <- st_as_sf(artist_avg_followers, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Realizar un join espacial
