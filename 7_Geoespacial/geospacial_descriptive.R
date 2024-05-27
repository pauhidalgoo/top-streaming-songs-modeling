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
  geom_sf(data = data_sf, color = "red", size = 0.5) +  # Afegim els punts
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

# Fer els ggsave per algun lloc que se m'ha oblidat


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
  labs(title = "Mapa d'Artist_followers per anys", fill = "Artist Followers") +
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
######################## ENERGY ########################
#########################################################

data_grouped <- data %>%
  group_by(track_name) %>% 
  summarize(
    energy = mean(energy, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),  
    latitude = mean(latitude, na.rm = TRUE),
    .groups = "drop"
  )

data_sf <- st_as_sf(data_grouped, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

data_joined <- st_join(world_cities, data_sf, join = st_intersects)

data_summarized <- data_joined %>%
  group_by(name) %>%
  summarize(avg_energy = mean(energy), .groups = "drop")

# Creem un mapa de calor segons els followers dels artistes
ggplot(data = data_summarized) +
  geom_sf(aes(fill = avg_energy), color = "grey") +
  scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar") +
  labs(title = "Mapa de Calor de Energy per País",
       fill = "Avg Energy") +
  theme_minimal()

#########################################################
######################## GÈNERES ########################
#########################################################

#### NOMBRE DE CANÇONS QUE HI HA DE CADA GÈNERE A CADA PAÍS

# Recordar que això s'ha de filtrar per cancons, sinó les contarà repetides
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)

generos <- c("pop", "hip_hop", "rock", "christmas", "cinema", "latino", "electro")

plot_genre_heatmap <- function(genre) {
  # Filtrar les dades per gènere
  data_filtered <- data %>%
    filter(genre == genre) %>%
    group_by(track_name) %>%
    summarize(longitude = mean(longitude, na.rm = TRUE),
              latitude = mean(latitude, na.rm = TRUE),
              nationality = first(nationality),
              .groups = "drop")
  
  # Agrupem per nacionalitat i comptem el nombre de cançons
  data_grouped <- data_filtered %>%
    group_by(nationality) %>%
    summarize(
      count = n(),  # Comptem el nombre de cançons
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
  
  # Dibuixem el mapa de calor
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
  ggsave(filename = paste("heatmap_", generos[i], ".png", sep = ""),
         plot = plots[[i]], width = 10, height = 8)
}


generos <- c("pop", "hip_hop", "rock", "christmas", "cinema", "latino", "electro")
plot_genre_heatmap <- function(genre) {
  # Agrupem i sumem la variable per nacionalitat
  data_per_cançons <- data %>%
    group_by(track_name) %>%
    summarize(genre = mean(genre, na.rm = TRUE), longitude = mean(longitude, na.rm = TRUE),
              latitude = mean(latitude, na.rm = TRUE), nationality = first(nationality))
  
  data_grouped <- data_per_cançons %>%
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
  ggsave(filename = paste("heatmap_", generos[i], ".png", sep = ""),
         plot = plots[[i]], width = 10, height = 8)
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
ploteee <- ggplot(data = data_joined) +
  geom_sf(aes(fill = most_popular_genre), color = "black") +  # Pinta cada país con el color del género más popular
  scale_fill_manual(values = genre_colors, na.value = "grey") +  # Usa la paleta de colores definida
  labs(title = "Gènere Més Popular per País", fill = "Gènere") +
  theme_minimal()

ggsave(filename = paste("genere_mes_popular.png", sep = ""),
       plot = ploteee, width = 10, height = 8)
##############################

# 1. Porcentaje de Canciones de un Género por Paísp

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
    scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar", name = "Percentatge de cançons") +
    labs(title = paste("Percentatge de ", genre, "per País"), fill = "Percentatge de cançons") +
    theme_minimal()
  
  ggsave(filename = paste("popularity_", genre, ".png", sep = ""),
         plot = p, width = 10, height = 8)
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

##EEUU


###### MODELADO Datos Tipo II : Procesos Puntuales
######### Se aconsejan estos links
#Points/Punctual Process in R--LIBRARY(spatstat)
#############  https://spatstat.org/
#https://kevintshoemaker.github.io/NRES-746/sppm.html
#https://cran.r-project.org/web/packages/pointdensityP/pointdensityP.pdf

#### EXAMPLE with a transactional report of crimes in USA
### MODELLING DENSITY and INTENSITY

###MODELADO CON DATOS GEOESPACIALES
#### Paquetes necesarios
# Load the packages
list.of.packages = c("geoR", "sm", "sp", "gstat", "npsp", "geohashTools",
                     "rgdal", "ggmap", "ggplot2", "dplyr", "gridExtra", "maps", 
                     "rnaturalearth", "rnaturalearthdata", "osmdata") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)
data(crime)
head(crime)
crime <- crime[complete.cases(crime), ]
ggplot(crime, aes(x = lon, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = crime) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

ggplot(crime, aes(x = lon, y = lat)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 h = .02, n = 300,
                 geom = "polygon", data = crime) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

ggplot(crime, aes(x = lon, y = lat)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  coord_cartesian(xlim = c(-95.1, -95.7), 
                  ylim = c(29.5, 30.1))

# plot a ggmap basemap
## us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
## map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite",legend="none")
## plot(map)
## scatterplot_murder <- qmplot(x=lon,y=lat,data=filter(crime,offense=="murder"),legend="none",color=I("darkred"))
## plot(scatterplot_murder)

us <- map_data("state")
names(data)
data_eeuu <- dplyr::filter(data, nationality == "United States")

data_eeuu <- filter(data, nationality == "United States")
# Crear el mapa base con ggplot2
us_map <- ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_fixed(1.3) +
  theme_void()
print(us_map)


scatterplot_murder <- us_map +
  geom_point(data = data_eeuu, aes(x = longitude, y = latitude), color = "darkred", size = 1, alpha = 0.6) +
  theme(legend.position = "none")
print(scatterplot_murder)

### Comments about distribution patterns
## The experiment above could be repeated by using other levels for "offense")

# create other types of plots with the ggmap package
densityplot_murder <- qmplot(x=longitude, y=latitude,data = filter(data,nationality=="United States"), 
                             geom = "blank",  
                             darken = .7, egend = "topright") + stat_density_2d(aes(fill = ..level..), 
                                                                                geom = "polygon",alpha = .5,
                                                                                color = NA) + scale_fill_gradient2(low = "blue",mid = "green", 
                                                                                                                   high = "red")
plot(densityplot_murder)
####
#### Repeat the analysis by putting a third dimension --> to use date data
#### to filter temporal patterns and compare month by month in order to
#### find out if there are relationships between time and space.



remove.packages("ggmap")
install.packages("devtools")
devtools::install_github("stadiamaps/ggmap")
library("ggmap")
register_stadiamaps(key = "740eec49-70d9-4974-b3f3-1928c7795f7f")
atlCan <- get_stadiamap(bbox = c(left   = -70.2, 
                                 bottom = 43.0,
                                 right  = -49.8,
                                 top    = 54.5), 
                        zoom = 3, maptype = "stamen_terrain_background")
atlCan <- get_stadiamap(bbox = c(left   = -125.0, 
                                 bottom = 24.5,
                                 right  = -66.9,
                                 top    = 49.0), 
                        zoom = 3, maptype = "stamen_terrain_background")

print(atlCan)
ggmap::ggmap(atlCan)

hola <- qmplot(longitude, latitude, data = data_eeuu, geom = "blank", maptype = "toner-background", 
               darken = .7, legend = "topright")
# create other types of plots with the ggmap package
plot(hola)
densityplot_murder <- qmplot(x=longitude, y=latitude,data_eeuu, 
                             geom = "blank", maptype = "toner-background", 
                             darken = .7, legend = "topright") + stat_density_2d(aes(fill = ..level..), 
                                                                                geom = "polygon",alpha = .5,
                                                                                color = NA) + scale_fill_gradient2(low = "blue",mid = "green", 
                                                                                                                  high = "red")
plot(densityplot_murder)
get_stadiamap()
ggmap::ggmap(get_stadiamap())
hola <- qmplot(longitude, latitude, data = data_eeuu, geom = "blank", maptype = "terrain", 
               darken = .7, legend = "topright")

library(ggplot2)

scatterplot_murder <- us_map + ggplot(data_eeuu, aes(x = longitude, y = latitude)) +
  geom_blank() +
  stat_density2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.5) +
  scale_fill_gradient2(low = "blue", mid = "green", high = "red")

scatterplot_murder <- us_map +
  geom_point(data = data_eeuu, aes(x = longitude, y = latitude), color = "darkred", size = 1, alpha = 0.6) +
  theme(legend.position = "none")

library(ggplot2)
library(maps)

# Cargar los datos del mapa
us <- map_data("state")

us_map <- ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_fixed(1.3) +
  theme_void()

# Crear el mapa base con ggplot2
us_map <- ggplot(data = us, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +  # Define las características del polígono
  coord_fixed(1.3) +  # Fijar la proporción de aspecto para el mapa de USA
  theme_void()  # Remover elementos adicionales como texto y etiquetas de los ejes

# Agregar la densidad de los datos sobre el mapa
scatterplot_murder <- us_map + 
  geom_density_2d_filled(data = data_eeuu, aes(x = longitude, y = latitude, fill = after_stat(level)), alpha = 0.5) +
  scale_fill_gradient2(low = "blue", mid = "green", high = "red")

us_map <- ggplot(data = us, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_fixed(1.3) +
  theme_void()

# Agregar la densidad de los datos sobre el mapa
scatterplot_murder <-ggplot() + geom_density_2d_filled(data = data_eeuu, aes(x = longitude, y = latitude, fill = after_stat(density)), alpha = 0.5)+
  scale_fill_gradient2(low = "blue", mid = "green", high = "red")

# Mostrar el gráfico
print(scatterplot_murder)




########## VAAA AMB EL PUTU MAPA

library(ggmap)
min_longitude <- min(data_eeuu$longitude, na.rm = TRUE)
max_longitude <- max(data_eeuu$longitude, na.rm = TRUE)
min_latitude <- min(data_eeuu$latitude, na.rm = TRUE)
max_latitude <- max(data_eeuu$latitude, na.rm = TRUE)

bbox <- c(left = min_longitude, bottom = min_latitude, right = max_longitude, top = max_latitude)

atlCan <- get_stadiamap(bbox = bbox, 
                        zoom = 6, maptype = "stamen_terrain_background")
base_map <- ggmap(atlCan)
base_map

library(ggplot2)
data_eeuu <- data_eeuu[!is.na(data_eeuu$longitude) & !is.na(data_eeuu$latitude),]

enhanced_map <- base_map + 
  geom_density_2d_filled(data = data_eeuu, aes(x = longitude, y = latitude, fill = after_stat(level)), alpha = 0.5) +
  scale_fill_gradient2(low = "blue", mid = "green", high = "red") +
  labs(fill = "Density Level")

enhanced_map <- base_map + 
  stat_density_2d(geom = "polygon", contour = TRUE,
                  aes(fill = after_stat(level)), colour = "black",
                  bins = 5)+
  geom_point()+
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_classic()

verification_map <- base_map + 
  geom_point(data = data_eeuu, aes(x = longitude, y = latitude), color = "red", size = 1)
# Asumiendo que base_map es tu mapa base generado previamente
enhanced_map <- base_map + 
  geom_density_2d_filled(
    data = data_eeuu, 
    aes(x = longitude, y = latitude, fill = after_stat(density)),  # Usando density
    alpha = 0.5
  ) +
  scale_fill_gradient2(low = "blue", mid = "green", high = "red") +
  labs(fill = "Density Level")

# Mostrar el mapa con los puntos
print(verification_map)
# Mostrar el gráfico mejorado
print(enhanced_map)


########us <- map_data("state")
murder_data <- filter(crime, offense == "murder")
# Crear el mapa base con ggplot2
us_map <- ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  coord_fixed(1.3) +
  theme_void()

scatterplot_murder <- us_map +
  geom_point(data = murder_data, aes(x = lon, y = lat), color = "darkred", size = 1, alpha = 0.6) +
  theme(legend.position = "none")
print(scatterplot_murder)

### Comments about distribution patterns
## The experiment above could be repeated by using other levels for "offense")

# create other types of plots with the ggmap package
densityplot_murder <- qmplot(x=lon, y=lat,data = filter(crime,offense=="murder"), 
                             geom = "blank", maptype = "toner-background", 
                             darken = .7, egend = "topright") + stat_density_2d(aes(fill = ..level..), 
                                                                                geom = "polygon",alpha = .5,
                                                                                color = NA) + scale_fill_gradient2(low = "blue",mid = "green", 
                                                                                                                   high = "red")
plot(densityplot_murder)
# plot a ggmap basemap
us <- c(left = -125, bottom = 25.75, right = -67, top = 49)

data(crime)
View(crime)
get_stadiamap()
ggmap::ggmap(get_stadiamap())
map <- get_stadiamap(us, zoom = 3, maptype = "stamen_terrain_background",legend="none")
plot(map)

head(crime)
crime <- crime[complete.cases(crime), ]
sum(crime$offense == "murder", na.rm = TRUE)
dato=filter(crime,offense=="murder")
scatterplot_murder <- qmplot(x=lon,y=lat,data=crime,legend="none",color=I("darkred"))
plot(scatterplot_murder)
densityplot_murder <- qmplot(x=longitude, y=latitude,data = data_eeuu, 
                             geom = "blank", maptype = "stamen_toner_background", 
                             darken = .7) + stat_density_2d(aes(fill = ..level..), 
                                                                                 geom = "polygon",alpha = .5,
                                                                                 color = NA) + scale_fill_gradient2(low = "blue",mid = "green", 
                                                                                                                    high = "red")
densityplot_murder <- qmplot(x=lon, y=lat,data = crime, 
                             geom = "blank", maptype = "stamen_toner_background", 
                             darken = .7, legend = "topright") + stat_density_2d(aes(fill = ..level..), 
                                                                                geom = "polygon",alpha = .5,
                                                                                color = NA) + scale_fill_gradient2(low = "blue",mid = "green", 
                                                                                                                   high = "red")
plot(densityplot_murder)


############# EUROPA

# Ejemplo de cómo podrías filtrar los datos para incluir varios países europeos.
# Esto dependerá de cómo está estructurada tu columna 'nationality' o similar.
european_countries <- c("United Kingdom", "France", "Germany", "Spain", "Italy", "Netherlands", "Sweden","Norway")
data_europa <- dplyr::filter(data, nationality %in% european_countries)
dades_proba <- dplyr::filter(data, nationality == "United Kingdom")

library(dplyr)

# Asumiendo que tu dataframe se llama 'data' y tienes una columna 'artist_name' y otra 'city'
unique_artists <- data %>%
  distinct(track_name, .keep_all = TRUE)  # Esto retiene la primera aparición de cada artista
dades_proba <- dplyr::filter(unique_artists, nationality == "United Kingdom")
# Ahora cuenta cuántas veces aparece cada ciudad en este dataframe filtrado
city_counts <- dades_proba %>%
  group_by(city) %>%
  summarise(count = n(), .groups = "drop")  # '.groups = "drop"' elimina el agrupamiento después de summarise


#data_europa <- dplyr::filter(data, nationality == "United Kingdom")
#çdata_europa <- dplyr::filter(data, city == "Glasgow")

densityplot_europe <- qmplot(x=longitude, y=latitude,data = data_europa, 
                             geom = "blank", maptype = "stamen_toner_background", 
                             darken = .7) + stat_density_2d(aes(fill = ..level..), 
                                                            geom = "polygon",alpha = .5,
                                                            color = NA) + scale_fill_gradient2(low = "blue",mid = "green", 
                                                                                               high = "red")

plot(densityplot_europe)




######################################################
############ PART PAU
######################################################

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
library(dplyr)

popularity_by_country <- data2024 %>%
  group_by(country) %>%
  summarise(avg_popularity = mean(popularity, na.rm = TRUE))

#world <- st_read(system.file("./7_Geoespacial/countries_map.shp", package = "maptools"))

world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")
popularity_by_country

# Merge popularity data with world spatial dataset
world <- left_join(world_cities, popularity_by_country, by = c("iso_3166_1_" = "country"))

# Plot choropleth map of average Spotify popularity by country
tm_shape(world) +
  tm_borders() +
  tm_fill("avg_popularity", palette = "Blues", style = "quantile") +
  tm_layout(title = "Average Spotify Popularity by Country")

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

# Create animation animació afegirli el mapa per sota
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

<<<<<<< HEAD
############### DATA 2024 ###############
=======
  <<<<<<< HEAD

load('./7_Geoespacial/mean_new_data.RData')

mean_new_data$popularity
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = mean_new_data, ~lon, ~lat, popup = ~country)


new_world <- left_join(world_cities, mean_new_data, by = c("iso_3166_1_" = "country"))

tm_shape(new_world) +
  tm_borders() +
  tm_fill("instrumentalness", palette = "Blues", style = "quantile") +
  tm_layout(main.title = "Average Spotify Popularity by Country", frame=FALSE, legend.outside = TRUE)

ggplot() +
  geom_sf(data = new_world, color = "black", mapping= aes(fill=energy)) +
  theme_minimal() +
  labs(title = "Mapa del món")

ggplot(data = new_world) +
  geom_sf(aes(fill = energy), color = "grey") +
  scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar") +
  labs(title = "Mapa de Calor de Energy per País",
       fill = "Avg Energy") +
  theme_minimal()


?aes

?tm_layout


?leaflet
m <- leaflet() %>% addTiles()
m 

=======
  ############### DATA 2024
  >>>>>>> 2d64e171e5c953c707f6ec290c270747a9c7f3be

### IS_EXPLICIT

filtered_data <- data2024 %>%
  select(country, is_explicit)

filtered_data <- filter(filtered_data, country != '')
filtered_data$is_explicit <- as.logical(filtered_data$is_explicit)

# Agrupar por país y calcular el porcentaje de canciones explícitas
explicit_percentage_by_country <- filtered_data %>%
  group_by(country) %>%
  summarise(percent_explicit = mean(is_explicit, na.rm = TRUE) * 100)  # Multiplica por 100 para obtener el porcentaje

world <- left_join(world_cities, explicit_percentage_by_country, by = c("iso_3166_1_" = "country"))

# Asegúrate que 'world_cities' y 'explicit_percentage_by_country' estén correctamente definidos y unidos
world <- left_join(world_cities, explicit_percentage_by_country, by = c("iso_3166_1_" = "country"))

# Crear el mapa usando tmap
tm_map <- tm_shape(world) +
  tm_borders() +
  tm_fill("percent_explicit", palette = "Blues", style = "quantile") +  # Corregido: nombre de columna entre comillas
  tm_layout(legend.position = c("left", "bottom"))
#tm_layout(title = "Average Spotify Explicit Content by Country")  # Corregido: título simplificado sin 'names[i]'

print(tm_map)


###########aaaaaaaaa

library(tmap)
library(ggplot2)

# Configura tmap para usar el modo plot (basado en ggplot2)
tmap_mode("plot")

# Crea el mapa
tm_map <- tm_shape(world) +
  tm_borders() +
  tm_fill("percent_explicit", palette = "Blues", style = "quantile") +
  tm_layout(legend.position = c("left", "bottom"))

# Muestra el mapa (opcional, para verificar su apariencia antes de guardar)
map_ggplot <- tmap_arrange(tm_map)

# Guardar el mapa con ggsave
ggsave("mapa_guardado.png", map_ggplot, width = 10, height = 8, dpi = 300)

### Mapa pero amb la logica

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


tm_map <- tm_shape(world) +
  tm_borders() +
  tm_fill("majority_explicit", palette = c("red", "lightgreen"), style = "cat" +
            labels = c("No", "Yes"), title = "Contingut Explicit")
# Imprimir el mapa
print(tm_map)

# Opcional: Guardar el mapa como PNG
tmap_save(tm_map, filename = paste("map_explicites_logit.png"))
# Crear el mapa con tmap

######### ES AQUEST
tm_map <- tm_shape(world) +
  tm_borders() +
  tm_fill("majority_explicit", palette = c("red", "lightgreen"), style = "cat", 
          labels = c("No", "Yes"), title = "Explicit Content") +
  tm_layout(legend.position = c("left", "bottom"),main.title = "Països amb la majoria de cançons explícites")
# tm_layout(title = "Countries with Majority Explicit Songs")

#### VARIABLES NUMÈRIQUES

world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")

# Calcular la media de las variables de interés por país
averages_by_country <- data2024 %>%
  group_by(country) %>%
  summarise(
    avg_energy = mean(energy, na.rm = TRUE),
    avg_valence = mean(valence, na.rm = TRUE), 
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_speechiness = mean(speechiness, na.rm = TRUE),
    avg_loudness = mean(loudness, na.rm = TRUE)
  )


world <- left_join(world_cities, averages_by_country, by = c("iso_3166_1_" = "country"))

variables <- c("avg_energy","avg_valence", "avg_danceability", "avg_speechiness", "avg_loudness")
names <- c("energy", "valence", "danceability", "speechiness", "loudness")  # Nombres para los títulos

# Ciclo para crear un mapa para cada variable
for (i in seq_along(variables)) {
  tm_map <- tm_shape(world) +
    tm_borders() +
    tm_fill(variables[i], palette = "Blues", style = "quantile") +
    tm_layout(main.title = paste("Mitjana de", names[i], "per País"), legend.position = c("left", "bottom"))
  # Imprimir el mapa
  print(tm_map)
  
  # Opcional: Guardar el mapa como PNG
  tmap_save(tm_map, filename = paste("map_", names[i], ".png"))
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
variables <- c("avg_energy", "avg_danceability", "avg_speechiness", "avg_loudness")
names <- c("energy", "danceability", "speechiness", "loudness")  # Nombres para los títulos

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
>>>>>>> 078c10fc41b95958798a0ff1f574f4a91196211c