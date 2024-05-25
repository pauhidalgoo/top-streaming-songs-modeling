library("rgdal")
library("sp")
library("sf")

library(sf)
library(ggplot2)
library(dplyr)
library(gstat)
library(gstat)
library(gstat)
library(sp)
# Carreguem el dataset amb les coordenades (latitude i longitude)

load('./7_Geoespacial/2024_top5_countries.RData')

# Treiem les files amb (artistes, cançons) repetides

data <- data2024[data2024$snapshot_date=="2024-05-22",]




#Farem la mitjana dels valors, i ens quedarem amb 1 per país

mode_func <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


summary_data <- data %>%
  group_by(country) %>%
  summarise(
    mean_popularity = mean(popularity, na.rm = TRUE),
    mean_duration_ms = mean(duration_ms, na.rm = TRUE),
    mean_danceability = mean(danceability, na.rm = TRUE),
    mean_energy = mean(energy, na.rm = TRUE),
    mean_loudness = mean(loudness, na.rm = TRUE),
    mean_speechiness = mean(speechiness, na.rm = TRUE),
    mean_acousticness = mean(acousticness, na.rm = TRUE),
    mean_instrumentalness = mean(instrumentalness, na.rm = TRUE),
    mean_liveness = mean(liveness, na.rm = TRUE),
    mean_valence = mean(valence, na.rm = TRUE),
    mean_tempo = mean(tempo, na.rm = TRUE),
    mean_time_signature = mean(time_signature, na.rm = TRUE),
    mode_is_explicit = mode_func(is_explicit),
    mode_key = mode_func(key),
    mode_mode = mode_func(mode),
    mode_album_name = mode_func(album_name),
    mode_album_release_date = mode_func(album_release_date),
    lat = mean(lat, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE)
  )

world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")
plot(world_cities$geometry)
points(data2024, col = 'red', pch = 20)


world <- left_join(world_cities, popularity_by_country, by = c("iso_3166_1_" = "country"))


leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = summary_data, ~lon, ~lat, popup = ~country)

summary_data <- na.omit(summary_data) # Borrem el global

names(summary_data)
# variograma i kriging ----------------------------------------------------

df <- summary_data

lon_range <- range(df$lon)
lat_range <- range(df$lat)
resolution <- 10  # Adjust as needed
lon_grid <- seq(lon_range[1], lon_range[2], by = resolution)
lat_grid <- seq(lat_range[1], lat_range[2], by = resolution)
grid <- expand.grid(lon = lon_grid, lat = lat_grid)

# Step 3: Variogram estimation
coordinates(df) <- ~lon + lat
v <- variogram(mean_popularity ~ 1, data = df)
plot(v)
# Step 4: Variogram modeling
v_model <- fit.variogram(v, model = vgm(psill = 200, model = "Sph", range = 20, nugget = 20))

model_gau <- vgm(psill = 100, model = "Gau", range = 20, nugget = 50)
plot(v, model = model_gau, main = "Ajust del Model Gaussià")




coordinates(df) <- c("lon", "lat")  # Set coordinates
proj4string(df) <- CRS("+proj=longlat +datum=WGS84")

coordinates(grid) <- c("lon", "lat")
gridded(grid) <- TRUE

# Set the same CRS as the original data
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")
# Step 5: Kriging interpolation
kriged <- krige(mean_popularity ~ 1, locations = df, newdata = grid, model = model_gau)

# Plot the result
plot(kriged)
kriged_df <- as.data.frame(kriged)
colnames(kriged_df) <- c("lon", "lat", "mean_popularity", "var1.var")

# Plot the result using ggplot2
world_map <- map_data("world")

ggplot() +
  geom_tile(data = kriged_df, aes(x = lon, y = lat, fill = mean_popularity)) +
  scale_fill_viridis_c() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), color = "white", fill = NA) +
  coord_fixed(1) +
  theme_minimal() +
  labs(title = "Kriging Interpolation of Mean Popularity",
       fill = "Mean Popularity")
