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
library(leaflet)
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

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = summary_data, ~lon, ~lat, popup = ~country)

summary_data <- na.omit(summary_data) # Borrem el global

names(summary_data)
# variograma i kriging ----------------------------------------------------
df <- summary_data

world <- ne_countries(scale = "medium", returnclass = "sf")

lon_range <- c(-174, 174)
lat_range <- c(-84, 84)
resolution <- 1  # Adjust resolution as needed

# Create longitude and latitude grids
lon_grid <- seq(lon_range[1], lon_range[2], by = resolution)
lat_grid <- seq(lat_range[1], lat_range[2], by = resolution)

# Create a grid covering the entire world
grid <- expand.grid(lon = lon_grid, lat = lat_grid)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
grid_land <- st_join(grid_sf, world, join = st_within)

# Filter out points that are not over land (NA values)
grid_land <- grid_land[!is.na(grid_land$admin), ]

# Convert back to sp object for kriging
grid_land_sp <- as(grid_land, "Spatial")
gridded(grid_land_sp) <- TRUE

grid <- grid_land_sp


# Step 3: Variogram estimation
coordinates(df) <- ~lon + lat
v <- variogram(mean_danceability ~ 1, data = df, cutoff = 65, width = 10)
plot(v)
show.vgms()

# Define the variogram model
variogram_model <- vgm(psill = 0.007, model = "Gau", range = 60, nugget = 0.0035)

# Plot the variogram
plot(v, pl = TRUE, model = variogram_model)

# Step 4: Variogram modeling
v_model <- fit.variogram(v, model = variogram_model)

# Display the variogram model
v_model

# Create and plot a Gaussian variogram model
model_gau <- vgm(psill = 100, model = "Gau", range = 20, nugget = 50)
plot(v, model = model_gau, main = "Gaussian Model Fit")

# Set coordinates and CRS for data and grid
coordinates(df) <- c("lon", "lat")
proj4string(df) <- CRS("+proj=longlat +datum=WGS84")

coordinates(grid) <- c("lon", "lat")
gridded(grid) <- TRUE

# Set the same CRS as the original data for the grid
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

# Step 5: Kriging interpolation
kriged <- krige(mean_popularity ~ 1, locations = df, newdata = grid, model = v_model)

plot(kriged)

spplot(kriged["var1.pred"], main = "Kriged mean_popularity")

kriged$var1.pred <- pmax(pmin(kriged$var1.pred, 100), 0)

spplot(kriged["var1.pred"], main = "Kriged mean_popularity")


kriged_df <- as.data.frame(kriged)

# Convert original data points to an sf object
df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

# Plot using ggplot2
ggplot() +
  geom_tile(data = kriged_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0, 100), name = "Predicted Popularity") +
  geom_sf(data = df_sf, color = "black", size = 2) +
  theme_minimal() +
  ggtitle("Kriged mean_popularity with Original Data Points") +
  coord_sf(xlim = lon_range, ylim = lat_range)



# MITJANA de tots els dies ------------------------------------------------------

load('./7_Geoespacial/mean_new_data.RData')

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = mean_new_data, ~lon, ~lat, popup = ~country)

mean_new_data <- na.omit(mean_new_data) # Borrem el global

names(mean_new_data)
# variograma i kriging ----------------------------------------------------

df <- mean_new_data

lon_range <- range(df$lon)
lat_range <- range(df$lat)
resolution <- 1  # Adjust as needed
lon_grid <- seq(lon_range[1], lon_range[2], by = resolution)
lat_grid <- seq(lat_range[1], lat_range[2], by = resolution)
grid <- expand.grid(lon = lon_grid, lat = lat_grid)

# Step 3: Variogram estimation
names(df)
coordinates(df) <- ~lon + lat
v <- variogram(danceability ~ 1, data = df,width = 13)
plot(v)
show.vgms()

variogram_model <- vgm(psill = 50, model = "Wav", range = 25, nugget = 20)

plot(v, pl = T, model = variogram_model)

# Step 4: Variogram modeling
v_model <- fit.variogram(v, model = variogram_model)

v_model
model_gau

model_gau <- vgm(psill = 100, model = "Gau", range = 20, nugget = 50)
plot(v, model = model_gau, main = "Ajust del Model Gaussià")


coordinates(df) <- c("lon", "lat")  # Set coordinates
proj4string(df) <- CRS("+proj=longlat +datum=WGS84")

coordinates(grid) <- c("lon", "lat")
gridded(grid) <- TRUE

# Set the same CRS as the original data
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")
# Step 5: Kriging interpolation
kriged <- krige(popularity ~ 1, locations = df, newdata = grid, model = v_model)

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

