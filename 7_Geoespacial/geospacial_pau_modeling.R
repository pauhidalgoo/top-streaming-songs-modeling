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
perform_kriging <- function(df, variable_name, rangemax, rangemin) {
  # Load necessary libraries

  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  lon_range <- c(-174, 174)
  lat_range <- c(-84, 84)
  resolution <- 0.5  # Adjust resolution as needed
  
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
  proj4string(df) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  formula <- as.formula(paste(variable_name, "~ 1"))
  
  repeat {
    cutoff <- as.numeric(readline(prompt = "Enter variogram cutoff: "))
    width <- as.numeric(readline(prompt = "Enter variogram width: "))
    
    v <- variogram(formula, data = df, cutoff = cutoff, width = width)
    print(plot(v))
    
    if (tolower(readline(prompt = "Are you satisfied with the cutoff and width? (yes/no): ")) == "yes") {
      break
    }
  }
  
  print(show.vgms())
  model <- readline(prompt = "Enter the model: ")
  
  repeat {
    psill <- as.numeric(readline(prompt = "Enter partial sill: "))
    range <- as.numeric(readline(prompt = "Enter range: "))
    nugget <- as.numeric(readline(prompt = "Enter nugget: "))
    
    variogram_model <- vgm(psill = psill, model = model, range = range, nugget = nugget)
    print(plot(v, pl = TRUE, model = variogram_model))
    
    if (tolower(readline(prompt = "Are you satisfied with the variogram model? (yes/no): ")) == "yes") {
      break
    }
  }
  
  # Step 4: Variogram modeling
  v_model <- fit.variogram(v, model = variogram_model)
  
  # Display the variogram model
  print(plot(v, model = v_model, main = "Ajust del Model automàtic"))
  
  # Ask user which variogram model to use
  use_fitted <- tolower(readline(prompt = "Do you want to use the fitted variogram model? (yes/no): "))
  if (use_fitted == "yes") {
    final_model <- v_model
  } else {
    final_model <- variogram_model
  }
  
  # Set coordinates and CRS for data and grid
  gridded(grid) <- TRUE
  proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")
  
  # Step 5: Kriging interpolation
  kriged <- krige(formula, locations = df, newdata = grid, model = final_model)
  
  plot(kriged)
  
  spplot(kriged["var1.pred"], main = paste("Kriged", variable_name))
  
  kriged$var1.pred <- pmax(pmin(kriged$var1.pred, rangemax), rangemin)
  
  spplot(kriged["var1.pred"], main = paste("Kriged", variable_name))
  
  kriged_df <- as.data.frame(kriged)
  
  # Convert original data points to an sf object
  df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  
  # Plot using ggplot2
  ggplot() +
    geom_tile(data = kriged_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
    scale_fill_gradient(low = "blue", high = "red", limits = c(rangemin, rangemax), name = paste("Predicted", variable_name)) +
    geom_sf(data = df_sf, color = "black", size = 2) +
    theme_minimal() +
    ggtitle(paste("Kriged", variable_name, "with Original Data Points")) +
    coord_sf(xlim = lon_range, ylim = lat_range)
}
df <- summary_data  # Your dataset
perform_kriging(df, "mean_danceability", rangemin = 0, rangemax = 1)

df <- summary_data 
perform_kriging(df, "mean_popularity", rangemin = 0, rangemax = 100)

names(df)
df <- summary_data 
perform_kriging(df, "mean_valence", rangemin = 0, rangemax = 1)



coordinates(df) <- ~lon + lat
proj4string(df) <- CRS("+proj=longlat +datum=WGS84 +no_defs")



# MITJANA de tots els dies ------------------------------------------------------
# És millor
load('./7_Geoespacial/mean_new_data.RData')

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = mean_new_data, ~lon, ~lat, popup = ~country)

mean_new_data <- na.omit(mean_new_data) # Borrem el global

names(mean_new_data)

df <- mean_new_data
perform_kriging(df, "valence", rangemin = 0, rangemax = 1)

df <- mean_new_data
perform_kriging(df, "popularity", rangemin = 0, rangemax = 100)

