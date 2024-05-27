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
PATH_PLOTS = paste(getwd(),"./Media/Geoespacial_new",sep="")

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

summary_data <- na.omit(summary_data) # Borrem el global

names(summary_data)
# variograma i kriging ----------------------------------------------------
perform_kriging <- function(df, variable_name, rangemax, rangemin, continent = NULL, data_name = "data") {
  
  # Països i informació de continents
  world <- ne_countries(scale = "medium", returnclass = "sf")

  # Filtrem les dades per continent si cal
  if (!is.null(continent)) {
    if (continent == "Europe") {
      european_countries <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", 
                              "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", 
                              "Czechia", "Denmark", "Estonia", "Finland", "France", "Georgia", 
                              "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", 
                              "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", 
                              "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", 
                              "North Macedonia", "Norway", "Poland", "Portugal", "Romania", 
                              "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", 
                              "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", 
                              "Vatican City")
      overseas_territories <- c("French Guiana", "Guadeloupe", "Martinique", "Réunion", "Mayotte")
      world <- world[world$name %in% european_countries, ]
      world <- world[!world$name %in% overseas_territories, ]
      df <- df[df$continent == continent,]
    }else{
      world <- world[world$continent == continent, ]
      df <- df[df$continent == continent,]
    }
  }
  
  lon_range <- c(-174, 174)
  lat_range <- c(-84, 84)
  resolution <- 0.5
  
  lon_grid <- seq(lon_range[1], lon_range[2], by = resolution)
  lat_grid <- seq(lat_range[1], lat_range[2], by = resolution)
  
  # Creem le grid
  grid <- expand.grid(lon = lon_grid, lat = lat_grid)
  grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
  grid_land <- st_join(grid_sf, world, join = st_within)
  
  # Filtrem els punts que no estan a sobre el mapa (oceà)
  grid_land <- grid_land[!is.na(grid_land$admin), ]
  
  # Reconvertim a spatial per fer el kriging
  grid_land_sp <- as(grid_land, "Spatial")
  gridded(grid_land_sp) <- TRUE
  
  grid <- grid_land_sp
  
  # Variograma
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
  
  # Variograma "automàtic"
  v_model <- fit.variogram(v, model = variogram_model)
  print(plot(v, model = v_model, main = "Ajust del Model automàtic"))
  
  # Pregunta quin vols
  use_fitted <- tolower(readline(prompt = "Do you want to use the fitted variogram model? (yes/no): "))
  if (use_fitted == "yes") {
    final_model <- v_model
    print(v_model)
  } else {
    final_model <- variogram_model
  }
  
  # Mateix tipus de coordenades
  gridded(grid) <- TRUE
  proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")
  
  
  # Validació
  kriging_cv <- krige.cv(formula, df, model = final_model, nfold = nrow(data), verbose = FALSE)
  
  me <- mean(kriging_cv$residual)
  rmse <- sqrt(mean(kriging_cv$residual^2))
  msre <- mean(kriging_cv$zscore^2)
  
  cat("Error medio (ME):", me, "\n")
  cat("Raíz del error cuadrático medio (RMSE):", rmse, "\n")
  cat("Error cuadrático medio de los z-scores (MSRE):", msre, "\n")
  
  
  # Interpol·lació
  kriged <- krige(formula, locations = df, newdata = grid, model = final_model)
  
  plot(kriged)
  
  spplot(kriged["var1.pred"], main = paste("Kriged", variable_name))
  
  kriged$var1.pred <- pmax(pmin(kriged$var1.pred, rangemax), rangemin)
  
  spplot(kriged["var1.pred"], main = paste("Kriged", variable_name))
  
  kriged_df <- as.data.frame(kriged)
  
  df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  
  # Plot amb ggplot2
  a <- ggplot() +
    geom_tile(data = kriged_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
    scale_fill_gradient(low = "blue", high = "red", limits = c(rangemin, rangemax), name = paste("Predicted", variable_name)) +
    geom_sf(data = df_sf, color = "black", size = 2) +
    theme_minimal() +
    ggtitle(paste("Kriged", variable_name, "with Original Data Points")) +
    coord_sf(xlim = lon_range, ylim = lat_range)
  print(a)
  
  print(paste0("Values used: cutoff - ", cutoff, ", width - ", width, ", psill - ", psill, ", range - ", range, ", nugget - ", nugget))
  kriged_df
}
# Tan sols de l'últim dia, no usat 

df <- summary_data
perform_kriging(df, "mean_danceability", rangemin = 0, rangemax = 1)

df <- summary_data 
perform_kriging(df, "mean_popularity", rangemin = 0, rangemax = 100)

names(df)
df <- summary_data 
perform_kriging(df, "mean_valence", rangemin = 0, rangemax = 1)

#coordinates(df) <- ~lon + lat
#proj4string(df) <- CRS("+proj=longlat +datum=WGS84 +no_defs")



# MITJANA de tots els dies ------------------------------------------------------
# És millor
load('./7_Geoespacial/mean_new_data.RData')

mean_new_data <- na.omit(mean_new_data) # Borrem el global

#names(mean_new_data)

df <- mean_new_data
k <- perform_kriging(df, "energy", rangemin = 0, rangemax = 1)
# cutoff: 5000
# width: 300

# psill: 0.0025
# range: 2200
# nugget: 0.0002

# o bé

# cutoff: 10000
# width: 500
# psill: 0.003
# range: 3000
# nugget: 0.0005 i auto
coordinates(k) <- ~coords.x1 + coords.x2

spplot(k["var1.pred"], main = paste("Kriged", "Energy"))

k <- as.data.frame((k))
lon_range <- c(-174, 174)
lat_range <- c(-84, 84)

png(file=paste0(PATH_PLOTS, "/energy_interpolation.png"),
    width=1920, height=1080, units="px", res=130)
ggplot() +
  geom_tile(data = k, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(min(k$var1.pred), max(k$var1.pred)), name = paste("Predicted", "Energy")) +
  theme_minimal() +
  ggtitle(paste("Kriged", "energy", "")) +
  coord_sf(xlim = lon_range, ylim = lat_range)+ 
  paletteer::scale_fill_paletteer_c("viridis::magma", name="Energy")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(face="bold", size = 15),
        legend.title = element_text( size = 10))
dev.off()


df <- mean_new_data
perform_kriging(df, "valence", rangemin = 0, rangemax = 1)

df <- mean_new_data
perform_kriging(df, "energy", rangemin = 0, rangemax = 1, continent="Europe")

df <- mean_new_data
perform_kriging(df, "danceability", rangemin = 0, rangemax = 1, continent="Europe")


df <- mean_new_data
k <- perform_kriging(df, "popularity", rangemin = 0, rangemax = 100)
# 6000, 400

# 50, 2000, 7
k <- as.data.frame((k))
lon_range <- c(-174, 174)
lat_range <- c(-84, 84)

png(file=paste0(PATH_PLOTS, "/popularity_interpolation.png"),
    width=1920, height=1080, units="px", res=130)
ggplot() +
  geom_tile(data = k, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(min(k$var1.pred), max(k$var1.pred)), name = paste("Predicted", "Energy")) +
  theme_minimal() +
  ggtitle(paste("Kriged", "popularity", "")) +
  coord_sf(xlim = lon_range, ylim = lat_range)+ 
  paletteer::scale_fill_paletteer_c("viridis::magma", name="Popularity")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(face="bold", size = 15),
        legend.title = element_text( size = 10))
dev.off()



world_data_new <- data.frame(country = world$iso_a2_eh, continent = world$continent)

new_get_continent <- function(nationality) {
  continent <- world_data_new$continent[match(nationality, world_data_new$country)]
  return(continent)
}

mean_new_data$continent <- sapply(mean_new_data$country, new_get_continent)
k <- perform_kriging(mean_new_data, "valence", rangemin = 0, rangemax = 1, continent="Europe")
# 2000 250 6e-04 700 5e-04 Wav

k <- as.data.frame((k))
lon_range <- c(-48, 110)
lat_range <- c(24, 84)

png(file=paste0(PATH_PLOTS, "/valence_interpolation.png"),
    width=1920, height=1080, units="px", res=130)
ggplot() +
  geom_tile(data = k, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(min(k$var1.pred), max(k$var1.pred)), name = paste("Predicted", "Energy")) +
  theme_minimal() +
  ggtitle(paste("Kriged", "valence", "")) +
  coord_sf(xlim = lon_range, ylim = lat_range)+ 
  paletteer::scale_fill_paletteer_c("viridis::magma", name="Valence")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(face="bold", size = 15),
        legend.title = element_text(size = 10))
dev.off()



# DAdes nostres ----------------------------------------------------------------
load('./7_Geoespacial/data_coordenades.RData')

# Verificar que data es un data.frame
data <- as.data.frame(data)

# Eliminar duplicados
data <- data[!duplicated(data[c("artist_name", "track_name")]), ]

world <- ne_countries(scale = "medium", returnclass = "sf")
world_data <- data.frame(country = world$admin, continent = world$continent)

get_continent <- function(nationality) {
  continent <- world_data$continent[match(nationality, world_data$country)]
  return(continent)
}

data[data$nationality=="United States",]$nationality <- "United States of America"
data[data$nationality=="PuertoRico",]$nationality <- "Puerto Rico"
# Add continent column to data
data$continent <- sapply(data$nationality, get_continent)

data$continent <- as.factor(data$continent)

# Seleccionar las columnas necesarias
data <- data[, c("artist_name", "track_name", "latitude", "continent", "longitude", "energy")]


mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data <- data %>%
  group_by(latitude, longitude) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(is.factor), mode_function),
            .groups = 'drop')

names(data)
df <- data
df$lon <- df$longitude
df$lat <- df$latitude


k <- perform_kriging(df, "energy", rangemin = 0, rangemax = 1, continent="North America")
11010

k <- perform_kriging(df, "energy", rangemin = 0, rangemax = 1, continent="Europe")
700

