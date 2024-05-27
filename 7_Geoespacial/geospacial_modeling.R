######################################
## MODELADO CON DATOS GEOESPACIALES ##
######################################

# Descarregar paquets necessaris

list.of.packages <- c("geoR", "sm", "sp", "gstat", "npsp", "geohashTools",
                      "rgdal", "ggmap", "ggplot2", "dplyr", "gridExtra", "maps", 
                      "rnaturalearth", "rnaturalearthdata", "osmdata", "sf", "raster", "dplyr") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

library(paletteer)
library(dplyr)

# Carregar el dataset amb les coordenades

load('./7_Geoespacial/data_coordenades.RData')

data <- as.data.frame(data)

data <- data[!duplicated(data[c("artist_name", "track_name")]), ]

data <- data[, c("artist_name", "track_name", "latitude", "longitude", "energy")]

mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data <- data %>%
  group_by(latitude, longitude) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(is.factor), mode_function),
            .groups = 'drop')

###### MODELAT Dades Tipus I : Geoestadística (Variogrames & Kriging)

# SHAPEFILE

world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")

plot(world_cities$geometry)
points(data, col = 'red', pch = 20)

#################
#  VARIOGRAMA  #
#################

hist(data$energy, breaks = 16) 

# Convertir dades a SpatialPointsDataFrame
coordinates(data) <- ~longitude + latitude
proj4string(data) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Crear el variograma experimental
ve <- variogram(energy ~ 1, data, cutoff = 10000, width = 500)
print(ve)
plot(ve, main = "Variograma Experimental de Energy")

# Ajustar diversos models de variograma
model_sph <- vgm(psill = 0.01, model = "Sph", range = 5000, nugget = 0.02)
model_exp <- vgm(psill = 0.01, model = "Exp", range = 5000, nugget = 0.02)
model_gau <- vgm(psill = 0.01, model = "Gau", range = 5000, nugget = 0.02)

va_sph <- fit.variogram(ve, model_sph)
va_exp <- fit.variogram(ve, model_exp)
va_gau <- fit.variogram(ve, model_gau)

print(va_sph)
print(va_exp)
print(va_gau)

plot(ve, model = va_sph, main = "Ajust del Model Esfèric")
plot(ve, model = va_exp, main = "Ajust del Model Exponencial")
plot(ve, model = va_gau, main = "Ajust del Model Gaussià")


#########################
#  VALIDACIÓ DEL MODEL  #
#########################

# Realitzar validació creuada amb leave-one-out (LOOCV)
kriging_cv <- krige.cv(energy ~ 1, data, model = va_exp, nfold = nrow(data), verbose = FALSE)

me <- mean(kriging_cv$residual)  # Error medio
rmse <- sqrt(mean(kriging_cv$residual^2))  # Raíz del error cuadrático medio
msre <- mean(kriging_cv$zscore^2)  # Error cuadrático medio de los z-scores

cat("Error medio (ME):", me, "\n")
cat("Raíz del error cuadrático medio (RMSE):", rmse, "\n")
cat("Error cuadrático medio de los z-scores (MSRE):", msre, "\n")


##############################
#  INTERPOLACIÓ AMB KRIGING  #
##############################

world <- ne_countries(scale = "medium", returnclass = "sf")

lon_range <- c(-174, 174)
lat_range <- c(-84, 84)
resolution <- 0.5  # Ajustar resolución según sea necesario

lon_grid <- seq(lon_range[1], lon_range[2], by = resolution)
lat_grid <- seq(lat_range[1], lat_range[2], by = resolution)

grid <- expand.grid(lon = lon_grid, lat = lat_grid)
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
grid_land <- st_join(grid_sf, world, join = st_within)

grid_land <- grid_land[!is.na(grid_land$admin), ]

grid_land_sp <- as(grid_land, "Spatial")
gridded(grid_land_sp) <- TRUE

grid <- grid_land_sp

gridded(grid) <- TRUE
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

formula <- energy ~ 1
df <- data
final_model <- va_sph
kriged <- krige(formula, locations = df, newdata = grid, model = final_model)

rangemin <- min(kriged$var1.pred, na.rm = TRUE)
rangemax <- max(kriged$var1.pred, na.rm = TRUE)
kriged$var1.pred <- pmax(pmin(kriged$var1.pred, rangemax), rangemin)

kriged_df <- as.data.frame(kriged)

df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

variable_name <- "energy"

ggplot() +
  geom_tile(data = kriged_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(rangemin, rangemax), name = paste("Predicted", variable_name)) +
  theme_minimal() +
  ggtitle(paste("Interpolació Kriging d'Energy")) +
  coord_sf(xlim = lon_range, ylim = lat_range) +
  paletteer::scale_fill_paletteer_c("viridis::magma", name="Energy") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        title = element_text(face = "bold", size = 15),
        legend.title = element_text(size = 10))
