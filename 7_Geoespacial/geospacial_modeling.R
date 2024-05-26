######################################
## MODELADO CON DATOS GEOESPACIALES ##
######################################

# Descargar paquetes necesarios
list.of.packages <- c("geoR", "sm", "sp", "gstat", "npsp", "geohashTools",
                      "rgdal", "ggmap", "ggplot2", "dplyr", "gridExtra", "maps", 
                      "rnaturalearth", "rnaturalearthdata", "osmdata", "sf", "raster", "dplyr") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

# Asegurarnos de cargar dplyr
library(dplyr)

# Cargar el dataset con coordenadas
load('./7_Geoespacial/data_coordenades.RData')

# Verificar que data es un data.frame
data <- as.data.frame(data)

# Eliminar duplicados
data <- data[!duplicated(data[c("artist_name", "track_name")]), ]

# Seleccionar las columnas necesarias
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

###### MODELADO Datos Tipo I : Geoestadística (Variogramas & Kriging)

# SHAPEFILE

world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")

plot(world_cities$geometry)
points(data, col = 'red', pch = 20)

#################
#  VARIOGRAMA  #
#################

hist(data$energy, breaks = 16) 

# Convertir datos a SpatialPointsDataFrame
coordinates(data) <- ~longitude + latitude
proj4string(data) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Crear el variograma experimental
ve <- variogram(energy ~ 1, data, cutoff = 10000, width = 500)
print(ve)
plot(ve, main = "Variograma Experimental de Energy")

# Ajustar varios modelos de variograma
model_sph <- vgm(psill = 0.01, model = "Sph", range = 5000, nugget = 0.02)
model_exp <- vgm(psill = 0.01, model = "Exp", range = 5000, nugget = 0.02)
model_gau <- vgm(psill = 0.01, model = "Gau", range = 5000, nugget = 0.02)

# Ajustar los modelos
va_sph <- fit.variogram(ve, model_sph)
va_exp <- fit.variogram(ve, model_exp)
va_gau <- fit.variogram(ve, model_gau)

# Imprimir los modelos ajustados
print(va_sph)
print(va_exp)
print(va_gau)

# Graficar los modelos ajustados
plot(ve, model = va_sph, main = "Ajust del Model Esfèric")
plot(ve, model = va_exp, main = "Ajust del Model Exponencial")
plot(ve, model = va_gau, main = "Ajust del Model Gaussià")


#########################
#  VALIDACIÓN DEL MODELO  #
#########################

# Realizar validación cruzada con leave-one-out (LOOCV)
kriging_cv <- krige.cv(energy ~ 1, data, model = va_exp, nfold = nrow(data), verbose = FALSE)

# Calcular métricas de desempeño
me <- mean(kriging_cv$residual)  # Error medio
rmse <- sqrt(mean(kriging_cv$residual^2))  # Raíz del error cuadrático medio
msre <- mean(kriging_cv$zscore^2)  # Error cuadrático medio de los z-scores

cat("Error medio (ME):", me, "\n")
cat("Raíz del error cuadrático medio (RMSE):", rmse, "\n")
cat("Error cuadrático medio de los z-scores (MSRE):", msre, "\n")

##############################
#  INTERPOLACIÓN CON KRIGING  #
##############################

# Crear una cuadrícula para la interpolación usando la extensión del shapefile
extent_shape <- st_bbox(world_cities)
r <- raster(xmn = extent_shape["xmin"], xmx = extent_shape["xmax"], 
            ymn = extent_shape["ymin"], ymx = extent_shape["ymax"], 
            ncol = 100, nrow = 100)  # Ajusta la resolución según sea necesario
values(r) <- 1:ncell(r)  # Asignar valores al raster
grid <- as(r, "SpatialPixelsDataFrame")
proj4string(grid) <- CRS(proj4string(data))

# Verificar la superposición de la cuadrícula con los datos y el shapefile
plot(grid, main = "Cuadrícula y Datos")
plot(st_geometry(world_cities), add = TRUE)
points(data, col = 'red', pch = 20)

# Realizar la interpolación Kriging con nmax para limitar el número de vecinos
ok <- krige(energy ~ 1, locations = data, newdata = grid, model = va_sph, nmax = 30)

# Convertir el resultado de kriging a raster para aplicar la máscara
ok_raster <- raster(ok, layer = "var1.pred")

# Aplicar la máscara del shapefile
masked_ok <- mask(ok_raster, st_as_sf(world_cities))

# Convertir a data.frame para ggplot2
ok_df <- as.data.frame(masked_ok, xy = TRUE)
names(ok_df) <- c("x", "y", "var1.pred")

# Convertir a SpatialPixelsDataFrame para spplot
masked_ok_sp <- as(masked_ok, "SpatialPixelsDataFrame")

# Crear puntos para superponer en el gráfico
pts.s <- list("sp.points", data, col = "white", pch = 1, cex = 4 * data$energy / max(data$energy))

# Visualizar los resultados de la interpolación con spplot
spplot(masked_ok_sp, "var1.pred", asp = 1, col.regions = rev(heat.colors(50)),
       main = "Interpolación Kriging de Energy", sp.layout = list(pts.s, 
                                                                  list("sp.polygons", as(world_cities, "Spatial"), col = "black")))

