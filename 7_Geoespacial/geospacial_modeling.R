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
ok <- krige(energy ~ 1, locations = data, newdata = grid, model = va_exp, nmax = 30)

# Verificar los resultados
summary(ok)

# Convertir a data.frame para ggplot2
ok_df <- as.data.frame(ok)
head(ok_df)

# Crear puntos para superponer en el gráfico
pts.s <- list("sp.points", data, col = "white", pch = 1, cex = 4 * data$energy / max(data$energy))

# Visualizar los resultados de la interpolación
spplot(ok, "var1.pred", asp = 1, col.regions = rev(heat.colors(50)),
       main = "Interpolación Kriging de Energy", sp.layout = list(pts.s))

# Visualización detallada con ggplot2
ggplot(ok_df, aes(x = x1, y = x2, fill = var1.pred)) +
  geom_tile() +
  scale_fill_gradientn(colors = rev(heat.colors(50))) +
  geom_point(data = as.data.frame(data), aes(x = longitude, y = latitude, size = energy), color = "white") +
  labs(title = "Interpolación Kriging de Energy",
       x = "Longitude", y = "Latitude", fill = "Predicción") +
  theme_minimal()










######################
##EJEMPLO 2 - Variogramas & Kriging con Library(geoR)
###########################
# data()                    # lista todos los conjuntos de datos disponibles
# data(package = "geoR")    # lista los conjuntos de datos en el paquete geoR

data(wolfcamp)              # carga el archivo de datos wolfcamp
help(wolfcamp)
summary(wolfcamp)


#Se pueden importar directamente un archivo de datos en formato texto:
#ncep <- read.geodata('ncep.txt', header = FALSE, coords.col = 1:2, data.col = 4)
# plot(ncep)
# summary(ncep)

#TambiÃ©n se puede convertir un data.frame a un objeto geodata:

#ncep.df <- read.table('ncep.txt', header = FALSE)
# names(ncep.df) <- c('x', 'y', 't', 'z')
# str(ncep.df)
# Nota: los datos son espacio-temporales, pero geoR sÃ³lo admite datos 2D
#datgeo <- as.geodata(ncep.df, coords.col = 1:2, data.col = 4)
# plot(datgeo)
# summary(datgeo)

###Data Analysis
head(wolfcamp)
plot(wolfcamp)

#Los grÃ¡ficos de dispersiÃ³n de los datos frente a las coordenadas nos pueden
# ayudar a determinar si hay una tendencia. TambiÃ©n, en lugar del histograma,
# nos puede interesar un grÃ¡fico de dispersiÃ³n 3D

plot(wolfcamp, lowess = TRUE, scatter3d = TRUE)

#Si se asume que hay una tendencia puede interesar eliminarla:
plot(wolfcamp, trend=~coords) 

points(wolfcamp)
points(wolfcamp, col = "gray", pt.divide = "equal")
#pt.divide = c("data.proportional", "rank.proportional", "quintiles", "quartiles", "deciles", "equal")

####Modelado de la dependencia (Nuevos DATOS ejemplo 3)

##En la primera parte de esta secciÃ³n consideraremos un proceso espacial
## sin tendencia:
data(s100) # Cargar datos estacionarios
summary(s100)
plot(s100)
###  Semiovariogramas Empiricos
oldpar <- par(mfrow=c(1,2)) 
plot(variog(s100))
plot(variog(s100, max.dist = 0.6))
par(oldpar)
varior.b <- variog(s100, estimator.type = "modulus", max.dist=0.6)
vario.60 <- variog(s100, max.dist = 0.6, direction = pi/3) #variograma en la direcciÃ³n de 60 grados
vario.4 <- variog4(s100, max.dist = 0.6)
oldpar <- par(mfrow=c(1,1)) 
plot(vario.60)
title(main = expression(paste("direccional, angulo = ", 60 * degree)))
plot(vario.4, lwd = 1)
par(oldpar)
###Ajuste de un modelo de variograma
vario.b <- variog(s100, max.dist=0.6) #discretizado
vario.s <- variog(s100, max.dist=0.6,option = "smooth", kernel = "normal", band = 0.2)  #suavizado
plot(vario.b)
lines(vario.s, type = "l", lty = 2)
lines.variomodel(cov.model = "exp", cov.pars = c(1,0.3), nugget = 0, max.dist = 0.6, lwd = 3)
legend(0.3, 0.3, c("empirico", "suavizado", "modelo exponencial"), lty = c(1, 2, 1), lwd = c(1, 1, 3))
###More options
plot(vario.b)
lines.variomodel(cov.model = "exp", cov.pars = c(0.9,0.3), nug = 0.1, max.dist = 0.6)
lines.variomodel(cov.model = "mat", cov.pars = c(0.85,0.2), nug = 0.1, kappa = 1, max.dist = 0.6,lty = 2)
lines.variomodel(cov.model = "sph", cov.pars = c(0.8,0.8), nug = 0.1, max.dist = 0.6, lwd = 2)

#Cuando se utilizan las funciones variofit y likfit para la estimaciÃ³n de parÃ¡metros, el efecto pepita (nugget) puede ser estimado o establecido a un valor fijo. Lo mismo ocurre con los parÃ¡metros de suavidad, anisotropÃ­a y transformaciÃ³n de los datos. TambiÃ©n se dispone de opciones para incluir una tendencia. Las tendencias pueden ser polinomios en funciÃ³n de las coordenadas y/o funciones lineales de otras covariables.
vario.ols <- variofit(vario.b, ini = c(1, 0.5), weights = "equal")  #ordinarios
vario.wls <- variofit(vario.b, ini = c(1, 0.5), weights = "cressie")  #ponderados
vario.wls
summary(vario.wls)
vario.ml <- likfit(s100, ini = c(1, 0.5)) #Modelo exponencial con par ini umbral y escala (1/3 rango)
vario.reml <- likfit(s100, ini = c(1, 0.5), lik.method = "RML")
plot(vario.b, main = "Estimador empÃ­rico y modelos ajustados")
lines(vario.ml, max.dist = 0.6)
lines(vario.reml, lwd = 2, max.dist = 0.6)
lines(vario.ols, lty = 2, max.dist = 0.6)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 0.6)
legend(0.3, 0.3, legend = c("ML", "REML", "OLS", "WLS"), lty = c(1, 1, 2, 2), lwd = c(0.5, 0,5,0.5,0.5)) 


#####Para estudiar si hay una dependencia espacial â€œsignificativaâ€ se puede emplear tambiÃ©n la rutina sm.variogram del paquete sm. Estableciendo model = "independent" devuelve un p-valor para contrastar la hipÃ³tesis nula de independencia (i.e. se acepta que hay una dependencia espacial si pâ‰¤Î±=0.05) y un grÃ¡fico en el que se muestra el estimador empÃ­rico robusto, un estimador suavizado y una regiÃ³n de confianza
## para el variograma suponiendo que el proceso es independiente (i.e. considerarÃ­amos que hay dependencia espacial
## si el variograma suavizado no estÃ¡ contenido en esa regiÃ³n).
sm.variogram(s100$coords, s100$data, model = "independent")
### Revise el parÃ¡metro model ya que el comando anterior permite ver si un proceso es estacionario tambiÃ©n.
###ValidaciÃ³n
xv.wls <- xvalid(s100, model = vario.wls)
summary(xv.wls)
xv.reml <- xvalid(s100, model = vario.reml)
summary(xv.reml)
plot(xv.wls, ask = FALSE)

###EstimaciÃ³n del variograma en procesos no estacionarios

#Cuando el proceso no es estacionario (no se puede emplear directamente los estimadores empÃ­ricos)
# hay que eliminar la tendencia para estimar el variograma:

plot(variog(wolfcamp, max.dist = 200)) # Supone que el proceso es estacionario
plot(variog(wolfcamp, trend = ~coords, max.dist = 200)) # Asume una tendencia lineal en las coordenadas

#PredicciÃ³n espacial (kriging)

#El paquete geoR dispone de opciones para los mÃ©todos kriging
#tradicionales, que dependiendo de las suposiciones acerca de la funciÃ³n de tendencia
#se clasifican en:
#   Kriging simple (KS): media conocida
#
#   Kriging ordinario (KO): se supone que la media es constante y desconocida.

#   Kriging universal (KU): tambiÃ©n denominado kriging con modelo de tendencia, se supone que la media es una combinaciÃ³n lineal (desconocida) de las coordenadas o de otras variables explicativas.

# Rejilla regular 51x51 en cuadrado unidad
xx <- seq(0, 1, l = 51)
yy <- seq(0, 1, l = 51)
pred.grid <- expand.grid(x = xx, y = yy) 
plot(s100$coords, pch = 20)
points(pred.grid, pch = 3, cex = 0.2)

#Kriging ordinario
ko.wls <- krige.conv(s100, loc = pred.grid, krige = krige.control(obj.m = vario.wls))
names(ko.wls)
image(ko.wls) #superficie de predicciÃ³n
title("Predicciones")
points(s100$coords, pch=20) #aÃ±adir posiciones datos
contour(ko.wls,add=T) #aÃ±adir grÃ¡fico de contorno

image(ko.wls, val = ko.wls$krige.var) #superficie de varianzas
title("Superficie de varianzas")
points(s100$coords, pch=20)
contour(ko.wls,val=sqrt(ko.wls$krige.var),add=T)

contour(ko.wls,filled = TRUE)
fcol <- topo.colors(10)[cut(matrix(ko.wls$pred,nrow=51,ncol=51)[-1,-1],10,include.lowest=TRUE)]
persp(ko.wls, theta=-60, phi=40, col=fcol)

if(!require(plot3D)) 
  stop('Required pakage `plot3D` not installed.') # install.packages('plot3D')

# Loading required package: plot3D

persp3D(xx, yy, matrix(ko.wls$predict, nrow = length(xx)), theta=-60, phi=40)
spersp(xx, yy, ko.wls$predict, theta=-60, phi=40)


###### MODELADO Datos Tipo II : Procesos Puntuales
######### Se aconsejan estos links
#Points/Punctual Process in R--LIBRARY(spatstat)
#############  https://spatstat.org/
#https://kevintshoemaker.github.io/NRES-746/sppm.html
#https://cran.r-project.org/web/packages/pointdensityP/pointdensityP.pdf





#### EXAMPLE with a transactional report of crimes in USA
### MODELLING DENSITY and INTENSITY


# Crear un objeto sf para tus datos
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# Crear el mapa de fondo y añadir tus datos
ggplot() +
  geom_sf(data = world_cities, fill = "white", color = "black") +  # Dibuja el shapefile
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level..), 
                 alpha = .5, geom = "polygon", data = data) + 
  scale_fill_viridis_c() + 
  coord_sf() + 
  theme_minimal() +
  labs(title = "Mapa con Puntos de Datos", x = "Longitude", y = "Latitude") +
  theme(legend.position = 'none')

library(ggplot2)
head(data)
# treure NA
ggplot(data, aes(x = longitude, y = latitude)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = data) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')


# plot a ggmap basemap
## us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
## map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite",legend="none")
## plot(map)
## scatterplot_murder <- qmplot(x=lon,y=lat,data=filter(crime,offense=="murder"),legend="none",color=I("darkred"))
## plot(scatterplot_murder)
install.packages("ggplot2")
install.packages("sf")
install.packages("viridis")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(ggplot2)
library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

data(crime)
us <- map_data("state")
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
####
#### Repeat the analysis by putting a third dimension --> to use date data
#### to filter temporal patterns and compare month by month in order to
#### find out if there are relationships between time and space.

###Proceed with Google maps if you want to improve the visualizations
# remove any rows with missing data
crime <- crime[complete.cases(crime), ]
# look at the structure of the crime data
str(crime)
# load a basemap
basemap <- get_map(location = "Houston, TX", zoom = 9)
#Register to Google API
# and use same code and also examples in https://cran.r-project.org/web/packages/pointdensityP/pointdensityP.pdf

####################################################