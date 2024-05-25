######################################
## MODELADO CON DATOS GEOESPACIALES ##
######################################

# Descarregar paquets necessàris

list.of.packages = c("geoR", "sm", "sp", "gstat", "npsp", "geohashTools",
                     "rgdal", "ggmap", "ggplot2", "dplyr", "gridExtra", "maps", 
                     "rnaturalearth", "rnaturalearthdata", "osmdata") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

install.packages("rgdal")
library("rgdal")
library("sp")
library("sf")

library(sf)
library(ggplot2)
library(dplyr)

# Carreguem el dataset amb les coordenades (latitude i longitude)

load('./7_Geoespacial/data_coordenades.RData')
View(data)
# Treiem les files amb (artistes, cançons) repetides

data <- data %>%
  distinct(artist_name, .keep_all = TRUE)
View(data)

# 1. Eliminar duplicados para cada combinación única de artist_name y year_week
data <- data %>%
  distinct(artist_name, year_week, .keep_all = TRUE)

# 2. Calcular la media de artist_followers para cada artist_name
data <- data %>%
  group_by(artist_name, latitude, longitude) %>%
  summarize(artist_followers = mean(artist_followers, na.rm = TRUE))

###### MODELADO Datos Tipo I : Geoestadística (Variogramas & Kriging)

# MAPA AMB ELS PUNTS DE LES DADES DEL DATASET

world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")

data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

ggplot() +
  geom_sf(data = world_cities, fill = "white", color = "black") + 
  geom_sf(data = data_sf, color = "darkgreen", size = 1) +  
  theme_minimal() +
  labs(title = "Mapa amb els punts de dades")


#################
#  VARIOGRAMA  #
#################

hist(data$artist_followers, breaks = 16) 
data$logartist_followers <- log10(data$artist_followers +1)
hist(data$logartist_followers, breaks = 16)

# Quantitat de parelles de punts
n <- length(data$logartist_followers)
n * (n - 1)/2

coordinates(data) <- ~longitude + latitude
head(coordinates(data))

# Coordenades dels dos primers punts
coord1 <- coordinates(data)[1, ]
coord2 <- coordinates(data)[2, ]

# Calcular la distancia entre els dos primers punts
sep <- dist(rbind(coord1, coord2))
print(sep) # distancia

# Calcular la semivariança entre els valores d'artist_followers
gamma <- 0.5 * (data$logartist_followers[1] - data$logartist_followers[2])^2
print(gamma)

# Distancies entre els punts de la nostra base de dades
distances <- spDists(as.matrix(coordinates(data)), longlat = TRUE)
hist(distances, breaks = 50, main = "Histogram of Distances", xlab = "Distance (km)")


### TRIEM ELS PARÀMETRES CUTOFF I WIDTH

# CUTOFF =  distancia máxima que se considera al calcular el variograma
# WIDTH = mida intervals en els que s'agrupen les parelles de punts

ve <- variogram(logartist_followers ~ 1, data, cutoff = 5000, width=35)
print(ve)
plot(ve)

plot(ve, plot.numbers = T, asp=1)
show.vgms()

# Ajustar diversos models de variograma

#psill = sill - nugget = 1 - 0.5

# Esfèric
model_sph <- vgm(psill = 0.6, model = "Sph", range = 350, nugget = 0.4)

# Exponencial
model_exp <- vgm(psill = 0.6, model = "Exp", range = 350, nugget = 0.4)

# Gaussià
model_gau <- vgm(psill = 0.6, model = "Gau", range = 350, nugget = 0.4)

# Graficar el variograma empíric amb els models ajustats
plot(ve, model = model_sph, main = "Ajust del Model Esfèric")
plot(ve, model = model_exp, main = "Ajust del Model Exponencial")
plot(ve, model = model_gau, main = "Ajust del Model Gaussià")

#Ajust automàtic
#fit.variogram: ajusta el modelo de variograma a un variograma empírico.

va <- fit.variogram(ve, vt) 
va
plot(ve, pl = T, model = va)


##############################
#  INTERPOLACIÓ AMB KRIGING  #
##############################

# Predir valors en ubicacions no mostrejades basant-se en ubicacions mostrejades

# Cargar los paquetes necesarios
install.packages("sp")
install.packages("gstat")


# Crear una malla per a la interpolació

long_range <- range(data$longitude, na.rm = TRUE)
lat_range <- range(data$latitude, na.rm = TRUE)
resolution <- 1

long_seq <- seq(long_range[1], long_range[2], by = resolution)
lat_seq <- seq(lat_range[1], lat_range[2], by = resolution)
grid <- expand.grid(longitude = long_seq, latitude = lat_seq)

# Verificar la malla
print(head(grid))
print(dim(grid))

coordinates(grid) <- ~longitude + latitude
gridded(grid) <- TRUE

print(str(grid))

num_punts <- nrow(as.data.frame(grid))
cat("Número de puntos en la malla:", num_punts, "\n")


# Calcular el semivariograma y ajustar un modelo (ja fet prèviament)

ve <- variogram(logartist_followers ~ 1, data, cutoff = 1300, width=35)
model_exp <- vgm(psill = 0.6, model = "Exp", range = 350, nugget = 0.4)
plot(ve, model = model_exp)


# Realitzar kriging ordinari amb el model ajustat

#ok <- krige(artist_followers ~ 1, locations = data, newdata = grid, model = model_exp)
#ok$pred <- ok$var1.pred # Asumiendo que no necesitas transformar de nuevo
#str(ok)

ok <- krige(logartist_followers ~ 1, locations = data, newdata = grid, model = model_exp)
ok$pred <- 10^(ok$var1.pred) 
str(ok)

# Visualització
pts.s <- list("sp.points", data, col = "white", pch = 1, cex = 4 * data$logartist_followers / max(data$logartist_followers))
print(spplot(ok, "var1.pred", asp = 1, col.regions = rev(heat.colors(50)),
             main = "Predicció OK, log-Artist Followers", sp.layout = list(pts.s)), 
      split = c(1, 1, 2, 1), more = TRUE)
pts.s <- list("sp.points", data, col = "black", pch = 20)
print(spplot(ok, zcol = "var1.var", col.regions = rev(gray(seq(0, 1, .01))), asp = 1,
             main = "Variança OK, log-Artist Followers^2", sp.layout = list(pts.s)), 
      split = c(2, 1, 2, 1), more = FALSE)


# Validació del Model (Opcional)
ok.cv.a <- krige.cv(logartist_followers ~ 1, locations = data, model = model_exp)
cor(ok.cv.a$var1.pred, ok.cv.a$observed)  # Correlació idealment propera a 1
mean(ok.cv.a$residual)  # Promig dels residus idealment proper a 0
sd(ok.cv.a$residual)  # Desviació estàndard dels residus idealment petita
boxplot(ok.cv.a$residual, main = "Model en la variable cridat OK")
mean(ok.cv.a$residual^2)  # MSPE (error quadràtic mitjà predictor), idealment petit
sqrt(mean(ok.cv.a$residual^2))  # RMSE (error quadràtic mitjà arrel), idealment petit
var(ok.cv.a$residual, na.rm = TRUE)  # Variància dels residus, idealment petita


# MSPE (mean square predictor error), idealmente pequeño
print(mean(ok.cv.a$residual^2))

# Error medio cuadrático (RMSE) es una medida general. Idealmente pequeño
print(sqrt(sum(ok.cv.a$residual^2) / length(ok.cv.a$residual)))
print(var(ok.cv.a$residual, na.rm = TRUE)) # Idealmente pequeño








longitudes <- seq(from = -180, to = 180, by = 5)  # Longitudes de -180 a 180 grados
latitudes <- seq(from = -90, to = 90, by = 5)     # Latitudes de -90 a 90 grados

# Crear una malla combinando todas las combinaciones de coordenadas
malla_mundo <- expand.grid(Longitude = longitudes, Latitude = latitudes)

# Convertir la malla en un objeto SpatialPointsDataFrame
coords <- cbind(malla_mundo$Longitude, malla_mundo$Latitude)
malla_sp <- SpatialPointsDataFrame(coords, data = malla_mundo)

# Verificar la estructura de la malla
print(head(malla_sp))
print(dim(malla_sp))

# Número de puntos en la malla
num_punts <- nrow(malla_sp)
cat("Número de puntos en la malla:", num_punts, "\n")

# Calcular el semivariograma y ajustar un modelo (ya hecho previamente)
ve <- variogram(logartist_followers ~ 1, data, cutoff = 1300, width = 35)
model_exp <- vgm(psill = 0.6, model = "Exp", range = 350, nugget = 0.4)
plot(ve, model = model_exp)

# Realizar kriging ordinario con la malla de resolución ajustada
ok <- krige(logartist_followers ~ 1, locations = data, newdata = malla_sp, model = model_exp)
ok$pred <- 10^(ok$var1.pred)  # Volver a valores originales después del kriging log-transformación
print(str(ok))










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