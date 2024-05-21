#### ANNA CASANOVAS I ABRIL RISSO
#### ModelizaciÃ³n de Datos Geoespaciales

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

if (!require("rgdal")) install.packages("rgdal")

library(rgdal)
library("sp")
library("sf")

load('data_coordenades.RData')

###### MODELADO Datos Tipo I : GeoestadÃ­stica (Variogramas & Kriging)
#########Ejemplo 1
hist(data$artist_followers, breaks = 16) #DistribuciÃ³n no simÃ©trica, sesgada hacia la derecha
#data$logartist_followers <- log10(data$artist_followers)
#hist(data$logartist_followers, breaks = 16)

coordinates(data) <- c("latitude", "longitude")
class(data)
str(data)

#Visualizar datos. Los puntos no estÃ¡n regularmente distribuidos, 
#sino que son mas densos en la cercanÃ­a del rÃ­o.
plot(data, asp = 1, pch = 1) #asp=1 las dos escalas son iguales
#cargar otro conjunto de datos llamado meuse.riv que contiene lineas de las mÃ¡rgenes del rÃ­o.
data(meuse.riv)
lines(meuse.riv)

#Otra visualizaciÃ³n con sÃ­mbolos proporcionales
plot(meuse, asp = 1, cex = 4 * meuse$zinc/max(meuse$zinc),pch = 1) 
#cex tamaÃ±o de circulos proporcionales al valor
lines(meuse.riv)

#La dependencia espacial local significa que cuanto mas cerca estÃ©n los puntos en el espacio geogrÃ¡fico,
# mas cerca tambiÃ©n lo estÃ¡n en el espacio de atributos. Esto se llama autocorrelaciÃ³n.
#Los valores de un atributo pueden estar correlacionados con si mismos, y la fuerza de esta correlaciÃ³n
#depende de la distancia de separaciÃ³n entre puntos.
#Cada par de puntos, estarÃ¡ separado por una distancia en el espacio geogrÃ¡fico
#y una semivarianza en el espacio de atributos.

#Calcule cuÃ¡ntos pares de puntos hay en el dataset meuse.
n <- length(meuse$logZn)
n * (n - 1)/2

#calcule la distancia y semivarianza entre los dos puntos primeros puntos del dataset
coordinates(meuse)[1, ]#punto 1
coordinates(meuse)[2, ]#punto 2
sep <- dist(coordinates(meuse)[1:2, ])
sep #distancia
gamma <- 0.5 * (meuse$logZn[1] - meuse$logZn[2])^2 # semivarianza, unidades log(mg kg-1)^2
gamma

ve <- variogram(logZn ~ 1, meuse, cutoff = 1300,width = 90)
#variogram: genera la nube de variograma. logZn ~ 1: logZn es dependiente de si misma â€“> autocorrelaciÃ³n.
#Por defecto (si no se especifica cutoff y with) el cutoff es 1/3
#de la mÃ¡xima distancia (diagonal del bbox). Esto es dividido en 15
# clases igualmente espaciadas.

#Â¿CuÃ¡l es la evidencia de dependencia espacial local? np=nÃºmero de pares de puntos para cada una de las 15 clases,
# dist=distancia media, gamma=semivarianza media. A medida que la distancia aumenta,
# tambiÃ©n lo hace la semivarianza, pero hasta una distancia determinada
# donde la semivarianza se estabiliza.

plot(ve, plot.numbers = T, asp=1)
show.vgms()

#CÃ³mo escoger el ajuste para el variograma empirico?
#Ajuste visual
#Range o rango: separaciÃ³n o distancia entre pares de puntos en la cual ya no hay dependencia espacial,
## aprox 850m
#Nugget o pepita: semivarianza a la separaciÃ³n de 0m. aprox 0.01
#Total-sill o meseta: semivarianza a la distancia del rango. aprox 0.13
#Partial-sill o meseta parcial: total sill - nugget. aprox 0.12

#vgm genera el modelo de variograma

vt <- vgm(psill = 0.12, model = "Sph", range = 850,nugget = 0.01) 
vt
plot(ve, pl = T, model = vt)

#Ajuste automÃ¡tico

#fit.variogram: ajuta el modelo de variograma a un variograma empÃ­rico.
va <- fit.variogram(ve, vt) 
va
plot(ve, pl = T, model = va)
####InterpolaciÃ³n
###################     Kriging ordinario

#Usualmente kriging se utiliza para predecir los pÃ­xeles (o nodos) de una malla regular que cubre la zona
# de estudio. kriging ordinario â€œordinaryâ€ significa que (1) la variable es modelada a partir de si misma; 
#(2) la media espacial no es conocida a priori, sino estimada de los datos.

data(meuse.grid) #malla de 40m x 40m, disponible con el dataset meuse.
coordinates(meuse.grid) <- c("x", "y")
gridded(meuse.grid) <- T #indica que el conjunto de datos es un raster

ok <- krige(logZn ~ 1, locations = meuse, newdata = meuse.grid, model = va) 
ok$pred <- 10^(ok$var1.pred)#volver a valores originales
str(ok)

pts.s <- list("sp.points", meuse, col="white",pch=1, cex=4*meuse$zinc/max(meuse$zinc))
print(spplot(ok, "var1.pred", asp=1, col.regions=rev(heat.colors(50)),
             main="PredicciÃ³n OK, log-ppm Zn",sp.layout = list(pts.s)), 
      split=c(1,1,2,1), more=TRUE)
pts.s <- list("sp.points", meuse, col="black", pch=20)
print(spplot(ok, zcol="var1.var",col.regions=rev(gray(seq(0,1,.01))), asp=1,
             main="Varianza OK, log-ppm Zn^2",sp.layout = list(pts.s)), 
      split=c(2,1,2,1), more=FALSE)

###ValidaciÃ³n Modelo
ok.cv.a <- krige.cv(log10(zinc) ~ 1, locations = meuse, model = va)
print(plot(var1.pred~observed,ok.cv.a, main="OK"), split=c(3,1,2,1), more=FALSE)
cor(ok.cv.a$var1.pred,ok.cv.a$observed) ###ideal 1
mean(ok.cv.a$residual) ###ideal cercana a 0
sd(ok.cv.a$residual) ##ideal pequeÃ±a
boxplot(ok.cv.a$residual, main="Modelo en la variable llamado como OK")
# MSPE (mean square predictor error), idealmente pequeÃ±o
mean(ok.cv.a$residual^2)
#Error medio cuadrÃ¡tico (RMSE) es una medida general. Idealmente pequeÃ±o
sqrt(sum(ok.cv.a$residual^2)/length(ok.cv.a$residual))
var(ok.cv.a$residual, na.rm=T) #ideal pequeÃ±a
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