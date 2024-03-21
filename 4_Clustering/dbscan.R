# ==============================================================================
# [IDEAI] -  Advanced clustering algorithm 
# 
# Author(s):    Dante Conti and Sergi RamÃƒÂrez, IDEAI  (c)
# Date:         10th March 2023
# Description: 
#             Este script trata los algoritmos de clasificaciÃƒÂ³n por densidad 
#             DBSCAN y OPTICS
# ==============================================================================
# Cargamos las librerias necesarias
library(cluster)
library(fpc)
library(pracma)
library(factoextra)
library(dbscan)

# =============================================================================
### Generamos una semilla para poder ejecutar los datos
set.seed(04102022)

# ==============================================================================


load("./3_Preprocessing/data_knn_imputed.RData")
variables_numericas <- c("track_popularity", "album_popularity", "artist_popularity", 
                         "artist_num", "energy", "loudness", "speechiness", "acousticness", 
                         "danceability", "liveness", "valence", "tempo", "duration", "streams")
datos <- data.frame(scale(data_knn_imputed[variables_numericas]))


### Printamos la imagen que hemos obtenido de los datos a clasificar
ggplot2::ggplot(datos, aes(x = x, y = y)) + 
  ggplot2::geom_point(color='#3333FF')

# ==============================================================================
# KMEANS: 
### GrÃƒÂ¡ficamos los datos a travÃƒÂ©s de un k-means para visualizar como quedarian los 
### grupos cuando utilizamos unos algoritmos de agrupaciÃƒÂ³n a partir de la inercia
km_clusters <- kmeans(x = datos, centers = 5, nstart = 50)
fviz_cluster(object = km_clusters, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

### Como podemos ver, Kmeans ha hecho una muy mala clusterizaciÃƒÂ³n, puesto que:
###  - No ha conseguido clusterizar segÃƒÂºn las formas complejas del modelo.
###  - No ha tenido en cuenta que existen outliers, incluyÃƒÂ©ndolos en los distintos clusters

# ==============================================================================
# DBSCAN: 

### DBSCAN parte de dos parÃƒÂ¡metros que son: 
### - eps: distancia mÃƒÂ¡xima a la que debe haber otra observaciÃƒÂ³n para ser considerar 
###        que cumple con el criterio de "estar cerca"
### - minPts: parÃƒÂ¡metro que controla la densidad mÃƒÂ­nima requerida para que un punto 
###           sea considerado un nÃƒÂºcleo y se incluya en un grupo/clÃƒÂºster.

### Para un punto p, si existen al menos minPts puntos dentro del radio eps alrededor de p, 
### entonces p se considera un nÃƒÂºcleo (core point) y se incluye en el mismo grupo/clÃƒÂºster 
### que los demÃƒÂ¡s puntos dentro del radio eps. 
### Si no hay suficientes puntos dentro del radio eps, p se considera un punto frontera (border point) 
### y se incluye en el mismo grupo/clÃƒÂºster que su punto nÃƒÂºcleo mÃƒÂ¡s cercano. 
### Si no hay ningÃƒÂºn punto dentro del radio eps, p se considera un punto de ruido (noise point) 
### y no se incluye en ningÃƒÂºn grupo/clÃƒÂºster.

### Aplicamos el algoritmo de dbscan para classificar los datos
dbscan_res <- dbscan::dbscan(datos, eps = 2, minPts = 75)

### Graficamos el dbscan obtenido 
fviz_cluster(object = dbscan_res, data = datos, geom = "point", ellipse = TRUE,
             show.clust.cent = TRUE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

### Para escoger los valores de eps i minPts, necesitaremos optimizar el proceso. Para ello, 
### realizaremos la siguiente tÃƒÂ©cnica de optimizaciÃƒÂ³n. 

# ---------------------------------------------------------------------------------------------------
# CÃƒÂ¡lculo de min_pts

### El parÃƒÂ¡metro min_pts establece el nÃƒÂºmero de puntos mÃƒÂ­nimo que, dado un radio eps, tiene
### que haber para que se considere que dichos puntos forman un clÃƒÂºster.
### Un valor bajo de min_pts asegurarÃƒÂ¡ que mÃƒÂ¡s puntos son agrupados, pero se corre el riesgo de 
### agrupar outliers. Por el contrario, un valor muy alto de min_pts puede descartar valores que 
### no son anÃƒÂ³malos.

### En la literatura hablan de usar un valor entre 3 y 5 ya que funcionan bastante bien en la mayorÃƒÂ­a de los casos. min Pts igual 2 cuando tenemos una distribuciÃƒÂ³n normal y otra nube de outliers

### Para calcularlo de manera empÃƒÂ­rica, diremos que el mÃƒÂ­nimo de puntos sea igual al 0.2% - 0.25% del total de los datos teniendo en cuenta que: 

### - El minimo serÃƒÂ¡ de 2 para datos que sean muy pequeÃƒÂ±os
### - El mÃƒÂ¡ximo serÃƒÂ¡ de 10 para datos con mucha informaciÃƒÂ³n o quizÃ¡s
### - un poco mÃ¡s dependiendo del tamaÃ±o de la base de datos

#### CÃƒÂ¡lculo de min_pts
porcentaje <- 0.0025 

# CÃƒÂ¡lculo de min_pts. 
min_pts <- round(nrow(datos) * porcentaje) 
min_pts
# Realizamos los cortes de 2 y 10 que se mencionan anteriormente como validaciÃ³n
# adicional, pero lineas 98 y 99 pueden comentarse.
#min_pts <- ifelse(min_pts <= 1, 2, min_pts)
#min_pts <- ifelse(min_pts >= 10, 10, min_pts)

# ---------------------------------------------------------------------------------------------------
# NormalizaciÃƒÂ³n de los datos (SIEMPRE HAY QUE HACERLO)
### Cuando trabajamos con distÃƒÂ¡ncias es aconsejable normalizar los datos para que ningÃƒÂºno tenga un peso que no le corresponde
datos_norm <- data.frame(lapply(datos, scales::rescale))

### Como podemos ver, ahora tendremos los valores entre el intervalo [0, 1]

# -----------------------------------------------------------------------------
# Calculo de la Epsilon (eps)
### Realizamos el cÃƒÂ¡lculo de las distancias mas cercanas en una matriz de puntos
#### distanciasVecinas <- dbscan::kNNdist(datos, k = min_pts)

### Ordenamos los puntos de menos a mayor y lo guardamos en un vector.
### Cuando realicemos el grÃƒÂ¡fico elbow, serÃƒÂ¡ nuestro eje de las Y
#### Y <- distanciasVecinas[order(distanciasVecinas)]

### Calculamos el ÃƒÂ­ndice del eje de la X
#### X <- c(0:(length(Y) - 1))

### A continuaciÃƒÂ³n calculamos las pendientes
#### pendientes <- c()
#### for (i in 1:length(X)) {
####	pendientes[i] <- (Y[i + 1] - Y[i])/(X[i+1] - X[i])
#### }

#### m <- which.max(pendientes)
#### primer <- gdata::first(which(pendientes >= m))
#### epsilon <- Y[primer]
###NOTa, ejecutar lineas 131 y 132 para decidir el corte en el mÃ¡ximo
### cambio de la pendiente, como podeÃ­s apreciar ocurre alrededor de 0.15
### Esto se hace trazando una recta horizontal desde el mayor cambio y viendo
### su valor en el eje Y
### GrÃƒÂ¡ficamos los epsilons ordenados
kNNdistplot(datos_norm, k = 5, minPts = min_pts)
abline(h = 0.4, lty = 2, col = "red")

### Mirando el grÃƒÂ¡fico elbow vemos que el epsilon es 0.15
epsilon <- 0.4

# -----------------------------------------------------------------------------
### Volvemos a ejecutar el DBSCAN con los parÃƒÂ¡metros ÃƒÂ³ptimos
res <- dbscan(datos_norm, eps = epsilon, minPts = min_pts) 
### Aqui podeis graficar los resultados como se hizo en lineas precedentes
### AÃƒÂ±ado la columna clÃƒÂºster a mis datos.
datos$cluster <- res$cluster

### Guardo datos limpios.
datos_limpios <- dplyr::filter(datos, cluster != 0)

### Guardo outliers.
outliers <- dplyr::filter(datos, cluster == 0) 

### Graficamos el dbscan obtenido. Es el mismo grÃ¡fico anterior pero en PCA 
fviz_cluster(object = res, data = datos, geom = "point", ellipse = TRUE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

### Otra manera de visualizar los clusters obtenidos
hullplot(datos, res$cluster, main = paste0("Convex cluster Hulls, eps = ", epsilon))

# =============================================================================
# OPTICS

### Ejecutamos el algoritmo OPTICS con un radio de vecindad de 0.5 y un nÃƒÂºmero mÃƒÂ­nimo de puntos de 5
optics <- dbscan::optics(datos, eps = 0.5, minPts = 5)
#### optics <- optics::optics(data, eps = 0.5, minPts = 5)

# -----------------------------------------------------------------------------
### Creamos un grÃƒÂ¡fico que muestra la distancia alcanzable de cada punto
plot(optics, reachability = TRUE)

# -----------------------------------------------------------------------------
### Optimizamos la bÃƒÂºsqueda de parÃƒÂ¡metros para epsilon y minPts en Optics
library(doParallel)
library(foreach)

### Definimos los valores que se van a probar para eps y minPts para conformar un
### Grid Search
eps_values <- seq(0.1, 2.0, by = 0.2)
minPts_values <- seq(10, 40, by = 5)

### Crear una cuadrÃƒÂcula de bÃƒÂºsqueda de los valores de eps y minPts
grid <- expand.grid(eps = eps_values, minPts = minPts_values)

### Establecemos el nÃƒÂºmero de nÃƒÂºcleos que se van a usar para realizar la optimizaciÃƒÂ³n en paralelo
cores <- detectCores()
registerDoParallel(cores = cores)

### Creamos una funciÃƒÂ³n para ejecutar OPTICS con una combinaciÃƒÂ³n de parÃƒÂ¡metros y calcular el coeficiente de silueta. 
### Esta funciÃ³n puede adaptarse por si se desea aplicar una grid search similar en DBSCAN
run_optics <- function(data, eps, minPts) {
  optics <- dbscan::optics(data, eps = eps, minPts = minPts)
  res <- dbscan::extractDBSCAN(optics, eps_cl = eps)
  sil <- cluster::silhouette(res$cluster, dist(data))
  return(ifelse(is.na(sil), sil, mean(sil[, 3])))
}
### Con esta funciÃƒÂ³n nos permitirÃƒÂ¡ luego paralelizar le proceso

### Ejecutar la cuadrÃƒÂ­cula de bÃƒÂºsqueda en paralelo para la funciÃƒÂ³n dada
results <- foreach(i = 1:nrow(grid), .combine = rbind) %dopar% {
  eps <- grid$eps[i]
  minPts <- grid$minPts[i]
  score <- run_optics(datos[, -3], eps, minPts)
  c(eps, minPts, score)
}

results <- results[, c(1:3)]

### Seleccionamos la combinaciÃƒÂ³n de parÃƒÂ¡metros que produjo el mejor resultado
best_params <- grid[which.max(results[, 3]), ]
best_params
### Creamos el modelo con los mejores parÃƒÂ¡metros
optics <- dbscan::optics(datos, eps = best_params$eps, minPts = best_params$minPts)
## Aqui debeis rescatar los resultados del cluster pero debias decidir donde cortar
## el reachability plot
# -----------------------------------------------------------------------------
### Metodo de la silueta

#### Ejecutar OPTICS para diferentes valores de eps
eps_values <- seq(0.1, 1, by = 0.1)
optics_results <- lapply(eps_values, function(e) optics(datos[, -3], eps = e, minPts = 5))

#### Obtener los agrupamientos para cada valor de eps
clusters <- lapply(optics_results, function(x) extractDBSCAN(x, eps = x$eps))http://127.0.0.1:28647/graphics/6140a8ae-b6b4-4568-9dd9-5f75c657c04a.png

#### Calcular la medida de silhouette promedio para cada valor de eps
silhouette_avg <- sapply(clusters, function(x) mean(cluster::silhouette(x$cluster, dist(datos[, -3]))))

# Graficar la medida de silhouette promedio en funciÃƒÂ³n de eps
plot(eps_values, silhouette_avg, type = "b", pch = 20, main = "Silhouette Plot")

# Agregar una lÃƒÂ­nea vertical en el valor ÃƒÂ³ptimo de eps, el que maximiza la silhoutte
## o podeis escoger un valor arbitrario para cortar el reachability plot y probar
## diferentes configuraciones que os daran diferentes cantidades de clusters.
opt_eps <- eps_values[which.max(silhouette_avg)]
# Fijaros que del grÃ¡fico anterior se aconseja cortar en 0.10 (ya que maximiza)
# la silhoutte
abline(v = opt_eps, lty = 2, col = "red")


# -----------------------------------------------------------------------------------
### extract a DBSCAN clustering by cutting the reachability plot at eps_cl
### La elecciÃ³n del eps_cl puede obtenerse segun lo explicado anteriormente
### o podeis "jugar con el obtener diferentes configuraciones de cluster como
### se hace con los dendogramas de clustering jerarquico.
opt_eps
res <- dbscan::extractDBSCAN(optics, eps_cl = opt_eps)

### black is noise
plot(res)  

### Visualizamos el grÃƒÂ¡fico con los grupos creados
dbscan::hullplot(datos, res)
res$cluster
table(res$cluster)
## No olvideis guardar los resultados del clustering como se hizo con DBSCAN
### Aqui se han formado 3 clusters, uno de ellos es NOISE o posibles OUTLIERS.
### Conviene cortar el reachability plot con otros valores diferentes de 0.10
### para obtener mÃ¡s clustering. Queda de EJERCICIO para CASA.
# ==============================================================================