library(cluster)
library(fpc)
library(pracma)
library(factoextra)
library(dbscan)
library(dplyr)
# =============================================================================
### Generamos una semilla para poder ejecutar los datos
set.seed(04102022)

# ==============================================================================


load("./3_Preprocessing/data_knn_imputed.RData")
variables_numericas <- c("track_popularity", "album_popularity", "artist_popularity", 
                         "artist_num", "energy", "loudness", "speechiness", "acousticness", 
                         "danceability", "liveness", "valence", "tempo", "duration", "streams")
datos <- data[variables_numericas]

datos_norm <- data.frame(lapply(datos, scales::rescale))


### Ejecutamos el algoritmo OPTICS con un radio de vecindad de 0.5 y un nÃƒÂºmero mÃƒÂ­nimo de puntos de 5
optics <- dbscan::optics(datos_norm, eps = 0.39, minPts = 3)
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
eps_values <- seq(0.1, 1.0, by = 0.05)
minPts_values <- seq(10, 70, by = 5)

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
  score <- run_optics(datos_norm[, -3], eps, minPts)
  c(eps, minPts, score)
}

results <- results[, c(1:3)]

### Seleccionamos la combinaciÃƒÂ³n de parÃƒÂ¡metros que produjo el mejor resultado
best_params <- grid[which.max(results[, 3]), ]
best_params

print(best_params["eps"])
### Creamos el modelo con los mejores parÃƒÂ¡metros
optics <- dbscan::optics(datos_norm, eps = 0.9, minPts = 10)
## Aqui debeis rescatar los resultados del cluster pero debias decidir donde cortar

res <- dbscan::extractDBSCAN(optics, eps_cl = 0.4)
plot(res)
dbscan::hullplot(datos_norm, res)

object = list(data = datos_norm, cluster = res$cluster)

fviz_cluster(object = object, data = datos_norm, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, palette = "lancet", outlier.color = rgb(0,0,0,10, maxColorValue = 255)) +
  theme_bw() +
  theme(legend.position = "none")

length(unique(res$cluster))

ggsave("C:/Users/Usuario/Pictures/PMAAD/DBSCAN/opticsgrid.png", width=8,height=6, dpi=300)

## el reachability plot
plot(optics)
# -----------------------------------------------------------------------------
### Metodo de la silueta

#### Ejecutar OPTICS para diferentes valores de eps
eps_values <- seq(0.1, 2, by = 0.1)
optics_results <- lapply(eps_values, function(e) optics(datos_norm[, -3], eps = e, minPts = best_params$minPts))

#### Obtener los agrupamientos para cada valor de eps
clusters <- lapply(optics_results, function(x) extractDBSCAN(x, eps = x$eps))

#### Calcular la medida de silhouette promedio para cada valor de eps
silhouette_avg <- sapply(clusters, function(x) mean(cluster::silhouette(x$cluster, dist(datos_norm[, -3]))))

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
opt_eps <- 0.2
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
res <- dbscan::extractDBSCAN(optics, eps_cl = 0.4)
data$cluster <- res$cluster

### Guardo datos limpios.
datos_limpios <- dplyr::filter(data, cluster %in% c(1,2))

### Guardo outliers.
outliers <- dplyr::filter(data, !(cluster %in% c(1,2)))

table(res$cluster)

# save(data, file = "./4_Preprocessing/optics_clustering.RData")