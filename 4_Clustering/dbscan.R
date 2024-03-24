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

pca <- prcomp(datos_norm, scale = TRUE)
iden = 0
varianza <- pca$sdev^2
total_varianza <- sum(varianza)
perc_varianza <- 100*varianza/total_varianza
perc_var1 <- round(perc_varianza[1], 2)
perc_var2 <- round(perc_varianza[2], 2)
label_x <- paste("1r component (", perc_var1, "%)")
label_y <- paste("2n component (", perc_var2, "%)")

df_psi <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], color = iden)
### Printamos la imagen que hemos obtenido de los datos a clasificar
p <- ggplot(df_psi, aes(x = PC1, y = PC2)) +
  geom_vline(aes(xintercept = 0), color = "black", linewidth = 0.75) +
  geom_hline(aes(yintercept = 0), color = "black", linewidth = 0.75) +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_line(linetype = "dashed", color = rgb(0, 0, 0, 100, maxColorValue = 255)),
        legend.position = "none") +
  geom_point(aes(color =color), alpha = 0.35) +
  labs(x = label_x,y = label_y,
       title = "Projecció de totes les dades sobre els 2 primers Components Principals")
print(p)
ggsave("C:/Users/Usuario/Pictures/PMAAD/DBSCAN/dbscanpca.png", width=8,height=6, dpi=300)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

ggsave("C:/Users/Usuario/Pictures/PMAAD/DBSCAN/dbscanpcacomponents.png", width=8,height=6, dpi=300)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(5,6),
             repel = TRUE     # Avoid text overlapping
)
ggsave("C:/Users/Usuario/Pictures/PMAAD/DBSCAN/dbscanpcacomponents34.png", width=8,height=6, dpi=300)
# ==============================================================================
# KMEANS: 
### GrÃƒÂ¡ficamos los datos a travÃƒÂ©s de un k-means para visualizar como quedarian los 
### grupos cuando utilizamos unos algoritmos de agrupaciÃƒÂ³n a partir de la inercia
km_clusters <- kmeans(x = datos_norm, centers = 5, nstart = 50)
fviz_cluster(object = km_clusters, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")+
  ggtitle("K-MEANS cluster")

ggsave("C:/Users/Usuario/Pictures/PMAAD/DBSCAN/kmeans.png", width=8,height=6, dpi=300)

km_clusters <- kmeans(x = datos_norm, centers = 5, nstart = 50)
fviz_cluster(object = km_clusters, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco", axes  =  c ( 3 , 4 )) +
  theme_bw() +
  theme(legend.position = "none")+
  ggtitle("K-MEANS cluster")

ggsave("C:/Users/Usuario/Pictures/PMAAD/DBSCAN/kmeans34.png", width=8,height=6, dpi=300)

#data$cluster <- km_clusters$cluster

# save(data, file = "./4_Preprocessing/kmeansclustering.RData")

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
dbscan_res <- dbscan::dbscan(datos_norm, eps = 0.15, minPts = 20)

### Graficamos el dbscan obtenido 
fviz_cluster(object = dbscan_res, data = datos_norm, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco", outlier.color = rgb(0,0,0,10, maxColorValue = 255)) +
  theme_bw() +
  theme(legend.position = "none")+
  ggtitle("DBSCAN cluster")

length(unique(dbscan_res$cluster))

ggsave("C:/Users/Usuario/Pictures/PMAAD/DBSCAN/baddbscan.png", width=8,height=6, dpi=300)

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
distanciasVecinas <- dbscan::kNNdist(datos_norm, k = min_pts)

### Ordenamos los puntos de menos a mayor y lo guardamos en un vector.
### Cuando realicemos el grÃƒÂ¡fico elbow, serÃƒÂ¡ nuestro eje de las Y
Y <- distanciasVecinas[order(distanciasVecinas)]

### Calculamos el ÃƒÂ­ndice del eje de la X
X <- c(0:(length(Y) - 1))

### A continuaciÃƒÂ³n calculamos las pendientes
slopes <- diff(Y) / diff(X)

# Find the point with the maximum difference in slope
max_diff_index <- which.max(diff(slopes))

primer <- gdata::first(which(slopes >= slopes[max_diff_index]))
epsilon <- Y[primer]
###NOTa, ejecutar lineas 131 y 132 para decidir el corte en el mÃ¡ximo
### cambio de la pendiente, como podeÃ­s apreciar ocurre alrededor de 0.15
### Esto se hace trazando una recta horizontal desde el mayor cambio y viendo
### su valor en el eje Y
### GrÃƒÂ¡ficamos los epsilons ordenados
epsilon

kNNdistplot(datos_norm, k = 22, minPts = min_pts)
abline(h = epsilon, lty = 2, col = "blue")
abline(h = 0.55, lty = 2, col = "red")
### Mirando el grÃƒÂ¡fico elbow vemos que el epsilon es 0.15
epsilon <- 0.55

# -----------------------------------------------------------------------------
### Volvemos a ejecutar el DBSCAN con los parÃƒÂ¡metros ÃƒÂ³ptimos
res <- dbscan(datos_norm, eps = epsilon, minPts = min_pts) 
### Aqui podeis graficar los resultados como se hizo en lineas precedentes

### Graficamos el dbscan obtenido 
fviz_cluster(object = res, data = datos_norm, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco", outlier.color = rgb(0,0,0,10, maxColorValue = 255)) +
  theme_bw() +
  theme(legend.position = "none")+
  ggtitle("DBSCAN cluster")

length(unique(res$cluster))

ggsave("C:/Users/Usuario/Pictures/PMAAD/DBSCAN/dbscanauto.png", width=8,height=6, dpi=300)

res <- dbscan(datos_norm, eps = 0.47, minPts = 70) 
### Aqui podeis graficar los resultados como se hizo en lineas precedentes

### Graficamos el dbscan obtenido 
fviz_cluster(object = res, data = datos_norm, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco", outlier.color = rgb(0,0,0,10, maxColorValue = 255)) +
  theme_bw() +
  theme(legend.position = "none")+
  ggtitle("DBSCAN cluster")

length(unique(res$cluster))

ggsave("C:/Users/Usuario/Pictures/PMAAD/DBSCAN/dbscanforca.png", width=8,height=6, dpi=300)

fviz_cluster(object = res, data = datos_norm, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, axes = c(3,4), pallete = "jco", outlier.color = rgb(0,0,0,10, maxColorValue = 255)) +
  theme_bw() +
  theme(legend.position = "none")+
  ggtitle("DBSCAN cluster")
ggsave("C:/Users/Usuario/Pictures/PMAAD/DBSCAN/dbscanforcaalt34.png", width=8,height=6, dpi=300)

### AÃƒÂ±ado la columna clÃƒÂºster a mis datos.
data$cluster <- res$cluster

### Guardo datos limpios.
datos_limpios <- dplyr::filter(data, cluster != 0)

### Guardo outliers.
outliers <- dplyr::filter(data, cluster == 0) 

### Graficamos el dbscan obtenido. Es el mismo grÃ¡fico anterior pero en PCA 
fviz_cluster(object = res, data = datos_norm, geom = "point", ellipse = TRUE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

### Otra manera de visualizar los clusters obtenidos
hullplot(datos_norm, res$cluster, main = paste0("Convex cluster Hulls, eps = ", epsilon))

# =============================================================================

# save(data, file = "./4_Preprocessing/dbscan_clustering.RData")