# ==============================================================================
# Adaptación del script para agrupación mensual de datos de Spotify sin errores
# 
# Descripción: Clustering de series temporales para las canciones más
# escuchadas mensualmente en Spotify entre 2017 y 2021, corrigiendo errores.
# ==============================================================================
# Cargamos las librerías necesarias
library(dtw)
library(tidyverse)
library(reshape2)
library(proxy)
library(dtwclust)

load("./3_Preprocessing/data_knn_imputed_unknown.RData")

# ==============================================================================
# Preparación de los datos

# Transformamos year_week y month_week a una única columna de texto para el mes y año,
# asegurando que los meses menores que 10 incluyan un 0 adelante.
data <- data %>% 
  mutate(year_month = sprintf("%s-%02d", year_week, as.integer(month_week))) %>%
  select(track_name, year_month, streams)


# Sumamos los streams por track_name y year_month
aggregated_data <- data %>%
  group_by(track_name, year_month) %>%
  summarise(streams = sum(streams), .groups = 'drop')

# Preparación de los datos antes del pivot
aggregated_data <- aggregated_data %>%
  arrange(year_month) # Aseguramos que los datos estén ordenados por year_month antes del pivot

# Pivotamos para tener year_month en columnas y track_name en filas
# Aseguramos que las columnas estén correctamente ordenadas por year_month durante el pivot
datos <- pivot_wider(aggregated_data, names_from = year_month, values_from = streams, values_fill = list(streams = 0))

# Convertimos a data frame si es necesario y establecemos track_name como los nombres de fila
# Nota: pivot_wider puede devolver un tibble, así que lo convertimos a data.frame
datos <- as.data.frame(datos)

# Establecemos track_name como los nombres de las filas
rownames(datos) <- datos$track_name
# Eliminamos la columna track_name ya que ahora es el identificador de fila
datos <- datos[ , !(names(datos) %in% c("track_name"))]

# ==============================================================================
# Calculamos la distancia DTW
distMatrix <- proxy::dist(datos, method = "DTW")

# Generamos el clustering
hcc <- hclust(distMatrix, method = "ward.D2")
plot(hcc, hang = -1, cex = 0.6, labels = FALSE) # Ajustamos las etiquetas y las omitimos

# Realizamos el corte en un número deseado de clases
k <- 5 # Ajusta este valor según el análisis deseado
clusters <- cutree(hcc, k = k)

# Print cluster tables
table(clusters)

# ==============================================================================
# Clusterización con dendrograma coloreado
library(dendextend)

# Convertimos hcc a dendrograma para colorear los clusters
dend <- as.dendrogram(hcc)
dend_colored <- color_branches(dend, k = k)

# Eliminamos etiquetas
dend_colored <- set_labels(dend_colored, labels=NA)

# Ploteamos el dendrograma coloreado sin etiquetas
plot(dend_colored, main = "Dendrograma de Clustering de Canciones")

# ==============================================================================
# Clustering Particional y Jerárquico con dtwclust

# Particional
pc <- tsclust(datos, type = "partitional", k = 20L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))
plot(pc)

# Hierarchical
hc <- tsclust(datos, type = "hierarchical", k = 20L, 
              distance = "dtw_basic", trace = TRUE,
              control = hierarchical_control(method = "ward.D2"))

# Asumimos que 'hc' es un objeto resultante de tsclust
# Convertimos a un objeto hclust
hclust_obj <- as.hclust(hc)

# Luego, convertimos el objeto hclust a un dendrograma
dendrogram_obj <- as.dendrogram(hclust_obj)

# Removemos las etiquetas de los nodos
dendrogram_obj <- set_labels(dendrogram_obj, labels = NA)

# Ahora, graficamos el dendrograma sin etiquetas de nodos pero con las alturas
plot(dendrogram_obj, main = "Dendrograma del Clustering")

