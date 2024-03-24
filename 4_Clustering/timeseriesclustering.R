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

# ==============================================================================
# Preparación de los datos
# Asumimos que 'data' ya está cargado

# Transformamos year_week y month_week a una única columna de texto para el mes y año
data <- data %>% 
  mutate(year_month = paste(year_week, month_week, sep = "-")) %>%
  select(track_name, year_month, streams)

# Sumamos los streams por track_name y year_month
aggregated_data <- data %>%
  group_by(track_name, year_month) %>%
  summarise(streams = sum(streams), .groups = 'drop')

# Pivotamos para tener year_month en columnas y track_name en filas
datos <- pivot_wider(aggregated_data, names_from = year_month, values_from = streams, values_fill = list(streams = 0))

# Usamos track_name como identificador de fila en lugar de nombres de fila
datos <- datos %>%
  remove_rownames() %>%
  column_to_rownames(var = "track_name")

# ==============================================================================
# Calculamos la distancia DTW
distMatrix <- proxy::dist(as.matrix(datos), method = "DTW")

# Generamos el clustering
hcc <- hclust(distMatrix, method = "ward.D2")
plot(hcc, hang = -1, cex = 0.6, labels=FALSE) # Ajustamos las etiquetas: hang = -1

# Realizamos el corte en 3 clases
k <- 3
clusters <- cutree(hcc, k = k)

# ------------------------------------------------------------------------------
# Clusterización con dendrograma coloreado
datos <- as.data.frame(datos) # Convertimos datos a data.frame para evitar problemas con tibbles
datos$track_name <- rownames(datos) # Guardamos track_name para usar en el plot

datos %>% 
  select(-track_name) %>% 
  proxy::dist(method = "DTW") %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram() -> dend


library(dendextend)

dend %>% set("branches_k_color", k = k) %>% plot(hang = -1)


# PROVES

dend <- as.dendrogram(hcc) # Asegúrate de que 'hcc' es tu objeto hclust

# Coloreamos las ramas basadas en los clusters identificados
dend_colored <- color_branches(dend, k = k)

# Ploteamos el dendrograma sin etiquetas y con las ramas coloreadas
plot(dend_colored %>% set_labels(labels = NA))

# ==============================================================================
# Clustering Particional y Jerárquico con dtwclust

# Ajustamos los datos para tsclust, eliminando la columna track_name
datos_clust <- datos %>% select(-track_name)

# Particional
pc <- tsclust(as.data.frame(datos_clust), type = "partitional", k = 20L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))
plot(pc)

# Hierarchical
hc <- tsclust(as.data.frame(datos_clust), type = "hierarchical", k = 20L, 
              distance = "dtw_basic", trace = TRUE,
              control = hierarchical_control(method = "ward.D2"))
plot(hc, labels=FALSE)

# PROVA

hclust_obj <- as.hclust(hc)

# Luego, convertimos el objeto hclust a un dendrograma
dend <- as.dendrogram(hclust_obj)

# Ahora, usaremos 'plot' directamente en el dendrograma con 'labels=FALSE'
plot(dend, main = "Dendrograma del Clustering", labels = FALSE)

# ==============================================================================
# Notas: Este script es una adaptación y puede requerir ajustes adicionales
# basados en las especificidades de 'data'.
