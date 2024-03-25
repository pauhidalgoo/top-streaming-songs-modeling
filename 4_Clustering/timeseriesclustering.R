# ==============================================================================
# Descripció: Clustering de sèries temporals per a les cançons més
# escoltades mensualment a Spotify entre 2017 i 2021.
# ==============================================================================
# Carreguem les llibreries necessàries
library(dtw)
library(tidyverse)
library(reshape2)
library(proxy)
library(dtwclust)

load("./3_Preprocessing/data_knn_imputed_unknown.RData")

# ==============================================================================
# Preparació de les dades

# Transformem year_week i month_week en una única columna de text per al mes i any,
# assegurant que els mesos menors que 10 incloguin un 0 davant.

data <- data %>% 
  mutate(year_month = sprintf("%s-%02d", year_week, as.integer(month_week))) %>%
  select(track_name, year_month, streams)

# Sumem els streams per track_name i year_month
aggregated_data <- data %>%
  group_by(track_name, year_month) %>%
  summarise(streams = sum(streams), .groups = 'drop')

# Preparació de les dades abans del pivot
aggregated_data <- aggregated_data %>%
  arrange(year_month) # Assegurem que les dades estan ordenades per year_month abans del pivot

# Pivotem per tenir year_month en columnes i track_name en files
# Assegurem que les columnes estan correctament ordenades per year_month durant el pivot
datos <- pivot_wider(aggregated_data, names_from = year_month, values_from = streams, values_fill = list(streams = 0))

# Convertim a data frame si és necessari i establim track_name com els noms de fila
# Nota: pivot_wider pot retornar un tibble, així que ho convertim a data.frame
datos <- as.data.frame(datos)

# Establim track_name com els noms de les files
rownames(datos) <- datos$track_name
# Eliminem la columna track_name ja que ara és l'identificador de fila
datos <- datos[ , !(names(datos) %in% c("track_name"))]

# ==============================================================================
# Calculem la distància DTW
distMatrix <- proxy::dist(datos, method = "DTW")

# Generem el clustering
hcc <- hclust(distMatrix, method = "ward.D2")
plot(hcc, hang = -1, cex = 0.6, labels = FALSE) # Ajustem les etiquetes i les ometem

# Realitzem la tallada en un nombre desitjat de classes
k <- 5
clusters <- cutree(hcc, k = k)

# Imprimim les taules de clusters
table(clusters)

# ==============================================================================
# Clusterització amb dendrograma colorat
library(dendextend)

# Convertim hcc a dendrograma per colorar els clusters
dend <- as.dendrogram(hcc)
dend_colored <- color_branches(dend, k = k)

# Eliminem etiquetes
dend_colored <- set_labels(dend_colored, labels = NA)

# Plotejem el dendrograma colorat sense etiquetes
plot(dend_colored, main = "Dendrograma de Clustering de Cançons")

# ==============================================================================
# Clustering Particional i Jeràrquic amb dtwclust

# Particional
pc <- tsclust(datos, type = "partitional", k = 20L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))
plot(pc)

# Jeràrquic
hc <- tsclust(datos, type = "hierarchical", k = 20L, 
              distance = "dtw_basic", trace = TRUE,
              control = hierarchical_control(method = "ward.D2"))

# Convertim el cluster a un objecte d'hclust per poder representar millor el plot
hclust_obj <- as.hclust(hc)

# Posteriorment ho canviem a dendrograma
dendrogram_obj <- as.dendrogram(hclust_obj)

# Eliminem les etiquetes horitzontals (noms de les cançons)
dendrogram_obj <- set_labels(dendrogram_obj, labels = NA)

# Grafiquem el dendrograma final
plot(dendrogram_obj, main = "Dendrograma del Clustering")