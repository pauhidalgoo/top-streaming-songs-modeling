# ==============================================================================
# Descripció: Clustering de sèries temporals per a les cançons més
# escoltades mensualment a Spotify entre 2017 i 2021.
# ==============================================================================
# Carreguem les llibreries necessàries
library(proxy)
library(dtw)
library(tidyverse)
library(reshape2)
library(dtwclust)

load("./3_Preprocessing/data_knn_imputed_unknown.RData")

# ==============================================================================
# Preparació de les dades

# Transformem year_week i month_week en una única columna de text per al mes i any,
# assegurant que els mesos menors que 10 incloguin un 0 davant.

data <- data %>% 
  mutate(year_month = sprintf("%s-%02d", year_week, as.integer(month_week))) %>%
  select(track_name, year_month, streams)

# Pivotem per tenir year_month en columnes i track_name en files
datos <- reshape2::dcast(data, track_name ~ year_month, value.var = "streams", fun.aggregate = sum, na.rm = TRUE, fill = 0)

# Establim track_name com els noms de les files i l'eliminem de les variables
rownames(datos) <- datos$track_name
datos[, "track_name"] <- NULL

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
pc <- tsclust(datos, type = "partitional", k = 5L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 5L)))
plot(pc, type = "sc")

#hc <- tsclust(datos, type = "h", k = 5L, 
#              distance = "dtw", 
#              trace = TRUE,
#              args = tsclust_args(dist = list(window.size = 5L)))
#plot(hc, type = "sc")
