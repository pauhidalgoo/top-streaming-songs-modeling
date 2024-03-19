# ==============================================================================
# [PMAAD] - TimesSeriesClustering.R
# 
# Autor(es):    Sergi Ramírez, IDEAI  (c)
# Fecha:         12 de Marzo de 2024
# Descripción: 
# ==============================================================================
# Cargamos las librerías necesarias
library(dtw)
library(tidyverse)

# ==============================================================================
# Cargamos la base de datos
dad <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/2013.csv?nocab=1")
dad[, 2] <- NULL

# ==============================================================================
# Transformamos la bbdd a la tabla necesaria
dad[, "Total"] <- as.numeric(dad[, "Total"])
dad[, "Zonas.Turísticas"] <- gsub(":", "_", dad[, "Zonas.Turísticas"])
dad[, "Zonas.Turísticas"] <- gsub(" ", "", dad[, "Zonas.Turísticas"])
datos <- reshape2::dcast(dad, Zonas.Turísticas ~ Periodo, fun.aggregate = sum, na.rm = TRUE)
rownames(datos) <- datos[, "Zonas.Turísticas"]
datos[, "Zonas.Turísticas"] <- NULL


# ==============================================================================
# Calculamos la distancia DTW
distMatrix <- proxy::dist(datos, method = "DTW")

# Generamos el clustering
hcc <- hclust(distMatrix, method = "ward.D2")
plot(hcc, hang = -1, cex = 0.6) # Ponemos las etiquetas a la misma altura: hang = -1

# Realizamos el corte en 4 clases
k <- 4
clusters <- cutree(hcc, k = k)

# ------------------------------------------------------------------------------
# Clusterización usando 3 variables
datos %>% 
  proxy::dist(method = "DTW") %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram() -> dend

dend %>% set("branches_k_color", k = k) %>% plot(hang = -1)

# ==============================================================================
library(dtwclust)

## Particional
pc <- tsclust(datos[, -1], type = "partitional", k = 20L, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))
plot(pc)

# ------------------------------------------------------------------------------
## Hierarchical
hc <- tsclust(CharTraj, type = "hierarchical", k = 20L, 
              distance = "dtw_basic", trace = TRUE,
              control = hierarchical_control(method = "ward.D2"))
plot(hc)

# ==============================================================================
# Bibliografía: 
## https://github.com/asardaes/dtwclust
## https://rpubs.com/Edison-D/615477
## https://rpubs.com/sebas_Alf/684217
## http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
## https://plotly.com/ggplot2/dendrogram/
## https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html

# ==============================================================================
