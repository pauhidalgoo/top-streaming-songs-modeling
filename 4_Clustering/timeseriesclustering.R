# ==============================================================================
# [PMAAD] - TimesSeriesClutering.R
# 
# Author(s):    Sergi RamÃ­rez, IDEAI  (c)
# Date:         12th March 2024
# Description: 
# ==============================================================================
# Carreguem les llibreries necessaries
library(dtw)
library(tidyverse)
library(dplyr)
library(dendextend)
# ==============================================================================
# Carreguem la base de dades
dad <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/2013.csv?nocab=1")
dad[, 2] <- NULL

# ==============================================================================
# Transformem la bbdd a la taula necessaria
dad[, "Total"] <- as.numeric(dad[, "Total"])
dad[, "Zonas.Turísticas"] <- gsub(":", "_", dad[, "Zonas.Turísticas"])
dad[, "Zonas.Turísticas"] <- gsub(" ", "", dad[, "Zonas.Turísticas"])
datos <- reshape2::dcast(dad, Zonas.Turísticas ~ Periodo, fun.aggregate = sum, na.rm = TRUE)
rownames(datos) <- datos[which(dad$Establecimientos.y.personal.empleado..plazas. == "Número de establecimientos abiertos estimados"),]
datos[, "Zonas.Turísticas"] <- NULL


# ==============================================================================
# Calculamos la distancia DTW
distMatrix <- proxy::dist(datos, method = "DTW")

# Generamos el clustering
hcc <- hclust(distMatrix, method = "ward.D2")
plot(hcc, hang = -1, cex = 0.6) # Put the labels at the same height: hang = -1

# Realizamos el corte en 4 clases
k <- 4
clusters <- cutree(hcc, k = k)

# ------------------------------------------------------------------------------
# Clusterisation using 3 variables
datos %>% 
  proxy::dist(method = "DTW") %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram() -> dend
dend %>% set("branches_k_color", k = k) %>% plot(hang = -1)

# ==============================================================================
library(dtwclust)

## Partitional
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
# Bibliografia: 
## https://github.com/asardaes/dtwclust
## https://rpubs.com/Edison-D/615477
## https://rpubs.com/sebas_Alf/684217
## http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
## https://plotly.com/ggplot2/dendrogram/
## https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html

# ==============================================================================