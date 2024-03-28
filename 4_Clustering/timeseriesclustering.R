# ==============================================================================
# Descripció: Clustering de sèries temporals per a les cançons més
# escoltades mensualment a Spotify entre 2017 i 2021.
# ==============================================================================
# Carreguem les llibreries necessàries
library(dplyr)
library(tidyr)
library(proxy)
library(dtw)
library(tidyverse)
library(reshape2)
library(dtwclust)

# Carreguem les dades
load("./3_Preprocessing/data_knn_imputed_unknown.RData")

# ------------------------------------------------------------------------------
# Preparació de les dades

# Transformem year_week i month_week en una única columna de text per al mes i any,
# assegurant que els mesos menors que 10 incloguin un 0 davant
data <- data %>% 
  mutate(year_month = sprintf("%s-%02d", year_week, as.integer(month_week))) %>%
  select(track_name, year_month, streams)

# Pivotem per tenir year_month en columnes i track_name en files
datos <- reshape2::dcast(data, track_name ~ year_month, value.var = "streams", fun.aggregate = sum, na.rm = TRUE, fill = 0)

# Establim track_name com els noms de les files i l'eliminem de les variables
rownames(datos) <- datos$track_name
datos[, "track_name"] <- NULL

# ==============================================================================
# Hierarchical clústering amb distància DTW de proxy i funció hclust amb ward d2

# Calculem la distància DTW amb proxy
# Tarda molt, només necessari per escollir k inicial
# Posteriorment es pot passar directament al clustering jeràrquic fet amb tsclust (utilitza dtw_basic però obté mateixos resultats)
distMatrix <- proxy::dist(datos, method = "DTW")

# Generem el clustering
hc_o <- hclust(distMatrix, method = "ward.D2")
plot(hc_o, hang = -1, cex = 0.6, labels = FALSE) # Ajustem les etiquetes i les ometem

# Realitzem la tallada en un nombre desitjat de classes
k <- 5
clusters_original <- cutree(hc_o, k = k)

# Imprimim les taules de clusters
table(clusters_original)

# ------------------------------------------------------------------------------
# Clusterització amb dendrograma colorat
library(dendextend)

# Convertim hcc a dendrograma per colorar els clusters
dend <- as.dendrogram(hc_o)
dend_colored <- color_branches(dend, k = k)

# Eliminem etiquetes
dend_colored <- set_labels(dend_colored, labels = NA)

# Plotejem el dendrograma colorat sense etiquetes
plot(dend_colored, main = "Dendrograma de clustering de cançons")

# ==============================================================================
# Clustering amb DTW TSCLUST

k <- 5

# Jeràrquic
hc_ts <- tsclust(datos, type = "hierarchical", k = k, 
              distance = "dtw_basic", 
              trace = TRUE,
              control = hierarchical_control(method = "ward.D2"))
plot(hc_ts, type = "sc")
plot(hc_ts, type = 'centroids')

clusters_ts <- hc_ts@cluster
table(clusters_ts)

# ------------------------------------------------------------------------------
# Comparar el clustering original amb el nou

identical(clusters_original, clusters_ts)

# ------------------------------------------------------------------------------

# Gráfics de línies amb els streams de cada clúster per mes i amb les mitjanes

datos_cluster <- datos
datos_cluster$cluster <- clusters_ts

datos_with_id <- tibble::rownames_to_column(datos_cluster, var = "track_name")

datos_long = pivot_longer(datos_with_id, cols = -c(track_name, cluster), names_to = "mes", values_to = "valor")

medias_cluster = datos_long %>%
  group_by(mes, cluster) %>%
  summarize(valorMedio = mean(valor), .groups = 'drop')

p1 <- ggplot() +
  geom_line(data = datos_long, aes(x = mes, y = valor, group = track_name, color = as.factor(cluster)), linewidth = 0.5) +
  geom_line(data = medias_cluster, aes(x = mes, y = valorMedio, group = cluster), linewidth = 1.5, linetype = "dashed", color = "#404040") +
  theme_minimal() +
  labs(title = "Evolució de streams mensuals per clúster amb mitjanes", x = "Mes", y = "Valor", color = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), legend.position = "right") +
  facet_wrap(~cluster, scales = "free_y", ncol = 1)  # Usa facet_wrap para dividir por cluster

p1

ggsave(plot = p1, filename = paste('streams_cada_mes_per_cluster_amb_mitjana', '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Media/Clustering/TimeSeriesClustering', sep = ""), width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# Calcular la durada mitjana de les cançons de cada clúster en el top 40 (valor de streams != 0)
# Es farà un barplot i un boxplot

datos_filtrats <- datos_long %>%
  filter(valor != 0)

# Contar els mesos actius per cada cançó en cada clúster
count_actius <- datos_filtrats %>%
  group_by(track_name, cluster) %>%
  summarise(mesos_actius = n_distinct(mes), .groups = 'drop')

# Calcular la mitjana de mesos actius per cluster
mitjana_mesos_actius_per_cluster <- count_actius %>%
  group_by(cluster) %>%
  summarise(mitjana_mesos_actius = mean(mesos_actius), .groups = 'drop')

# Mostrar els resultats de les mitjanes
print(mitjana_mesos_actius_per_cluster)

# Barplot amb les mitjanes de mesos actius per cluster
p2 <- ggplot(mitjana_mesos_actius_per_cluster, aes(x = as.factor(cluster), y = mitjana_mesos_actius)) +
  geom_col(fill = "#1DB954") +
  theme_minimal() +
  labs(title = "Mitjana de mesos actius en cada cluster",
       x = "Cluster",
       y = "Mitjana de mesos actius") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

p2

ggsave(plot = p2, filename = paste('barplot_mitjanes_mesos_actius', '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Media/Clustering/TimeSeriesClustering', sep = ""), width = 8, height = 6, dpi = 300)

# Boxplot amb les distribucions de mesos actius per cluster
p3 <- ggplot(count_actius, aes(x = as.factor(cluster), y = mesos_actius)) +
  geom_boxplot(fill = "#1DB954") +
  theme_minimal() +
  labs(title = "Boxplot del nombre de mesos actius per cançó en cada cluster",
       x = "Cluster",
       y = "Nombre de mesos actius") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

p3

ggsave(plot = p3, filename = paste('boxplot_mesos_actius', '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Media/Clustering/TimeSeriesClustering', sep = ""), width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# Gráfic de línes amb la mitjana de streams per mes i per clúster

# Preparació de dades
datos_long_month <- datos_long
datos_long_month$month <- as.numeric(sub("^\\d{4}-", "", datos_long$mes))

# Calculem les mitjanes mensuals
mitjanes_mensuals <- datos_long_month %>%
  group_by(cluster, month) %>%
  summarize(mitjana_streams = mean(valor), .groups = 'drop')

# Gráfic de línies
p4 <- ggplot(mitjanes_mensuals, aes(x = month, y = mitjana_streams, group = cluster, color = as.factor(cluster))) +
  geom_line(aes(color = as.factor(cluster)), linewidth = 0.8) +
  geom_point() +
  theme_minimal() +
  labs(title = "Mitjana de streams per mes i per cluster",
       x = "Mes",
       y = "Mitjana de streams",
       color = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = c('Gen', 'Feb', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Oct', 'Nov', 'Des'))

p4

ggsave(plot = p4, filename = paste('evol_streams_per_mes_i_cluster', '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Media/Clustering/TimeSeriesClustering', sep = ""), width = 8, height = 6, dpi = 300)


# ------------------------------------------------------------------------------

