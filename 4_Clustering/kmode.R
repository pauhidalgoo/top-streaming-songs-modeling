load("./3_Preprocessing/data_knn_imputed_unknown.RData")
# ==============================================================================
# Instal·lar els paquets necessaris
#Carregar el paquet
library(klaR)

# ------------------------------------------------------------------------------
# Cargamos los datos
set.seed(2004)

variables_cat <- c("album_type","pop", "hip_hop", "rock", "electro", "latino", "christmas", "cinema", "collab", "explicit", "key", "major_mode", "time_signature", "rank_group","gender", "is_group", "nationality","city")

data_cat <- data[,variables_cat]

for (nom_columna in names(data_cat)) {
  # Verifica si la columna es de tipo lógico
  if (is.logical(data_cat[[nom_columna]])) {
    # Convierte la columna a factor
    data_cat[[nom_columna]] <- as.factor(data_cat[[nom_columna]])
  }
}

str(data_cat)

# ------------------------------------------------------------------------------
# Ejecutamos el algoritmo de clustering K-Modes

cl <- klaR::kmodes(data_cat, 4) # 2 hace referencia al nÃºmero de clusters

# Verificamos los resultados
print(cl)

# ------------------------------------------------------------------------------
# Visualizamos los clusters obtenidos 

# ==============================================================================
#PROTOTYPES!

library(clustMixType)

# Suponiendo que df es tu dataframe y ya está cargado.

# Realizar el agrupamiento con K-Prototypes.
# kproto() realiza el agrupamiento. 
# Debes especificar el número de clusters (k) y el conjunto de datos.
# lambda es el parámetro que equilibra la importancia entre los tipos de variables.
# Puedes ajustar este parámetro según tus necesidades.

variables <- c("nationality", "track_popularity", "album_type", "artist_num", "pop", "hip_hop", "rock", "electro", "latino", "christmas", "cinema", "collab", "explicit", "danceability", "energy", "key", "major_mode", "time_signature", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo", "duration", "streams", "year_release", "year_week", "month_week", "rank_group", "gender", "is_group")
data_variables <- data[,variables]
result <- kproto(x = data_variables, k = 4, lambda = 0.5, iter.max = 10, nstart = 5)

# Ver los resultados del agrupamiento.
print(result)

# Para obtener los clusters asignados a cada observación:
clusters <- result$cluster
print(clusters)

# Para ver los centros de los clusters:
centers <- result$centers
print(centers)

# Para visualizar los clústers
library(factoextra)

variables_numericas <- c("track_popularity", "album_popularity", "artist_popularity", 
                         "artist_num", "energy", "loudness", "speechiness", "acousticness", 
                         "danceability", "liveness", "valence", "tempo", "duration", "streams")
datos <- data[variables_numericas]

object = list(data = datos, cluster = result$cluster)

fviz_cluster(object = object, data = datos_norm, geom = "point", ellipse = TRUE,
             show.clust.cent = FALSE, palette = "npg", axes=c(5,6),outlier.color = rgb(0,0,0,10, maxColorValue = 255)) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("KMODE cluster")

