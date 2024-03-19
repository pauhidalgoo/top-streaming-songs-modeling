load("./3_Preprocessing/data_knn_imputed.RData")
data <- data_knn_imputed
# ==============================================================================
# [PMAAD] - KMODE Algoirthm
# 
# Author(s):    Dante Conti and Sergi RamÃ­rez, IDEAI  (c)
# Date:         6th 2023
# Description: 
#             This file contains all KMODE algorithm to use when ours databases contains
#.            mixture databases
# ==============================================================================
# Instalamos los paquetes necesarios

install.packages("klaR") #instalamos el paquete
library(klaR) #cargamos el paquete

# ------------------------------------------------------------------------------
# Cargamos los datos
set.seed(2004)

variables_cat <- c("album_type","pop", "hip_hop", "rock", "electro", "latino", "christmas", "cinema", "collab", "explicit", "key", "major_mode", "time_signature", "rank_group","gender", "is_group", "nationality")
#falta ciutat!
data_reduida <- na.omit(data[,variables_cat])


for (nom_columna in names(data_reduida)) {
  # Verifica si la columna es de tipo lógico
  if (is.logical(data_reduida[[nom_columna]])) {
    # Convierte la columna a factor
    data_reduida[[nom_columna]] <- as.factor(data_reduida[[nom_columna]])
  }
}
str(data_reduida)
sum(is.na(data_reduida))
# Verificamos los datos
head(data_reduida)

for (col in names(data_reduida)) {
  # Cuenta el número de NA en la columna
  na_count <- sum(is.na(data_reduida[[col]]))
  if (na_count > 0) {
    # Imprime el nombre de la columna y la cantidad de NA
    cat(col, "tiene", na_count, "NA(s)\n")
  }
}# ------------------------------------------------------------------------------
# Ejecutamos el algoritmo de clustering K-Modes
cl <- klaR::kmodes(data_reduida, 5) # 2 hace referencia al nÃºmero de clusters

# Verificamos los resultados
print(cl)

# ------------------------------------------------------------------------------
# Visualizamos los clusters obtenidos 
plot(jitter(datos$edad, datos$ingresos), col = cl$cluster)
# ==============================================================================
#PROTOTYPES!

install.packages("clustMixType")
library(clustMixType)

# Suponiendo que df es tu dataframe y ya está cargado.

# Realizar el agrupamiento con K-Prototypes.
# kproto() realiza el agrupamiento. 
# Debes especificar el número de clusters (k) y el conjunto de datos.
# lambda es el parámetro que equilibra la importancia entre los tipos de variables.
# Puedes ajustar este parámetro según tus necesidades.

variables <- c("nationality", "track_popularity", "album_type", "artist_num", "pop", "hip_hop", "rock", "electro", "latino", "christmas", "cinema", "collab", "explicit", "danceability", "energy", "key", "major_mode", "time_signature", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo", "duration", "streams", "year_release", "year_week", "month_week", "rank_group", "gender", "is_group")
data_reduida <- na.omit(data[,variables])
result <- kproto(x = data_reduida, k = 5, lambda = 0.5, iter.max = 10, nstart = 5)

# Ver los resultados del agrupamiento.
print(result)

# Para obtener los clusters asignados a cada observación:
clusters <- result$cluster
print(clusters)

# Para ver los centros de los clusters:
centers <- result$centers
print(centers)