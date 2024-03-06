load("./3_Preprocessing/data_knn_imputed.RData")

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

variables <- c("track_popularity", "album_type", "artist_num", "artist_followers", "pop", "hip_hop", "rock", "electro", "latino", "christmas", "cinema", "collab", "explicit", "danceability", "energy", "key", "major_mode", "time_signature", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo", "duration", "streams", "year_release", "year_week", "month_week", "rank_group", "nationality", "gender", "is_group")
variabless <- c("pop", "gender")
data_reduida <- data[,variables]

for (nom_columna in names(data_reduida)) {
  # Verifica si la columna es de tipo lógico
  if (is.logical(data_reduida[[nom_columna]])) {
    # Convierte la columna a factor
    data_reduida[[nom_columna]] <- as.factor(data_reduida[[nom_columna]])
  }
}
str(data_reduida)

# Verificamos los datos
head(data_reduida)

# ------------------------------------------------------------------------------
# Ejecutamos el algoritmo de clustering K-Modes
cl <- klaR::kmodes(data_reduida, 5) # 2 hace referencia al nÃºmero de clusters

# Verificamos los resultados
print(cl)

# ------------------------------------------------------------------------------
# Visualizamos los clusters obtenidos 
plot(jitter(datos$edad, datos$ingresos), col = cl$cluster)
# ==============================================================================