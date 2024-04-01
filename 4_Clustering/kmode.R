load("./3_Preprocessing/data_knn_imputed_unknown.RData")
# ==============================================================================
# Instal·lar els paquets necessaris
#Carregar el paquet
library(klaR)

# ------------------------------------------------------------------------------
# Carreguem les dades
set.seed(2004)

#Agafem les variables categòriques
variables_cat <- c("album_type","pop", "hip_hop", "rock", "electro", "latino", "christmas", "cinema", "collab", "explicit", "key", "major_mode", "time_signature", "rank_group","gender", "is_group", "nationality","city")

data_cat <- data[,variables_cat]

# Afafem també les booleanes
for (nom_columna in names(data_cat)) {
  # Verifica si la columna és de tipus lògic
  if (is.logical(data_cat[[nom_columna]])) {
    # Passem columna a factor
    data_cat[[nom_columna]] <- as.factor(data_cat[[nom_columna]])
  }
}

str(data_cat)

# ------------------------------------------------------------------------------
# Executem el K-modes amb 4 clústers

cl <- klaR::kmodes(data_cat, 4)
print(cl)

# ------------------------------------------------------------------------------
# Visualitzem els resultats

object = list(data = datos, cluster = result$cluster)

# Visualitzem en les dimensions 5 i 6 del PCA
fviz_cluster(object = object, data = datos_norm, geom = "point", ellipse = TRUE,
             show.clust.cent = FALSE, palette = "npg", axes=c(5,6),outlier.color = rgb(0,0,0,10, maxColorValue = 255)) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("KMODE cluster")


fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(5,6),
             repel = TRUE     # Avoid text overlapping
)


# ==============================================================================
#PROTOTYPES!

# Barreja entre K-means i K-modes

library(clustMixType)

variables <- c("nationality", "track_popularity", "album_type", "artist_num", "pop", "hip_hop", "rock", "electro", "latino", "christmas", "cinema", "collab", "explicit", "danceability", "energy", "key", "major_mode", "time_signature", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo", "duration", "streams", "year_release", "year_week", "month_week", "rank_group", "gender", "is_group")
data_variables <- data[,variables]
result <- kproto(x = data_variables, k = 4, lambda = 0.5, iter.max = 10, nstart = 5) # lambda és el paràmetre que equilibra la importància entre els tipus de variables.

print(result)

# Per veure els clústers assignats
clusters <- result$cluster
print(clusters)

# Per veure els centres dels clústers
centers <- result$centers
print(centers)

# Per visualizar els clústers
library(factoextra)

variables_numericas <- c("track_popularity", "album_popularity", "artist_popularity", 
                         "artist_num", "energy", "loudness", "speechiness", "acousticness", 
                         "danceability", "liveness", "valence", "tempo", "duration", "streams")
datos <- data[variables_numericas]