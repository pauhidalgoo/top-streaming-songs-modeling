######HIERARCHICAL CLUSTERING######

load("./3_Preprocessing/data_knn_imputed_unknown.RData")

var_num <- data[,c("track_popularity", "album_popularity", "artist_num", "artist_followers", "artist_popularity", "danceability", "energy", "loudness", "speechiness", "acousticness", "valence", "liveness", "tempo", "duration", "streams")]

### ESTANDARDITZAR DADES
min_max_normalization <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

numeriques_normalitzades <- lapply(var_num, min_max_normalization)

#variables <- c("nationality", "track_popularity", "album_type", "artist_num", "pop", "hip_hop", "rock", "electro", "latino", "christmas", "cinema", "collab", "explicit", "danceability", "energy", "key", "major_mode", "time_signature", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo", "duration", "streams", "year_release", "year_week", "month_week", "rank_group", "gender", "is_group")

######HIERARCHICAL CLUSTERING######
#CATEGÒRIQUES I NUMÈRIQUES

library(cluster)

var_num_cols <- numeriques_normalitzades
categorical_vars <- data[, sapply(data, is.factor)]
categorical_vars <- subset(categorical_vars, select = -c(track_id, track_name, album_name, artist_name))

actives <- data.frame(categorical_vars, var_num_cols)
vars <- subset(actives, select = -c(week_index, day_release))

dissimMatrix <- daisy(vars, metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST

plot(h1)
rect.hclust(h1, k=4, border="red") 

# k triada: 4
# dendograma amb colors
#install.packages("dendextend")

library(dendextend)
dend <- as.dendrogram(h1)
dend <- color_branches(dend, k=4)
plot(dend, main='Hierarchical Clustering')