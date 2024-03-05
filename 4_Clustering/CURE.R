library(dplyr)

k <- 3
r <- 0.2
metodo <- "gower"

load("C:/Users/Usuario/Documents/Universitat/4rt Quatri/PMAAD/Preprocessing/data_na_added.RData")

variables <- c("track_popularity", "album_type", "artist_num", "artist_followers", "pop", "hip_hop", "rock", "electro", "latino", "christmas", "cinema", "collab", "explicit", "danceability", "energy", "key", "major_mode", "time_signature", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo", "duartion", "streams", "year_release", "year_week", "month_week", "rank_group", "nationality", "gender", "is_group")
data <- data[,varibles]
n <- ceiling(0.3 * nrow(data))

ids <- sample(1:nrow(data), replace = FALSE, size = n)
dataNoMuestra <- data[-ids, ]
data <- data[ids, ]
dataMuestra <- data

hclust <- hclust(dist(data, method = metodo), method = "single")
subsets <- cutree(hclust, k)
dataMuestra$cluster <- subsets

centroides <- matrix(0, k, ncol(data))
representativos <- list()
noRepresentativos <- list()
indicesRepresentativos <- list()

for (i in 1:k) {
  subset <- data[subsets == i, ]
  subset$id <- rownames(subset)
  kmeans_result <- kmeans(subset[, -ncol(subset)], 1)
  centroides[i, ] <- kmeans_result$centers
  
  matriz <- rbind(centroides[i, ], subset[, -ncol(subset)])
  distancias <- dist(x = matriz, method = metodo)
  distanciaFinal <- as.matrix(distancias)[1, -1]
  names(distanciaFinal) <- subset$id
  pesosOrdenados <- sort(distanciaFinal, decreasing = FALSE)
  indices <- as.numeric(names(pesosOrdenados)[1:(r * nrow(subset))])
  indicesRepresentativos[[i]] <- as.numeric(names(pesosOrdenados)[1:(r * nrow(subset))])
  representativos[[i]] <- subset[which(rownames(subset) %in% indices), ]
  noRepresentativos[[i]] <- subset[which(!rownames(subset) %in% indices), ]
}

noRepresentativos <- dplyr::bind_rows(noRepresentativos)

merged_representatives <- list()
for (i in 1:k) {
  bbdd <- rbind(centroides[i, ], representativos[[i]][, -ncol(representativos[[i]])])
  bbdd$cluster <- i
  merged_representatives[[i]] <- bbdd
}

centroidesRepresentativos <- dplyr::bind_rows(merged_representatives)

NuevosCentroides <- centroidesRepresentativos %>%
  group_by(cluster) %>%
  summarise_all(mean) %>% data.frame()

clusterPertenece <- c()
for (i in 1:nrow(dataNoMuestra)) {
  bbdd <- dataNoMuestra[i, ]
  bbdd$cluster <- 0
  agregado <- rbind(bbdd, NuevosCentroides)
  distanciaCorr <- as.matrix(dist(agregado[, -1], method = metodo))[1, -1]
  quienEsMenor <- as.numeric(which.min(distanciaCorr))
  clusterPertenece <- c(clusterPertenece, agregado[quienEsMenor + 1, "cluster"])   
}

dataNoMuestra$cluster <- clusterPertenece

dataMuestra
dataNoMuestra

table(dataMuestra$cluster)
table(dataNoMuestra$cluster)