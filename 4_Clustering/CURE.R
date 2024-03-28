library(dplyr)

k <- 4
r <- 0.15

metodo <- "euclidean"
set.seed(04102022)
load("./3_Preprocessing/data_knn_imputed_unknown.RData")

# Definir variables numèriques i categòriques
variables_numericas <- c("track_popularity", "album_popularity", "artist_popularity", 
                         "artist_num", "energy", "loudness", "speechiness", "acousticness", 
                         "danceability", "liveness", "valence", "tempo", "duration", "streams")


variables_categoricas <- c("pop", "hip_hop", "electro", "christmas", "cinema", "latino", "collab", "explicit", "major_mode", "is_group", "gender", "rank_group")


# Separar les variables

data_numericas <- data.frame(scale(data[variables_numericas]))

data_categoricas <- data[variables_categoricas]

# Codificació one-hot amb model.matrix
data_categorica_one_hot <- model.matrix(~ . - 1, data = data_categoricas)

# Convertir a dataframe
data_categorica_one_hot_df <- as.data.frame(data_categorica_one_hot)

# Combinar les dades en un dataset
data <- cbind(data_numericas, data_categorica_one_hot_df)
datainit <- data

n <- ceiling(0.3 * nrow(data))

ids <- sample(1:nrow(data), replace = FALSE, size = n)
dataNoMuestra <- data[-ids, ]
data <- data[ids, ]
dataMuestra <- data



hclust <- hclust(dist(data, method = metodo), method = "ward.D2")

plot(hclust)
# dissimMatrix <- daisy(data[,colsNoMiss], metric = "gower", stand=TRUE)
# distMatrix<-dissimMatrix^2
# hclust <- hclust(dist(data, method = metodo), method = "ward.D2")


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
names(NuevosCentroides)[names(NuevosCentroides) == "gendernon.binary"] <- "gendernon-binary"
names(NuevosCentroides)[names(NuevosCentroides) == "rank_group1.10"] <- "rank_group1-10"
names(NuevosCentroides)[names(NuevosCentroides) == "rank_group11.20"] <- "rank_group11-20"
names(NuevosCentroides)[names(NuevosCentroides) == "rank_group21.30"] <- "rank_group21-30"
names(NuevosCentroides)[names(NuevosCentroides) == "rank_group31.40"] <- "rank_group31-40"
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

# Visualitzem
clusterPertenece <- c()
data <- datainit
for (i in 1:nrow(data)) {
  bbdd <- data[i, ]
  bbdd$cluster <- 0
  agregado <- rbind(bbdd, NuevosCentroides)
  distanciaCorr <- as.matrix(dist(agregado[, -1], method = metodo))[1, -1]
  quienEsMenor <- as.numeric(which.min(distanciaCorr))
  clusterPertenece <- c(clusterPertenece, agregado[quienEsMenor + 1, "cluster"])   
}

data <- data[,variables_numericas]
object = list(data = data, cluster = clusterPertenece)


fviz_cluster(object = object, data = data, geom = "point", ellipse = TRUE,
             show.clust.cent = FALSE, palette = "npg", axes=c(1,2),outlier.color = rgb(0,0,0,10, maxColorValue = 255)) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("CURE cluster")
ggsave("./Media/Clustering/CURE/cure.png", width=8,height=6, dpi=300)

