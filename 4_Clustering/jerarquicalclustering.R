######HIERARCHICAL CLUSTERING######

load("./3_Preprocessing/data_knn_imputed_unknown.RData")

var_num <- data[,c("track_popularity", "album_popularity", "artist_num", "artist_followers", "artist_popularity", "danceability", "energy", "loudness", "speechiness", "acousticness", "valence", "liveness", "tempo", "duration", "streams")]

### ESTANDARDITZAR DADES
min_max_normalization <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

numeriques_normalitzades <- as.data.frame(lapply(var_num, min_max_normalization))

#variables <- c("nationality", "track_popularity", "album_type", "artist_num", "pop", "hip_hop", "rock", "electro", "latino", "christmas", "cinema", "collab", "explicit", "danceability", "energy", "key", "major_mode", "time_signature", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo", "duration", "streams", "year_release", "year_week", "month_week", "rank_group", "gender", "is_group")

######HIERARCHICAL CLUSTERING######
#CATEGÒRIQUES I NUMÈRIQUES

library(cluster)

#var_num_cols <- numeriques_normalitzades
categorical_vars <- data[, sapply(data, is.factor)]
categorical_vars <- subset(categorical_vars, select = -c(track_id, track_name, album_name, artist_name))

actives <- data.frame(categorical_vars, numeriques_normalitzades)
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

clusteres <- cutree(h1, k=4)
data$cluster_hier <- clusteres

save(data, file = "./4_Clustering/jerarquic_cluster.RData")

###########PCA###########
library(ggplot2)
library(factoextra)

datos_norm <- as.data.frame(numeriques_normalitzades)

pca <- prcomp(datos_norm, scale = TRUE)
iden = 0
varianza <- pca$sdev^2
total_varianza <- sum(varianza)
perc_varianza <- 100*varianza/total_varianza
perc_var1 <- round(perc_varianza[1], 2)
perc_var2 <- round(perc_varianza[2], 2)
label_x <- paste("1r component (", perc_var1, "%)")
label_y <- paste("2n component (", perc_var2, "%)")

df_psi <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], color = iden)
### Printamos la imagen que hemos obtenido de los datos a clasificar
p <- ggplot(df_psi, aes(x = PC1, y = PC2)) +
  geom_vline(aes(xintercept = 0), color = "black", linewidth = 0.75) +
  geom_hline(aes(yintercept = 0), color = "black", linewidth = 0.75) +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_line(linetype = "dashed", color = rgb(0, 0, 0, 100, maxColorValue = 255)),
        legend.position = "none") +
  geom_point(aes(color =color), alpha = 0.35) +
  labs(x = label_x,y = label_y,
       title = "Projecció de totes les dades sobre els 2 primers Components Principals")
print(p)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             axes = c(3,4),
             repel = TRUE     # Avoid text overlapping
)


df_psi$cluster <- data$cluster_hier

fviz_cluster(list(data = df_psi[, 1:2], cluster = df_psi$cluster), 
             geom = "point", ellipse = TRUE, show.clust.cent = FALSE, 
             palette = "jco") + theme_bw() + theme(legend.position = "none")

df_psi$PC3 <- pca$x[, 3]
df_psi$PC4 <- pca$x[, 4]

fviz_cluster(list(data = df_psi[, c("PC3", "PC4")], cluster = df_psi$cluster), 
             geom = "point", ellipse = TRUE, show.clust.cent = FALSE, 
             palette = "jco") + theme_bw() + theme(legend.position = "none")

# Añadir las columnas de la 5ª y 6ª dimensiones del PCA al dataframe df_psi
df_psi$PC2 <- pca$x[, 2]
df_psi$PC3 <- pca$x[, 3]

# Ahora puedes usar fviz_cluster para visualizar la 5ª y 6ª componentes principales
fviz_cluster(list(data = df_psi[, c("PC2", "PC3")], cluster = df_psi$cluster), 
             geom = "point", ellipse = TRUE, show.clust.cent = FALSE, 
             palette = "jco") + theme_bw() + theme(legend.position = "none")

