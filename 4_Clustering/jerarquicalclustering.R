######HIERARCHICAL CLUSTERING######

load("./3_Preprocessing/data_knn_imputed.RData")
data <- data_knn_imputed

#CATEGÒRIQUES I NUMÈRIQUES

library(cluster)

var_num_cols <- var_num
categorical_vars <- data[, sapply(data, is.factor)]
categorical_vars <- subset(categorical_vars, select = -c(track_id, track_name, album_name, artist_name))

actives <- data.frame(categorical_vars, var_num_cols)

dissimMatrix <- daisy(actives, metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST

plot(h1)

#Triar k
c2 <- cutree(h1,2)
c3 <- cutree(h1,3)
c4 <- cutree(h1,4)
c5 <- cutree(h1,5)
c6 <- cutree(h1,6)
c7 <- cutree(h1,7)
c8 <- cutree(h1,8)

summary(silhouette(c2, distMatrix))
summary(silhouette(c3, distMatrix))
summary(silhouette(c4, distMatrix))
summary(silhouette(c5, distMatrix))
summary(silhouette(c6, distMatrix))
summary(silhouette(c7, distMatrix))
summary(silhouette(c8, distMatrix))

rect.hclust(h1, k=4, border='red')

table(c4)

data$cluster_hierarchical<- c4