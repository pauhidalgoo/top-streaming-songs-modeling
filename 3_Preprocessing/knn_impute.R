
# Importar dataset
load("./3_Preprocessing/data_na_added.RData")


################################################################################

# Mirem el nombre de files i columnes amb el que ens hem quedat
cat("Dimensions del dataset:\n", dim(data))

# Mirem la quantitat missing data que tenim (NA)
cat("Missing data de tota la taula -->", sum(is.na(data)), ";", (sum(is.na(data)) / (ncol(data) * nrow(data))) * 100, "%." )
cat("Missing data de cada una de les variables:")
for ( col in colnames(data) ) {
  na_variable <- (sum(is.na(data[col])) / nrow(data)) * 100
  cat(col, "-->", sum(is.na(data[col])), ";", na_variable, "%.\n")
}

library(class)

sum(is.na(data))
numeric_cols <- c("track_popularity","album_popularity", "artist_popularity", "artist_num","energy","loudness","speechiness","acousticness", "danceability","liveness","valence","tempo", "duration","streams")
dd<- data[,numeric_cols]
uncompleteVar<-function(vector){any(is.na(vector))}

uncompleteVars<-which(sapply(dd, uncompleteVar))
as.vector(uncompleteVars)
fullVariables<-setdiff(c(1:dim(dd)[2]),as.vector(uncompleteVars))
aux<-dd[,fullVariables]
sum(is.na(aux))
dim(aux)
names(aux)
for (k in uncompleteVars){
  aux1 <- aux[!is.na(dd[,k]),]
  dim(aux1) 
  aux2 <- aux[is.na(dd[,k]),]
  dim(aux2)
  sum(is.na(aux1))
  sum(is.na(aux2))
  
  RefValues<- dd[!is.na(dd[,k]),k]
  sum(is.na(RefValues))
  
  knn.values = knn(aux1,aux2,RefValues)
  print(k)
  print("-----")
  print(knn.values)
  dd[is.na(dd[,k]),k] = as.numeric(as.character(knn.values))
  fullVariables<-c(fullVariables, k)
  aux<-dd[,fullVariables]
}

data[,numeric_cols] <- dd
#Creates a document with the resulting dataset
data_knn_imputed <- data
sum(is.na(data_knn_imputed))
data_knn_imputed <- data
save(data_knn_imputed, file = "./3_Preprocessing/data_knn_imputed.RData")
