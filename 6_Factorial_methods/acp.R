load('final_d3_data.RData')

################################################################################

# Variables numèriques
numerical_vars <- sapply(data, is.numeric)
sum(numerical_vars == T)

# Dataset només amb numèriques
data_numerical <- data[, numerical_vars]
names(data_numerical)

# Eliminem les variables que no aporten informació nova
data_numerical <- subset(data_numerical, select = -c(album_popularity, artist_followers))
names(data_numerical)

columnas_a_mantener <- setdiff(colnames(data), colnames(data_numerical))

data_categorical <- data[, columnas_a_mantener]
names(data_categorical)

data_categorical <- subset(data_categorical, select = -c(track_id, track_name, album_name, album_label, artist_name, day_release, year_release, artist_followers, album_popularity, lyrics, city))

################################################################################
print(data_numerical)
# PCA
pc1 <- prcomp(data_numerical, scale = TRUE)
dim(pc1)
names(pc1)
pc1$rotation
pc1$sdev
pc1$x

################################################################################

# Guardem els valors de la variança
varianza <- pc1$sdev^2
varianza
total_varianza <- sum(varianza)
total_varianza
perc_varianza <- 100*varianza/total_varianza

perc_varianza_acum <- 100*cumsum(varianza[1:ncol(data_numerical)])/ncol(data_numerical)
perc_varianza


# Trobem el número de componenets que expliquen el 80% de les dades
num_comp <- which(cumsum(perc_varianza) >= 80)[1]
num_comp

psi = pc1$x[, 1:num_comp] # ENS QUEDEM NOMÉS AMB LES DIMENSIONS QUE HEM TRIAT (9)
dim(psi)

data_psi <- as.data.frame(psi)
save(data_psi, file = "./6_Facorial_methods/acp_data.RData")
