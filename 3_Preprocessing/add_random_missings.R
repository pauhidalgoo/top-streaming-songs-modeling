load("./data.RData")

# Variables per afegir NA
names_vars_add_na <- c("danceability", "speechiness", "duration")

# Comprovar NA inicial
cat("Missing data de tota la taula -->", sum(is.na(data)), ";", (sum(is.na(data)) / (ncol(data) * nrow(data))) * 100, "%." )
cat("Missing data de cada una de les variables:")
for ( col in names_vars_add_na ) {
	na_variable <- (sum(is.na(data[col])) / nrow(data)) * 100
	cat(col, "-->", sum(is.na(data[col])), ";", na_variable, "%.\n")
}

# Seed per poder-ho replicar
set.seed(33)

indexs_to_remove <- c()

# Bucle per escollir el percentatge de NA a afegir a cada variable
for (col in names_vars_add_na){
	perc_add_na <- sample(3:6, 1) # Entre 3% i 6% de NA afegir per cada variable
	
	# Bucle per afegir NA a les variables fins que s'assoleixi el percentatge determinat prèviament
	while ( ((sum(is.na(data[, col])) / nrow(data)) * 100) < perc_add_na ){
		index_na <- sample(1:nrow(data), 1)
		
		# Afegir NA a totes les instàncies de la mateixa cançó
		track_id_na <- data$track_id[index_na]
		
    indexs_same_track_id <- which(data$track_id == track_id_na)
		
		data[indexs_same_track_id, col] <- NA
		
		indexs_to_remove <- c(indexs_to_remove, indexs_same_track_id[-1]) # Mantindrem només 1 instància de la canço amb NA
		
	}
}

# Comporvem que s'han inserit els NA correctament
cat("Missing data de tota la taula -->", sum(is.na(data)), ";", (sum(is.na(data)) / (ncol(data) * nrow(data))) * 100, "%." )
cat("Missing data de cada una de les variables:")
for ( col in names_vars_add_na ) {
	na_variable <- (sum(is.na(data[col])) / nrow(data)) * 100
	cat(col, "-->", sum(is.na(data[col])), ";", na_variable, "%.\n")
}

data_unique_missings <- data # Dataset amb un únic valor per cada NA (s'eliminaran totes les altres instàncies de les variables amb NA)

data_unique_missings <- data[-indexs_to_remove, names_vars_add_na]

# Test de Little per comprovar que s'hagin afegit correctament els NA
if (!requireNamespace("naniar", quietly = TRUE)) {
  install.packages("naniar")
}

library(naniar)

# Falta agrupar per track_id
result_mcar_test <- naniar::mcar_test(data_unique_missings)

result_mcar_test

# Guardar la nova base de dades
save(data, file = "./data_na_added.RData")
