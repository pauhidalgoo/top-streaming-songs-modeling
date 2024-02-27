setwd("C:/Users/Cai Selvas Sala/GIA_UPC/2nC/2nQ/PMAAD")

load("data.RData")

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

# Bucle per escollir el percentatge de NA a afegir a cada variable
for (col in names_vars_add_na){
	perc_add_na <- sample(3:6, 1)
	
	# Bucle per afegir NA a les variables fins que s'assoleixi el percentatge determinat prèviament
	while ( ((sum(is.na(data[, col])) / nrow(data)) * 100) < perc_add_na ){
		index_na <- sample(1:nrow(data), 1)
		
		# Afegir NA a totes les instàncies de la mateixa cançó
		track_id_na <- data$track_id[index_na]
		
		data[which(data$track_id == track_id_na), col] <- NA
	}
}

# Comporvem que s'han inserit els NA correctament
cat("Missing data de tota la taula -->", sum(is.na(data)), ";", (sum(is.na(data)) / (ncol(data) * nrow(data))) * 100, "%." )
cat("Missing data de cada una de les variables:")
for ( col in names_vars_add_na ) {
	na_variable <- (sum(is.na(data[col])) / nrow(data)) * 100
	cat(col, "-->", sum(is.na(data[col])), ";", na_variable, "%.\n")
}

# Guardar la nova base de dades
save(data, file = "data_na_added.RData")
