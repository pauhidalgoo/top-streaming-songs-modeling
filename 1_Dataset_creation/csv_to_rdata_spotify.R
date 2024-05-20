setwd("C:/Users/Usuario/Documents/Universitat/4rt Quatri/PMAAD/Preprocessing")

data <- read.csv("spotify_new.csv", sep = ",", na.strings = "NA")

data <- subset(data, select = -c(X))

colnames(data)[c(42, 43, 44, 45, 46)] <- c("nationality", "city", "gender", "is_group", "lyrics")

data$week_index <- factor(data$week_index, levels = 1:221)

colnames(data)[which(names(data) == "mode")] <- "major_mode"

categorical_vars <- c("track_id", "track_name", "album_name", "album_type", "album_label", "artist_name", "key", 
							"time_signature", "day_release", "month_release", "year_release", "weekday_release", 
							"year_week", "month_week", "rank_group", "nationality", "city", "gender", "is_group")

for (col in categorical_vars) {
	data[[col]] <- as.factor(data[[col]])
}

logical_vars <- c("major_mode", "pop", "hip_hop", "rock", "electro", "christmas", "cinema", "latino", "collab", "explicit", "is_group")

for (col in logical_vars) {
	data[[col]] <- ifelse(data[[col]] == 1 | data[[col]]=="True", TRUE, FALSE)
	data[[col]] <- as.logical(data[[col]])
}

for (col in logical_vars) {
	data[[col]] <- as.logical(data[[col]])
}

save(data, file = "data.RData")

