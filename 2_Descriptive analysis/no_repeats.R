library(dplyr)

load("./3_Preprocessing/data_knn_imputed_unknown.RData")
unique_tracks <- data %>%
  group_by(track_name) %>%
  slice(1) %>%
  ungroup()

save(unique_tracks, file = "./2_Descriptive analysis/unique_tracks.RData")


unique_artists <- data %>%
  group_by(artist_name) %>%
  slice(1) %>%
  ungroup()

save(unique_artists, file = "./2_Descriptive analysis/unique_artists.RData")
