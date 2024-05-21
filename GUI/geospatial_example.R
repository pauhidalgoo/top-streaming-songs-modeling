library(sf)
genre_columns <- c("pop", "hip_hop", "rock", "electro", "christmas", "cinema", "latino")

artists_data <- readRDS("./7_Geoespacial/artists_data.rds")

unique_artists_images <- images_unique %>%
  group_by(artist_name) %>%
  summarize(artist_image = first(artist_img), .groups = 'drop')

artists_data <- artists_data %>%
  left_join(unique_artists_images, by = "artist_name")

