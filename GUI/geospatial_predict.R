load("GUI/allkrigeresultstopics.RData")
load("GUI/unique_images.RData")
load("7_Geoespacial/artists_with_topics.RData")

merged_data <- left_join(images_unique, data, by = "track_name")

df_filled <- merged_data %>%
  group_by(city) %>%
  fill(starts_with("topic"), .direction = "downup") %>%
  fill("latitude", .direction = "downup") %>%
  fill("longitude", .direction = "downup") %>%
  ungroup()


df_filled <- df_filled[ , !(names(df_filled) == "energy.y")]

df_filled <- na.omit(df_filled)


scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

#scale values in 'sales' column to be between 0 and 1
allkrige$energy <- scale_values(allkrige$energy)
allkrige$danceability <- scale_values(allkrige$danceability)
allkrige$valence <- scale_values(allkrige$valence)
allkrige$track_popularity <- scale_values(allkrige$track_popularity) * 100

allkrige$topic1 <- scale_values(allkrige$topic1)
allkrige$topic2 <- scale_values(allkrige$topic2)
allkrige$topic3 <- scale_values(allkrige$topic3)
allkrige$topic4 <- scale_values(allkrige$topic4)
allkrige$topic5 <- scale_values(allkrige$topic5)
allkrige$topic6 <- scale_values(allkrige$topic6)







find_similar_song <- function(kriged_point, images_unique) {
  
  kriged_values <- as.numeric(as.data.frame(kriged_point)[, 1:9])
  print(kriged_values)
  
  distances <- apply(df_filled[, c( "topic1", "topic2", "topic3", "topic4","topic5","topic6", "energy.x", "danceability", "valence")], 1, function(row) {
    sqrt(sum((row - kriged_values)^2))
  })
  df_filled[which.min(distances), "track_name"]
}
