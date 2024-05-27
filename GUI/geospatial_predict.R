load("GUI/allkrigeresults.RData")
load("GUI/unique_images.RData")

scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

#scale values in 'sales' column to be between 0 and 1
allkrige$energy <- scale_values(allkrige$energy)
allkrige$danceability <- scale_values(allkrige$danceability)
allkrige$valence <- scale_values(allkrige$valence)
allkrige$track_popularity <- scale_values(allkrige$track_popularity) * 100




find_similar_song <- function(kriged_point, images_unique) {
  
  kriged_values <- as.numeric(as.data.frame(kriged_point)[, 1:4])
  
  distances <- apply(images_unique[, c("energy", "danceability", "valence", "track_popularity")], 1, function(row) {
    sqrt(sum((row - kriged_values)^2))
  })
  images_unique[which.min(distances), "track_name"]
}
