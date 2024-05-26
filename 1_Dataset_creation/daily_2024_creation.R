universal_top_spotify_songs <- read.csv("./7_Geoespacial/universal_top_spotify_songs.csv")


top5 <- universal_top_spotify_songs[universal_top_spotify_songs$daily_rank <= 5,]
thisyear <- top5[top5$snapshot_date >= "2024",]


length(unique(thisyear$name))


total_countries <- unique(thisyear$country)

print(total_countries)
library(sf)
library(dplyr)
library(leaflet)
library(gstat)
library(ggplot2)

countries <- giscoR::gisco_get_countries() %>% 
  st_centroid()
countries <- countries %>%
  mutate(lat = st_coordinates(geometry)[,2],
         lon = st_coordinates(geometry)[,1])

countries <- countries %>% rename(country = CNTR_ID)

countries <- countries[c("country", "lat", "lon")]

# Merge to add latitude and longitude
thisyear_with_coords <- thisyear %>%
  left_join(countries, by = "country")


extract_first_artist <- function(artist_string) {
  # Split the string by comma if there are multiple artists
  artists <- strsplit(artist_string, ",")[[1]]
  # Trim whitespace from each artist name
  artists <- trimws(artists)
  # Return the first artist name
  return(artists[1])
}

# Apply the function to the artist_names column
thisyear_with_coords$artists <- sapply(thisyear_with_coords$artists, extract_first_artist)



write.csv(thisyear_with_coords,"./1_Dataset_creation/data2024.csv", row.names = FALSE)



global <- universal_top_spotify_songs[universal_top_spotify_songs$country == "",]

length(unique(global$name))

write.csv(global,"./1_Dataset_creation/top50_global.csv", row.names = FALSE)

