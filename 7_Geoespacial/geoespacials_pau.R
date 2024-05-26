# DESCRIPTIVA ------------------------------------------------------------------

load("./7_Geoespacial/2024_top5_countries.RData")

names(data2024)
#install.packages(c("ggplot2", "sf", "leaflet", "tidygeocoder"))
library(ggplot2)
library(sf)
library(leaflet)
library(tidygeocoder)
library(sf)
library(tmap)
library(leaflet)
library(gifski)
library(tidyverse)

# Create a leaflet map
popularity_by_country <- data2024 %>%
  group_by(country) %>%
  summarise(avg_popularity = mean(popularity, na.rm = TRUE))

# Load world spatial dataset
#world <- st_read(system.file("./7_Geoespacial/countries_map.shp", package = "maptools"))

world_cities <- read_sf(dsn = "./7_Geoespacial", layer = "countries_map")
ggplot() +
  geom_sf(data = world_cities, fill = "white", color = "black") +
  theme_minimal() +
  labs(title = "Mapa del món")

popularity_by_country

# Merge popularity data with world spatial dataset
world <- left_join(world_cities, popularity_by_country, by = c("iso_3166_1_" = "country"))

# Plot choropleth map of average Spotify popularity by country
tm_shape(world) +
  tm_borders() +
  tm_fill("avg_popularity", palette = "Blues", style = "quantile") +
  tm_layout(title = "Average Spotify Popularity by Country")

# For interactive map using leaflet, you can do something like this:
# Assuming your data2024 has latitude and longitude columns named "lat" and "lon"
data2024$snapshot_date <- as.Date(data2024$snapshot_date)

# Create tmap object

noglobal <- na.omit(data2024)

data2024_sf <- st_as_sf(noglobal, coords = c("lon", "lat"), crs = 4326)

# Create frames for animation
frames <- tm_shape(data2024_sf) +
  tm_dots(col = "popularity", size = 0.5) +
  tm_facets(along = "snapshot_date")

# Create animation
tmap_animation(frames, filename = "./7_Geoespacial/paugifpopularity.gif", delay = 2)

# Create leaflet map



top_songs <- data2024 %>%
  filter(snapshot_date == as.Date("2024-05-22")) %>%
  group_by(country) %>%
  filter(daily_rank == 1) %>%
  ungroup()

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = top_songs, ~lon, ~lat, popup = ~name)


top_songs <- na.omit(top_songs)

# Plotting with ggplot2
ggplot(data = top_songs) +
  geom_sf(aes(geometry = geometry, color = name), size = 3) +
  labs(title = "Top Songs for Each Country on 2024-05-22", color = "Song Name") +
  theme_minimal()


world <- st_as_sf(rworldmap::getMap(resolution = "low"))

# Join data with world map
popularity_data <- data2024 %>%
  group_by(country) %>%
  summarize(avg_popularity = mean(popularity))

world <- world %>%
  left_join(popularity_data, by = c("ISO_A3" = "country"))

# Plotting
ggplot(data = world) +
  geom_sf(aes(fill = avg_popularity), color = NA) +
  scale_fill_viridis_c(na.value = "white") +
  labs(title = "Heatmap of Song Popularity by Region", fill = "Average Popularity") +
  theme_minimal()


last_day <- data2024[data2024$snapshot_date=="2024-05-22",]


leaflet(last_day) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = ~paste("Song:", name, "<br>",
                                              "Artist:", artists, "<br>",
                                              "Popularity:", popularity),
                   color = ~ifelse(is_explicit, "red", "blue"), radius = 5,
                   fillOpacity = 0.7) %>%
  addLegend("bottomright", colors = c("red", "blue"), labels = c("Explicit", "Non-Explicit"),
            title = "Song Type")


load('./7_Geoespacial/mean_new_data.RData')

mean_new_data$popularity
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = mean_new_data, ~lon, ~lat, popup = ~country)


new_world <- left_join(world_cities, mean_new_data, by = c("iso_3166_1_" = "country"))

tm_shape(new_world) +
  tm_borders() +
  tm_fill("instrumentalness", palette = "Blues", style = "quantile") +
  tm_layout(main.title = "Average Spotify Popularity by Country", frame=FALSE, legend.outside = TRUE)

ggplot() +
  geom_sf(data = new_world, color = "black", mapping= aes(fill=energy)) +
  theme_minimal() +
  labs(title = "Mapa del món")

ggplot(data = new_world) +
  geom_sf(aes(fill = energy), color = "grey") +
  scale_fill_viridis_c(option = "C", na.value = "grey", guide = "colorbar") +
  labs(title = "Mapa de Calor de Energy per País",
       fill = "Avg Energy") +
  theme_minimal()


?aes

?tm_layout


?leaflet
m <- leaflet() %>% addTiles()
m 



