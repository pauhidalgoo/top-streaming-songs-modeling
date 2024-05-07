library(sf)

my_sf <- st_read("./7_Geoespacial/ne_10m_populated_places.shp")
plot(st_geometry(my_sf))

library(ggplot2)
ggplot(my_sf) +
  geom_sf(fill = "#69b3a2", color = "red") +
  theme_void()

ggplot(my_sf) +
  geom_sf(fill = "#69b3a2", color = "red") +  # Draw the points or other geometries
  geom_sf_text(aes(label = NAME), color = "black", size = 2, nudge_y = 0.1) +  # Add city names
  theme_void()

# ------------------------------------------------------------------------------
if(!require(tidygeocoder)) install.packages("tidygeocoder")
if(!require(sf)) install.packages("sf")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(leaflet)) install.packages("leaflet")

library(tidygeocoder)
library(sf)
library(ggplot2)
library(leaflet)


data$full_address <- paste(data$city, data$country, sep = ", ")
data_coords <- geocode(data, address = full_address, method = "osm")

data_sf <- st_as_sf(data_coords, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

st_write(data_sf, "./7_Geoespacial/cities_shapefile.shp")

# ------------------------------------------------------------------------------

world <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = data_sf, color = "red", size = 3) +
  theme_minimal()

map <- leaflet(data_sf) %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(city), label = ~as.character(city)) %>%
  setView(lng = mean(range(data_sf$longitude, na.rm = TRUE)), 
          lat = mean(range(data_sf$latitude, na.rm = TRUE)), zoom = 3)

map  # Mostrar el mapa

map <- leaflet(data_sf) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addAwesomeMarkers(~longitude, ~latitude, icon = icons(icon = "flag", iconColor = "white", markerColor = "red"),
                    popup = ~paste("<strong>City:</strong>", city, "<br><strong>Country:</strong>", country)) %>%
  addScaleBar(position = "bottomleft") %>%
  setView(lng = mean(range(data_sf$longitude, na.rm = TRUE)), 
          lat = mean(range(data_sf$latitude, na.rm = TRUE)), zoom = 3)

map



