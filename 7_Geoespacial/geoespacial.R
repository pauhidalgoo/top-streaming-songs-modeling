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
