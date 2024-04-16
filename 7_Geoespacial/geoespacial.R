library(sf)

my_sf <- st_read("./7_Geoespacial/world-administrative-boundaries.shp")
plot(st_geometry(my_sf))

library(ggplot2)
ggplot(my_sf) +
  geom_sf(fill = "#69b3a2", color = "black") +
  theme_void()
