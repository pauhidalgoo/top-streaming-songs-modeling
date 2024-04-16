library(sf)

my_sf <- read_sf('./7_Geoespacial/world-administrative-boundaries.shp')
plot(st_geometry(my_sf), col = "#f2f2f2", bg = "skyblue", lwd = 0.25, border = 0)

library(ggplot2)
ggplot(my_sf) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()
