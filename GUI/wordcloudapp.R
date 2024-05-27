library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(sf)
source("./GUI/api_configuration.R")
source("./GUI/global.R")
source("./GUI/predict_playlist.R")
source("./GUI/predict_genre.R")
source("./GUI/geospatial_example.R")
source("./GUI/geospatial_predict.R")
# Define UI
source("./GUI/ui.R")

# Define server logic
source("./GUI/server.R")

plot.new()
# Run the application
shinyApp(ui = ui, server = server)

