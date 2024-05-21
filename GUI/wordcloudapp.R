library(shiny)
library(leaflet)
source("./GUI/api_configuration.R")
source("./GUI/global.R")
source("./GUI/predict_playlist.R")
source("./GUI/predict_genre.R")
source("./GUI/geospatial_example.R")

# Define UI
source("./GUI/ui.R")

# Define server logic
source("./GUI/server.R")

plot.new()
# Run the application
shinyApp(ui = ui, server = server)

