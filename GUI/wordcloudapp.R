library(shiny)
source("./GUI/global.R")
source("./GUI/predict_playlist.R")

# Define UI
source("./GUI/ui.R")

# Define server logic
source("./GUI/server.R")

plot.new()
# Run the application
shinyApp(ui = ui, server = server)

