ui <- navbarPage(
  "Multi-page App",
  
  tabPanel("Word Cloud",
           sidebarLayout(
             sidebarPanel(
               selectInput("selection", "Choose a book:",
                           choices = books),
               actionButton("update", "Change"),
               hr(),
               sliderInput("freq",
                           "Minimum Frequency:",
                           min = 1,  max = 50, value = 15),
               sliderInput("max",
                           "Maximum Number of Words:",
                           min = 1,  max = 300,  value = 100)
             ),
             
             mainPanel(
               plotOutput("plot"),
               uiOutput("artist_image")
             )
           )
  ),
  
  tabPanel("Text Input",
           sidebarLayout(
             sidebarPanel(
               textInput("text", "Enter text:"),
               actionButton("playlist", "Create")
             ),
             
             mainPanel(
               uiOutput("playlist_output")
             )
           )
  )
)