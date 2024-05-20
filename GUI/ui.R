ui <- navbarPage(
  "Spotify PMAAD App",
  
  tabPanel("Word Cloud",
           sidebarLayout(
             sidebarPanel(
               selectInput("selection", "Choose a song:",
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
  
  tabPanel("Playlist Creator",
           sidebarLayout(
             sidebarPanel(
               textInput("text", "Enter text:"),
               textInput("request", "Enter some requeriments:"),
               actionButton("playlist", "Create"),
               textInput("user_id", "Enter your user ID:"),
               textInput("playlist_name", "Enter the playlist name:"),
               actionButton("spotify", "Add to Spotify"),
               uiOutput("spotify_done")
             ),
             
             mainPanel(
               uiOutput("playlist_output")
             )
           )
  ),
  tabPanel("Genre prediction",
           sidebarLayout(
             sidebarPanel(
               uiOutput("genre_output")
               
             ),
             
             mainPanel(
               tags$style(type="text/css", "textarea {width:95%; height: 50vh; resize:none;}") ,
               tags$textarea(id="genre_text", rows=5,placeholder =  "Add the lyrics of the song", ""), 
               actionButton("genre", "Predict genre")
             )
           )
  ),
  
  tags$head(
    tags$style(HTML("
      #map {
        height: calc(100vh - 80px) !important;
      }
    "))
  ),
  
  tabPanel("Artists locations",
           sidebarLayout(
             sidebarPanel(
                 h4("Filter by Genre"),
                 checkboxGroupInput("genres", "Genres:",
                                    choices = genre_columns,
                                    selected = genre_columns)  # Default to all selected
               
             ),
             
             mainPanel(
               leafletOutput("map")
             )
           )
  )
)