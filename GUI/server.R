server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  dev.off()
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  output$artist_image <- renderUI({
    v <- terms()
    selected_track <- images_unique[images_unique$track_name == input$selection, ]
    img_path <- selected_track$artist_img
    if (!is.null(img_path) && img_path != " ") {
      img_tag <- tags$img(src = img_path, width = "50%")
    } else {
      img_tag <- tags$img(src = "./GUI/error.png", width = "50%")
    }
    tags$div(style = "text-align: center;", img_tag)
  })

  
  playlist <- reactive({
    # Change when the "playlist" button is pressed...
    input$playlist
    
    # ...but not for anything else
    req(input$playlist)
    isolate({
      withProgress({
        setProgress(message = "Creating playlist...")
        top_similar_docs <- perform_lsa(input$text, input$request, n=30)
        top_similar_ids1 <- top_similar_docs$ids
        saveRDS(top_similar_ids1, "./GUI/playlist_ids.rds")
        top_similar_docs <- top_similar_docs$documents
        top_similar_ids1
      })
    })
  })
  
  output$playlist_output <- renderUI({
    playlist_data <- playlist()  # Assuming playlist() returns a list of track indices
    
    playlist_items <- lapply(playlist_data, function(track_id) {
      # Fetch track information using the track index
      track_info <- track_info_from_index(track_id)  # You need to implement this function
      
      # Create HTML elements for displaying track info
      print(track_info)
      track_name <- tags$h2(track_info$track_name)
      artist_name <- tags$p(track_info$artist_name)
      album_image <- tags$img(src = track_info$album_img, width = "50%")
      
      # Combine all elements into a div for each track
      tags$div(
        track_name,
        artist_name,
        album_image,
        style = "margin-bottom: 20px;"  # Add some space between tracks
      )
    })
    
    # Wrap all track divs into a single container
    playlist_container <- tags$div(
      playlist_items,
      style = "overflow-y: scroll; max-height: 600px;"  # Add scroll for overflow
    )
    
    # Return the playlist container
    playlist_container
  })
  
  spotify_publish <- reactive({
    # Change when the "playlist" button is pressed...
    input$spotify
    # ...but not for anything else
    req(input$spotify)
    playlist_data <- readRDS("./GUI/playlist_ids.rds")
    isolate({
      withProgress({
        setProgress(message = "Publishing playlist to spotify...")
        print(input$playlist_name)
        create_new_playlist(playlist_data, input$playlist_name, input$user_id)
      })
    })
    
  })
  
  output$spotify_done <- renderUI({
    a <- spotify_publish()
    tags$h3("Playlist created successfully!")
  })
  
  
  genre <- reactive({
    # Change when the "playlist" button is pressed...
    input$genre
    
    # ...but not for anything else
    req(input$genre)
    isolate({
      withProgress({
        setProgress(message = "Predicting genre...")
        print(input$genre_text)
        predicted_genre <- predict_genre(input$genre_text, 5, lsa_prep)
        predicted_genre
      })
    })
  })
  
  output$genre_output <- renderUI({
    genre_name <- genre()  # Assuming playlist() returns a list of track indices
    tags$h2(genre_name)
  })
  
  
  filtered_data <- reactive({
    req(input$genres)  # Ensure there is at least one genre selected
    
    filtered_df <- artists_data %>%
      filter_at(vars(input$genres), any_vars(. == TRUE))
    
  })
  
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    
    leafIcons <- icons(
      iconUrl = data$artist_image,
      iconWidth = 50, iconHeight = 50
    )
    
    
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(~longitude, ~latitude, popup = ~as.character(city), label = ~as.character(artist_name), clusterOptions = markerClusterOptions(), icon = leafIcons) %>%
      setView(lng = mean(range(data$longitude, na.rm = TRUE)), 
              lat = mean(range(data$latitude, na.rm = TRUE)), zoom = 3)
  })
  
  output$new_map <- renderLeaflet({
    leaflet(as.data.frame(allkrige)) %>% 
      addTiles() %>% 
      addCircleMarkers(data = allkrige, 
                       lng = ~allkrige$coords.x1, lat = ~allkrige$coords.x2,
                       layerId = ~paste(allkrige$coords.x1, allkrige$coords.x2, sep="_"),
                       
                       clusterOptions = markerClusterOptions())
    
  })
  
  observeEvent(input$new_map_marker_click, {
    click <- input$new_map_marker_click
    coords_click <- as.numeric(unlist(strsplit(click$id, "_")))
    clicked_point <- allkrige[allkrige$coords.x1 == coords_click[1] & allkrige$coords.x2 == coords_click[2], ]
    
    selected_song <- find_similar_song(clicked_point, images_unique)
    output$kriged_song <- renderUI({
      selected_track <- df_filled[df_filled$track_name == selected_song, ]
      print(selected_track)
      img_path <- selected_track$album_img
      print(img_path)
      if (!is.null(img_path)) {
        img_tag <- tags$img(src = img_path, width = "50%")
      } else {
        img_tag <- tags$img(src = "./GUI/error.png", width = "50%")
      }
      text_tag <- tags$h2(selected_song)
      tags$div(style = "text-align: center;", text_tag,img_tag)
    })
  })
}

