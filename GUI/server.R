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
    selected_track <- images_unique[images_unique$track_name == input$selection, ]
    img_path <- selected_track$artist_img
    if (!is.null(img_path)) {
      img_tag <- tags$img(src = img_path, width = "50%")
      img_tag
    }
    else{
      img_tag <- tags$img(src = "./GUI/error.png", width = "50%")
      img_tag
    }
  })
  
  playlist <- reactive({
    # Change when the "playlist" button is pressed...
    input$playlist
    # ...but not for anything else
    req(input$playlist)
    isolate({
      withProgress({
        setProgress(message = "Creating playlist...")
        print(input$text)
        top_similar_docs <- perform_lsa(input$text, n=30)
        top_similar_ids1 <- top_similar_docs$ids
        top_similar_docs <- top_similar_docs$documents
        print(top_similar_docs)
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
}

