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
    
}
