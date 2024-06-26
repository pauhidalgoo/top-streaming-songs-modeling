library(tm)
library(lsa)
library(LSAfun)
library(SnowballC)
library(stringr)

# Cargar el dataset
load("./8_Textual_analysis/unique_tracks_translated.RData")

# Function to clean the corpus
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c("tis"))
  return(corpus)
}

# Function to ensure minimum length of phrase
extend_phrase <- function(phrase, min_length = 1500) {
  while (nchar(phrase) < min_length) {
    phrase <- paste(phrase, phrase)
  }
  return(substr(phrase, 1, min_length))  # Ensure the phrase is exactly min_length
}

# Prepare LSA space for the initial corpus
prepare_lsa_space <- function() {
  corpus <- Corpus(VectorSource(unique_translated$lyrics))
  clean_corpus_data <- clean_corpus(corpus)
  track_names <- unique_translated$track_name
  td.mat <- TermDocumentMatrix(clean_corpus_data)
  min_freq <- ceiling(0.03 * length(track_names))
  terms_freq <- findFreqTerms(td.mat, lowfreq = min_freq)
  td.mat_freq <- td.mat[terms_freq, ]
  td.mat <- as.matrix(td.mat_freq)
  td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)
  lsaSpace <- lsa(td.mat.lsa)
  
  return(list(lsaSpace = lsaSpace, terms_freq = terms_freq, td.mat = td.mat))
}

lsa_prep <- prepare_lsa_space()

# Function to predict genre based on KNN using LSA
predict_genre <- function(song_lyrics, k = 5, lsa_prep) {
  # Preprocess the song lyrics
  song_lyrics <- extend_phrase(song_lyrics)
  
  # Combine the new song lyrics with the existing corpus terms
  combined_corpus <- Corpus(VectorSource(c(unique_translated$lyrics, song_lyrics)))
  clean_combined_corpus <- clean_corpus(combined_corpus)
  new_td.mat <- TermDocumentMatrix(clean_combined_corpus)
  new_td.mat <- as.matrix(new_td.mat[lsa_prep$terms_freq, ])
  
  # Perform LSA for the new term-document matrix
  new_td.mat.lsa <- lw_bintf(new_td.mat) * gw_idf(new_td.mat)
  new_lsaSpace <- lsa(new_td.mat.lsa)
  
  # Calculate similarity
  results <- as.textmatrix(new_lsaSpace)
  new_doc_vector <- results[, ncol(results)]
  similarities <- cosine(new_doc_vector, results[, -ncol(results)])
  
  # Get the top k most similar documents
  top_k <- order(similarities, decreasing = TRUE)[1:k]
  print(top_k)
  top_k_genres <- unique_translated[top_k, c("pop", "hip_hop", "rock", "electro", "christmas", "cinema", "latino")]
  # Remove 'pop' and 'hip_hop' if 'latino' or 'christmas' are present
  adjusted_top_k_genres <- top_k_genres
  adjusted_top_k_genres[,"pop"] <- adjusted_top_k_genres[,"pop"] & !(adjusted_top_k_genres[,"latino"] | adjusted_top_k_genres[,"christmas"])
  adjusted_top_k_genres[,"hip_hop"] <- adjusted_top_k_genres[,"hip_hop"] & !(adjusted_top_k_genres[,"latino"] | adjusted_top_k_genres[,"christmas"])
  
  # Double the count for 'latino' and 'christmas'
  genre_counts <- apply(adjusted_top_k_genres, 2, sum)
  genre_counts["latino"] <- genre_counts["latino"] * 2
  genre_counts["christmas"] <- genre_counts["christmas"] * 2
  # Predict the genre based on the most common genre among the top k similar songs
  predicted_genre <- names(genre_counts)[genre_counts == max(genre_counts)]
  return(predicted_genre[1])  # Return the first genre if there are ties
}

# Function to evaluate the model
evaluate_model <- function(test_lyrics, test_genres, k = 5, lsa_prep) {
  correct_predictions <- 0
  total_tests <- length(test_lyrics)
  
  for (i in 1:total_tests) {
    predicted_genre <- predict_genre(test_lyrics[i], k, lsa_prep)
    song_title <- unique_translated$track_name[test_indices[i]]
    print(paste("Title:", song_title, "| Predicted genre:", predicted_genre))
    
    if (test_genres[i, predicted_genre] == TRUE) {
      correct_predictions <- correct_predictions + 1
    }
    
    # Print progress
    progress <- (i / total_tests) * 100
    print(paste("Progress:", round(progress, 2), "% | Correct predictions:", correct_predictions))
  }
  
  accuracy <- correct_predictions / total_tests
  return(accuracy)
}

# Small test dataset
set.seed(42)  # For reproducibility
test_indices <- sample(1:nrow(unique_translated), 100)  # Randomly sample 100 songs
test_lyrics <- unique_translated$lyrics[test_indices]
test_genres <- unique_translated[test_indices, c("pop", "hip_hop", "rock", "electro", "christmas", "cinema", "latino")]

# Evaluate the model
accuracy <- evaluate_model(test_lyrics, test_genres, k = 3, lsa_prep)
print(paste("Accuracy:", accuracy))


pop_song <- "On a starry night, we danced all through the light.
Holding hands, feeling right, under the moon so bright.
We laughed and we sang, hearts intertwined,
In this moment so divine, forever you and I."
print(paste("SONG:", pop_song))
print(predict_genre(pop_song, 5, lsa_prep))

latino_song <- "Bajo la luna llena, bailamos sin parar,
Ritmo en nuestras venas, el mundo a celebrar.
Tus ojos me encantan, tu risa es un mar,
Juntos hasta el amanecer, no hay nada igual."
print(paste("SONG:", latino_song))
print(predict_genre(latino_song, 5, lsa_prep))

hip_hop_song <- "In the fucking streets, lights flashing, grinding every night,
Fuck the haters, I'm smashing, gotta keep it tight."
print(paste("SONG:", hip_hop_song))
print(predict_genre(hip_hop_song, 5, lsa_prep))


christmas_song <- "Snowflakes falling, carols in the air,
Family's calling, love everywhere.
Lights on the tree, gifts by the fire,
Joy, hearts full of desire."
print(paste("SONG:", christmas_song))
print(predict_genre(christmas_song, 5, lsa_prep))