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