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
  
  # Remove 'pop' if 'latino' or 'christmas' are present
  adjusted_top_k_genres <- top_k_genres
  adjusted_top_k_genres[,"pop"] <- adjusted_top_k_genres[,"pop"] & !(adjusted_top_k_genres[,"latino"] | adjusted_top_k_genres[,"christmas"])
  
  # Predict the genre based on the most common genre among the top k similar songs
  predicted_genre <- colnames(adjusted_top_k_genres)[apply(adjusted_top_k_genres, 2, sum) == max(apply(adjusted_top_k_genres, 2, sum))]
  return(predicted_genre[1])  # Return the first genre if there are ties
}