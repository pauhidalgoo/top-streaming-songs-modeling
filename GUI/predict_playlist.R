library(tm)
library(lsa)
library(LSAfun)
library(SnowballC)

load("./2_Descriptive_analysis/unique_tracks.RData")

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

# Main LSA function
perform_lsa <- function(phrase, n = 5) {
  track_names <- unique_tracks$track_name
  uncleaned_corpus <- Corpus(VectorSource(unique_tracks$lyrics))
  
  # Clean the corpus
  clean_corpus_data <- clean_corpus(uncleaned_corpus)
  
  # Extend the phrase to the minimum length
  phrase <- extend_phrase(phrase)
  
  # Add the new phrase to the corpus
  new_corpus <- Corpus(VectorSource(c(sapply(clean_corpus_data, as.character), phrase)))
  new_corpus <- clean_corpus(new_corpus)
  
  # Create the term-document matrix
  td.mat <- TermDocumentMatrix(new_corpus)
  min_freq <- ceiling(0.1 * length(track_names))
  terms_freq_10 <- findFreqTerms(td.mat, lowfreq = min_freq)
  td.mat_freq_10 <- td.mat[terms_freq_10, ]
  td.mat <- as.matrix(td.mat_freq_10)
  
  # Perform LSA
  td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)
  lsaSpace <- lsa(td.mat.lsa)
  
  # Calculate similarity
  results <- as.textmatrix(lsaSpace)
  new_doc_vector <- results[, ncol(results)]
  similarities <- cosine(new_doc_vector, results[, -ncol(results)])
  
  # Get top n most similar documents
  top_n <- order(similarities, decreasing = TRUE)[1:n]
  similar_docs <- track_names[top_n]
  similar_ids <- unique_tracks$track_id[top_n]
  llista <- list("documents" = similar_docs, "ids" = similar_ids) 
  return(llista)
}