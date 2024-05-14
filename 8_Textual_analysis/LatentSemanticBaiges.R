# Load required libraries
library(tm)
library(ggplot2)
library(lsa)
library(LSAfun)

# Load data
load("./2_Descriptive_analysis/unique_tracks.RData")
track_names <- unique_tracks$track_name
uncleaned_corpus <- Corpus(VectorSource(unique_tracks$lyrics))

# Display the initial content of the first document
writeLines(head(strwrap(uncleaned_corpus[[1]]), 7))

# Cleaning the corpus
clean_corpus <- tm_map(uncleaned_corpus, content_transformer(tolower))
clean_corpus <- tm_map(clean_corpus, removeNumbers)
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords("english"))
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords("SMART"))
clean_corpus <- tm_map(clean_corpus, removePunctuation)
clean_corpus <- tm_map(clean_corpus, stripWhitespace)

# Display the cleaned content of the first document
writeLines(head(strwrap(clean_corpus[[1]]), 7))

# Create a Term-Document Matrix
corpus <- clean_corpus
td.mat <- TermDocumentMatrix(corpus)
terms <- findFreqTerms(td.mat, lowfreq = 10)

# Keep TermDocumentMatrix filtered by frequent terms
td.mat <- td.mat[terms, ]

# Store the dictionary from the TermDocumentMatrix before any conversion
dictionary <- Terms(td.mat)

# Apply LSA
td.mat.lsa <- lw_bintf(as.matrix(td.mat)) * gw_idf(as.matrix(td.mat))
lsaSpace <- lsa(td.mat.lsa)

# Functions to process and project new documents, and find similar documents
process_and_project_new_document <- function(new_text, lsa_space, dictionary) {
  new_corpus <- Corpus(VectorSource(new_text))
  new_corpus <- tm_map(new_corpus, content_transformer(tolower))
  new_corpus <- tm_map(new_corpus, removePunctuation)
  new_corpus <- tm_map(new_corpus, removeNumbers)
  new_corpus <- tm_map(new_corpus, removeWords, stopwords("english"))
  new_corpus <- tm_map(new_corpus, stripWhitespace)
  
  # Check if the new corpus is empty after cleaning
  if (length(new_corpus[[1]]) == 0) {
    stop("Error: The input document has become empty after cleaning.")
  }
  
  new_td <- TermDocumentMatrix(new_corpus, control=list(dictionary=dictionary))
  if (nrow(as.matrix(new_td)) == 0) {
    stop("Error: There are no terms matching the original dictionary.")
  }
  
  new_td_mat <- as.matrix(new_td)
  new_td_lsa <- new_td_mat %*% diag(lsa_space$tk) %*% t(lsa_space$sk)
  return(new_td_lsa)
}

find_similar_documents <- function(new_td_lsa, lsa_results, track_names, top_n = 5) {
  similarities <- cosine(new_td_lsa, as.matrix(lsa_results))
  top_indices <- order(similarities, decreasing = TRUE)[1:top_n]
  return(list(tracks = track_names[top_indices], scores = similarities[top_indices]))
}

# Example usage of the functions with new text
new_text <- "I hate niggas and tits"
new_td_lsa <- process_and_project_new_document(new_text, lsaSpace, dictionary)
similar_documents <- find_similar_documents(new_td_lsa, as.textmatrix(lsaSpace), track_names, top_n = 5)
print(similar_documents)

# Continue with visualizations or additional analysis as required
