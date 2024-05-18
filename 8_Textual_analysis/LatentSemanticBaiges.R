library(tm)
library(lsa)
library(LSAfun)
library(SnowballC)
library(stringr)

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

# Function to interpret user input and set filters
interpret_request <- function(request) {
  filters <- list()
  
  # Detect genre
  genre_keywords <- list(
    "pop" = "pop",
    "hip hop" = "hip_hop|rap",
    "rock" = "rock",
    "electro" = "electro|electrónica|electronic",
    "christmas" = "christmas",
    "cinema" = "cinema",
    "latino" = "latino"
  )
  
  for (genre in names(genre_keywords)) {
    if (grepl(genre_keywords[[genre]], tolower(request), ignore.case = TRUE)) {
      filters$genre <- genre
      break
    }
  }
  
  # Detect danceability
  if (grepl("very danceable|muy bailable", request, ignore.case = TRUE)) {
    filters$danceability <- 0.8
  } else if (grepl("danceable|bailable", request, ignore.case = TRUE)) {
    filters$danceability <- 0.65
  }
  
  return(filters)
}

# Main LSA function with genre and danceability filtering
perform_lsa <- function(phrase, request, n = 5) {
  # Interpret the user request to set filters
  filters <- interpret_request(request)
  
  # Apply genre filter to the dataset
  filtered_tracks <- unique_tracks
  if (!is.null(filters$genre)) {
    filtered_tracks <- filtered_tracks[filtered_tracks[[filters$genre]] == TRUE, ]
  }
  
  # Apply danceability filter to the dataset
  if (!is.null(filters$danceability)) {
    filtered_tracks <- filtered_tracks[filtered_tracks$danceability >= filters$danceability, ]
  }
  
  # If no tracks match the filters, return empty results
  if (nrow(filtered_tracks) == 0) {
    return(list("documents" = character(0), "ids" = character(0)))
  }
  
  # Update the corpus and track names with the filtered data
  track_names <- filtered_tracks$track_name
  clean_corpus_data <- Corpus(VectorSource(filtered_tracks$lyrics))
  clean_corpus_data <- clean_corpus(clean_corpus_data)
  
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
  similar_ids <- filtered_tracks$track_id[top_n]
  llista <- list("documents" = similar_docs, "ids" = similar_ids) 
  return(llista)
}

# Example usage
phrase1 <- "Me gustas mucho pero la relación no puede seguir"
phrase2 <- "Baila conmigo toda la noche, bajo la luna llena y las estrellas brillantes."
phrase3 <- "Late at night, the streets are alive with the fucking pulse of the city. The bass hits hard as hell, and the beat is relentless, pounding like my heartbeat. I'm out here, grinding, spitting raw-ass bars that tell my fucked-up story. Fuck the haters, fuck the bullshit, I'm rising above all that shit. The struggle is fucking real, but so is my motherfucking hustle. Every verse is a big fuck you to the doubters, every rhyme a testament to my grind. This ain't just music, it's a goddamn way of life. In the concrete jungle, you either make it or you fucking don't. No bullshit, no excuses, just raw, unfiltered truth. I'm here to claim my spot, and ain't nobody gonna fucking stop me. The mic is my weapon, and every word is fucking loaded. This is hip hop, motherfucker. It's gritty, it's real, it's raw as fuck, and it's unapologetically me. This is my fucking anthem, my story told in rhymes and beats, a middle finger to the world, a declaration of my fucking existence. Every beat is a battle, every rhyme a war cry. This is hip hop, and I'm here to fucking conquer it."

request1 <- "Quiero una canción del género latino que sea muy bailable"
request2 <- "Quiero una canción del género latino bailable"
request3 <- "Quiero una canción hip hop"

print(phrase1)
top_similar_docs <- perform_lsa(phrase1, request1, n=30)
top_similar_ids1 <- top_similar_docs$ids
top_similar_docs <- top_similar_docs$documents
print(top_similar_docs)

print(phrase2)
top_similar_docs <- perform_lsa(phrase2, request2, n = 30)
top_similar_ids2 <- top_similar_docs$ids
top_similar_docs <- top_similar_docs$documents
print(top_similar_docs)

print(phrase3)
top_similar_docs <- perform_lsa(phrase3, request3, n = 30)
top_similar_ids3 <- top_similar_docs$ids
top_similar_docs <- top_similar_docs$documents
print(top_similar_docs)

