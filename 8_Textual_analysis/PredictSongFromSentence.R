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

# Example usage
phrase1 <- "Me gustas mucho pero la relación no puede seguir"
phrase2 <- "Baila conmigo toda la noche, bajo la luna llena y las estrellas brillantes."
phrase3 <- "Late at night, the streets are alive with the fucking pulse of the city. 
            The bass hits hard as hell, and the beat is relentless, pounding like my
            heartbeat. I'm out here, grinding, spitting raw-ass bars that tell my
            fucked-up story. Fuck the haters, fuck the bullshit, I'm rising above all
            that shit. The struggle is fucking real, but so is my motherfucking hustle. 
            Every verse is a big fuck you to the doubters, every rhyme a testament to my
            grind. This ain't just music, it's a goddamn way of life. In the concrete jungle,
            you either make it or you fucking don't. No bullshit, no excuses, just raw, unfiltered
            truth. I'm here to claim my spot, and ain't nobody gonna fucking stop me. 
            The mic is my weapon, and every word is fucking loaded. This is hip hop, 
            motherfucker. It's gritty, it's real, it's raw as fuck, and it's unapologetically me. 
            This is my fucking anthem, my story told in rhymes and beats, a middle finger 
            to the world, a declaration of my fucking existence. Every beat is a battle, 
            every rhyme a war cry. This is hip hop, and I'm here to fucking conquer it."


print(phrase1)
top_similar_docs <- perform_lsa(phrase1, n=30)
top_similar_ids1 <- top_similar_docs$ids
top_similar_docs <- top_similar_docs$documents
print(top_similar_docs)

print(phrase2)
top_similar_docs <- perform_lsa(phrase2, n = 30)
top_similar_ids2 <- top_similar_docs$ids
top_similar_docs <- top_similar_docs$documents
print(top_similar_docs)

print(phrase3)
top_similar_docs <- perform_lsa(phrase3, n = 30)
top_similar_ids3 <- top_similar_docs$ids
top_similar_docs <- top_similar_docs$documents
print(top_similar_docs)


# TO SPOTIFY PLAYLIST USING API -------------------------------

library("spotifyr")


# Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXX')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXX')


create_new_playlist <- function(track_ids, playlist_name) {
  # Authenticate with Spotify API
  my_token <- spotifyr::get_spotify_authorization_code(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                   client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                                   scope = 'playlist-modify-public playlist-read-private')
  my_token
  # Create a new playlist
  new_playlist <- create_playlist(user_id = "31kej347oroci5uq32dpg6wpnpae",
                                  name = playlist_name,
                                  public = TRUE,
                                  collaborative = FALSE,
                                  description = "Playlist created with LSA for PMAAD",
                                  authorization  = my_token)
  
  # Add tracks to the playlist
  add_tracks_to_playlist(playlist_id = new_playlist$id,
                         uris = track_ids,
                         position = NULL,
                         authorization = my_token)
  
  cat("Playlist", playlist_name, "created successfully!\n")
}
create_new_playlist(top_similar_ids1, "Dejarnos de querer")

create_new_playlist(top_similar_ids2, "Bailando de noche")

create_new_playlist(top_similar_ids3, "Hip hop life")

