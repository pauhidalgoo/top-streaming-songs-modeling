
# BASICALLY A COPY OF Latent Semantic Baiges


library(tm)
library(lsa)
library(LSAfun)
library(SnowballC)

load("./8_Textual_analysis/unique_tracks_translated.RData")
load("./8_Textual_analysis/unique_translated_2024.RData")

# Function to interpret user input and set filters
interpret_request <- function(request) {
  filters <- list()
  
  # Detect genre
  genre_keywords <- list(
    "pop" = "pop",
    "hip_hop" = "hip hop|rap",
    "rock" = "rock",
    "electro" = "electro|electrónica|electronic",
    "christmas" = "christmas",
    "cinema" = "cinema",
    "latino" = "latino|reggaetón|reggaeton"
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
  
  if (grepl("acoustic|acústica", request, ignore.case = TRUE)) {
    filters$acousticness <- 0.3
  }
  
  if (grepl("sad|triste", request, ignore.case = TRUE)) {
    filters$valence <- 0.4
  }
  
  # Detect explicit content
  if (grepl("no explicit|not explicit|no explícita|no explícito", request, ignore.case = TRUE)) {
    filters$explicit <- FALSE
  } else if (grepl("explicit|explícita|explícito", request, ignore.case = TRUE)) {
    filters$explicit <- TRUE
  }
  
  # Detect country
  unique_countries <- unique(unique_translated$nationality)
  for (country in unique_countries) {
    if (grepl(country, request, ignore.case = TRUE)) {
      filters$country <- country
      break
    }
  }
  
  # Detect artist
  unique_artists <- unique(unique_translated$artist_name)
  for (artist in unique_artists) {
    if (grepl(artist, request, ignore.case = TRUE)) {
      filters$artist <- artist
      break
    }
  }
  
  # Detect gender
  gender_keywords <- list(
    "female" = "chica|mujer|girl|woman|female|chicas|mujeres|girls|women|females",
    "male" = "chico|hombre|boy|man|male|chicos|hombres|boys|men|males",
    "non-binary" = "non-binary|no binario|nonbinary|no binarios"
  )
  
  for (gender in names(gender_keywords)) {
    if (grepl(gender_keywords[[gender]], tolower(request), ignore.case = TRUE)) {
      filters$gender <- gender
      break
    }
  }
  
  # Detect language
  language_keywords <- list(
    "en" = "english|inglés|ingles",
    "es" = "spanish|español|castellano|castellana",
    "de" = "german|alemán|aleman",
    "fr" = "french|francés|frances",
    "ko" = "korean|coreano|coreana",
    "ja" = "japanese|japonés|japones",
    "kri" = "krio",
    "pt" = "portuguese|portugués|portuguesa"
  )
  
  for (language in names(language_keywords)) {
    if (grepl(language_keywords[[language]], tolower(request), ignore.case = TRUE)) {
      filters$language <- language
      break
    }
  }
  
  if (grepl("2024", request, ignore.case = TRUE)) {
    filters$new_data <- TRUE
  } else {
    filters$new_data <- FALSE
  }
  
  return(filters)
}

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
perform_lsa <- function(phrase, request, n = 5) {
  # Interpret the user request to set filters
  filters <- interpret_request(request)
  
  if (filters$new_data == TRUE){
    print("yess")
    dataset <- combined_df
    
  } else {
    print("no")
    dataset <- unique_translated
    
  }
  uncleaned_corpus <- Corpus(VectorSource(dataset$lyrics))
  clean_corpus_data <- clean_corpus(uncleaned_corpus)

  # Extend the phrase to the minimum length
  phrase <- extend_phrase(phrase)
  
  # Add the new phrase to the corpus
  new_corpus <- Corpus(VectorSource(c(sapply(clean_corpus_data, as.character), phrase)))
  new_corpus <- clean_corpus(new_corpus)
  
  # Create the term-document matrix
  td.mat <- TermDocumentMatrix(new_corpus)
  min_freq <- ceiling(0.1 * length(dataset$track_name))
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
  
  # Create a data frame with similarities and track details
  similarity_df <- data.frame(
    track_name = dataset$track_name,
    track_id = dataset$track_id,
    artist_name = dataset$artist_name,
    acousticness = dataset$acousticness,
    valence = dataset$valence,
    similarity = similarities,
    danceability = dataset$danceability,
    explicit = dataset$explicit,
    country = dataset$nationality,
    gender = dataset$gender,
    language = dataset$lyrics_language,
    stringsAsFactors = FALSE
  )
  
  print(similarity_df[similarity_df$artist_name == "SZA", ])
  
  # Add genre columns to similarity_df
  genre_keywords <- list(
    "pop" = "pop",
    "hip_hop" = "hip hop|rap",
    "rock" = "rock",
    "electro" = "electro",
    "christmas" = "christmas",
    "cinema" = "cinema",
    "latino" = "latino|reggaetón|reggaeton"
  )
  
  for (genre in names(genre_keywords)) {
    similarity_df[[genre]] <- dataset[[genre]]
  }
  
  # Apply filters to the similarity data frame
  if (!is.null(filters$genre)) {
    similarity_df <- similarity_df[similarity_df[[filters$genre]] == TRUE, ]
  }
  
  if (!is.null(filters$danceability)) {
    similarity_df <- similarity_df[similarity_df$danceability >= filters$danceability, ]
  }
  
  if (!is.null(filters$acousticness)) {
    similarity_df <- similarity_df[similarity_df$acousticness >= filters$acousticness, ]
  }
  
  if (!is.null(filters$valence)) {
    similarity_df <- similarity_df[similarity_df$valence <= filters$valence, ]
  }
  
  if (!is.null(filters$explicit)) {
    similarity_df <- similarity_df[similarity_df$explicit == filters$explicit, ]
  }
  
  if (!is.null(filters$country)) {
    similarity_df <- similarity_df[similarity_df$country == filters$country, ]
  }
  
  if (!is.null(filters$artist)) {
    similarity_df <- similarity_df[similarity_df$artist_name == filters$artist, ]
  }
  
  if (!is.null(filters$gender)) {
    similarity_df <- similarity_df[similarity_df$gender == filters$gender, ]
  }
  
  if (!is.null(filters$language)) {
    similarity_df <- similarity_df[similarity_df$language == filters$language, ]
  }
  
  # Order by similarity and get top n
  similarity_df <- similarity_df[order(similarity_df$similarity, decreasing = TRUE), ]
  top_n_df <- head(similarity_df, n)
  
  llista <- list("documents" = top_n_df$track_name, "ids" = top_n_df$track_id) 
  return(llista)
}




library("spotifyr")


create_new_playlist <- function(track_ids, playlist_name, user_id) {
  print("hello, its me")
  # Authenticate with Spotify API
  my_token <- spotifyr::get_spotify_authorization_code(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                       client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                                       scope = 'playlist-modify-public playlist-read-private')
  my_token
  # Create a new playlist
  new_playlist <- create_playlist(user_id = user_id,
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