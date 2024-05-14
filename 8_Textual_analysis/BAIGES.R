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

# Main LSA function
perform_lsa <- function(phrase, n = 5) {
  track_names <- unique_tracks$track_name
  uncleaned_corpus <- Corpus(VectorSource(unique_tracks$lyrics))
  
  # Clean the corpus
  clean_corpus_data <- clean_corpus(uncleaned_corpus)
  
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
  
  return(similar_docs)
}

# Example usage
phrase <- "Ayy I'm tryna put you in the worst mood, ah P1 cleaner than your church shoes, ah Milli point two just to hurt you, ah All red Lamb' just to tease you, ah None of these toys on lease too, ah Made your whole year in a week too, yah Main bitch outta your league too, ah Side bitch outta your league too, ah House so empty, need a centerpiece Twenty racks a table, cut from ebony Cut that ivory into skinny pieces Then she clean it with her face, man, I love my baby, ah You talkin' money, need a hearin' aid You talkin' 'bout me, I don't see the shade Switch up my style, I take any lane I switch up my cup, I kill any pain Look what you've done I'm a motherfuckin' starboy Look what you've done I'm a motherfuckin' starboy Every day, a nigga try to test me, ah Every day, a nigga try to end me, ah Pull off in that Roadster SV, ah Pockets overweight, gettin' hefty, ah Comin' for the king, that's a far cry, I I come alive in the fall time, I The competition, I don't really listen I'm in the blue Mulsanne, bumpin' New Edition House so empty, need a centerpiece Twenty racks a table, cut from ebony Cut that ivory into skinny pieces Then she clean it with her face, man, I love my baby, ah You talkin' money, need a hearin' aid You talkin' 'bout me, I don't see the shade Switch up my style, I take any lane I switch up my cup, I kill any pain Look what you've done I'm a motherfuckin' starboy Look what you've done I'm a motherfuckin' starboy Let a nigga brag Pitt Legend of the fall, took the year like a bandit Bought Mama a crib and a brand new wagon Now she hit the grocery shop lookin' lavish Star Trek roof in that Wraith of Khan Girls get loose when they hear this song A hundred on the dash get me close to God We don't pray for love, we just pray for cars House so empty, need a centerpiece Twenty racks a table, cut from ebony Cut that ivory into skinny pieces Then she clean it with her face, man, I love my baby, ah You talkin' money, need a hearin' aid You talkin' 'bout me, I don't see the shade Switch up my style, I take any lane I switch up my cup, I kill any pain Look what you've done I'm a motherfuckin' starboy Look what you've done I'm a motherfuckin' starboy Look what you've done I'm a motherfuckin' starboy Look what you've done I'm a motherfuckin' starboy
"
top_similar_docs <- perform_lsa(phrase, n = 1)
print(top_similar_docs)
