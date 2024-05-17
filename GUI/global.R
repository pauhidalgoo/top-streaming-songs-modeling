library(tm)
library(wordcloud)
library(memoise)

# The list of valid books

load("./8_Textual_analysis/unique_tracks_translated.RData")
load("./GUI/unique_images.RData")
PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis/Analysis/Translated",sep="")


track_names = unique_translated$track_name

uncleaned_corpus<- Corpus(VectorSource(unique_translated$translated_lyrics))
uncleaned_corpus

writeLines(head(strwrap(uncleaned_corpus[[1]]), 7))

# Convert to lowercase
clean_corpus <- tm_map(uncleaned_corpus, content_transformer(tolower))
# Remove numbers
clean_corpus <- tm_map(clean_corpus, removeNumbers)
# Remove conjunctions etc.: "and",the", "of"
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords("english"))
# Remove words like "you'll", "will", "anyways", etc.
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords("SMART"))
# Remove commas, periods, etc.
clean_corpus <- tm_map(clean_corpus, removePunctuation)
# Strip unnecessary whitespace
clean_corpus <- tm_map(clean_corpus, stripWhitespace)
# Customize your own list of words for removal
clean_corpus <- tm_map(clean_corpus, removeWords, c("tis"))

books <<- track_names

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  i <- which(books == book)
  corpus <- content(clean_corpus[[i]])
  name = track_names[i]
  print(name)
  name = gsub(" ", "", name , fixed = TRUE)
  name = gsub("...", "", name, fixed= TRUE)
  name = gsub("'", "", name, fixed= TRUE)
  name = gsub("?", "", name, fixed= TRUE)
  print(name)
  i = i + 1
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  
  sort(rowSums(tdm), decreasing = TRUE)
})