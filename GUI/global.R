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
wc_clean_corpus <- tm_map(uncleaned_corpus, content_transformer(tolower))
# Remove numbers
wc_clean_corpus <- tm_map(wc_clean_corpus, removeNumbers)
# Remove conjunctions etc.: "and",the", "of"
wc_clean_corpus <- tm_map(wc_clean_corpus, removeWords, stopwords("english"))
# Remove words like "you'll", "will", "anyways", etc.
wc_clean_corpus <- tm_map(wc_clean_corpus, removeWords, stopwords("SMART"))
# Remove commas, periods, etc.
wc_clean_corpus <- tm_map(wc_clean_corpus, removePunctuation)
# Strip unnecessary whitespace
wc_clean_corpus <- tm_map(wc_clean_corpus, stripWhitespace)
# Customize your own list of words for removal
wc_clean_corpus <- tm_map(wc_clean_corpus, removeWords, c("tis"))

books <<- track_names

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  i <- which(books == book)
  corpus <- content(wc_clean_corpus[[i]])
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


track_info_from_index <- memoise(function(track_id){
  row = images_unique[images_unique$track_id == track_id, ]
  row
})


