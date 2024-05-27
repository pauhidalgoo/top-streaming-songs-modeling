library(topicmodels)
library(ldatuning)
library(tm)
library(quanteda)
library(MASS)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidytext)
library(gridExtra)



load('./7_Geoespacial/data_coordenades.RData')

# Verificar que data es un data.frame
data <- as.data.frame(data)

# Eliminar duplicados
data <- data[!duplicated(data[c("artist_name")]), ]


uncleaned_corpus<- Corpus(VectorSource(data$lyrics))
#-------------------------------------------------------------------------------
# PREPROCESAMENT DEL TEXT


preprocess <- function(input_corpus){
  texto <- readLines("./8_Textual_Analysis/gh-stopwords-json-es.txt")
  palabras_es <- unlist(strsplit(texto, "\n"))
  # cleaning corpus
  writeLines(head(strwrap(input_corpus[[1]]), 7))
  
  clean_corpus <- tm_map(input_corpus, content_transformer(tolower))
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
  
  
  # Parte manual, palabras que queramos eliminar
  old_clean_corpus <- clean_corpus
  # Customize your own list of words for removal
  clean_corpus <- tm_map(clean_corpus, removeWords, c("yeah", "ere", "yeh", "ahn"))
  #stopwords en castellano
  stopwords_es <- stopwords(language= "es")
  clean_corpus <- tm_map(clean_corpus, removeWords, stopwords_es)
  clean_corpus <- tm_map(clean_corpus, removeWords, palabras_es)
  
  corpus <- clean_corpus
  return(corpus)
}

corpus <- preprocess(uncleaned_corpus)


#-------------------------------------------------------------------------------
# MODELO LDA
#-------------------------------------------------------------------------------

# Creación  de la TermDocMatrix (3% de frecuencia)
td.mat <- as.matrix(TermDocumentMatrix(corpus, control = list(bounds=list(global = c(2, Inf)))))

sum(is.na(t(td.mat)))
ap_lda <- LDA(t(td.mat), k = 6, control = list(seed = 42))




ap_topics <- tidy(ap_lda, matrix = "gamma")


doc_topic_dist <- posterior(ap_lda)$topics

length(doc_topic_dist[,1])

data$topic1 <- doc_topic_dist[,1]
data$topic2 <- doc_topic_dist[,2]
data$topic3 <- doc_topic_dist[,3]
data$topic4 <- doc_topic_dist[,4]
data$topic5 <- doc_topic_dist[,5]
data$topic6 <- doc_topic_dist[,6]


# Seleccionar las columnas necesarias
data <- data[, c("artist_name", "track_name", "latitude", "longitude", "energy", "topic1", "topic2", "topic3", "topic4", "topic5","topic6")]

mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data <- data %>%
  group_by(latitude, longitude) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(is.factor), mode_function),
            .groups = 'drop')
save(data, file="7_Geoespacial/artists_with_topics.RData")
