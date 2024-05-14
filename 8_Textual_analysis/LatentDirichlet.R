library(topicmodels)
library(ldatuning)
library(tm)
library(quanteda)

load("./2_Descriptive_analysis/unique_tracks.RData")


track_names = unique_tracks$track_name

uncleaned_corpus<- Corpus(VectorSource(unique_tracks$lyrics))
uncleaned_corpus

#-------------------------------------------------------------------------------
# PREPROCESAMENT DEL TEXT

#stopwords en castellano
texto <- readLines("./8_Textual_Analysis/gh-stopwords-json-es.txt")
palabras_es <- unlist(strsplit(texto, "\n"))

# cleaning corpus
writeLines(head(strwrap(uncleaned_corpus[[1]]), 7))

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


# Parte manual, palabras que queramos eliminar
old_clean_corpus <- clean_corpus
# Customize your own list of words for removal
clean_corpus <- tm_map(clean_corpus, removeWords, c("yeah", "ere", "yeh", "ahn"))
stopwords_es <- stopwords(language= "es")
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords_es)
clean_corpus <- tm_map(clean_corpus, removeWords, palabras_es)

corpus <- clean_corpus










#-------------------------------------------------------------------------------

# Creación  de la TermDocMatrix
td.mat <- as.matrix(TermDocumentMatrix(corpus, control = list(global = c(5, Inf))))
dim(td.mat)



# Finetunning para escoger el número de topics
result <- ldatuning::FindTopicsNumber(
  td.mat,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014", 'Griffiths2004', 'Arun2010'),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# Se ve que el 9, 15 e incluso 20 podrían ser buenos números en función de las métricas



ap_lda <- LDA(t(td.mat), k = 9, control = list(seed = 42), lowfreq=15)
ap_lda
#Word-topic probabilities

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics



library(ggplot2)
library(dplyr)

dimnames(td.mat)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#-------------------------------------------------------------------------------
years<-as.integer(levels(unique_tracks$year_release))

2000:2021


















