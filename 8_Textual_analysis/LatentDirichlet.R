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
  stopwords_es <- stopwords(language= "es")
  clean_corpus <- tm_map(clean_corpus, removeWords, stopwords_es)
  clean_corpus <- tm_map(clean_corpus, removeWords, palabras_es)
  
  corpus <- clean_corpus
  return(corpus)
}

corpus <- preprocess(uncleaned_corpus)







#-------------------------------------------------------------------------------

# Creación  de la TermDocMatrix
corpus

td.mat <- as.matrix(TermDocumentMatrix(corpus, control = list(global = c(5, Inf))))
dim(td.mat)

td.mat
track_names



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
# Time Series con distintos topics según el topic por año

library(topicmodels)

# Obtener los topics para cada uno de los documentos, seleccionando el más probable para
# cada uno de los documentos

posterior_probs <- posterior(ap_lda)$topics
posterior_df <- as.data.frame(posterior_probs)
posterior_df$document <- track_names

document_topics <- posterior_df %>%
  mutate(predominant_topic = apply(posterior_probs, 1, which.max))

document_topics$predominant_topic <- as.integer(document_topics$predominant_topic)
document_topics$year_release <- as.numeric(as.character(unique_tracks$year_release))
document_topics$month_release <- as.numeric(as.character(unique_tracks$month_release))

View(document_topics)

dc_cpy <- document_topics



dc_cpy <- dc_cpy %>% 
  mutate(
  year_season = ifelse(month_release %in% c(12), year_release + 1, year_release)
  )

dc_cpy <- dc_cpy %>%  mutate(season = case_when(
  month_release %in% c(1, 2, 12) ~ "1",
  month_release %in% c(3, 4, 5) ~ "2",
  month_release %in% c(6, 7, 8) ~ "3",
  month_release %in% c(9,10,11) ~ "4"
))




# Paso 1: Contar la cantidad de productos por año y tópico
count_data <- dc_cpy %>%
  dplyr::count(year_season, season, predominant_topic, name = "count")

count_data$predominant_topic <- as.factor(count_data$predominant_topic)

count_data <- count_data[count_data[,"year_season"]>2015,]


my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#eebb44", "#bcbd22")


ggplot(data=count_data, aes(x=paste(year_season, season, sep="-"), y=count, group=predominant_topic, color = predominant_topic)) +
  geom_line(size=0.6) +
  theme_minimal() +
  scale_color_manual(values = my_colors) 

#, color=predominant_topic)


#-------------------------------------------------------------------------------
# Estudio según año/mes/época en la que fueron populares los tópicos

load("./3_Preprocessing/data_knn_imputed_unknown.RData")
weekly_data <- data %>% select(track_name, year_week, month_week)
weekly_data$track_name <- as.factor(weekly_data$track_name)

songs_to_topics <- dc_cpy %>% select(document, predominant_topic)
names(songs_to_topics) <- c("track_name", "predominant_topic")

merged_data <- weekly_data %>%
  left_join(songs_to_topics, by = "track_name")

merged_data$year_week <- as.numeric(as.character(merged_data$year_week))
merged_data$month_week <- as.numeric(as.character(merged_data$month_week))

merged_data <- merged_data %>% 
  mutate(
    year_season = ifelse(month_week %in% c(12), year_week + 1, year_week)
  )

merged_data <- merged_data %>%  mutate(season = case_when(
  month_week %in% c(1, 2, 12) ~ "1",
  month_week %in% c(3, 4, 5) ~ "2",
  month_week %in% c(6, 7, 8) ~ "3",
  month_week %in% c(9,10,11) ~ "4"
))



count_data <- merged_data %>%
  dplyr::count(year_season, season, predominant_topic, name = "count")

count_data$predominant_topic <- as.factor(count_data$predominant_topic)


my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#eebb44", "#bcbd22")


ggplot(data=count_data, aes(x=paste(year_season, season, sep="-"), y=count, group=predominant_topic, color = predominant_topic)) +
  geom_line(size=0.6) +
  theme_minimal() +
  scale_color_manual(values = my_colors) 








lyrics_pop <- unique_tracks[unique_tracks[,"hip_hop"]==TRUE,]

lyrics_pop 

uncleaned_pop<- Corpus(VectorSource(lyrics_pop$lyrics))



cleaned_pop <- preprocess(uncleaned_pop)

td.mat <- as.matrix(TermDocumentMatrix(cleaned_pop, control = list(global = c(5, Inf))))


term_frequencies <- rowSums(td.mat)
term_frequencies_df <- data.frame(term = names(term_frequencies), frequency = term_frequencies)





dd
n=20
library(dplyr)
top_terms <- term_frequencies_df %>%
  arrange(desc(frequency)) %>%
  top_n(n = n)
top_terms



# Paso 3: Combinar las palabras más frecuentes con las palabras del modelo LDA y encontrar los tópicos

# Filtrar los términos más frecuentes que están en los tópicos del modelo LDA
common_terms <- ap_topics %>%
  filter(term %in% top_terms$term)

# Imprimir los términos más frecuentes y sus tópicos
print(common_terms)




years<-as.integer(unique_tracks$year_release)
ap_lda
td.mat

length(topics)
length(years)

as.data.frame(years, topics)


2000:2021


















