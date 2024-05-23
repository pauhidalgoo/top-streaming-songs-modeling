library(topicmodels)
library(ldatuning)
library(tm)
library(quanteda)
library(ggplot2)
library(dplyr)
library(tidytext)
library(Rtsne)
library(plotly)
library(ggrepel)
library(ggtext)
# PREPROCESSAMENT DEL TEXT ------------------------------------------------------
load("./8_Textual_analysis/Music_genres/music_genres_descriptions.RData")
PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis/Genres",sep="")


uncleaned_corpus<- Corpus(VectorSource(genres_df$content))
uncleaned_corpus

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

# stemDocument tb es pot fer

old_clean_corpus <- clean_corpus
# Customize your own list of words for removal
clean_corpus <- tm_map(clean_corpus, removeWords, c("yeah"))

writeLines(head(strwrap(clean_corpus[[1]]), 7))

corpus <- clean_corpus
td.mat <- TermDocumentMatrix(corpus)


# Get the terms that appear in at least 10% of the documents
terms_freq_10 <- findFreqTerms(td.mat, lowfreq = 8)

# Subset the term-document matrix to include only these terms
td.mat_freq_10 <- td.mat[terms_freq_10, ]

td.mat <- as.matrix(td.mat_freq_10)

# LDA -----------------------------------------------------------------------------
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


ap_lda <- LDA(t(td.mat), k = 9, control = list(seed = 42), lowfreq=15)
ap_lda
#Word-topic probabilities

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

dimnames(td.mat)

# VISUALITZACIÓ DELS TOPICS -------------------------------------------------------

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

png(file=paste0(PATH_PLOTS, "/topic_terms.png"),
    width=1920, height=1080, units="px", res=130)
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

dev.off()

doc_topic_dist <- posterior(ap_lda)$topics

duplicate_indices <- which(duplicated(doc_topic_dist))

rownames(doc_topic_dist) <- genres_df$title
# Remove duplicate rows
doc_topic_dist <- unique(doc_topic_dist)

# Perform t-SNE to reduce dimensionality
tsne_result <- Rtsne(doc_topic_dist)

tsne_df <- data.frame(tsne_result$Y)

ggplot(tsne_df, aes(x = X1, y = X2)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +
  geom_text_repel(data = tsne_df, aes(label = rownames(doc_topic_dist)),
                  box.padding = 0.3, point.padding = 0.1,
                  segment.color = "transparent", size = 3) +
  theme_minimal() +
  labs(title = "t-SNE Visualization of Documents") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(range(tsne_df$X1)) +
  ylim(range(tsne_df$X2))

# CLUSTERING ------------------------------------------------------------------------


# Perform K-means clustering
k <- 8  # Number of clusters, you can adjust this based on your needs
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(doc_topic_dist, centers = k)

# Get cluster assignments
cluster_assignments <- kmeans_result$cluster

p <- ggplot(tsne_df, aes(x = X1, y = X2, color = factor(cluster_assignments))) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label = rownames(doc_topic_dist)),
            size = 3, vjust = 1, hjust = 1) +
  theme_minimal() +
  labs(title = "t-SNE Visualization of Music Genres", color = "Cluser") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(range(tsne_df$X1)) +
  ylim(range(tsne_df$X2))

png(file=paste0(PATH_PLOTS, "/clustering_results.png"),
    width=1920, height=1080, units="px", res=130)
print(p)
dev.off()
# Unique cluster codes
unique_clusters <- unique(cluster_assignments)

# Names for each cluster code
cluster_names <- c("Hip hop", "Latin dance", "Metal", "Electro", "Alternative", "Weird", "Pop", "Rock")

# Create a named vector mapping cluster codes to names
cluster_names_mapping <- setNames(cluster_names, unique_clusters)

# Assign names to cluster codes
cluster_names_assigned <- cluster_names_mapping[cluster_assignments]

cluster_names_assigned <- unname(cluster_names_assigned)

cluster_doc_df <- data.frame(Cluster = cluster_names_assigned, Document = rownames(doc_topic_dist))

# Create a table showing document names by cluster
doc_by_cluster <- table(cluster_doc_df$Cluster, cluster_doc_df$Document)

# Print the table
print(doc_by_cluster)
ggplotly(p)


# PLOT DELS TOPICS -----------------------------------------------------------------


doc_topic_dist <- posterior(ap_lda)$topics

duplicate_indices <- which(duplicated(doc_topic_dist))


duplicate_indices

topics_each <- topics(ap_lda)

topics_each <- topics_each[!duplicated(doc_topic_dist)]
rownames(doc_topic_dist) <- genres_df$title
doc_topic_dist <- unique(doc_topic_dist)

special_points <- c("Hip hop music", "Afro house", "Pop music")

rownames(doc_topic_dist)
# Add a new column for point sizes
tsne_df$point_size <- ifelse(rownames(doc_topic_dist) %in% special_points, 10, 1)  # Size 5 for special points, 2 for others

# Plot
p <- ggplot(tsne_df, aes(x = X1, y = X2, color = factor(unname(topics_each)), size = point_size)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = rownames(doc_topic_dist)),
            size = 3, vjust = 1, hjust = 1) +
  theme_minimal() +
  labs(title = "t-SNE Visualization of Music Genres", color = "Topic") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(range(tsne_df$X1)) +
  ylim(range(tsne_df$X2))

png(file=paste0(PATH_PLOTS, "/topic_results.png"),
    width=1920, height=1080, units="px", res=130)
print(p)
dev.off()

unique_topics <- unique(topics_each)

# Names for each cluster code
topic_names <- c("Electro house", "Reggae Alt", "Regional", "Hip hop", "R&B", "Rock", "Latin & Funk", "Pop", "Folk")

# Create a named vector mapping cluster codes to names
topic_names_mapping <- setNames(topic_names, unique_topics)

# Assign names to cluster codes
topic_names_assigned <- topic_names_mapping[topics_each]

topic_names_assigned

topic_names_assigned <- unname(topic_names_assigned)

topic_names_assigned


topic_doc_df <- data.frame(Topic = topic_names_assigned, Original = rownames(doc_topic_dist))

# Create a table showing document names by cluster
topic_by_cluster <- table(topic_doc_df$Topic, topic_doc_df$Original)

head(topic_doc_df,200)

# PASSEM ELS TOPICS A LES DADES -----------------------------------------------------


original_dataset <- read.csv("~/Universitat/4rt Quatri/PMAAD_git/PMAAD_GIA/8_Textual_analysis/Music_genres/spotify-top-200-dataset.csv", sep=";")

load("./8_Textual_analysis/unique_tracks_translated.RData")

merged_data <- unique_translated %>%
  left_join(distinct(original_dataset, track_id, .keep_all = TRUE), by = "track_id")

definitive_data <- unique_translated

definitive_data$artist_genres <- merged_data$artist_genres

definitive_data$artist_genres


definitive_data <- definitive_data %>%
  mutate(artist_genres = str_split(artist_genres, ", "))


unique(definitive_data$artist_genres)

check_genre_match <- function(artist_genres, topic) {
  for (genre in topic_doc_df$Original[topic_doc_df$Topic == topic]) {
    for (artist_genre in artist_genres) {
      artist_genre <- as.character(artist_genre) 
      if (tolower(genre) == str_to_lower(artist_genre)) {
        return(TRUE)
      }
      if ((str_to_lower(artist_genre) == "r&b") && (tolower(topic) == "r&b")){
        return(TRUE)
      }
      if ((str_to_lower(artist_genre) == "rap") && (tolower(topic) == "hip hop")){
        return(TRUE)
      }
      if ((str_to_lower(artist_genre) == "pop") && (tolower(topic) == "pop")){
        return(TRUE)
      }
      if ((str_to_lower(artist_genre) == "hip hop") && (tolower(topic) == "hip hop")){
        return(TRUE)
      }
      if ((str_to_lower(artist_genre) == "rock") && (tolower(topic) == "rock")){
        return(TRUE)
      }
      if ((str_to_lower(artist_genre) == "edm") && (tolower(topic) == "electro house")){
        return(TRUE)
      }
      if ((str_to_lower(artist_genre) == "house") && (tolower(topic) == "electro house")){
        return(TRUE)
      }
      first_word <- str_split(str_to_lower(artist_genre), " ")[[1]][1]
      if (first_word == tolower(genre)) {
        return(TRUE)
      }
      if ((first_word == "r&b") && (tolower(topic) == "r&b")){
        return(TRUE)
      }
      if ((first_word == "rap") && (tolower(topic) == "hip hop")){
        return(TRUE)
      }
      if ((first_word == "pop") && (tolower(topic) == "pop")){
        return(TRUE)
      }
      if ((first_word == "rock") && (tolower(topic) == "rock")){
        return(TRUE)
      }
      if ((first_word == "edm") && (tolower(topic) == "electro house")){
        return(TRUE)
      }
      if ((first_word == "house") && (tolower(topic) == "electro house")){
        return(TRUE)
      }
      
      # Search only the last word of artist_genre
      last_word <- tail(str_split(str_to_lower(artist_genre), " ")[[1]], 1)
      if (last_word == tolower(genre)) {
        return(TRUE)
      }
      if ((last_word == "r&b") && (tolower(topic) == "r&b")){
        return(TRUE)
      }
      if ((last_word == "rap") && (tolower(topic) == "hip hop")){
        return(TRUE)
      }
      if ((last_word == "hop") && (tolower(topic) == "hip hop")){
        return(TRUE)
      }
      if ((last_word == "pop") && (tolower(topic) == "pop")){
        return(TRUE)
      }
      if ((last_word == "rock") && (tolower(topic) == "rock")){
        return(TRUE)
      }
      if ((last_word == "edm") && (tolower(topic) == "electro house")){
        return(TRUE)
      }
      if ((last_word == "house") && (tolower(topic) == "electro house")){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

head(topic_doc_df,140)

# TARDA UNA MICA ÉS NORMAL
for(topic in unique(topic_doc_df$Topic)) {
  definitive_data <- definitive_data %>%
    mutate(!!paste0(topic, "_match") := map_lgl(artist_genres, check_genre_match, topic))
}

# Ns si fa falta crec que no
definitive_data[is.na(definitive_data)] <- FALSE
unique(topic_doc_df$Topic)

# Mirem els que no tenen cap gènere (son només 20)
no_genre <- definitive_data %>%
  rowwise() %>%
  filter(all(across(ends_with("_match"), ~ . == FALSE)))

no_genre

generate_barplot <- function(topic) {
  topic_data <- definitive_data %>%
    filter(!!sym(paste0(topic, "_match")))
  
  genre_counts <- topic_data %>%
    unnest(artist_genres) %>%
    count(artist_genres)
  
  # Create the bar plot
  p <- ggplot(genre_counts, aes(x = artist_genres, y = n)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = paste("Genres for Topic:", topic),
         x = "Genre",
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

plots <- list()

for(topic in unique(topic_doc_df$Topic)) {
  plots[[topic]] <- generate_barplot(topic)
}

for(i in seq_along(plots)) {
  png(file=paste0(PATH_PLOTS, paste0("/", unique(topic_doc_df$Topic)[i],"_subgenres_freq.png")),
      width=1920, height=1080, units="px", res=130)
  print(plots[[i]])
  dev.off()
}

definitive_data["artist_genres"] <- NULL

load("./8_textual_analysis/emotions_unique_translated.RData")

new_names <- colnames(emotions_df[,49:56])

definitive_data[new_names] <- emotions_df[,49:56]

# Guardem les dades, també amb les emocions
save(definitive_data, file = "./8_textual_analysis/genres_emotions_translated.RData")

