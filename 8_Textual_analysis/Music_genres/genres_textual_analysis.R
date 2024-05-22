library(topicmodels)
library(ldatuning)
library(tm)
library(quanteda)


load("./8_Textual_analysis/Music_genres/music_genres_descriptions.RData")


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

library(Rtsne)
# Extract document-topic distributions from the LDA model
# Extract document-topic distributions from LDA model
doc_topic_dist <- posterior(ap_lda)$topics

duplicate_indices <- which(duplicated(doc_topic_dist))

rownames(doc_topic_dist) <- genres_df$title
# Remove duplicate rows
doc_topic_dist <- unique(doc_topic_dist)



# Perform t-SNE to reduce dimensionality
tsne_result <- Rtsne(doc_topic_dist)


library(ggplot2)
library(ggrepel)
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


# Perform K-means clustering
k <- 8  # Number of clusters, you can adjust this based on your needs
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(doc_topic_dist, centers = k)

# Get cluster assignments
cluster_assignments <- kmeans_result$cluster

library(ggplot2)
library(ggrepel)
library(ggtext)

library(ggrepel)

# Assuming your tsne_df dataset contains the necessary columns X1, X2, and cluster_assignments

p <- ggplot(tsne_df, aes(x = X1, y = X2, color = factor(cluster_assignments))) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label = rownames(doc_topic_dist)),
            size = 3, vjust = 1, hjust = 1) +
  theme_minimal() +
  labs(title = "t-SNE Visualization of Documents", color = "Topic") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(range(tsne_df$X1)) +
  ylim(range(tsne_df$X2))

print(p)

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


print(p)

library(plotly)
ggplotly(p)


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
  labs(title = "t-SNE Visualization of Documents", color = "Topic") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(range(tsne_df$X1)) +
  ylim(range(tsne_df$X2))

print(p)

p <- ggplot(tsne_df, aes(x = X1, y = X2, color = factor(unname(topics_each)))) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label = rownames(doc_topic_dist)),
            size = 3, vjust = 1, hjust = 1) +
  theme_minimal() +
  labs(title = "t-SNE Visualization of Documents", color = "Topic") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(range(tsne_df$X1)) +
  ylim(range(tsne_df$X2))

print(p)


length(unname(topics_each))
topics_each[1:length(topics_each)]

genres_df$title

