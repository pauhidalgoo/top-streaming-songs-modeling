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

min_freq <- ceiling(0.1 * length(track_names))

# Get the terms that appear in at least 10% of the documents
terms_freq_10 <- findFreqTerms(td.mat, lowfreq = 10)

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


ap_lda <- LDA(t(td.mat), k = 16, control = list(seed = 42), lowfreq=15)
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

rownames(doc_topic_dist) <- genres_df$title
# Remove duplicate rows
doc_topic_dist <- unique(doc_topic_dist)



# Perform t-SNE to reduce dimensionality
tsne_result <- Rtsne(doc_topic_dist)


library(ggplot2)
library(ggrepel)
tsne_df <- data.frame(tsne_result$Y)

ggplot(tsne_df, aes(x = X1, y = X2)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_text_repel(data = tsne_df, aes(label = rownames(doc_topic_dist)),
                  box.padding = 0.3, point.padding = 0.1,
                  segment.color = "transparent", size = 3) +
  theme_minimal() +
  labs(title = "t-SNE Visualization of Documents") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(range(tsne_df$X1)) +
  ylim(range(tsne_df$X2))




td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
lsaSpace <- lsa(td.mat.lsa)


dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=9)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=points$view)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=genres_df$title))
















