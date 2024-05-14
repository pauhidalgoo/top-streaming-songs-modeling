# LSA Approach
# Obtenim una matriu amb tantes columnes com paraules i files com documents. Term frequency matrix
# Podem fer un PCA, o un clústering (Latent Dirichlet Allocation)
# SVD
library(tm)
library(ggplot2)
library(lsa)
library(LSAfun)



load("./2_Descriptive_analysis/unique_tracks.RData")

track_names = unique_tracks$track_name

uncleaned_corpus<- Corpus(VectorSource(unique_tracks$lyrics))
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
clean_corpus <- tm_map(clean_corpus, removeWords, c("tis"))

writeLines(head(strwrap(clean_corpus[[1]]), 7))

corpus <- clean_corpus
td.mat <- TermDocumentMatrix(corpus)

min_freq <- ceiling(0.1 * length(track_names))

# Get the terms that appear in at least 10% of the documents
terms_freq_10 <- findFreqTerms(td.mat, lowfreq = 10)

# Subset the term-document matrix to include only these terms
td.mat_freq_10 <- td.mat[terms_freq_10, ]

td.mat <- as.matrix(td.mat_freq_10)

td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
lsaSpace <- lsa(td.mat.lsa) # create LSA space
as.textmatrix(lsaSpace)
head(as.textmatrix(lsaSpace))
#EXPLORING RESULTS
results<- as.textmatrix(lsaSpace)
head(results)

# compare two terms with the cosine measure ##ALSO AVAILABLE FOR ALL TERMS
cosine(results["hate",], results["nigga",])

# compare two documents with pearson ### ALSO AVAILABLR FOR ALL DOCUMENTS
cor(results[,1], results[,2], method="pearson")

# calc associations for "water"
associate(results, "nigga")

###Library(LSAfun)
plot_neighbors("bitch", #single word
               n = 10, #number of neighbors
               tvectors = results, #matrix space
               method = "MDS", #PCA or MDS
               dims = 2) #number of dimensions

list1 = c("car", "global")

plot_wordlist(list1, #put in the list above 
              method = "MDS", 
              dims = 2, #pick the number of dimensions
              tvectors = results)

rownames(lsaSpace$tk)
plot_wordlist(rownames(lsaSpace$tk)[1:500], 
              method = "MDS", 
              dims = 2, #pick the number of dimensions
              tvectors = results)

multicos(list1,tvectors = results)


doc_texts <- sapply(old_clean_corpus, paste, collapse = " ")
plot_doclist(doc_texts[1:100], tvectors=results, method="MDS", dims=2, doc_names=track_names[1:10])

dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
dist.mat.lsa # check distance matrix for DOCUMENTS
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=points$view)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=track_names))

row.names(df)

library(scatterplot3d)
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=3)
colors <- ifelse(unique_tracks$latino, "blue", "red")

scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color=colors, pch=16, 
              main="Semantic Space Scaled to 3D", xlab="x", ylab="y", zlab="z", type="h")


# cLUSTER SONGS -----------------------------------
k <- 5 # Number of clusters (you can adjust this)
set.seed(123) # Set seed for reproducibility
km_clusters <- kmeans(t(as.textmatrix(lsaSpace)), centers = k)

# Add cluster labels to the data
cluster_labels <- as.factor(km_clusters$cluster)
points$cluster <- cluster_labels

# Visualize clusters
ggplot(points, aes(x = x, y = y, color = points$cluster)) + 
  geom_point() + 
  geom_text(aes(label = track_names), vjust = -0.5) +
  ggtitle("Clusters of Songs based on Lyrics Similarity")

# Cluster centers
cluster_centers <- as.textmatrix(lsaSpace)[km_clusters$centers, ]


# WORDS ------------------------------------------
dist.mat.lsa <- dist((as.textmatrix(lsaSpace))) # compute distance matrix
dist.mat.lsa # check distance matrix for TERMS
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(points)))
points <- data.frame(x=fit)
