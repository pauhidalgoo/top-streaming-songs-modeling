# LSA Approach
# Obtenim una matriu amb tantes columnes com paraules i files com documents. Term frequency matrix
# Podem fer un PCA, o un clústering (Latent Dirichlet Allocation)
# SVD
library(tm)
library(ggplot2)
library(lsa)
library(LSAfun)



load("./2_Descriptive_analysis/unique_tracks.RData")
PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis/LSA/Original",sep="")



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
cosine(results["love",], results["baby",])

# compare two documents with pearson ### ALSO AVAILABLR FOR ALL DOCUMENTS
cor(results[,1], results[,2], method="pearson")

# calc associations for "water"
associate(results, "lover")

###Library(LSAfun)

png(file=paste0(PATH_PLOTS, "/lover_neighbors.png"),
    width=1920, height=1080, units="px", res=130)

plot_neighbors("love", #single word
               n = 10, #number of neighbors
               tvectors = results, #matrix space
               method = "MDS", #PCA or MDS
               dims = 2) #number of dimensions

dev.off()



list1 = c("love", "baby", "hate", "bad", "anger", "bed")

plot_wordlist(list1, #put in the list above 
              method = "MDS", 
              tvectors = results, 
              dims = 2)

rownames(lsaSpace$tk)


png(file=paste0(PATH_PLOTS, "/first_50_words.png"),
    width=1920, height=1080, units="px", res=130)

plot_wordlist(rownames(lsaSpace$tk)[1:50], 
              method = "MDS", 
              dims = 2, #pick the number of dimensions
              tvectors = results)

dev.off()

multicos(list1,tvectors = results)


doc_texts <- sapply(old_clean_corpus, paste, collapse = " ")

png(file=paste0(PATH_PLOTS, "/all_songs_MDS.png"),
    width=1920, height=1080, units="px", res=130)

results_with_names <- results
colnames(results_with_names)
colnames(results_with_names) <- track_names
plot_doclist(doc_texts, tvectors=results_with_names, method="MDS", dims=2)

dev.off()

library(ggrepel)

dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
dist.mat.lsa # check distance matrix for DOCUMENTS
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])

png(file=paste0(PATH_PLOTS, "/distances_all.png"),
    width=1920, height=1080, units="px", res=130)

points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=points$view)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=track_names))

dev.off()

png(file=paste0(PATH_PLOTS, "/distances_outliers.png"),
    width=1920, height=1080, units="px", res=130)
ggplot(points, aes(x=x, y=y)) + 
  geom_point(aes(color=points$view)) + 
  geom_text_repel(aes(label=track_names), 
                  nudge_y = -0.2, 
                  box.padding = 0.3, 
                  point.padding = 0.3, 
                  segment.color = 'grey50') +
  theme_minimal()

dev.off()

png(file=paste0(PATH_PLOTS, "/distances_readable.png"),
    width=1920, height=1080, units="px", res=130)

labels_to_show <- sample(track_names, size = 0.05 * length(track_names))

# Create a logical vector indicating which labels to show
show_label <- track_names %in% labels_to_show

# Plot
ggplot(points, aes(x = x, y = y)) + 
  geom_point(aes(color = points$view)) +  # Adjusted 'points$view' to 'view'
  geom_text_repel(data = points[show_label, ], aes(x=x, y=y, label=track_names[show_label]), 
                  nudge_y = -0.4,
                  nudge_x = 0.4,# Adjust the nudging if necessary
                  max.overlaps = Inf)

dev.off()


png(file=paste0(PATH_PLOTS, "/distances_latino.png"),
    width=1920, height=1080, units="px", res=130)

colors <- ifelse(unique_tracks$latino, "Latino", "No latino")
# Plot
ggplot(points, aes(x = x, y = y)) + 
  geom_point(aes(color = colors))
dev.off()

png(file=paste0(PATH_PLOTS, "/distances_hip_hop.png"),
    width=1920, height=1080, units="px", res=130)

colors <- ifelse(unique_tracks$hip_hop, "Hip hop", "No hip hop")
# Plot
ggplot(points, aes(x = x, y = y)) + 
  geom_point(aes(color = colors))
dev.off()



row.names(df)

library(scatterplot3d)
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=3)
colors <- ifelse(unique_tracks$latino, "blue", "red")

png(file=paste0(PATH_PLOTS, "/3d_latino.png"),
    width=1920, height=1080, units="px", res=130)

scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color=colors, pch=16, 
              main="Semantic Space Scaled to 3D", xlab="x", ylab="y", zlab="z", type="h")
dev.off()

# cLUSTER SONGS -----------------------------------
k <- 5 # Number of clusters (you can adjust this)
set.seed(123) # Set seed for reproducibility
km_clusters <- kmeans(t(as.textmatrix(lsaSpace)), centers = k)

# Add cluster labels to the data
cluster_labels <- as.factor(km_clusters$cluster)
points$cluster <- cluster_labels

png(file=paste0(PATH_PLOTS, "/kmeans_clustering.png"),
    width=1920, height=1080, units="px", res=130)

# Visualize clusters
ggplot(points, aes(x = x, y = y, color = points$cluster)) + 
  geom_point() + 
  geom_text(aes(label = track_names), vjust = -0.5) +
  ggtitle("Clusters of Songs based on Lyrics Similarity")

dev.off()
# Cluster centers
cluster_centers <- as.textmatrix(lsaSpace)[km_clusters$centers, ]


# WORDS ------------------------------------------
dist.mat.lsa <- dist((as.textmatrix(lsaSpace))) # compute distance matrix
dist.mat.lsa # check distance matrix for TERMS
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])


png(file=paste0(PATH_PLOTS, "/words_space.png"),
    width=1920, height=1080, units="px", res=130)


ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(points)))
points <- data.frame(x=fit)

dev.off()




