# CORRESPONDENCE ANALYSIS --------------------------------------
library(tm) # text mining package
library(wordcloud) # word cloud generator
library(SnowballC) # text stemming
library(ggplot2) # graphs
library(tidyverse) # data manipulation
library(tidytext) # word lexicon dictionaries for sentiments
library(reshape2) # data transformation
library(textdata) # provides access to lexicon dictionaries
library(knitr) # used to make kable tables


load("./2_Descriptive_analysis/unique_tracks.RData")
load("./8_Textual_analysis/unique_tracks_translated.RData")
PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis/CA",sep="")


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

########## LOAD FACTOMINER
library(FactoMineR)

clean_corpus <- tm_map(clean_corpus, removeWords, c("unknown"))

########## EXAMPLE 1: DEFINITON OF HEALTH
td.mat <- TermDocumentMatrix(clean_corpus)

min_freq <- ceiling(0.1 * length(track_names))

# Get the terms that appear in at least 10% of the documents
terms_freq_10 <- findFreqTerms(td.mat, lowfreq = 20)

# Subset the term-document matrix to include only these terms
td.mat_freq_10 <- td.mat[terms_freq_10, ]

td.mat <- as.matrix(td.mat_freq_10)

########## PERFORM CORRESPONDENCE ANALYSIS (CA)
?CA

text_matrix <- t(as.matrix(td.mat))
rownames(text_matrix) <- track_names
text_df <- as.data.frame(text_matrix)


res.ca<-CA(text_df, graph=TRUE)
res.ca

########## EIGENVALUES
res.ca$eig

########## TRACKS
names(res.ca$row)
res.ca$row$coord
res.ca$row$cos2
res.ca$row$contr
res.ca$row$inertia

########## WORDS
names(res.ca$col)
res.ca$col$coord
res.ca$col$cos2
res.ca$col$contr
res.ca$col$inertia

########## WEIGHTS
names(res.ca$call)
res.ca$call$marge.row
res.ca$call$marge.col
res.ca$call$ncp

########## SUMMARY
summary(res.ca)

########## HIGHER CONTRIBUTIONS OF THE FREQUENCIES
res.ca$col$contr[order(apply(res.ca$col$contr[,1:2],1,sum),decreasing=TRUE)[1:10],1:2]
res.ca$col$contr[order(apply(res.ca$col$contr[,3:4],1,sum),decreasing=TRUE)[1:10],3:4]

library(factoextra)
########## CA PLOTS
rotate_labels <- function(angle = 45) {
  theme(axis.text.x = element_text(angle = angle, hjust = 1))
}

fviz_contrib(res.ca, axes = 1, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 1")+
  rotate_labels()


fviz_contrib(res.ca, axes = 2, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 2")+
  rotate_labels()


fviz_contrib(res.ca, axes = 3, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 3")+
  rotate_labels()


fviz_contrib(res.ca, axes = 4, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 4")+
  rotate_labels()


fviz_contrib(res.ca, axes = 1, choice = "col", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 1")+
  rotate_labels()



png(file=paste0(PATH_PLOTS, "/ca_words_12.png"),
    width=1920, height=1080, units="px", res=130)
fviz_ca_col(res.ca, repel = TRUE, title = "CA Row Plot", select.col = list(contrib = 20)) + # Show top 10 contributing columns
  theme_minimal() +
  labs(color = "Row Points") +
  scale_color_manual(values = c("blue"))
dev.off()

png(file=paste0(PATH_PLOTS, "/ca_documents_12.png"),
    width=1920, height=1080, units="px", res=130)

fviz_ca_row(res.ca, repel = TRUE, title = "CA Column Plot", select.row = list(contrib = 10)) + # Show top 10 contributing rows
  theme_minimal() +
  labs(color = "Column Points") +
  scale_color_manual(values = c("red"))
dev.off()

# Plot for dimensions 3 and 4 with row points and selectively visible column labels
png(file=paste0(PATH_PLOTS, "/ca_words_34.png"),
    width=1920, height=1080, units="px", res=130)
fviz_ca_col(res.ca, axes = c(3, 4), repel = TRUE,  title = "CA Row Plot (Dim 3 and 4)", select.col = list(contrib = 20)) + # Show top 10 contributing columns
  theme_minimal() +
  labs(color = "Row Points") +
  scale_color_manual(values = c("blue"))

dev.off()

png(file=paste0(PATH_PLOTS, "/ca_documents_34.png"),
    width=1920, height=1080, units="px", res=130)

# Plot for dimensions 3 and 4 with column points and selectively visible row labels
fviz_ca_row(res.ca, axes = c(3, 4), repel = TRUE, title = "CA Column Plot (Dim 3 and 4)", select.row = list(contrib = 10)) + # Show top 10 contributing rows
  theme_minimal() +
  labs(color = "Column Points") +
  scale_color_manual(values = c("red"))

dev.off()


# USING FACTOMINER CA-GALT

subset <- unique_translated

names(unique_translated)
head(td.mat)
GALT <- cbind(t(as.matrix(td.mat)), unique_translated[, c('danceability', 'streams', 'track_popularity', 'energy', 'acousticness', 'liveness', 'valence', 'duration')])

rownames(GALT) <- unique_translated$track_name
# Step 3: Perform Correspondence Analysis
# Note: You might need to install the 'ca' package if you haven't already
ca_output <- CA(GALT)

# Step 4: Interpret the results
summary(ca_output)

fviz_contrib(ca_output, axes = 1, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 1")+
  rotate_labels()

fviz_contrib(ca_output, axes = 2, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 1")+
  rotate_labels()
