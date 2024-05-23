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


a_corpus <- clean_corpus
########## LOAD FACTOMINER
library(FactoMineR)

clean_corpus <- tm_map(clean_corpus, removeWords, c("unknown"))

########## EXAMPLE 1: DEFINITON OF HEALTH
td.mat <- TermDocumentMatrix(clean_corpus)

# Get the terms that appear in at least 10% of the documents
terms_freq_10 <- findFreqTerms(td.mat, lowfreq = 10)

# Subset the term-document matrix to include only these terms
td.mat_freq_10 <- td.mat[terms_freq_10, ]


#td.mat_freq_10 <- weightTfIdf(td.mat_freq_10)

td.mat <- as.matrix(td.mat_freq_10)

########## PERFORM CORRESPONDENCE ANALYSIS (CA)
?CA



text_matrix <- t(as.matrix(td.mat))
rownames(text_matrix) <- track_names
text_df <- as.data.frame(text_matrix)



res.ca<-CA(text_df, ncp=30, graph=TRUE)
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

png(file=paste0(PATH_PLOTS, "/1_dimension_docs.png"),
    width=1920, height=1080, units="px", res=130)

fviz_contrib(res.ca, axes = 1, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 1")+
  rotate_labels()
dev.off()

png(file=paste0(PATH_PLOTS, "/2_dimension_docs.png"),
    width=1920, height=1080, units="px", res=130)
fviz_contrib(res.ca, axes = 2, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 2")+
  rotate_labels()
dev.off()

png(file=paste0(PATH_PLOTS, "/3_dimension_docs.png"),
    width=1920, height=1080, units="px", res=130)

fviz_contrib(res.ca, axes = 3, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 3")+
  rotate_labels()
dev.off()

png(file=paste0(PATH_PLOTS, "/4_dimension_docs.png"),
    width=1920, height=1080, units="px", res=130)

fviz_contrib(res.ca, axes = 4, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 4")+
  rotate_labels()
dev.off()


png(file=paste0(PATH_PLOTS, "/1_dimension_words.png"),
    width=1920, height=1080, units="px", res=130)
fviz_contrib(res.ca, axes = 1, choice = "col", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 1")+
  rotate_labels()
dev.off()



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

# caGALT -----------------------

########## EXAMPLE 1: DEFINITON OF HEALTH
td.mat <- TermDocumentMatrix(a_corpus)

# Get the terms that appear in at least 10% of the documents
terms_freq_10 <- findFreqTerms(td.mat, lowfreq = 10)

# Subset the term-document matrix to include only these terms
td.mat_freq_10 <- td.mat[terms_freq_10, ]


td.mat_freq_10 <- weightTfIdf(td.mat_freq_10)

td.mat <- as.matrix(td.mat_freq_10)

text_matrix <- t(as.matrix(td.mat))
rownames(text_matrix) <- track_names
text_df <- as.data.frame(text_matrix)

min(text_df)

names(unique_translated)

subset <- unique_translated[c("explicit", "gender", "pop", "hip_hop", "rock", "latino","electro","christmas", "cinema")]

colnames(subset) <- c('explicit_var', 'gender_var', 'pop_var', 'hip_hop_var', 'rock_var', 'latino_var', 'electro_var', 'christmas_var', "cinema_var")

# LA FUNCIÓ DE LA LLIBRERIA S'HA MODIFICAT LLEUGERAMENT USANT
# trace JA QUE SI NO NO FUNCIONAVA (ES QUEDAVA ESTANCADA MOLT TEMPS)
# BÀSICAMENT EN EL SELECT DE LES ELIPSES NO FA REPLACE
# També s'ha canviat per poder plotjear els noms de les variables bé

res.cagalt<-CaGalt(Y=text_df,X=subset,type="n", graph=FALSE, nb.ellip=2)
res.cagalt

########## EIGENVALUES
res.cagalt$eig

########## RESPONDENTS
names(res.cagalt$ind)
res.cagalt$ind$coord
res.cagalt$ind$cos2

########## WORDS
names(res.cagalt$freq)
res.cagalt$freq$coord
res.cagalt$freq$cos2
res.cagalt$freq$contr

########## QUALITATIVE VARIABLES
names(res.cagalt$quali.var)
res.cagalt$quali.var$coord
res.cagalt$quali.var$cos2

########## ELLIPSES
names(res.cagalt$ellip)
res.cagalt$ellip$freq
res.cagalt$ellip$var

########## SUMMARY
summary(res.cagalt)

########## CA-GALT PLOTS
plot.CaGalt(res.cagalt,choix="freq",axes=c(1,2))

png(file=paste0(PATH_PLOTS, "/cagalt2_words_12.png"),
    width=1920, height=1080, units="px", res=130)

plot.CaGalt(res.cagalt,choix="freq",axes=c(1,2),select = "contrib 10")
dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt2_quanti_12.png"),
    width=1920, height=1080, units="px", res=130)

plot.CaGalt(res.cagalt,choix="quanti.var",axes=c(1,2))

dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt2_tracks_34.png"),
    width=1920, height=1080, units="px", res=130)
plot.CaGalt(res.cagalt,choix="ind",axes=c(3,4), select = "cos2 10")
dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt2_words_34.png"),
    width=1920, height=1080, units="px", res=130)

plot.CaGalt(res.cagalt,choix="freq",axes=c(3,4),select = "contrib 10")
dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt2_quanti_34.png"),
    width=1920, height=1080, units="px", res=130)

plot.CaGalt(res.cagalt,choix="quanti.var",axes=c(3,4))

dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt2_tracks_12.png"),
    width=1920, height=1080, units="px", res=130)
plot.CaGalt(res.cagalt,choix="ind",axes=c(1,2), select = "cos2 10")
dev.off()



?plot.CaGalt

res.cagalt$ind$coord
res.cagalt$ind$cos2
res.cagalt$freq$coord
res.cagalt$freq$contrib
res.cagalt$freq$cos2
res.cagalt$quanti.var$coord
res.cagalt$quanti.var$cor
res.cagalt$quanti.var$cos2

contrib_dim1 <- res.cagalt$freq$contrib[, 1]
contrib_dim2 <- res.cagalt$freq$contrib[, 2]

# Create data frames for the contributions
contrib_df1 <- data.frame(Feature = names(contrib_dim1), Contribution = contrib_dim1)
contrib_df2 <- data.frame(Feature = names(contrib_dim2), Contribution = contrib_dim2)

# Filter to keep only the most significant contributors
# Here, we assume 'most significant' means the top 10 contributors. You can adjust as needed.
top_n <- 10
top_contrib_df1 <- contrib_df1 %>% arrange(desc(Contribution)) %>% head(top_n)
top_contrib_df2 <- contrib_df2 %>% arrange(desc(Contribution)) %>% head(top_n)

# Plot contributions to the first dimension
plot1 <- ggplot(top_contrib_df1, aes(x = reorder(Feature, Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Contributions to Dimension 1", x = "Feature", y = "Contribution") +
  theme_minimal()

# Plot contributions to the second dimension
plot2 <- ggplot(top_contrib_df2, aes(x = reorder(Feature, Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Top Contributions to Dimension 2", x = "Feature", y = "Contribution") +
  theme_minimal()

# Print the plots
png(file=paste0(PATH_PLOTS, "/cagalt2_contrib_1.png"),
    width=1920, height=1080, units="px", res=130)
print(plot1)
dev.off()
png(file=paste0(PATH_PLOTS, "/cagalt2_contrib_2.png"),
    width=1920, height=1080, units="px", res=130)
print(plot2)
dev.off()

plot(res.cagalt, choix = "quanti.var", conf.ellip = TRUE, axes = c(3, 4))
plot(res.cagalt, choix = "freq", cex = 1.5, col.freq = "darkgreen",select = "contrib 10")


























subset <- unique_translated[c('danceability', 'streams', 'track_popularity', 'energy', 'acousticness', 'liveness', 'valence', 'duration')]
# Rename the specified columns
colnames(subset) <- c('danceability_var', 'streams_var', 'track_popularity_var', 'energy_var', 'acousticness_var', 'liveness_var', 'valence_var', 'duration_var')

res.cagalt<-CaGalt(Y=text_df,X=subset,type="s", graph=FALSE, nb.ellip=10)
res.cagalt

########## EIGENVALUES
res.cagalt$eig

########## RESPONDENTS
names(res.cagalt$ind)
res.cagalt$ind$coord
res.cagalt$ind$cos2

########## WORDS
names(res.cagalt$freq)
res.cagalt$freq$coord
res.cagalt$freq$cos2
res.cagalt$freq$contr

########## QUALITATIVE VARIABLES
names(res.cagalt$quali.var)
res.cagalt$quali.var$coord
res.cagalt$quali.var$cos2

########## ELLIPSES
names(res.cagalt$ellip)
res.cagalt$ellip$freq
res.cagalt$ellip$var

########## SUMMARY
summary(res.cagalt)

########## CA-GALT PLOTS
?plot.CaGalt
plot.CaGalt(res.cagalt,choix="freq",axes=c(1,2))

png(file=paste0(PATH_PLOTS, "/cagalt_words_12.png"),
    width=1920, height=1080, units="px", res=130)

plot.CaGalt(res.cagalt,choix="freq",axes=c(1,2),select = "contrib 10")
dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt_quanti_12.png"),
    width=1920, height=1080, units="px", res=130)

plot.CaGalt(res.cagalt,choix="quanti.var",axes=c(1,2))

dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt_tracks_34.png"),
    width=1920, height=1080, units="px", res=130)
plot.CaGalt(res.cagalt,choix="ind",axes=c(3,4), select = "cos2 10")
dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt_words_34.png"),
    width=1920, height=1080, units="px", res=130)

plot.CaGalt(res.cagalt,choix="freq",axes=c(3,4),select = "contrib 10")
dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt_quanti_34.png"),
    width=1920, height=1080, units="px", res=130)

plot.CaGalt(res.cagalt,choix="quanti.var",axes=c(3,4))

dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt_tracks_12.png"),
    width=1920, height=1080, units="px", res=130)
plot.CaGalt(res.cagalt,choix="ind",axes=c(1,2), select = "cos2 10")
dev.off()



?plot.CaGalt

res.cagalt$ind$coord
res.cagalt$ind$cos2
res.cagalt$freq$coord
res.cagalt$freq$contrib
res.cagalt$freq$cos2
res.cagalt$quanti.var$coord
res.cagalt$quanti.var$cor
res.cagalt$quanti.var$cos2

contrib_dim1 <- res.cagalt$freq$contrib[, 1]
contrib_dim2 <- res.cagalt$freq$contrib[, 2]

# Create data frames for the contributions
contrib_df1 <- data.frame(Feature = names(contrib_dim1), Contribution = contrib_dim1)
contrib_df2 <- data.frame(Feature = names(contrib_dim2), Contribution = contrib_dim2)

# Filter to keep only the most significant contributors
# Here, we assume 'most significant' means the top 10 contributors. You can adjust as needed.
top_n <- 10
top_contrib_df1 <- contrib_df1 %>% arrange(desc(Contribution)) %>% head(top_n)
top_contrib_df2 <- contrib_df2 %>% arrange(desc(Contribution)) %>% head(top_n)

# Plot contributions to the first dimension
plot1 <- ggplot(top_contrib_df1, aes(x = reorder(Feature, Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Contributions to Dimension 1", x = "Feature", y = "Contribution") +
  theme_minimal()

# Plot contributions to the second dimension
plot2 <- ggplot(top_contrib_df2, aes(x = reorder(Feature, Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Top Contributions to Dimension 2", x = "Feature", y = "Contribution") +
  theme_minimal()

# Print the plots
png(file=paste0(PATH_PLOTS, "/cagalt_contrib_1.png"),
    width=1920, height=1080, units="px", res=130)
print(plot1)
dev.off()
png(file=paste0(PATH_PLOTS, "/cagalt_contrib_2.png"),
    width=1920, height=1080, units="px", res=130)
print(plot2)
dev.off()

plot(res.cagalt, choix = "quanti.var", conf.ellip = TRUE, axes = c(3, 4))
plot(res.cagalt, choix = "freq", cex = 1.5, col.freq = "darkgreen",select = "contrib 10")

























# USING FACTOMINER CA-GALT -  UNUSED

subset <- unique_translated

names(unique_translated)
head(td.mat)

subset <- unique_translated[c('danceability', 'streams', 'track_popularity', 'energy', 'acousticness', 'liveness', 'valence', 'duration')]

# Rename the specified columns
colnames(subset) <- c('danceability_var', 'streams_var', 'track_popularity_var', 'energy_var', 'acousticness_var', 'liveness_var', 'valence_var', 'duration_var')


GALT <- cbind(t(as.matrix(td.mat)), subset)

rownames(GALT) <- unique_translated$track_name
# Step 3: Perform Correspondence Analysis
# Note: You might need to install the 'ca' package if you haven't already
ca_output <- CA(GALT)

# Step 4: Interpret the results
summary(ca_output)


rotate_labels <- function(angle = 45) {
  theme(axis.text.x = element_text(angle = angle, hjust = 1))
}

png(file=paste0(PATH_PLOTS, "/cagalt_1_dimension_docs.png"),
    width=1920, height=1080, units="px", res=130)

fviz_contrib(ca_output, axes = 1, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 1")+
  rotate_labels()
dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt_2_dimension_docs.png"),
    width=1920, height=1080, units="px", res=130)
fviz_contrib(ca_output, axes = 2, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 2")+
  rotate_labels()
dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt_3_dimension_docs.png"),
    width=1920, height=1080, units="px", res=130)

fviz_contrib(ca_output, axes = 3, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 3")+
  rotate_labels()
dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt_4_dimension_docs.png"),
    width=1920, height=1080, units="px", res=130)

fviz_contrib(ca_output, axes = 4, choice = "row", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 4")+
  rotate_labels()
dev.off()


png(file=paste0(PATH_PLOTS, "/cagalt_1_dimension_words.png"),
    width=1920, height=1080, units="px", res=130)

fviz_contrib(ca_output, axes = 1, choice = "col", top=5) +
  theme_minimal() +
  ggtitle("Contribution to Dimension 1")+
  rotate_labels()

dev.off()


png(file=paste0(PATH_PLOTS, "/cagalt_words_12.png"),
    width=1920, height=1080, units="px", res=130)
fviz_ca_col(ca_output, repel = TRUE, title = "CA Row Plot", select.col = list(contrib = 20)) + # Show top 10 contributing columns
  theme_minimal() +
  labs(color = "Row Points") +
  scale_color_manual(values = c("blue"))
dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt_documents_12.png"),
    width=1920, height=1080, units="px", res=130)

fviz_ca_row(ca_output, repel = TRUE, title = "CA Column Plot", select.row = list(contrib = 10)) + # Show top 10 contributing rows
  theme_minimal() +
  labs(color = "Column Points") +
  scale_color_manual(values = c("red"))
dev.off()

# Plot for dimensions 3 and 4 with row points and selectively visible column labels
png(file=paste0(PATH_PLOTS, "/cagalt_words_34.png"),
    width=1920, height=1080, units="px", res=130)
fviz_ca_col(ca_output, axes = c(3, 4), repel = TRUE,  title = "CA Row Plot (Dim 3 and 4)", select.col = list(contrib = 20)) + # Show top 10 contributing columns
  theme_minimal() +
  labs(color = "Row Points") +
  scale_color_manual(values = c("blue"))

dev.off()

png(file=paste0(PATH_PLOTS, "/cagalt_documents_34.png"),
    width=1920, height=1080, units="px", res=130)

# Plot for dimensions 3 and 4 with column points and selectively visible row labels
fviz_ca_row(ca_output, axes = c(3, 4), repel = TRUE, title = "CA Column Plot (Dim 3 and 4)", select.row = list(contrib = 10)) + # Show top 10 contributing rows
  theme_minimal() +
  labs(color = "Column Points") +
  scale_color_manual(values = c("red"))

dev.off()
