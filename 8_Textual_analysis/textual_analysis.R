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
PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis/Analysis/Translated",sep="")


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

writeLines(head(strwrap(clean_corpus[[1]]), 7))


# COMPLETE CORPUS ---------------------------

tdm = TermDocumentMatrix(clean_corpus)

tdm2 = as.matrix(tdm)
words = sort(rowSums(tdm2), decreasing = TRUE)
df = data.frame(word = names(words), freq = words)
dim(df)

head(df,10)

set.seed(1000)


png(file=paste0(PATH_PLOTS, "/complete_word_cloud.png"),
    width=1920, height=1080, units="px", res=130)
wordcloud(clean_corpus
          , scale=c(5,0.5)     # Set min and max scale
          , max.words=200      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.20       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "Set2"))# other palette options: Accent, Dark2, Set1
dev.off()

png(file=paste0(PATH_PLOTS, "/complete_word_freq.png"),
    width=1920, height=1080, units="px", res=130)

barplot(df[1:20,]$freq, las=2, names.arg = df[1:20,]$word,
        col="lightblue", main="Top 20 Most Frequent Words",
        ylab="Word frequencies")
dev.off()


## ASSOCIATIONS --------------------------------

freedom <-data.frame(findAssocs(tdm, "Baby", 1))
freedom
my_title <-expression(paste("Words Correlated with ", bold("Baby")))
freedom %>% rownames_to_column() %>%
  ggplot(aes(x=reorder(rowname, freedom), y=freedom)) + 
  geom_point(shape=20, size=3) +  
  coord_flip() + ylab("Correlation") + xlab("Word") +
  ggtitle(my_title) + theme(plot.title = element_text(hjust = 0.0))





## NRC LEXICON ---------------------------------------------
# Inner join words with NRC lexicon

nrc_sent <-get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# There are 55 negative terms and 71 positive terms
nrc_df <- df %>% inner_join(nrc_sent)
# Plot of negative and positive sentiments


png(file=paste0(PATH_PLOTS, "/complete_word_senti_nrc.png"),
    width=1920, height=1080, units="px", res=130)

nrc_df %>%
  group_by(sentiment) %>%
  #slice_max(order_by = freq, n=10) %>%
  do(head(., n=10)) %>% # top 20 words
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment (NRC lexicon)", x=NULL) +
  coord_flip()

dev.off()

# NRC: 
nrc_df %>% group_by(sentiment) %>%
  summarize(total=sum(n)) %>%
  spread(sentiment, total) %>%
  mutate((net.sentiment=positive-negative)) %>% 
  kable(align = 'l')

png(file=paste0(PATH_PLOTS, "/complete_senti_cloud_nrc.png"),
    width=1920, height=1080, units="px", res=130)

set.seed(123)
nrc_df %>% 
  acast(word ~ sentiment, value.var = "freq", fill=0) %>%
  comparison.cloud(colors = brewer.pal(8,"Set1")
                   ,scale =c(5,.5), rot.per=0.1, title.size=2, max.words=100)

dev.off()
## BING LEXICON ------------------------------------

bing_sent <- df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# Inner join words with Bing lexicon
bing_df <- df %>% inner_join(bing_sent)



# Plot positive and negative sentiments
png(file=paste0(PATH_PLOTS, "/complete_word_senti_bing.png"),
    width=1920, height=1080, units="px", res=130)
bing_df %>%
  group_by(sentiment) %>%
  do(head(., n=10)) %>% # top 20 words
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq, fill=sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment (Bing lexicon)", x=NULL) +
  coord_flip()

dev.off()

# Bing: There are 54 negative terms and 46 positive terms
bing_df %>% group_by(sentiment) %>% 
  summarize(total=sum(n)) %>%
  spread(sentiment, total) %>%
  mutate((net.sentiment=positive-negative)) %>% 
  kable(align = 'l')

set.seed(123)
png(file=paste0(PATH_PLOTS, "/complete_senti_cloud_bing.png"),
    width=1920, height=1080, units="px", res=130)
bing_df %>% 
  acast(word ~ sentiment, value.var = "freq", fill=0) %>%
  comparison.cloud(colors = brewer.pal(8,"Set1")
                   ,scale =c(5,.5), rot.per=0.1, title.size=2, max.words=100)

dev.off()
# FOR EACH TRACK CORPUS ---------------------------

PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis/Analysis/Translated/Tracks",sep="")


for (i in 1:min(10,length(clean_corpus))) {
  corpus <- content(clean_corpus[[i]])
  name = track_names[i]
  print(name)
  name = gsub(" ", "", name , fixed = TRUE)
  name = gsub("...", "", name, fixed= TRUE)
  name = gsub("'", "", name, fixed= TRUE)
  name = gsub("?", "", name, fixed= TRUE)
  print(name)
  i = i + 1
tdm = TermDocumentMatrix(corpus)

tdm2 = as.matrix(tdm)
words = sort(rowSums(tdm2), decreasing = TRUE)
df = data.frame(word = names(words), freq = words)
dim(df)


set.seed(1000)
png(file=paste0(PATH_PLOTS, paste0("/",name,"_word_cloud.png")),
    width=1920, height=1080, units="px", res=130)
wordcloud(corpus
          , scale=c(5,0.5)     # Set min and max scale
          , max.words=200      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.20       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "Set2"))# other palette options: Accent, Dark2, Set1
dev.off()
png(file=paste0(PATH_PLOTS, paste0("/",name,"_word_freq.png")),
    width=1920, height=1080, units="px", res=130)

barplot(df[1:20,]$freq, las=2, names.arg = df[1:20,]$word,
        col="lightblue", main=paste0("Top 20 Most Frequent Words in ",name),
        ylab=paste0(name, " Word frequencies"))
dev.off()


## NRC LEXICON ---------------------------------------------
# Inner join words with NRC lexicon

nrc_sent <-get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# There are 55 negative terms and 71 positive terms
nrc_df <- df %>% inner_join(nrc_sent)
# Plot of negative and positive sentiments

a <- nrc_df %>%
  group_by(sentiment) %>%
  #slice_max(order_by = freq, n=10) %>%
  do(head(., n=10)) %>% # top 20 words
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment (NRC lexicon)", x=NULL) +
  coord_flip()

ggsave(paste0("/",name,"_word_senti_nrc.png"),plot=a, bg="white", path = PATH_PLOTS,dpi=150,limitsize = FALSE,width = 1920, height=1080, units = "px")

# NRC: 
positive = 0
negative = 0
nrc_df %>% group_by(sentiment) %>%
  summarize(total=sum(n)) %>%
  spread(sentiment, total) %>%
  mutate((net.sentiment=positive-negative)) %>% 
  kable(align = 'l')

if (all(c("negative", "positive") %in% unique(nrc_df["sentiment"])[,])) {
  set.seed(123)
  png(file=paste0(PATH_PLOTS, paste0("/",name,"_senti_cloud_nrc.png")),
      width=1920, height=1080, units="px", res=130)
  nrc_df %>% 
    acast(word ~ sentiment, value.var = "freq", fill=0) %>%
    comparison.cloud(colors = brewer.pal(8,"Set1")
                     ,scale =c(5,.5), rot.per=0.1, title.size=2, max.words=100)
  dev.off()
}


## BING LEXICON ------------------------------------

bing_sent <- df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
# Inner join words with Bing lexicon
bing_df <- df %>% inner_join(bing_sent)
# Plot positive and negative sentiments
a <- bing_df %>%
  group_by(sentiment) %>%
  do(head(., n=10)) %>% # top 20 words
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq, fill=sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment (Bing lexicon)", x=NULL) +
  coord_flip()

ggsave(paste0("/",name,"_word_senti_bing.png"),plot=a, bg="white", path = PATH_PLOTS,dpi=150,limitsize = FALSE,width = 1920, height=1080, units = "px")

# Bing: There are 54 negative terms and 46 positive terms
bing_df %>% group_by(sentiment) %>% 
  summarize(total=sum(n)) %>%
  spread(sentiment, total) %>%
  mutate((net.sentiment=positive-negative)) %>% 
  kable(align = 'l')


if (all(c("negative", "positive") %in% unique(bing_df["sentiment"])[,])) {
set.seed(123)
  png(file=paste0(PATH_PLOTS, paste0("/",name,"_cloud_senti_bing.png")),
      width=1920, height=1080, units="px", res=130)
bing_df %>% 
  acast(word ~ sentiment, value.var = "freq", fill=0) %>%
  comparison.cloud(colors = brewer.pal(8,"Set1")
                   ,scale =c(5,.5), rot.per=0.1, title.size=2, max.words=100)
dev.off()
}
}






# CORRESPONDENCE ANALYSIS --------------------------------------

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

########## RESPONDENTS
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
fviz_ca_col(res.ca, repel = TRUE, title = "CA Row Plot", select.col = list(contrib = 20)) + # Show top 10 contributing columns
  theme_minimal() +
  labs(color = "Row Points") +
  scale_color_manual(values = c("blue"))

fviz_ca_row(res.ca, repel = TRUE, title = "CA Column Plot", select.row = list(contrib = 10)) + # Show top 10 contributing rows
  theme_minimal() +
  labs(color = "Column Points") +
  scale_color_manual(values = c("red"))

# Plot for dimensions 3 and 4 with row points and selectively visible column labels
fviz_ca_col(res.ca, axes = c(3, 4), repel = TRUE,  title = "CA Row Plot (Dim 3 and 4)", select.col = list(contrib = 20)) + # Show top 10 contributing columns
  theme_minimal() +
  labs(color = "Row Points") +
  scale_color_manual(values = c("blue"))

# Plot for dimensions 3 and 4 with column points and selectively visible row labels
fviz_ca_row(res.ca, axes = c(3, 4), repel = TRUE, title = "CA Column Plot (Dim 3 and 4)", select.row = list(contrib = 10)) + # Show top 10 contributing rows
  theme_minimal() +
  labs(color = "Column Points") +
  scale_color_manual(values = c("red"))




# MORE SENTIMENT -----------------------------

load("./8_Textual_analysis/unique_tracks_translated.RData")


# Step 1: Tokenize the lyrics and calculate sentiment for each word using Bing lexicon
unique_translated <- unique_translated %>%
  mutate(document = row_number())

# Step 2: Tokenize the lyrics and join with Bing lexicon
word_sentiments <- unique_translated %>%
  unnest_tokens(word, translated_lyrics) %>%
  inner_join(get_sentiments("bing"), by = "word")

# Step 3: Calculate positive and negative word counts for each song
song_sentiments <- word_sentiments %>%
  group_by(document) %>%
  summarize(positive = sum(sentiment == "positive"),
            negative = sum(sentiment == "negative")) %>%
  ungroup()
# Calculate sentiment score for each song
song_sentiments <- song_sentiments %>%
  mutate(total = positive + negative,
         score = case_when(
           total == 0 ~ 0.5, # Neutral score if no sentiment words
           TRUE ~ positive / total
         )) %>%
  select(document, score)

# Join the scores back to the unique_translated dataframe
unique_translated <- unique_translated %>%
  mutate(document = row_number()) %>%
  left_join(song_sentiments, by = "document") %>%
  select(-document)

# Display the dataframe with sentiment scores
print(unique_translated$score)

print(unique_translated[246,]$translated_lyrics)


library(dplyr)
library(ggplot2)
library(tidyr)

# Assuming unique_translated is the dataframe with sentiment scores and genre columns
# Calculate the mean sentiment score for each genre

# Genres: pop, hip_hop, rock, latino, electro, christmas, cinema
genre_columns <- c("pop", "hip_hop", "rock", "latino", "electro", "christmas", "cinema")

# Calculate mean sentiment scores for each genre
mean_scores_by_genre <- unique_translated %>%
  gather(genre, is_genre, all_of(genre_columns)) %>%
  filter(is_genre == TRUE) %>%
  group_by(genre) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ungroup()

png(file=paste0(PATH_PLOTS, "/mean_sentiment_genre.png"),
    width=1920, height=1080, units="px", res=130)
# Plot the mean sentiment scores by genre
ggplot(mean_scores_by_genre, aes(x = genre, y = mean_score, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Sentiment Score by Genre",
       x = "Genre",
       y = "Mean Sentiment Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Calculate average sentiment score per artist
artist_sentiments <- unique_translated %>%
  group_by(artist_name) %>%
  summarize(average_score = mean(score, na.rm = TRUE)) %>%
  arrange(desc(average_score))


png(file=paste0(PATH_PLOTS, "/mean_sentiment_artist.png"),
    width=1920, height=1080, units="px", res=130)

# Plot average sentiment scores by artist
ggplot(artist_sentiments, aes(x = reorder(artist_name, average_score), y = average_score)) +
  geom_bar(stat = "identity", fill = "cyan") +
  coord_flip() +
  labs(title = "Average Sentiment Score by Artist",
       x = "Artist",
       y = "Average Sentiment Score") +
  theme_minimal()

dev.off()

explicit_sentiments <- unique_translated %>%
  group_by(explicit) %>%
  summarize(average_score = mean(score, na.rm = TRUE)) %>%
  arrange(desc(average_score))

png(file=paste0(PATH_PLOTS, "/mean_sentiment_explicit.png"),
    width=1920, height=1080, units="px", res=130)
# Plot average sentiment scores by explicit
ggplot(explicit_sentiments, aes(x = reorder(explicit, average_score), y = average_score, fill=explicit)) +
  geom_bar(stat = "identity")+
  labs(title = "Average Sentiment Score by Explicit",
       x = "Explicit",
       y = "Average Sentiment Score") +
  theme_minimal()
dev.off()


mean(unique_translated$score, na.rm= TRUE)
