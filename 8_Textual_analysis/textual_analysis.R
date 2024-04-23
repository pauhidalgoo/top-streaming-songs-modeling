library(tm) # text mining package
library(wordcloud) # word cloud generator
library(SnowballC) # text stemming
library(ggplot2) # graphs
library(tidyverse) # data manipulation
library(tidytext) # word lexicon dictionaries for sentiments
library(reshape2) # data transformation
library(textdata) # provides access to lexicon dictionaries
library(knitr) # used to make kable tables


load("./3_Preprocessing/data_knn_imputed_unknown.RData")

text = data[1,"lyrics"]
text

uncleaned_corpus<- Corpus(VectorSource(text))
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

tdm = TermDocumentMatrix(clean_corpus)

tdm2 = as.matrix(tdm)
words = sort(rowSums(tdm2), decreasing = TRUE)
df = data.frame(word = names(words), freq = words)
dim(df)

set.seed(1000)
wordcloud(clean_corpus
          , scale=c(5,0.5)     # Set min and max scale
          , max.words=200      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.20       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "Set2"))# other palette options: Accent, Dark2, Set1

barplot(df[1:20,]$freq, las=2, names.arg = df[1:20,]$word,
        col="lightblue", main="Top 20 Most Frequent Words",
        ylab="Word frequencies")

# ASSOCIATIONS --------------------------------
freedom <-data.frame(findAssocs(tdm, "baby", 0.05))
freedom
my_title <-expression(paste("Words Correlated with ", bold("baby")))
freedom %>% rownames_to_column() %>%
  ggplot(aes(x=reorder(rowname, freedom), y=freedom)) + 
  geom_point(shape=20, size=3) +  
  coord_flip() + ylab("Correlation") + xlab("Word") +
  ggtitle(my_title) + theme(plot.title = element_text(hjust = 0.0))





# NRC LEXICON ---------------------------------------------
# Inner join words with NRC lexicon

nrc_sent <-get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

# There are 55 negative terms and 71 positive terms
nrc_df <- df %>% inner_join(nrc_sent)
# Plot of negative and positive sentiments
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

# NRC: 
nrc_df %>% group_by(sentiment) %>%
  summarize(total=sum(n)) %>%
  spread(sentiment, total) %>%
  mutate((net.sentiment=positive-negative)) %>% 
  kable(align = 'l')

set.seed(123)
nrc_df %>% 
  acast(word ~ sentiment, value.var = "freq", fill=0) %>%
  comparison.cloud(colors = brewer.pal(8,"Set1")
                   ,scale =c(5,.5), rot.per=0.1, title.size=2, max.words=100)

# BING LEXICON ------------------------------------

bing_sent <- df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
# Inner join words with Bing lexicon
bing_df <- df %>% inner_join(bing_sent)
# Plot positive and negative sentiments
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

# Bing: There are 54 negative terms and 46 positive terms
bing_df %>% group_by(sentiment) %>% 
  summarize(total=sum(n)) %>%
  spread(sentiment, total) %>%
  mutate((net.sentiment=positive-negative)) %>% 
  kable(align = 'l')

set.seed(123)
bing_df %>% 
  acast(word ~ sentiment, value.var = "freq", fill=0) %>%
  comparison.cloud(colors = brewer.pal(8,"Set1")
                   ,scale =c(5,.5), rot.per=0.1, title.size=2, max.words=100)
