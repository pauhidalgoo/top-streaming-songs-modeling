library(tm) # text mining package
library(wordcloud) # word cloud generator
library(SnowballC) # text stemming
library(ggplot2) # graphs
library(tidyverse) # data manipulation
library(tidytext) # word lexicon dictionaries for sentiments
library(reshape2) # data transformation
library(textdata) # provides access to lexicon dictionaries
library(knitr) # used to make kable tables


load("./2_Descriptive analysis/unique_tracks.RData")
PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis",sep="")


track_names = unique_tracks$track_name

uncleaned_corpus<- Corpus(VectorSource(unique_tracks$lyrics))
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

PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis/tracks",sep="")


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

