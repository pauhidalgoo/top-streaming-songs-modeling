library(syuzhet)
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

palabras <- syuzhet::get_tokens(clean_corpus)
cat("Tenemos", length(palabras), "palabras en nuestro Corpus\n")

#sentimentOpinion <- syuzhet::get_nrc_sentiment(char_v = palabras, language = "english")

#saveRDS(sentimentOpinion, file = "./8_textual_analysis/sentimentOpinion.rds")

readRDS(file = "./8_textual_analysis/sentimentOpinion.rds")

head(sentimentOpinion)

## Mostramos las emociones que podemos encontrarnos en la matriz
Emotions <- sentimentOpinion %>% select(-negative,-positive) %>% names()

## Visualizamos el nÃºmero de palabras que hay para cada categorias
Scoring <- sentimentOpinion %>% select(-negative,-positive) %>% colSums()

## Generamos un data frame con la informaciÃ³n del conteo de cada sentimiento
tblSentiment <- cbind(Emotions, Scoring) %>% 
  as.data.frame() %>% 
  mutate(Scoring = as.numeric(Scoring)) %>% 
  `rownames<-`( NULL )

## Graficamos el sentimiento 
ggpubr::ggbarplot(tblSentiment, x = "Emotions", y = "Scoring",
                  fill = "Emotions", color = "Emotions", palette = "rainbow",label=TRUE,
                  lab.pos = "out",lab.size=3.5, lab.col = "black")+theme(legend.position="none")

Assessment <- sentimentOpinion %>% select(negative, positive) %>% names()

## Visualizamos el nÃºmero de palabras que hay para cada categorias
Scoring <- sentimentOpinion %>% select(negative, positive) %>% colSums()

## Generamos un data frame con la informaciÃ³n del conteo de cada sentimiento
tblPosicion <- cbind(Assessment, Scoring) %>% 
  as.data.frame() %>% 
  mutate(Scoring = as.numeric(Scoring)) %>% 
  `rownames<-`( NULL )

## Graficamos la tipologia 
ggpubr::ggbarplot(tblPosicion, x = "Assessment", y = "Scoring",
                  fill = "Assessment", color = "Assessment", palette = "paired",label=TRUE,
                  lab.pos = "out",lab.size=3.5, lab.col = "black")+theme(legend.position="none")


# ==============================================================================
# Creamos la nube de palabras
## Creamos la matriz de los datos que contiene los sentimientos: anger, joy, sadness, trust
emotional_cloud <- c(
  paste(palabras[sentimentOpinion$anger > 0], collapse = " "),
  paste(palabras[sentimentOpinion$anticipation > 0], collapse = " "),
  paste(palabras[sentimentOpinion$disgust > 0], collapse = " "),
  paste(palabras[sentimentOpinion$fear > 0], collapse = " "),
  paste(palabras[sentimentOpinion$joy > 0], collapse = " "),
  paste(palabras[sentimentOpinion$sadness > 0], collapse = " "),
  paste(palabras[sentimentOpinion$surprise > 0], collapse = " "),
  paste(palabras[sentimentOpinion$trust > 0], collapse = " "))

## Cambiamos la codificaciÃ³n de las palabras
emotional_cloud <- iconv(emotional_cloud, "latin1", "UTF-8")

# ------------------------------------------------------------------------------
## Creamos el corpus de las palabras en las que generaremos el cloud
corpus_texto = tm::Corpus(tm::VectorSource(emotional_cloud))

# ------------------------------------------------------------------------------
## Ahora transformamos el corpus en una matriz â€œtÃ©rmino-documentoâ€ con la funcion 
## â€˜TermDocumentMatrix()â€™, y luego utilizamos â€˜as.matrixâ€™ para convertir esa matriz 
## â€œtermino-documentoâ€ en una matriz que cuenta con el listado de los terminos del 
## texto con un mayor valor a 0 dentro de cada una de las 4 emociones que hemos 
## extraido.
cloud_TDM <- tm::TermDocumentMatrix(corpus_texto) %>%
  as.matrix(cloud_TDM)
colnames(cloud_TDM) <- c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')

## Visualizamos la matriz obtenida
head(cloud_TDM)



# ------------------------------------------------------------------------------
## Realizamos la nube de palabras con las palabras que tenemos
set.seed(123)
wordcloud::comparison.cloud(cloud_TDM, random.order = FALSE,
                            colors = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#c9c365", "#336341","#66c2a5","#3288bd"),
                            title.size = 1, max.words = 150, scale = c(1.35, 0.8), rot.per = 0.4)



track_emotion_scores_df <- data.frame(track_id = numeric(),
                                      anger_score = numeric(),
                                      anticipation_score = numeric(),
                                      disgust_score = numeric(),
                                      fear_score = numeric(),
                                      joy_score = numeric(),
                                      sadness_score = numeric(),
                                      surprise_score = numeric(),
                                      trust_score = numeric())


for (i in 1:nrow(unique_tracks)) {
  # Get the lyrics for the current track
  lyrics <- unique_tracks[i, "lyrics"]
  
  # Skip tracks with empty lyrics
  if (is.na(lyrics) || lyrics == "") {
    next
  }
  
  # Convert lyrics to lowercase
  lyrics <- tolower(lyrics)
  
  # Tokenize the lyrics
  words <- strsplit(lyrics, "\\s+")
  words <- unlist(words)
  
  # Calculate emotion scores for each word
  word_emotion_scores <- data.frame(
    word = words,
    anger = sentimentOpinion$anger[match(words, palabras)],
    anticipation = sentimentOpinion$anticipation[match(words, palabras)],
    disgust = sentimentOpinion$disgust[match(words, palabras)],
    fear = sentimentOpinion$fear[match(words, palabras)],
    joy = sentimentOpinion$joy[match(words, palabras)],
    sadness = sentimentOpinion$sadness[match(words, palabras)],
    surprise = sentimentOpinion$surprise[match(words, palabras)],
    trust = sentimentOpinion$trust[match(words, palabras)]
  )
  
  # Aggregate scores for each emotion
  track_emotion_scores <- aggregate(. ~ word, data = word_emotion_scores, sum)
  
  track_emotion_scores <- colSums(track_emotion_scores[, c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")])
  track_emotion_scores
  names(track_emotion_scores) <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
  # Add the aggregated scores as new columns to the unique_tracks dataframe
  track_emotion_scores$track_id <- unique_tracks[i, "track_id"]
  
  # Bind the aggregated scores to the track_emotion_scores_df dataframe
  track_emotion_scores_df <- rbind(track_emotion_scores_df, track_emotion_scores)

}
#Track ID peta? 
track_emotion_scores_df <- subset(track_emotion_scores_df, select = -track_id)

emotions_df <- cbind(unique_tracks, track_emotion_scores_df)
save(emotions_df, file = "./8_textual_analysis/emotions_unique_tracks.RData")

