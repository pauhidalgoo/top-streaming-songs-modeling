library(topicmodels)
library(ldatuning)
library(tm)
library(quanteda)
library(MASS)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidytext)
library(gridExtra)


load("./2_Descriptive_analysis/unique_tracks.RData")
PATH_PLOTS <- "./Media/Textual_Analysis/LDA"

track_names = unique_tracks$track_name

uncleaned_corpus<- Corpus(VectorSource(unique_tracks$lyrics))
uncleaned_corpus

#-------------------------------------------------------------------------------
# PREPROCESAMENT DEL TEXT


preprocess <- function(input_corpus){
  texto <- readLines("./8_Textual_Analysis/gh-stopwords-json-es.txt")
  palabras_es <- unlist(strsplit(texto, "\n"))
  # cleaning corpus
  writeLines(head(strwrap(input_corpus[[1]]), 7))
  
  clean_corpus <- tm_map(input_corpus, content_transformer(tolower))
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
  
  
  # Parte manual, palabras que queramos eliminar
  old_clean_corpus <- clean_corpus
  # Customize your own list of words for removal
  clean_corpus <- tm_map(clean_corpus, removeWords, c("yeah", "ere", "yeh", "ahn"))
  #stopwords en castellano
  stopwords_es <- stopwords(language= "es")
  clean_corpus <- tm_map(clean_corpus, removeWords, stopwords_es)
  clean_corpus <- tm_map(clean_corpus, removeWords, palabras_es)
  
  corpus <- clean_corpus
  return(corpus)
}

corpus <- preprocess(uncleaned_corpus)


#-------------------------------------------------------------------------------
# MODELO LDA
#-------------------------------------------------------------------------------

# Creación  de la TermDocMatrix (3% de frecuencia)
td.mat <- as.matrix(TermDocumentMatrix(corpus, control = list(bounds=list(global = c(3, Inf)))))
dimnames(td.mat)

# Finetunning para escoger el número de topics
result <- ldatuning::FindTopicsNumber(
  td.mat,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014", 'Griffiths2004', 'Arun2010'),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

# Gràfic amb el finetunning del model
png(file=paste0(PATH_PLOTS, "/lda_finetunning.png"),
    width=1920, height=1080, units="px", res=130)
FindTopicsNumber_plot(result)
dev.off()

# Es veu que k=6, k=8 podrien ser bones mètriques

#-------------------------------------------------------------------------------
# MODELO LDA TRAS ESCOGER LA K
ap_lda <- LDA(t(td.mat), k = 6, control = list(seed = 42))

# Obtenció de les betes, importància d'una paraula per cadascun dels tòpics
ap_topics <- tidy(ap_lda, matrix = "beta")

# Selecció de les top 10 paraules segons les betes
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# Creació del plot amb les top 10 paraules per tòpic
png(file=paste0(PATH_PLOTS, "/lda_topics6.png"),
    width=1920, height=1080, units="px", res=200)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
dev.off()

# Anomenació dels tòpics
topic_names <- c("alegria", "malsonant", "castellà", "amor 1", "amor 2", "nostàlgia")


#-------------------------------------------------------------------------------
# Profiling dels Tòpics del LDA (k=6)
#-------------------------------------------------------------------------------

# Obtenció de les gammas per escollir el tòpic principal per cada cançó
gammas <- posterior(ap_lda)$topics
topics_prof <- as.data.frame(gammas)

document_topics <- topics_prof %>%
  mutate(predominant_topic = apply(gammas, 1, which.max))

# Afegir la nova columna a la base de dades
unique_tracks$LDA_topic <- factor(document_topics$predominant_topic, labels = topic_names)

# Funcions definides per crear els diferents gràfics de profiling
create_bar_plot_bin <- function(data, topic_col, categoric_col) {
  ggplot(data, aes_string(x = topic_col, fill = categoric_col)) +
    geom_bar(position = "fill") +
    labs(y = "Proporció", x="", fill = categoric_col) +
    scale_fill_manual(values = c("#cf2a25", "#1db954", "#c325cf", "#cf2a25", "#d46c06", "#205633"))
}

create_box_plot <- function(data, topic_col, numeric_var){
  ggplot(data, aes_string(x = topic_col, y = numeric_var)) +
    geom_boxplot() +
    labs(x = "Tòpics", y =  numeric_var) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))}

create_bar_plot <- function(data, topic_col, categoric_col) {
  ggplot(data, aes_string(x = topic_col, fill = categoric_col)) +
    geom_bar(position = position_dodge(), width = 0.7) +
    labs(y = "Proporció", x = "Topics", fill = categoric_col) +
    scale_fill_manual(values = c("#cf2a25", "#1db954", "#c325cf", "#d46c06", "#205633", "#67b1ff","#cf2555")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Llista de variables binàries
binary_vars <- c("pop", "hip_hop", "rock", "electro", "christmas", "cinema", "latino", "explicit")#, "gender")#, "collab", "explicit")

# Crear gráfics per cada variable binaria
plots_bin <- lapply(binary_vars, function(var) {
  create_bar_plot_bin(unique_tracks, "LDA_topic", var)
})

# Guardar els gràfics per les variables binàries
png(file=paste0(PATH_PLOTS, "/profiling_genres.png"),
    width=1920, height=1580, units="px", res=200)
do.call(grid.arrange, c(plots_bin, ncol = 2))
dev.off()


# Profiling per les variables numériques més destacades
numeric_vars <- c("artist_popularity", "artist_followers", "danceability", "energy", "loudness", "speechiness", "acousticness", "liveness", "valence", "tempo", "duration", "streams")

plots_num <- lapply(numeric_vars, function(var) {
  create_box_plot(unique_tracks, "LDA_topic", var)
})

png(file=paste0(PATH_PLOTS, "/profiling_nums.png"),
    width=1920, height=1580, units="px", res=130)
do.call(grid.arrange, c(plots_num, ncol = 3))
dev.off()


# Profiling per les variables categòriques restants
unique_tracks$artist_num <- as.factor(unique_tracks$artist_num)
categoric_vars <- c("artist_num",  "time_signature", "gender")

plots_cat <- lapply(categoric_vars, function(var) {
  create_bar_plot(unique_tracks, "LDA_topic", var)
})

png(file=paste0(PATH_PLOTS, "/profiling_cats_2.png"),
    width=1920, height=500, units="px", res=130)
do.call(grid.arrange, c(plots_cat, ncol = 3))
dev.off()


# Profiling per les variables binàries restants
bin_vars_2 <- c("collab", "major_mode","is_group")

plots_bin2 <- lapply(bin_vars_2, function(var) {
  create_bar_plot_bin(unique_tracks, "LDA_topic", var)
})

png(file=paste0(PATH_PLOTS, "/profiling_bins_2.png"),
    width=1920, height=500, units="px", res=130)
do.call(grid.arrange, c(plots_bin2, ncol = 3))
dev.off()


#-------------------------------------------------------------------------------
# REPRESENTACION DE EL ESPACIO DE LAS PALABRAS
#-------------------------------------------------------------------------------

# Obtener las probabilidades de las palabras en cada tema
betas <- t(posterior(ap_lda)$terms)
betas
# Calcular la matriz de distancia (usamos distancia euclidiana)
dist_matrix <- dist(betas)

# Aplicar MDS para reducir la dimensionalidad a 2D
mds_result <- cmdscale(dist_matrix, k = 2)

# Crear un data frame con los resultados de MDS y los nombres de las palabras
mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("Dim1", "Dim2")
mds_df$word <- rownames(betas)
mds_df$topic <- apply(betas, 1, function(row) {
  colnames(betas)[which.max(row)]
})

mds_df$topic <- factor(as.numeric(mds_df$topic), labels = topic_names)


png(file=paste0(PATH_PLOTS, "/MDS_paraules_full.png"),
    width=1720, height=1720, units="px", res=140)
ggplot(mds_df, aes(x = Dim1, y = Dim2, label = word)) +
  geom_point(color = "blue") +
  geom_text(vjust = 1, hjust = 1) +
  ggtitle("MDS de les paraules en funció dels tòpics LDA") +
  xlab("Dimensió 1") +
  ylab("Dimensió 2") +
  theme_minimal()
dev.off()

png(file=paste0(PATH_PLOTS, "/MDS_paraules_zoom.png"),
    width=1920, height=1920, units="px", res=140)
ggplot(mds_df, aes(x = Dim1, y = Dim2, label = word)) +
  geom_point(color = "blue") +
  geom_text(vjust = 1, hjust = 1) +
  ggtitle("Zoom del MDS de les paraules en funció dels tòpics LDA") +
  xlab("Dimensió 1") +
  ylab("Dimensió 2") +
  xlim(c(-0.03,0.0)) + 
  ylim(c(-0.02, 0.01)) +
  theme_minimal()
dev.off()
  
png(file=paste0(PATH_PLOTS, "/MDS_paraules_topic.png"),
    width=1920, height=1920, units="px", res=140)
ggplot(mds_df, aes(x = Dim1, y = Dim2, color = topic)) +
    geom_point(size=1.3) +
    ggtitle("MDS Plot of Words based on LDA Topics") +
    xlab("Dimensió 1") +
    ylab("Dimensió 2") +
    xlim(c(-0.03,0.0)) + 
    ylim(c(-0.02, 0.01)) +
    theme_minimal()
dev.off()


#-------------------------------------------------------------------------------
# REPRESENTACION DE EL ESPACIO DE LOS DOCUMENTOS
#-------------------------------------------------------------------------------

gammas <- posterior(ap_lda)$topics

doc_names <- as.character(unique_tracks$track_name)

#ddd <- data.frame(gammas)

dist_mat <- dist(gammas)

mds_result_docs <- cmdscale(dist_mat, k = 2)

mds_docs_df <- as.data.frame(mds_result_docs)
colnames(mds_docs_df) <- c("Dim1", "Dim2")
mds_docs_df$track_name <- unique_tracks$track_name


library(ggrepel)

set.seed(42)
mds_red <- mds_docs_df %>% sample_frac(0.1)


png(file=paste0(PATH_PLOTS, "/MDS_docs_10percnt.png"),
    width=1920, height=1920, units="px", res=120)
ggplot(mds_red, aes(x = Dim1, y = Dim2, label = track_name)) +
  geom_point(color = "blue") +
  geom_text_repel(aes(label = track_name), 
                  max.overlaps = Inf,
                  box.padding = 0.35, 
                  point.padding = 0.3, 
                  segment.color = 'grey50') +
  ggtitle("MDS Plot of Words based on LDA Topics") +
  xlab("Dimension 1") +
  ylab("Dimension 2") +
  theme_minimal()
dev.off()

variables_cat <- c("pop", "hip_hop", "rock", "electro", "christmas", "cinema", "latino", "explicit", "collab")#, "explicit")

for (cat_var in variables_cat){
  mds_docs_df[, cat_var] <- unique_tracks[, cat_var]
}
mds_docs_df$topic <- unique_tracks$LDA_topic

create_mds_filter_plot <- function(data, var){
  ggplot(data, aes_string(x = "Dim1", y = "Dim2", color = var)) +
    geom_point(size=1.4) +
    ggtitle(paste("Representació de les cançons segons ", var)) +
    xlab("Dimensió 1") +
    ylab("Dimensió 2") +
    theme_minimal() +
    scale_color_manual(values = c("#cf2a25", "#1db954", "#c325cf", "#d46c06", "#205633", "#67b1ff","#cf2555"))
    
}

plots_mds_doc_genre <- lapply(variables_cat, function(var) {
  create_mds_filter_plot(mds_docs_df, var)
})

png(file=paste0(PATH_PLOTS, "/MDS_docs_filtered.png"),
    width=1920, height=1920, units="px", res=120)
do.call(grid.arrange, c(plots_mds_doc_genre, ncol = 3))
dev.off()
plots_mds_doc_genre


png(file=paste0(PATH_PLOTS, "/MDS_docs_by_topic.png"),
    width=1700, height=1700, units="px", res=130)
ggplot(mds_docs_df, aes(x = Dim1, y = Dim2, color = topic)) +
  geom_point(size=2) +
  ggtitle(paste("Representació de les cançons segons el topic")) +
  xlab("Dimensió 1") +
  ylab("Dimensió 2") +
  theme_minimal() 
dev.off()


#-------------------------------------------------------------------------------
# Time Series amb Tòpics per any
#-------------------------------------------------------------------------------

library(topicmodels)

# Obtener los topics para cada uno de los documentos, seleccionando el más probable para
# cada uno de los documentos

posterior_probs <- posterior(ap_lda)$topics
posterior_df <- as.data.frame(posterior_probs)
posterior_df$document <- track_names

document_topics <- posterior_df %>%
  mutate(predominant_topic = apply(posterior_probs, 1, which.max))

document_topics$predominant_topic <- as.integer(document_topics$predominant_topic)
document_topics$year_release <- as.numeric(as.character(unique_tracks$year_release))
document_topics$month_release <- as.numeric(as.character(unique_tracks$month_release))

View(document_topics)

dc_cpy <- document_topics



dc_cpy <- dc_cpy %>% 
  mutate(
  year_season = ifelse(month_release %in% c(12), year_release + 1, year_release)
  )

dc_cpy <- dc_cpy %>%  mutate(season = case_when(
  month_release %in% c(1, 2, 12) ~ "1",
  month_release %in% c(3, 4, 5) ~ "2",
  month_release %in% c(6, 7, 8) ~ "3",
  month_release %in% c(9,10,11) ~ "4"
))




# Paso 1: Contar la cantidad de productos por año y tópico
count_data <- dc_cpy %>%
  dplyr::count(year_season, season, predominant_topic, name = "count")

count_data$predominant_topic <- as.factor(count_data$predominant_topic)

count_data <- count_data[count_data[,"year_season"]>2015,]


my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#eebb44", "#bcbd22")


ggplot(data=count_data, aes(x=paste(year_season, season, sep="-"), y=count, group=predominant_topic, color = predominant_topic)) +
  geom_line(size=0.6) +
  theme_minimal() +
  scale_color_manual(values = my_colors) 

# Plot separado por tópico predominante
ggplot(data=count_data, aes(x=paste(year_season, season, sep="-"), y=count, group=predominant_topic, color = predominant_topic)) +
  geom_line(size=0.6) +
  theme_minimal() +
  scale_color_manual(values = my_colors) +
  facet_wrap(~predominant_topic, ncol = 2)

#, color=predominant_topic)


#-------------------------------------------------------------------------------
# Time Series amb Tòpics per estació

load("./3_Preprocessing/data_knn_imputed_unknown.RData")
weekly_data <- data %>% select(track_name, year_week, month_week)
weekly_data$track_name <- as.factor(weekly_data$track_name)

songs_to_topics <- dc_cpy %>% select(document, predominant_topic)
names(songs_to_topics) <- c("track_name", "predominant_topic")

merged_data <- weekly_data %>%
  left_join(songs_to_topics, by = "track_name")

merged_data$year_week <- as.numeric(as.character(merged_data$year_week))
merged_data$month_week <- as.numeric(as.character(merged_data$month_week))

merged_data <- merged_data %>% 
  mutate(
    year_season = ifelse(month_week %in% c(12), year_week + 1, year_week)
  )

merged_data <- merged_data %>%  mutate(season = case_when(
  month_week %in% c(1, 2, 12) ~ "1",
  month_week %in% c(3, 4, 5) ~ "2",
  month_week %in% c(6, 7, 8) ~ "3",
  month_week %in% c(9,10,11) ~ "4"
))



count_data <- merged_data %>%
  dplyr::count(year_season, season, predominant_topic, name = "count")

count_data$predominant_topic <- as.factor(count_data$predominant_topic)


my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#eebb44", "#bcbd22")


ggplot(data=count_data, aes(x=paste(year_season, season, sep="-"), y=count, group=predominant_topic, color = predominant_topic)) +
  geom_line(size=0.6) +
  theme_minimal() +
  scale_color_manual(values = my_colors) 








lyrics_pop <- unique_tracks[unique_tracks[,"hip_hop"]==TRUE,]

lyrics_pop 

uncleaned_pop<- Corpus(VectorSource(lyrics_pop$lyrics))



cleaned_pop <- preprocess(uncleaned_pop)

td.mat <- as.matrix(TermDocumentMatrix(cleaned_pop, control = list(global = c(5, Inf))))


term_frequencies <- rowSums(td.mat)
term_frequencies_df <- data.frame(term = names(term_frequencies), frequency = term_frequencies)





dd
n=20
library(dplyr)
top_terms <- term_frequencies_df %>%
  arrange(desc(frequency)) %>%
  top_n(n = n)
top_terms



# Paso 3: Combinar las palabras más frecuentes con las palabras del modelo LDA y encontrar los tópicos

# Filtrar los términos más frecuentes que están en los tópicos del modelo LDA
common_terms <- ap_topics %>%
  filter(term %in% top_terms$term)

# Imprimir los términos más frecuentes y sus tópicos
print(common_terms)




years<-as.integer(unique_tracks$year_release)
ap_lda
td.mat

length(topics)
length(years)

as.data.frame(years, topics)


2000:2021


















