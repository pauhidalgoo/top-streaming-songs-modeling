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
library(ggrepel)


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

# Obtener los topics para cada uno de los documentos, seleccionando el más probable para
# cada uno de los documentos

# Por fecha de salida

release_date_topics <- unique_tracks[,c("LDA_topic", "track_name", "year_release", "month_release")]

release_date_topics$year_release <- as.numeric(as.character(release_date_topics$year_release))
release_date_topics$month_release <- as.numeric(as.character(release_date_topics$month_release))

release_date_topics <- release_date_topics %>% 
  mutate(
    year_season = ifelse(month_release %in% c(12), year_release + 1, year_release)
  )

release_date_topics <- release_date_topics %>%  mutate(season = case_when(
  month_release %in% c(1, 2, 12) ~ "1",
  month_release %in% c(3, 4, 5) ~ "2",
  month_release %in% c(6, 7, 8) ~ "3",
  month_release %in% c(9,10,11) ~ "4"
))


# Paso 1: Contar la cantidad de productos por año y tópico
release_date_topics <- release_date_topics %>%
  dplyr::count(year_season, season, LDA_topic, name = "count")

#release_date_topics$predominant_topic <- as.factor(count_data$LDA_topic)
release_date_topics <- release_date_topics[release_date_topics[,"year_season"]>2015,]

all_year_season <- unique(release_date_topics$year_season)
all_seasons <- unique(release_date_topics$season)
all_topics <- unique(release_date_topics$LDA_topic)

all_combinations <- expand.grid(year_season = all_year_season, season = all_seasons, LDA_topic = all_topics)


release_date_topics_complete <- all_combinations %>%
  left_join(release_date_topics, by = c("year_season", "season", "LDA_topic")) %>%
  replace_na(list(count = 0))

release_date_topics_complete <- release_date_topics_complete %>%
  filter(!(season %in% c("3", "4") & year_season == 2021))

my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#eebb44", "#bcbd22")

ggplot(data=release_date_topics_complete, aes(x=paste(year_season, season, sep="-"), y=count, group=LDA_topic, color = LDA_topic)) +
  geom_line(size=0.6) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = my_colors) 

# Plot separado por tópico predominante
png(file=paste0(PATH_PLOTS, "/topics_temporal_sortida.png"),
    width=1700, height=1100, units="px", res=130)
ggplot(data=release_date_topics_complete, aes(x=paste(year_season, season, sep="-"), y=count, group=LDA_topic, color = LDA_topic)) +
  xlab("Data de llençament") +
  geom_line(size=0.6) +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~LDA_topic, ncol = 2, scales = "free_x") +
  theme(panel.spacing.x = unit(1, "lines")) 
dev.off()

#-------------------------------------------------------------------------------
# Time Series amb Tòpics per estació

load("./3_Preprocessing/data_knn_imputed_unknown.RData")
data
weekly_data <- data %>% select(track_name, year_week, month_week)
weekly_data$track_name <- as.factor(weekly_data$track_name)

songs_to_topics <- unique_tracks %>% select(track_name, LDA_topic)
#names(songs_to_topics) <- c("track_name", "predominant_topic")

weekly_data_merged <- weekly_data %>%
  left_join(songs_to_topics, by = "track_name")

names(weekly_data_merged)[names(weekly_data_merged) == "predominant_topic"] <- "LDA_topic"

weekly_data_merged$year_week <- as.numeric(as.character(weekly_data_merged$year_week))
weekly_data_merged$month_week <- as.numeric(as.character(weekly_data_merged$month_week))

weekly_data_merged <- weekly_data_merged %>% 
  mutate(
    year_season = ifelse(month_week %in% c(12), year_week + 1, year_week)
  )

weekly_data_merged <- weekly_data_merged %>%  mutate(season = case_when(
  month_week %in% c(1, 2, 12) ~ "1",
  month_week %in% c(3, 4, 5) ~ "2",
  month_week %in% c(6, 7, 8) ~ "3",
  month_week %in% c(9,10,11) ~ "4"
))

weekly_data_merged <- weekly_data_merged %>%
  dplyr::count(year_season, season, LDA_topic, name = "count")


all_year_season <- unique(weekly_data_merged$year_season)
all_seasons <- unique(weekly_data_merged$season)
all_topics <- unique(weekly_data_merged$LDA_topic)

all_combinations <- expand.grid(year_season = all_year_season, season = all_seasons, LDA_topic = all_topics)


weekly_data_merged_complete <- all_combinations %>%
  left_join(weekly_data_merged, by = c("year_season", "season", "LDA_topic"))

weekly_data_merged_complete <- weekly_data_merged_complete %>%
  filter(!(season %in% c("3", "4") & year_season == 2021))

weekly_data_merged_complete[is.na(weekly_data_merged_complete)] <- 0

weekly_data_merged_complete$LDA_topic <- factor(weekly_data_merged_complete$LDA_topic, labels = topic_names)

my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#eebb44", "#bcbd22")

ggplot(data=weekly_data_merged_complete, aes(x=paste(year_season, season, sep="-"), y=count, group=LDA_topic, color = LDA_topic)) +
  geom_line(size=0.6) +
  theme_minimal() +
  scale_color_manual(values = my_colors) 


# Plot separado por tópico predominante
png(file=paste0(PATH_PLOTS, "/topics_temporal_setmana.png"),
    width=1700, height=1100, units="px", res=130)
ggplot(data=weekly_data_merged_complete, aes(x=paste(year_season, season, sep="-"), y=count, group=LDA_topic, color = LDA_topic)) +
  xlab("Data en la que les cançons es van trobar al top40") +
  geom_line(size=0.6) +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~LDA_topic, ncol = 2, scales = "free_x") +
  theme(panel.spacing.x = unit(1, "lines")) 
dev.off()
