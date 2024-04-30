library(ggplot2)
load("./3_Preprocessing/data_na_added.RData")
dataset <- na.omit(data)
# Categòrica i numèrica
graph1 <- ggplot(data = dataset, aes(x =album_type, y = track_popularity, fill = album_type)) +
  geom_bar(stat = "summary", fun = "mean")
print(graph1)
ggsave("./Media/Descriptive/Bivariate_raw/typepopularity.png", width=8,height=6, dpi=300)

graph1 <- ggplot(data = dataset, aes(x =pop, y = speechiness, fill = pop)) +
  geom_bar(stat = "summary", fun = "mean")
print(graph1)
ggsave("./Media/Descriptive/Bivariate_raw/popspeech.png", width=8,height=6, dpi=300)

graph1 <- ggplot(data = dataset, aes(x =hip_hop, y = speechiness, fill = hip_hop)) +
  geom_bar(stat = "summary", fun = "mean")
print(graph1)
ggsave("./Media/Descriptive/Bivariate_raw/hiphopspeech.png", width=8,height=6, dpi=300)

graph1 <- ggplot(data = dataset, aes(x =hip_hop, y = acousticness, fill = hip_hop)) +
  geom_bar(stat = "summary", fun = "mean")
print(graph1)
ggsave("./Media/Descriptive/Bivariate_raw/hiphopacoustic.png", width=8,height=6, dpi=300)

graph1 <- ggplot(data = dataset, aes(x =is_group, y = acousticness, fill = is_group)) +
  geom_bar(stat = "summary", fun = "mean")
print(graph1)
ggsave("./Media/Descriptive/Bivariate_raw/groupacoustic.png", width=8,height=6, dpi=300)


graph2 <- ggplot(data = dataset, aes(x = latino, y = valence, fill = latino)) +
  geom_boxplot()
print(graph2)
ggsave("./Media/Descriptive/Bivariate_raw/latinovalence.png", width=8,height=6, dpi=300)

graph1 <- ggplot(data = dataset, aes(x =collab, y = artist_num, fill = collab)) +
  geom_bar(stat = "summary", fun = "mean")
print(graph1)
ggsave("./Media/Descriptive/Bivariate_raw/collabnum.png", width=8,height=6, dpi=300)

graph1 <- ggplot(data = dataset, aes(x =gender, y = streams, fill = gender)) +
  geom_bar(stat = "summary", fun = "mean")
print(graph1)
ggsave("./Media/Descriptive/Bivariate_raw/genderstreams.png", width=8,height=6, dpi=300)

graph1 <- ggplot(data = dataset, aes(x = city, y = streams, fill = city)) +
  geom_bar(stat = "summary", fun = "sum") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(graph1)
ggsave("./Media/Descriptive/Bivariate_raw/gendernum.png", width=8,height=6, dpi=300)


# Numerica numèrica

graph3 <- ggplot(data = dataset) + 
  geom_point(mapping = aes(x = track_popularity, y = album_popularity), alpha = 1 / 30)
print(graph3)
ggsave("./Media/Descriptive/Bivariate_raw/trackalbum.png", width=8,height=6, dpi=300)

graph3 <- ggplot(data = dataset) + 
  geom_point(mapping = aes(x = acousticness, y = energy), alpha = 1 / 30)
print(graph3)
ggsave("./Media/Descriptive/Bivariate_raw/acousticenergy.png", width=8,height=6, dpi=300)


graph3 <- ggplot(data = dataset) + 
  geom_point(mapping = aes(x = energy, y = loudness), alpha = 1 / 30)
print(graph3)
ggsave("./Media/Descriptive/Bivariate_raw/energyloudness.png", width=8,height=6, dpi=300)

graph3 <- ggplot(data = dataset) + 
  geom_point(mapping = aes(x = danceability, y = valence), alpha = 1 / 30)
print(graph3)
ggsave("./Media/Descriptive/Bivariate_raw/danceabilityvalence.png", width=8,height=6, dpi=300)


library(ggmosaic)
graph4 <- ggplot(data = dataset, aes(x = pop, fill = gender)) +
  geom_bar(position = "dodge")
print(graph4)
ggsave("./Media/Descriptive/Bivariate_raw/popgender.png", width=8,height=6, dpi=300)

graph4 <- ggplot(data = dataset, aes(x = hip_hop, fill = gender)) +
  geom_bar(position = "dodge")
print(graph4)
ggsave("./Media/Descriptive/Bivariate_raw/hiphopgender.png", width=8,height=6, dpi=300)

graph4 <- ggplot(data = dataset, aes(x = is_group, fill = latino)) +
  geom_bar(position = "dodge")
print(graph4)
ggsave("./Media/Descriptive/Bivariate_raw/latinogroup.png", width=8,height=6, dpi=300)

graph5 <- ggplot(data = dataset) +
  geom_mosaic(aes(x = product(pop, electro), fill = pop))
print(graph5)
ggsave("./Media/Descriptive/Bivariate_raw/electropop.png", width=8,height=6, dpi=300)



