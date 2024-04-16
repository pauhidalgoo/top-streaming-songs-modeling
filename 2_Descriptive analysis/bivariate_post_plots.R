library(ggplot2)
load("./3_Preprocessing/data_knn_imputed_unknown.RData")
dataset <- na.omit(data)
graph1 <- ggplot(data = dataset, aes(x = nationality, y = streams, fill = nationality)) +
  geom_bar(stat = "summary", fun = "sum") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(graph1)
ggsave("./Media/Descriptive/Bivariate_post/nationalitystreams.png", width=8,height=6, dpi=300)

graph4 <- ggplot(data = dataset, aes(x = nationality, fill = latino)) +
  geom_bar(position = "dodge") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(graph4)
ggsave("./Media/Descriptive/Bivariate_post/nationalitylatino.png", width=8,height=6, dpi=300)


graph4 <- ggplot(data = dataset, aes(x = city, fill = collab)) +
  geom_bar(position = "dodge") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(graph4)

graph1 <- ggplot(data = dataset, aes(x = is_group, y = streams, fill = is_group)) +
  geom_bar(stat = "summary", fun = "mean") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(graph1)
ggsave("./Media/Descriptive/Bivariate_post/nationalitystreams.png", width=8,height=6, dpi=300)
