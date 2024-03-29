

library(ggplot2)
library(ggpubr)
#library(dplyr)





#Creation of testing variables
x <- runif(1000)
y <- rexp(1000)
cat <- sample.int(3,1000,prob=c(0.6,0.3, 0.1), replace=TRUE)
class <- ifelse(x>0.5, 1, 2)


data <- data.frame(x,y, cat, class)

data[,"cat2"] <- data["class"]

data[,"cat"] <- as.factor(data[,"cat"])
data[,"class"] <- as.factor(data[,"class"])
data[,"cat2"] <- as.factor(data[,"cat2"])



#Función de classPanelGraph
classPanelGraph <- function(var){
  if (is.factor(data[,var])){
    ggplot(data=data,aes(x=data[,var]))+
      geom_bar()+
      facet_grid(class ~ .)+ ylab( "") + xlab(var)
    
  } else {
    ggplot(data=data, aes(x=data[,var]))+
      geom_histogram()+
      facet_grid(class ~ .)+ ylab("") + xlab(var) +
        theme(panel.background = element_rect(fil=class~.))
  }
}  

plots <- lapply(names(data), classPanelGraph)

ggarrange(plotlist=plots,ncol=5)



#Test de coloreado
var="speechiness"








mediana_func <- function(clase, lim1, lim2){
  if (medianas_por_clase[clase]<lim1){
    color_ <- "red"
  } else if (medianas_por_clase[clase]>lim2) {
    color_ <- "green"
  } else {
    color_ <- "yellow"
  }
  ifelse(data[,"cluster_hier"]==clase, color_, color_vec)
}

medianas_por_clase <- tapply(data[,var], data[,"cluster_hier"], median)

color_vec <- integer(nrow(data))
lim1 <- 0.5
lim2 <- 0.2
for (clase in unique(data[,"cluster_hier"])){
  color_vec <- mediana_func(clase, lim1, lim2)
}


#-------------------------------------------------------------------------------




TLP_num <- function(var_lims, data){

  var <- var_lims[[1]]
  lim2 <- var_lims[[2]]
  lim1 <- var_lims[[3]]
    
  mediana_func <- function(clase, lim1, lim2, medianas_por_clase, color_vec){
    if (medianas_por_clase[clase]<lim1){
      color_ <- 1#"red"
    } else if (medianas_por_clase[clase]>lim2) {
      color_ <- 2#"green"
    } else {
      color_ <- 3#"yellow"
    }
    ifelse(data[,"cluster_hier"]==clase, color_, color_vec)
  }
  
  
  
  medianas_por_clase <- tapply(data[,var], data[,"cluster_hier"], median)
  color_vec <- integer(nrow(data))
  
  for (clase in unique(data[,"cluster_hier"])) {
    color_vec <- mediana_func(clase, lim1=lim1, lim2=lim2, medianas_por_clase=medianas_por_clase, color_vec=color_vec)
  }
  
  color_vec <- as.factor(color_vec)
  
  scale_ <- c("red", "green", "yellow")
  names(scale_) <- c(1,2,3)
  
  panel<-ggplot(data=data, aes(x=data[,var], fill=color_vec))+
    geom_histogram()+
    facet_grid(cluster_hier ~ .)+ ylab("") + xlab(var) +
    scale_fill_manual(values = scale_) +
    theme(legend.position = "none")

  return(panel)
}

numeric_cols <- c("track_popularity","album_popularity", "artist_popularity","energy","loudness","speechiness","acousticness", "danceability","liveness","valence","tempo", "duration","streams")

numeric_cols <- list(   list("track_popularity","76","50"),
                        list("album_popularity","72","25"),
                        list("artist_followers","60000000","30000000"),
                        list("artist_popularity","90","80"),
                        list("danceability","0.85","0.65"),
                        list("energy","0.8","0.6"),
                        list("loudness","-4.5","-7"),
                        list("speechiness","0.11","0.05"),
                        list("acousticness","0.5","0.2"),
                        list("liveness","0.2","0.1"),
                        list("valence","0.6","0.3"),
                        list("tempo","140","90"),
                        list("duration","220","180"),
                        list("streams","20000000","10000000"))

plots <- lapply(numeric_cols, TLP_num, data=data)
ggarrange(plotlist = plots, ncol = 5, nrow=3)


















