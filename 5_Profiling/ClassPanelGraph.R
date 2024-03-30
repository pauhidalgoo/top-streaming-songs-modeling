

library(ggplot2)
library(ggpubr)
#library(dplyr)

load("./4_Clustering/jerarquic_cluster.RData")




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






var_green_red_vecs <- list("electro", c("TRUE"), c("FALSE"))


cat_plot





#Test de coloreado
var = "hip_hop"

TLP_categoric <- function(var_green_red_vecs, data){
  var<- var_green_red_vecs[[1]]
  green_vec <- var_green_red_vecs[[2]]
  red_vec <- var_green_red_vecs[[3]]
  
  data_cpy <- data
  data_cpy[,var] <- as.factor(data[,var])
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  
  color_mode <- function(clase, data, modes_cluster, green_vec, red_vec, color_vec){
    using_mode <- levels(data[,var])[modes_cluster[clase]]#se accede al nivel correspondiente a la moda del cluster
    
    if (using_mode %in% green_vec){
      color_ <- 2 # green
    } else if (using_mode %in% red_vec){
      color_ <- 1 # red
    } else {
      color_ <- 3  #yellow
    }
    
    if (length(unique(data[,var]))==2){
      if (abs(diff(table(data[data[,"cluster_hier"]==clase,var])))<400){
        color_ <- 3 #yellow
      }
    }
    
    ifelse(data[,"cluster_hier"] == clase, color_, color_vec)
  }
  
  modes_cluster <- tapply(data_cpy[,var], data_cpy[,"cluster_hier"], Mode)
  abs(-3)
  
  
  color_vec <- integer(nrow(data_cpy))
  
  for (clase in unique(data_cpy[,"cluster_hier"])) {
    color_vec <- color_mode(clase, data=data_cpy, green_vec = green_vec, red_vec = red_vec, modes_cluster=modes_cluster, color_vec=color_vec)
  }
  
  color_vec <- as.factor(color_vec)
  
  scale_ <- c("red", "green", "yellow")
  names(scale_) <- c(1,2,3)
  
  cat_plot <- ggplot(data=data_cpy,aes(x=data_cpy[,var], fill = color_vec))+
    geom_bar()+
    facet_grid(cluster_hier ~ .)+ ylab( "") + xlab(var) +
    scale_fill_manual(values = scale_) +
    theme(legend.position="none")
  
  return(cat_plot)
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
    color_vec <- mediana_func(clase, lim1=as.numeric(lim1), lim2=as.numeric(lim2), medianas_por_clase=medianas_por_clase, color_vec=color_vec)
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


TLP <- function(var_vecs, data){
  if(is.factor(data[,var_vecs[1]])){
    result <- TLP_categoric(var_vecs, data)
  } else {
    result <- TLP_num(var_vecs, data)
  }
  return(result)
}

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

categoric_cols <- list(   list("album_type",c("single"),c("album")),
                          list("artist_num",c("1"),c("3","4","5","6","7")),
                          list("pop",c("TRUE"),c("FALSE")),
                          list("hip_hop",c("TRUE"),c("FALSE")),
                          list("rock",c("TRUE"),c("FALSE")),
                          list("electro",c("TRUE"),c("FALSE")),
                          list("christmas",c("TRUE"),c("FALSE")),
                          list("cinema",c("TRUE"),c("FALSE")),
                          list("latino",c("TRUE"),c("FALSE")),
                          list("collab",c("TRUE"),c("FALSE")),
                          list("explicit",c("TRUE"),c("FALSE")),
                          list("major_mode",c("TRUE"),c("FALSE")))


plots_cat <- lapply(categoric_cols, TLP_categoric, data=data)
plots_num <- lapply(numeric_cols, TLP_num, data=data)
plots <- c(plots_num, plots_cat)
TLP <- ggarrange(plotlist = plots, ncol = 6, nrow=5)
ggsave("./Media/TLP/TLP.png", TLP, dpi = 300, width = 1000, units ="px")
ggsave()















