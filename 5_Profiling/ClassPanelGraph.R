

library(ggplot2)
library(ggpubr)
#library(dplyr)

load("./4_Clustering/jerarquic_cluster.RData")

# VAR REDUCED
REDUCED = TRUE 
# FIN REDUCED

ClassPanelGraph <- function(var, data) {
  if (is.numeric(data[,var])){
    plot <- ggplot(data=data, aes(x=data[,var]))+
      geom_histogram(fill="gray", color="black")+
      facet_grid(cluster_hier ~ .)+ ylab("") + xlab(var)
  } else {

    plot <- ggplot(data=data,aes(x=data[,var]))+
      geom_bar(fill="gray", color="black")+
      facet_grid(cluster_hier ~ .)+ ylab( "") + xlab(var)
  }
  return(plot)
}



# VECTORES CLASS PANEL GRAPH
 CPG_variables <- list("artist_num","artist_followers","artist_popularity","danceability","energy","loudness","speechiness","acousticness","valence","streams","album_type","pop","hip_hop","electro","latino","collab","explicit","major_mode","rank_group","gender","is_group") 
# FIN CPGVECTORES

plots <- lapply(CPG_variables, ClassPanelGraph, data = data)
CPG <- ggarrange(plotlist=plots, ncol=6,nrow=as.integer(length(plots)/6)+1)
#print(CPG)


if (REDUCED){
  ggsave("./Media/TLP/CPG_reduced.png", CPG, dpi = 600, width = 20, height = 20, units ="in")
} else {
  ggsave("./Media/TLP/CPG.png", CPG, dpi = 600, width = 20, height = 20, units ="in")
}



#Test de coloreado

TLP_categoric <- function(var_green_red_vecs, data, umbral){
  var<- var_green_red_vecs[[1]]
  green_vec <- var_green_red_vecs[[2]]
  red_vec <- var_green_red_vecs[[3]]
  
  data_cpy <- data
  data_cpy[,var] <- as.factor(data[,var])
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  
  color_mode <- function(clase, data, modes_cluster, green_vec, red_vec, color_vec, umbral){
    using_mode <- levels(data[,var])[modes_cluster[clase]]#se accede al nivel correspondiente a la moda del cluster
    
    if (using_mode %in% green_vec){
      color_ <- 2 # green
    } else if (using_mode %in% red_vec){
      color_ <- 1 # red
    } else {
      color_ <- 3  #yellow
    }
    x <- as.vector(table(data[data["cluster_hier"]==clase,var]))
    
    m1 <- max(x)
    i <- which(x==max(x), arr.ind = TRUE)
    x <- x[-i]
    m2 <- max(x)
    
    if (m1-m2<umbral){
      color_ <- 3 #yellow
    }
    
    ifelse(data[,"cluster_hier"] == clase, color_, color_vec)
  }
  
  modes_cluster <- tapply(data_cpy[,var], data_cpy[,"cluster_hier"], Mode)
  
  color_vec <- integer(nrow(data_cpy))
  
  for (clase in unique(data_cpy[,"cluster_hier"])) {
    color_vec <- color_mode(clase, data=data_cpy, green_vec = green_vec, red_vec = red_vec, modes_cluster=modes_cluster, color_vec=color_vec, umbral=umbral)
  }
  
  color_vec <- as.factor(color_vec)
  
  scale_ <- c("#c6182c", "#1ed760", "#ffed33")
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
  
  scale_ <- c("#c6182c", "#1ed760", "#ffed33")
  names(scale_) <- c(1,2,3)
  
  panel<-ggplot(data=data, aes(x=data[,var], fill=color_vec))+
    geom_histogram()+
    facet_grid(cluster_hier ~ .)+ ylab("") + xlab(var) +
    scale_fill_manual(values = scale_) +
    theme(legend.position = "none")
  panel
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

#VECTORES NUMERICOS Y CATEGORICOS
 numeric_cols <- list(list("artist_num",2.5,1.5),
   list("artist_followers",70000000.0,30000000.0),
   list("artist_popularity",90.0,78.0),
   list("danceability",0.85,0.65),
   list("energy",0.8,0.6),
   list("loudness",-4.5,-7.0),
   list("speechiness",0.11,0.05),
   list("acousticness",0.5,0.2),
   list("valence",0.6,0.3),
   list("streams",20000000.0,10000000.0))

categoric_cols <- list(list("album_type",c("single"),c("album")),
   list("pop",c("TRUE"),c("FALSE")),
   list("hip_hop",c("TRUE"),c("FALSE")),
   list("electro",c("TRUE"),c("FALSE")),
   list("latino",c("TRUE"),c("FALSE")),
   list("collab",c("TRUE"),c("FALSE")),
   list("explicit",c("TRUE"),c("FALSE")),
   list("major_mode",c("TRUE"),c("FALSE")),
   list("rank_group",c("1-10"),c("31-40")),
   list("gender",c("female"),c("male")),
   list("is_group",c("TRUE"),c("FALSE")))

 
 # FIN VECTORES
 


plots_cat <- lapply(categoric_cols, TLP_categoric, data=data, umbral=300)
plots_num <- lapply(numeric_cols, TLP_num, data=data)
plots <- c(plots_num, plots_cat)
TLP <- ggarrange(plotlist = plots, ncol = 6, nrow=as.integer(length(plots)/6)+1)

if (REDUCED){
  ggsave("./Media/TLP/TLP_reduced.png", TLP, dpi = 600, width = 20, height = 20, units ="in")
} else {
  ggsave("./Media/TLP/TLP.png", TLP, dpi = 600, width = 20, height = 20, units ="in")
}


