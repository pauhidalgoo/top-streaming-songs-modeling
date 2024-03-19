## COMPARADOR DE DISTRIBUCIONS ##

library(ggplot2)

load("./3_Preprocessing/data_knn_imputed.RData")
data_knn_imputed <- data
load("./3_Preprocessing/data_mimmi_imputed.RData")
data_mimmi_imputed <- data
load("./3_Preprocessing/data_na_added.RData")
data_na_added <- data
load("./3_Preprocessing/data_mice_imputed.RData")
data_mice_imputed  <- data

vars <- c("speechiness", "danceability", "duration")   # variables imputadas por el algoritmo de imputación

dd <- na.omit(data_na_added)
lista_df <- list(data_knn_imputed, data_mimmi_imputed, data_mice_imputed, dd)

imputations <- c("KNN", "MIMMI","Mice", "Original")
df_num <- 1
for (i in 1:length(lista_df)){
  lista_df[[i]][,"df_num"] <- imputations[df_num]
  df_num <- df_num + 1
}


df <- lista_df[[1]]
for (i in 2:length(lista_df)){
  df <- rbind(df, lista_df[[i]])
}

df[,"df_num"] <- as.factor(df[,"df_num"])

for (var in vars){
  p <-ggplot(df, aes(x=df[,var], color=df_num)) +
    geom_density() + ggtitle(paste("Distribució de '", paste(var, "' segons l'imputació", sep=""), sep="")) +
    labs(color = "Tipus d'imputació") + xlab(var)

    
      image_title <- paste("./Media/Preprocessing/NA_Imputation/distrib_imputation_", var, ".png", sep="") 
  ggsave(image_title, p, device="png", dpi=200, width=3000, height=1200, units="px")
}














