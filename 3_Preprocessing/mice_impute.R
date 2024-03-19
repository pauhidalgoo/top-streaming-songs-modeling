load("./3_Preprocessing/data_na_added.RData")

#Instalar y cargar la librería dplyr si aún no está instalada

library(dplyr)
install.packages("mice")
library(mice)


columnas_numericas <- data %>% 
  select_if(is.numeric)

columnas_sin_streams_trackpopularity <- columnas_numericas %>%
  select(-album_popularity, -track_popularity, -streams, -artist_followers)

head(columnas_numericas)

data_mice_imputed <- mice(columnas_sin_streams_trackpopularity, method = 'pmm', m = 5, maxit = 5, seed = 43)

#method: norm, rf
View(dataset)

#Revisar los datos imputados (esto te mostrará las imputaciones para las primeras variables con datos faltantes)
summary(data_mice_imputed)

data <- data_mice_imputed
save(data_mice_imputed, file="./3_Preprocessing/data_mice_imputed.RData")
