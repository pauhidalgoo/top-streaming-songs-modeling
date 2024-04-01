load("./3_Preprocessing/data_knn_imputed.RData")

data$is_group <- as.factor(data$is_group)

contains_any_na = sapply(data, function(x) any(is.na(x)))
nanames <- names(data)[contains_any_na]

for(col in nanames) {
  if(is.factor(data[[col]])) {
    levels(data[[col]]) <- c(levels(data[[col]]), "Unknown")
  }
}

data <- replace(data, is.na(data), "Unknown")
data <- replace(data, data=="", "Unknown")
summary(data)


save(data, file='./3_Preprocessing/data_knn_imputed_unknown.RData')
