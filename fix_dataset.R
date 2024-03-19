load('ultra_updated_data.RData')

data$is_group <- data$Is_Group
data$gender <- data$Gender
data$city <- data$City
data$nationality <- data$Nationality

data <- subset(data, -c(Is_Group, Gender, City, Nationality))
data$city[data$city == ''] <- NA
data$nationality[data$nationality == 'England' || data$nationality == 'Scotland', 'nationality'] <- 'United Kingdom'
data[which(is.na(data$city) && data$nationality == 'United Kingdom')] <- 'London'

save(data, file='./3_Preprocessing/data_knn_imputed.RData')