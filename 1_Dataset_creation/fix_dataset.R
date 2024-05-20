load('ultra_updated_data.RData')

data$is_group <- data$Is_Group
data$gender <- data$Gender
data$city <- data$City
data$nationality <- data$Nationality

data <- subset(data, select = -c(Is_Group, Gender, City, Nationality))
data$city[data$city == ''] <- NA
data$nationality[data$nationality == 'England' | data$nationality == 'Scotland' | data$nationality == 'London'] <- 'United Kingdom'
data$nationality[data$nationality == 'Nordrhein-Westfalen'] <- 'Germany'
data$nationality[data$nationality == 'Toronto'] <- 'Canada'
data$nationality[data$nationality == 'Chicago' | data$nationality == 'New York' | data$nationality == 'Salt Lake City' | data$nationality == 'Los Angeles' | data$nationality == 'Nashville'] <- 'United States'

data$city[which(is.na(data$city) & data$nationality == 'United Kingdom')] <- 'London'
data$city[which(is.na(data$city) & data$nationality == 'Japan')] <- 'Tokyo'
data$city[which(is.na(data$city) & data$nationality == 'Australia')] <- 'Canberra'
data$city[which(is.na(data$city) & data$nationality == 'Sweden')] <- 'Stockholm'
data$city[which(is.na(data$city) & data$nationality == 'United States')] <- 'Washington'
data$city[which(is.na(data$city) & data$nationality == 'Colombia')] <- 'Medellín'
data$city[which(is.na(data$city) & data$nationality == 'Puerto Rico')] <- 'San Juan'
data$city[which(is.na(data$city) & data$nationality == 'Venezuela')] <- 'Caracas'
data$city[data$city == "The Bronx"] <- 'New York'



data$gender[data$gender == 'na'] <- NA
data$is_group[which(is.na(data$is_group))] <-FALSE

data$is_group <- as.logical(data$is_group)
data$gender <- as.factor(data$gender)
data$city <- as.factor(data$city)
data$nationality <- as.factor(data$nationality)

save(data, file='./3_Preprocessing/data_knn_imputed.RData')

