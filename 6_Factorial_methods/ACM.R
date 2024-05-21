# Load necessary libraries
library(FactoMineR)
library(factoextra)

# Load the data
load('final_d3_data.RData')

# Filter logical and factor data
data_logical <- Filter(is.logical, data)
data_factors <- Filter(is.factor, data)

# Combine categorical data
categorical_data <- cbind(data_factors, data_logical)
categorical_data <- subset(categorical_data, select = -c(track_id, track_name, album_name, day_release, month_release, year_release, weekday_release, year_week, month_week, week_index))
#categorical_data <- subset(categorical_data, select = -c(album_label, artist_name))

# Perform MCA
res.mca <- MCA(categorical_data, ncp = Inf, graph = FALSE)

# Extract the explained variance
eig.val <- res.mca$eig

# Calculate cumulative variance
cumulative_variance <- cumsum(eig.val[, 2])

# Determine the number of dimensions needed to explain 80% of the variance
num_dimensions <- which(cumulative_variance >= 80)[1]
cat("ACM dimensions:", num_dimensions, "\n")

# Extract the coordinates of individuals in these dimensions
individual_coordinates <- res.mca$ind$coord[, 1:num_dimensions]

# Show the coordinates of the individuals
head(individual_coordinates)

data_acm <- as.data.frame(individual_coordinates)

save(data_acm, file = "./6_Factorial_methods/acm_data.RData")
