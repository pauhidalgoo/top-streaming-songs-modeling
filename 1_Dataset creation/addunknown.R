load("./ultra_updated_data.RData")
data <- replace(data, is.na(data), "Unknown")

levels(data$nationality) <- c(levels(data$nationality), "Unknown")
data <- replace(data, data=="", "Unknown")
summary(data)
