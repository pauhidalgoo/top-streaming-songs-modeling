library(syuzhet)
library(tm) # text mining package
library(wordcloud) # word cloud generator
library(SnowballC) # text stemming
library(ggplot2) # graphs
library(tidyverse) # data manipulation
library(tidytext) # word lexicon dictionaries for sentiments
library(reshape2) # data transformation
library(textdata) # provides access to lexicon dictionaries
library(knitr) # used to make kable tables


load("./8_textual_analysis/emotions_unique_translated.RData")
PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis/Emotions_ts",sep="")

emotions_df$release_date <- as.Date(paste(emotions_df$year_release, emotions_df$month_release, "01", sep="-"), format="%Y-%m-%d")

emotion_counts <- aggregate(cbind(anger, anticipation, disgust, fear, joy, sadness, surprise, trust) ~ release_date, data = emotions_df, sum)

# Filter data to include only years from 2020 onwards
emotion_counts <- emotion_counts[format(emotion_counts$release_date, "%Y") >= "2017", ]

# Convert counts to percentages
emotion_counts[, -1] <- emotion_counts[, -1] / rowSums(emotion_counts[, -1]) * 100

# Convert to time series
emotion_ts <- ts(emotion_counts[, -1], start = c(2017, 1), frequency = 12)

# Define line width
line_width <- 2


png(file=paste0(PATH_PLOTS, "/all_time_series.png"),
    width=1600, height=1920, units="px", res=130)

# Plot each emotion in a separate plot
par(mfrow = c(4, 2))  # Setting up a 4x2 grid of plots

plot(emotion_ts[, "anger"], col = "red", type = "l", main = "Anger", ylab = "Percentage", xlab = "Year", lwd = line_width)
plot(emotion_ts[, "anticipation"], col = "blue", type = "l", main = "Anticipation", ylab = "Percentage", xlab = "Year", lwd = line_width)
plot(emotion_ts[, "disgust"], col = "green", type = "l", main = "Disgust", ylab = "Percentage", xlab = "Year", lwd = line_width)
plot(emotion_ts[, "fear"], col = "orange", type = "l", main = "Fear", ylab = "Percentage", xlab = "Year", lwd = line_width)
plot(emotion_ts[, "joy"], col = "purple", type = "l", main = "Joy", ylab = "Percentage", xlab = "Year", lwd = line_width)
plot(emotion_ts[, "sadness"], col = "brown", type = "l", main = "Sadness", ylab = "Percentage", xlab = "Year", lwd = line_width)
plot(emotion_ts[, "surprise"], col = "pink", type = "l", main = "Surprise", ylab = "Percentage", xlab = "Year", lwd = line_width)
plot(emotion_ts[, "trust"], col = "black", type = "l", main = "Trust", ylab = "Percentage", xlab = "Year", lwd = line_width)

# Reset to default plotting settings
par(mfrow = c(1, 1))
dev.off()
