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


load("./8_textual_analysis/emotions_unique_tracks.RData")
PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis",sep="")

emotions_df$release_date <- as.Date(paste(emotions_df$year_release, emotions_df$month_release, "01", sep="-"), format="%Y-%m-%d")

emotion_counts <- aggregate(cbind(anger, anticipation, disgust, fear, joy, sadness, surprise, trust) ~ release_date, data = emotions_df, sum)

# Convert to time series
emotion_ts <- ts(emotion_counts[, -1], start = c(min(as.integer(as.character(emotions_df$year_release))), 1), frequency = 12)

# Plot the time series for each emotion
plot(emotion_ts[, "anger"], col = "red", type = "l", main = "Emotion Time Series", ylab = "Count", xlab = "Year")
lines(emotion_ts[, "anticipation"], col = "blue")
lines(emotion_ts[, "disgust"], col = "green")
lines(emotion_ts[, "fear"], col = "orange")
lines(emotion_ts[, "joy"], col = "purple")
lines(emotion_ts[, "sadness"], col = "brown")
lines(emotion_ts[, "surprise"], col = "pink")
lines(emotion_ts[, "trust"], col = "black")
legend("topright", legend = colnames(emotion_ts), col = c("red", "blue", "green", "orange", "purple", "brown", "pink", "black"), lty = 1)