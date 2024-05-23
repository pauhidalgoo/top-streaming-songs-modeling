library(ggplot2)
library(ggpubr)

load("./8_textual_analysis/genres_emotions_translated.RData")
PATH_PLOTS = paste(getwd(),"./Media/Textual_Analysis/Genres",sep="")



names(definitive_data)


binary_colors <- c( "black", "#1db954")

new_genres <- c("R&B_match","Folk_match","Reggae Alt_match", "Regional_match",
                "Latin & Funk_match","Hip hop_match", "Electro house_match",
                "Pop_match", "Rock_match")


# Create plots for each categorical variable

genres_data <- definitive_data[new_genres]

plot_list <- list()
for (variable in new_genres) {
  # Count the frequency of each category
  counts <- table(definitive_data[[variable]])
  
  # Convert counts to a data frame
  counts_df <- as.data.frame(counts)
  counts_df$Category <- rownames(counts_df)
  
  # Rename columns for better visualization
  colnames(counts_df) <- c("Category", "Frequency")
  
  # Create the plot
  p <- ggplot(counts_df, aes(x = Category, y = Frequency, fill = Category)) +
    geom_bar(stat = "identity") +
    labs(title = variable) +
    scale_fill_manual(values = binary_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  # Save the plot as a PNG file
  #filename <- paste0(PATH_PLOTS, "/profiling_", variable, ".png")
  #png(filename, width = 800, height = 600, units = "px", res = 130)
  plot_list[[variable]] <- p
}

# Arrange the plots in a grid layout
png(file=paste0(PATH_PLOTS, "/genres_frequencies.png"),
    width=1920, height=1080, units="px", res=130)

ggarrange(plotlist = plot_list, ncol = 3, nrow = 3, common.legend=TRUE)

dev.off()

for (variable in new_genres){
  print(table(genres_data[variable]))
}

names(definitive_data)
old_genres <- c("pop", "hip_hop", "latino", "electro", "rock", "cinema", "christmas")
for (variable in old_genres){
  print(table(definitive_data[variable]))
}

