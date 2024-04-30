load("./3_Preprocessing/data_na_added.RData")
dataset <- na.omit(data)
dataset[,"lyrics"] <- NULL
library(ggplot2)
library(dplyr)
library(viridis)
library(summarytools)

spotyPalette <- c("#1db954","#ff7b24", "#c6182c", "#df75ff", "#eea990", "#98EEB7", "#e1ece3", "#cdf564", "#1ed760", "#4b917d","#2d6d62",
                  "#457e59", "#535353", "#3e3d54" , "#212121", "#121212")

dataset[,"major_mode"] <- as.factor(dataset[,"major_mode"])
dataset[,"key"] <- as.factor(dataset[,"key"])
dataset[,"month_week"] <- as.factor(dataset[,"month_week"])
dataset[,"year_week"] <- as.factor(dataset[,"year_week"])
dataset[,"month_release"] <- as.factor(dataset[,"month_release"])
dataset[,"time_signature"] <- as.factor(dataset[,"time_signature"])

univariate_numeric <- function(X, nom){
  hist_plot <- ggplot(data = dataset, aes(x = X)) +
    geom_histogram(bins = 15, fill = "#1db954", color = "#FFFFFF") +
    labs(title = paste("Histograma de ", nom), x = "Value", y = "Frequency") 
  
  box_plot <- ggplot(data = dataset, aes(y = X)) +
    geom_boxplot(fill = "#1db954", color = "black") +
    labs(title = paste("Boxplot de ", nom), x = "", y = "Value") +
    theme_minimal()
  
  print(hist_plot)
  ggsave(paste0("./Media/Descriptive/Univariate_raw/hist_",nom,".png"), width=8,height=6, dpi=300)
  
  print(box_plot)
  ggsave(paste0("./Media/Descriptive/Univariate_raw/box_",nom,".png"), width=8,height=6, dpi=300)
  
  
  summary_stats <- summary(X)
  cat('Mínim: ', summary_stats[1], '     ')
  cat('Màxim: ', summary_stats[6], '\n\n')
  cat('Primer quartil: ', summary_stats[2], '     ')
  cat('Mediana: ', summary_stats[3], '     ')
  cat('Tercer quartil: ', summary_stats[5], '     ')
  cat('Mitjana: ', summary_stats[4], '\n\n')
  cat(paste("Desviació estàndar: ", sd(X, na.rm = TRUE), "     "))
  cat(paste("Variància: ", sd(X, na.rm = TRUE) / mean(X, na.rm = TRUE), "\n\n"))
  cat("\n\n")
}

univariate_date <- function(X,nom){
  hist_plot <- ggplot(data = dataset, aes(x = X)) +
    geom_histogram(binwidth = 1, fill = "#1db954", color = "black", show.legend = FALSE) +
    labs(title = paste("Histograma de ", nom), x = "Value", y = "Frequency") +
    theme_minimal()
  box_plot <- ggplot(data = dataset, aes(x = "", y = X)) +
    geom_boxplot(fill = "#1db954", color = "black", show.legend = FALSE) +      labs(title = paste("Boxplot de ", nom), x = "", y = "Value") +
    theme_minimal()
  
  print(hist_plot)
  ggsave(paste0("./Media/Descriptive/Univariate_raw/hist_",nom,".png"), width=8,height=6, dpi=300)
  
  print(box_plot)
  ggsave(paste0("./Media/Descriptive/Univariate_raw/box_",nom,".png"), width=8,height=6, dpi=300)
  
  
  cat("\n\n")
}

univariate_cat <- function(X, nom){
  binary_color_mapping <- c("TRUE" = "#1db954", "FALSE" = "#ff7b24", "0" = "#1db954", "1" = "#ff7b24", "True" = "#1db954" , "False" = "#ff7b24" )
  category_counts <- table(X)
  category_df <- as.data.frame(category_counts)
  
  colnames(category_df) <- c("Categoria", "Freqüència")
  
  sub_category_df <- category_df
  
  order_indices <- order(sub_category_df$Freqüència, decreasing = TRUE)
  sub_category_df <- sub_category_df[order_indices, ]
  
  sub_category_df$Categoria <- factor(sub_category_df$Categoria, levels = sub_category_df$Categoria[order(sub_category_df$Freqüència, decreasing = TRUE)])
  
  sub_category_df$Freqüència
  
  if(nrow(sub_category_df) > 15){
    sub_category_df <- sub_category_df[1:15,]}
  # Bar plot
  if (length(unique(X)) == 2){
    bar_plot <- ggplot(data = sub_category_df, aes(x = Categoria, y = Freqüència, fill = Categoria)) +
      scale_fill_manual(values = binary_color_mapping, name = "Category") +
      geom_col(show.legend = FALSE) +
      labs(title = paste("Distribució de categories de ", nom),
           x = "Category",
           y = "Frequency") +
      theme_minimal() +
      geom_text(aes(label = Freqüència), position = position_dodge(width = 1), vjust = -0.5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    # Pie chart
    sub_category_df[,"Percentatge"] <- (sub_category_df[,"Freqüència"] / sum(sub_category_df[,"Freqüència"])) * 100
    
    pie_chart <- ggplot(data = sub_category_df, aes(x = "", y = Freqüència, fill = Categoria)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      labs(title = paste("Pie Chart de ", nom)) +
      theme_void() +
      geom_text(aes(label = paste0(round(Percentatge, 1), "%")), position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values = binary_color_mapping, name = "Category") +
      guides(fill = guide_legend(title = "Category"))
  }
  else{
    bar_plot <- ggplot(data = sub_category_df, aes(x = Categoria, y = Freqüència, fill = Categoria)) +
      scale_fill_manual(values = spotyPalette) +
      geom_col(show.legend = FALSE) +
      labs(title = paste("Distribució de categories de ", nom),
           x = "Category",
           y = "Frequency") +
      theme_minimal() +
      geom_text(aes(label = Freqüència), position = position_dodge(width = 1), vjust = -0.5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    # Pie chart
    sub_category_df[,"Percentatge"] <- (sub_category_df[,"Freqüència"] / sum(sub_category_df[,"Freqüència"])) * 100
    
    pie_chart <- ggplot(data = sub_category_df, aes(x = "", y = Freqüència, fill = Categoria)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      labs(title = paste("Pie Chart de ", nom)) +
      theme_void() +
      geom_text(aes(label = paste0(round(Percentatge, 1), "%")), position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values = spotyPalette, name = "Category") +
      guides(fill = guide_legend(title = "Category"))
  }
  
  print(bar_plot)
  ggsave(paste0("./Media/Descriptive/Univariate_raw/bar_",nom,".png"), width=8,height=6, dpi=300)
  
  print(pie_chart)
  ggsave(paste0("./Media/Descriptive/Univariate_raw/pie_",nom,".png"), width=8,height=6, dpi=300)
  
  cat(paste("Nombre de modalitats: ", length(category_df[,"Freqüència"]), '\n\n'))
  cat(paste("Moda: ", sub_category_df[,"Categoria"][1], "\n\n"))
  cat(paste("Freqüència màxima: ", sub_category_df[,"Freqüència"][1], "\n\n"))
  cat("\n\n")
}

univariate <- function(X, nom) {
  cat('##', nom, "\n\n")
  print(head(X))
  if (is.numeric(X)) {
    univariate_numeric(X,nom);
  } else if (class(X) == "Date") {
    univariate_date(X,nom);
  } else {
    # Bar plot and Pie chart for categorical data
    univariate_cat(X,nom);
  }
}

names <- c("track_name", "artist_name", "pop", "hip_hop", "electro", "latino", "danceability", "energy", "liveness", "acousticness", "collab", "explicit", "year_release", "nationality", "city", "gender", "is_group")

# Iterate through columns and create univariate plots
for (i in names) {
  univariate(dataset[, i], i)
}
