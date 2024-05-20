# Import libraries
if(!require(rsample)) install.packages("rsample")
if(!require(tensorflow)){
  install.packages("tensorflow")
  library(tensorflow)
  tensorflow::install_tensorflow()
}
if(!require(keras)){
  install.packages("keras3")
  library(keras3)
  keras3::install_keras()
}
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")

library(tensorflow)
library(keras3)
library(rsample) # Data split (with stratify)
library(dplyr)
library(caret)
library(ggplot2)
library(reshape2)
library(tidyr)

# ------------------------------------------------------------------------------
# Prepare the data

# Load acp & acm merged data
load("./6_Facorial_methods/data_merged_acp_acm.RData")
data_merged_acp_acm <- data

# Load full data
load("final_d3_data.RData")
full_data <- data

# Split data by unique songs
target_name <- "explicit"
train_proportion <- 0.8

# Concatenate columns to have the final dataset for the model
data <- cbind(full_data$track_id, data_merged_acp_acm, full_data[[target_name]])
colnames(data) <- c("track_id", colnames(data_merged_acp_acm), target_name)

# If the values of the target are boolean, convert them to 1 and 0
if (typeof(data[[target_name]]) == "logical"){
  data[[target_name]] <- as.integer(data[[target_name]])
}

set.seed(42)

get_unique_tracks <- function(data, target_col_name) {
  data %>%
    distinct(track_id, .keep_all = TRUE) %>%
    select(track_id, all_of(target_col_name))
}

unique_track_id <- get_unique_tracks(data, target_name)

split <- initial_split(unique_track_id, prop = train_proportion, strata = all_of(target_name))

train_track_id <- training(split)
test_track_id <- testing(split)

# Preparing data for the model (note: ensure that you align and preprocess the data for model input)
train_data <- data %>% filter(track_id %in% train_track_id$track_id)
test_data <- data %>% filter(track_id %in% test_track_id$track_id)
train_data <- subset(train_data, select = - c(track_id))
test_data <- subset(test_data, select = - c(track_id))

train_labels <- train_data[[target_name]]
test_labels <- test_data[[target_name]]
train_data <- select(train_data, -all_of(target_name))
test_data <- select(test_data, -all_of(target_name))

train_data <- as.matrix(train_data)
test_data <- as.matrix(test_data)

# ------------------------------------------------------------------------------
# Set up and train the model

# Setup early stopping for both models
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 5, restore_best_weights = TRUE)

# Main model
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(ncol(train_data))) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

# Train with validation split
history <- model %>% fit(
  x = train_data, 
  y = train_labels, 
  epochs = 100, 
  batch_size = 5,
  validation_split = 0.2, # Using part of training data as validation data
  callbacks = list(early_stop)
)

# Baseline model: Perceptron (Logistic Regression)
perceptron <- keras_model_sequential() %>%
  layer_dense(units = 1, activation = 'sigmoid', input_shape = c(ncol(train_data)))

perceptron %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

# Train with validation split
history_perceptron <- perceptron %>% fit(
  x = train_data, 
  y = train_labels, 
  epochs = 100, 
  batch_size = 5,
  validation_split = 0.2, # Using part of training data as validation data
  callbacks = list(early_stop)
)

# ------------------------------------------------------------------------------
# Plot learning curves
plot_training_history <- function(history) {
  # Create a data frame from the training history
  history_data <- data.frame(
    epoch = seq_along(history$metrics$acc),
    accuracy = history$metrics$acc,
    val_accuracy = history$metrics$val_acc,
    loss = history$metrics$loss,
    val_loss = history$metrics$val_loss
  )
  
  # Melt the data frame to make it suitable for ggplot
  history_melted <- pivot_longer(history_data, cols = c(accuracy, val_accuracy, loss, val_loss), 
                                 names_to = "metric", values_to = "value")
  
  # Split data into two separate data frames for accuracy and loss for easier plotting
  accuracy_data <- history_melted %>% filter(metric %in% c("accuracy", "val_accuracy"))
  loss_data <- history_melted %>% filter(metric %in% c("loss", "val_loss"))
  
  # Plotting
  # Plot for Accuracy
  p_acc <- ggplot(accuracy_data, aes(x = epoch, y = value, color = metric)) +
    geom_line(linewidth = 1.25) +
    labs(title = "Model Accuracy", x = "Epoch", y = "Accuracy") +
    scale_color_manual(values = c("#1ED760", "#ff7b24"), labels = c("Training Accuracy", "Validation Accuracy")) +
    ylim(0, 1) +
    theme_minimal()
  
  # Plot for Loss
  p_loss <- ggplot(loss_data, aes(x = epoch, y = value, color = metric)) +
    geom_line(linewidth = 1.25) +
    labs(title = "Model Loss", x = "Epoch", y = "Loss") +
    scale_color_manual(values = c("#1ED760", "#ff7b24"), labels = c("Training Loss", "Validation Loss")) +
    ylim(0, 1) +
    theme_minimal()
  
  # Print the plots
  print(p_acc)
  print(p_loss)
}

plot_training_history(history=history)
plot_training_history(history=history_perceptron)

# ------------------------------------------------------------------------------
# Test and evaluation

evaluate_model <- function(model, test_data, test_labels, target_name) {
  # Make predictions
  predictions_prob <- model %>% predict(test_data)
  predictions <- ifelse(predictions_prob >= 0.5, 1, 0) # Assuming binary classification and threshold of 0.5
  
  # Using caret for a detailed confusion matrix and metrics
  conf_matrix_caret <- confusionMatrix(as.factor(predictions), as.factor(test_labels), positive = "1")
  print(conf_matrix_caret)
  
  # Extract relevant metrics
  accuracy <- conf_matrix_caret$overall['Accuracy']
  precision <- conf_matrix_caret$byClass['Precision']
  recall <- conf_matrix_caret$byClass['Recall']
  f1_score <- conf_matrix_caret$byClass['F1']
  
  # Print the metrics
  print(paste("Accuracy:", accuracy))
  print(paste("Precision:", precision))
  print(paste("Recall:", recall))
  print(paste("F1 Score:", f1_score))
  
  # Confusion Matrix visualization
  conf_matrix_table <- as.table(conf_matrix_caret$table)
  conf_matrix_df <- melt(conf_matrix_table)
  colnames(conf_matrix_df) <- c("Reference", "Prediction", "Frequency")
  
  # Plot the confusion matrix
  conf_matrix_plot <- ggplot(conf_matrix_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
    geom_tile() +
    geom_text(aes(label = Frequency), color = "black", size = 4) +
    scale_fill_gradient(low = "white", high = "#1ED760") +
    labs(title = paste("Confusion Matrix for variable", target_name), x = "Actual", y = "Predicted") +
    scale_x_continuous(breaks = unique(conf_matrix_df$Reference)) +
    scale_y_continuous(breaks = unique(conf_matrix_df$Prediction)) +
    theme_minimal()
  
  print(conf_matrix_plot)
}

evaluate_model(model=model, test_data=test_data, test_labels=test_labels, target_name=target_name)

evaluate_model(model=perceptron, test_data=test_data, test_labels=test_labels, target_name=target_name)

