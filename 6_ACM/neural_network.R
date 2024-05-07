# Import libraries
if(!require(rsample)) install.packages("rsample")
if(!require(keras)) install.packages("keras")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")

library(keras) # Neural Network architecture
library(rsample) # Data split (with stratify)
library(dplyr)
library(caret)

# ------------------------------------------------------------------------------
# Prepare the data

# Load data
load("./9_Factorial_methods/data_acp_acm_merged.RData")

data <- NOM_ENTORN_GLOBAL

# Split data by unique songs
target_name <- "explicit"
train_proportion <- 0.8

set.seed(42)

get_unique_tracks <- function(data, target_col_name) {
  data %>%
    distinct(.data[["track_id"]], .keep_all = TRUE) %>%
    select(.data[["track_id"]], .data[[target_col_name]])
}

unique_track_id <- get_unique_tracks(data, target_name)

split <- initial_split(unique_track_id, prop = train_proportion, strata = target_name)

train_track_id <- training(split)
test_track_id <- testing(split)

# Preparing data for the model (note: ensure that you align and preprocess the data for model input)
train_data <- data %>% filter(track_id %in% train_track_id$track_id)
test_data <- data %>% filter(track_id %in% test_track_id$track_id)

train_labels <- train_data[[target_name]]
test_labels <- test_data[[target_name]]

# ------------------------------------------------------------------------------
# Set up and train the model

model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = ncol(data)) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

# Setup early stopping
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 10, restore_best_weights = TRUE)

# Train with validation split
history <- model %>% fit(
  x = train_data, 
  y = train_labels, 
  epochs = 1000, 
  batch_size = 5,
  validation_split = 0.2, # Using part of training data as validation data
  callbacks = list(early_stop)
)

# ------------------------------------------------------------------------------
# Plot learning curves
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
  geom_line() +
  labs(title = "Model Accuracy", x = "Epoch", y = "Accuracy") +
  scale_color_manual(values = c("darkgreen", "orange"), labels = c("Training Accuracy", "Validation Accuracy")) +
  theme_minimal()

# Plot for Loss
p_loss <- ggplot(loss_data, aes(x = epoch, y = value, color = metric)) +
  geom_line() +
  labs(title = "Model Loss", x = "Epoch", y = "Loss") +
  scale_color_manual(values = c("darkgreen", "orange"), labels = c("Training Loss", "Validation Loss")) +
  theme_minimal()

# Print the plots
print(p_loss)
print(p_acc)

# ------------------------------------------------------------------------------
# Test and evaluation

# Predict probabilities or classes
predictions_prob <- model %>% predict(test_data)
predictions <- ifelse(predictions_prob > 0.5, 1, 0) # Assuming binary classification and threshold of 0.5

# Using caret for a more detailed confusion matrix
conf_matrix_caret <- confusionMatrix(as.factor(predictions), as.factor(test_labels))
print(conf_matrix_caret)

# Visualization of Confusion Matrix (basic)
heatmap(as.matrix(conf_matrix), Rowv = NA, Colv = NA, col = colorRampPalette(c("blue", "white"))(256), 
        scale = "none", margins = c(5,5))

# Confusion Matrix using caret
conf_matrix_caret <- confusionMatrix(as.factor(predictions), as.factor(test_labels))

# Print the detailed confusion matrix and metrics
print(conf_matrix_caret)

# Accessing just the accuracy
accuracy_caret <- conf_matrix_caret$overall['Test accuracy']
print(paste("Test accuracy:", accuracy_caret))