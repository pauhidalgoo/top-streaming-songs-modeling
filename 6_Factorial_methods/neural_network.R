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

if (!require(ROSE)) install.packages("ROSE")

# ACM
library(FactoMineR)
library(factoextra)

# Neural Network
library(tensorflow)
library(keras3)
library(rsample) # Data split (with stratify)
library(dplyr)
library(caret)
library(ggplot2)
library(reshape2)
library(tidyr)
library(ROSE)

# ------------------------------------------------------------------------------
# Define the target variable (must be categorical)
target_name <- "explicit"

# Define parameters for the execution
load_acm_data <- FALSE
balance_method <- "weights" 
# Possible values: "oversampling", "undersampling", weights"

# ------------------------------------------------------------------------------

load('final_d3_data.RData')
full_data <- data

# Load ACP data
load("./6_Factorial_methods/acp_data.RData")
data_acp <- data_psi

if (!load_acm_data){
  # Prepare ACM data (depending on the target name chosen, to avoid data leakage)
  data_logical <- Filter(is.logical, full_data)
  data_factors <- Filter(is.factor, full_data)
  categorical_data <- cbind(data_factors, data_logical)
  categorical_data <- subset(categorical_data, select = -c(track_id, track_name, album_name, day_release, month_release, year_release, weekday_release, year_week, month_week, week_index))
  categorical_data <- subset(categorical_data, select = -c(artist_name, album_label, city)) # Remove variables with too many classes
  categorical_data <- categorical_data[, !names(categorical_data) %in% target_name] # Remove target variable
  
  res.mca <- MCA(categorical_data, ncp = Inf, graph = FALSE)
  eig.val <- res.mca$eig
  cumulative_variance <- cumsum(eig.val[, 2])
  num_dimensions <- which(cumulative_variance >= 80)[1]
  cat("ACM dimensions:", num_dimensions, "\n")
  
  data_acm <- as.data.frame(res.mca$ind$coord[, 1:num_dimensions])
  save(data_acm, file = "./6_Factorial_methods/acm_data.RData")

} else {
  # Load a precalculated acm_data
  load("./6_Factorial_methods/acm_data.RData")
}

# Merge datasets
data_merged_acp_acm <- cbind(data_acp, data_acm)
colnames(data_merged_acp_acm) <- make.names(colnames(data_merged_acp_acm))


# Clean environment before starting
rm(list = setdiff(ls(), c("data_merged_acp_acm", "full_data", "target_name", "balance_method")))

# ------------------------------------------------------------------------------
# Prepare the data
# Split data by unique songs

train_proportion <- 0.8
validation_proportion <- 0.2

# Concatenate columns to have the final dataset for the model
data <- cbind(full_data$track_id, data_merged_acp_acm, full_data[[target_name]])
colnames(data) <- c("track_id", colnames(data_merged_acp_acm), target_name)

# Shuffle
data <- data %>% sample_frac(1)

# If the values of the target are boolean, convert them to 1 and 0
if (typeof(data[[target_name]]) == "logical"){
  data[[target_name]] <- as.numeric(data[[target_name]])
}

set.seed(42)

get_unique_tracks <- function(data, target_col_name) {
  data %>%
    distinct(track_id, .keep_all = TRUE) %>%
    select(track_id, all_of(target_col_name))
}

unique_track_id <- get_unique_tracks(data, target_name)

# Split 1: INITIAL_TRAIN - TEST
split1 <- initial_split(unique_track_id, prop = train_proportion, strata = all_of(target_name))

initial_train_track_id <- training(split1)
test_track_id <- testing(split1)

# Split 2: FINAL_TRAIN - VAL
split2 <- initial_split(initial_train_track_id, prop = 1 - validation_proportion, strata = all_of(target_name))
train_track_id <- training(split2)
val_track_id <- testing(split2)

# Prepare final data
train_data <- data %>% filter(track_id %in% train_track_id$track_id)
test_data <- data %>% filter(track_id %in% test_track_id$track_id)
val_data <- data %>% filter(track_id %in% val_track_id$track_id)

train_labels <- train_data[[target_name]]
test_labels <- test_data[[target_name]]
val_labels <- val_data[[target_name]]

train_data <- subset(train_data, select = - c(track_id))
test_data <- subset(test_data, select = - c(track_id))
val_data <- subset(val_data, select = - c(track_id))

train_data <- select(train_data, - all_of(target_name))
test_data <- select(test_data, - all_of(target_name))
val_data <- select(val_data, - all_of(target_name))

train_data <- as.matrix(train_data)
val_data <- as.matrix(val_data)
test_data <- as.matrix(test_data)

# Calculate class weights for all classes
cat("Train target class values:\n", table(train_labels))
cat("Test target class values:\n", table(test_labels))
target_class_weights <- as.list(rep(1, length(unique(train_labels))))

# If balance method is weights, use the calculated class weights
if (balance_method == "weights") {
  target_class_counts <- table(train_labels)
  total_samples <- sum(target_class_counts)
  class_weights <- total_samples / (length(target_class_counts) * target_class_counts)
  class_weights_list <- as.list(class_weights)
  names(class_weights_list) <- as.character(names(target_class_counts))
  
  target_class_weights <- class_weights_list
  cat("Using class weights for balancing:\n")
  print(target_class_weights)
  
} else if (balance_method == "oversampling") {
  # Perform random oversampling
  oversampled_data <- ovun.sample(as.factor(train_labels) ~ ., data = as.data.frame(cbind(train_data, train_labels)), method = "over")$data
  train_data <- as.matrix(oversampled_data[, -ncol(oversampled_data)])
  train_labels <- oversampled_data[, ncol(oversampled_data)]
  cat("Train target class values after oversampling:\n", table(train_labels), "\n")
  
} else if (balance_method == "undersampling") {
  # Perform random undersampling
  undersampled_data <- ovun.sample(as.factor(train_labels) ~ ., data = as.data.frame(cbind(train_data, train_labels)), method = "under")$data
  train_data <- as.matrix(undersampled_data[, -ncol(undersampled_data)])
  train_labels <- undersampled_data[, ncol(undersampled_data)]
  cat("Train target class values after undersampling:\n", table(train_labels), "\n")
  
} else if (balance_method != ""){
  cat("The provided balance method does not match with any of the available ones.")
}

# ------------------------------------------------------------------------------
# Set up and train the model

# Setup parameters used in all models
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 10, restore_best_weights = TRUE)

batch_size = 8

activation_function = "tanh"

create_optimizer <- function(simple) {
  if (simple){
    lr_schedule = 0.003
  
  } else {
    lr_schedule <- tf$keras$optimizers$schedules$ExponentialDecay(
      initial_learning_rate = 0.005,
      decay_steps = 10000,
      decay_rate = 0.9,
      staircase = TRUE
    )
  }
  
  optimizer_adam(learning_rate = lr_schedule)
}


# ---------- Main model ----------
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = activation_function, input_shape = c(ncol(train_data))) %>%
  layer_activity_regularization(l2 = 0.01) %>%
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = create_optimizer(simple = FALSE),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x = train_data, 
  y = train_labels, 
  epochs = 100, 
  batch_size = batch_size,
  validation_data = list(val_data, val_labels),
  callbacks = list(early_stop),
  class_weight = target_class_weights
)

# ---------- Baseline model: Perceptron (Logistic Regression) ----------
perceptron <- keras_model_sequential() %>%
  layer_dense(units = 1, activation = 'sigmoid', input_shape = c(ncol(train_data)))

perceptron %>% compile(
  loss = 'binary_crossentropy',
  optimizer = create_optimizer(simple = FALSE),
  metrics = c('accuracy')
)

history_perceptron <- perceptron %>% fit(
  x = train_data, 
  y = train_labels, 
  epochs = 100, 
  batch_size = batch_size,
  validation_data = list(val_data, val_labels),
  callbacks = list(early_stop),
  class_weight = target_class_weights
)

# ------------------------------------------------------------------------------
# Plot learning curves
plot_training_history <- function(history, model_name) {
  # Extract the metrics from the history object
  epochs <- length(history$metrics$accuracy)
  accuracy <- unlist(history$metrics$accuracy)
  val_accuracy <- unlist(history$metrics$val_accuracy)
  loss <- unlist(history$metrics$loss)
  val_loss <- unlist(history$metrics$val_loss)
  
  # Create a data frame from the training history
  history_data <- data.frame(
    epoch = 1:epochs,
    accuracy = accuracy,
    val_accuracy = val_accuracy,
    loss = loss,
    val_loss = val_loss
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
    labs(title = paste(model_name, "Epoch Accuracy"), x = "Epoch", y = "Accuracy") +
    scale_color_manual(values = c("#1ED760", "#ff7b24"), labels = c("Training Accuracy", "Validation Accuracy")) +
    ylim(0, 1) +
    theme_minimal()
  
  # Plot for Loss
  p_loss <- ggplot(loss_data, aes(x = epoch, y = value, color = metric)) +
    geom_line(linewidth = 1.25) +
    labs(title = paste(model_name, "Epoch Loss"), x = "Epoch", y = "Loss") +
    scale_color_manual(values = c("#1ED760", "#ff7b24"), labels = c("Training Loss", "Validation Loss")) +
    theme_minimal()
  
  # Print the plots
  print(p_acc)
  print(p_loss)
}

plot_training_history(history=history, model_name="MLP")
plot_training_history(history=history_perceptron, model_name="Perceptron")

# ------------------------------------------------------------------------------
# Test and evaluation

evaluate_model <- function(model, test_data, test_labels, target_name, model_name) {
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
    labs(title = paste("Confusion Matrix: Target =", target_name, "& Model =", model_name), x = "Actual", y = "Predicted") +
    scale_x_continuous(breaks = unique(conf_matrix_df$Reference)) +
    scale_y_continuous(breaks = unique(conf_matrix_df$Prediction)) +
    theme_minimal()
  
  print(conf_matrix_plot)
}

evaluate_model(model=model, test_data=test_data, test_labels=test_labels, target_name=target_name, model_name="MLP")
evaluate_model(model=perceptron, test_data=test_data, test_labels=test_labels, target_name=target_name, model_name="Perceptron")

