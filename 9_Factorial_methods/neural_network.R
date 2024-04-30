library(keras)

load("./9_Factorial_methods/data_acp_acm_merged.RData")

data <- NOM

set.seed(123)

train_size <- floor(0.8 * nrow(data))

train_indices <- sample(seq_len(nrow(data)), size = trainSize)

trainData <- data[indices, ]
testData <- data[-indices, ]

model <- keras_model_sequential() %>%
  layer_dense(units = 3, activation = 'relu', input_shape = c(2)) %>%
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

# Generar datos (igual que antes)
data <- as.matrix(data[ , c("x1", "x2")])
labels <- as.matrix(data[ , "y"])

# Entrenar el modelo
model %>% fit(data, labels, epochs = 200, batch_size = 5)



