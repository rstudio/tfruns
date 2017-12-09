#' Trains a simple deep NN on the MNIST dataset.
#'
#' Gets to 98.40% test accuracy after 20 epochs
#' (there is *a lot* of margin for parameter tuning).
#' 2 seconds per epoch on a K520 GPU.
#'

library(keras)

FLAGS <- flags(
  flag_integer("batch_size", default = 128),
  flag_numeric("dropout1", default = 0.4),
  flag_numeric("dropout2", default = 0.3)
)

num_classes <- 10
epochs <- 5

# the data, shuffled and split between train and test sets
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

dim(x_train) <- c(nrow(x_train), 784)
dim(x_test) <- c(nrow(x_test), 784)

x_train <- x_train / 255
x_test <- x_test / 255

cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')

# convert class vectors to binary class matrices
y_train <- to_categorical(y_train, num_classes)
y_test <- to_categorical(y_test, num_classes)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 128, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = FLAGS$dropout2) %>%
  layer_dense(units = num_classes, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(lr = 0.001),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  batch_size = FLAGS$batch_size,
  epochs = epochs,
  verbose = 1,
  validation_split = 0.2
)

plot(history)

score <- model %>% evaluate(
  x_test, y_test,
  verbose = 0
)

cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')

# save the model
save_model_hdf5(model, "model.h5")
save_model_weights_hdf5(model, 'model_weights.h5')




