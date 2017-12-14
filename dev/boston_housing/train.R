library(keras)

FLAGS = flags(
  flag_integer('epochs1', 10)
)

data = dataset_boston_housing(path = "boston_housing.npz", test_split = 0.2,
                              seed = 113L)
# for simplicity
trainx = data$train$x
trainy = data$train$y
testx = data$test$x
testy = data$test$y
model1 = keras_model_sequential()
layer_dense(object = model1, units = 5, activation = 'relu', input_shape=13)
layer_dense(model1, units = 3, activation = 'relu')
layer_dense(model1, units = 1)
compile(model1, optimizer = 'rmsprop', loss = 'mse', metrics='mae')
trainx=normalize(data$train$x) # normalizes values to (0,1) trainy=keras_array(data$train$y) # produces a Python-formatted array testx=normalize(data$test$x)
# the training step
history = fit(model1,trainx,trainy, verbose= 0, epochs=FLAGS$epochs1)
# with test data
predi = predict(model1, testx)
MAE = mean(abs(predi[,1] - predi[,1]*testy)) # un-normalizing again MAE
score <- model1 %>% evaluate( testx, testy,
                              verbose = 0 )
cat('Test loss:', score$loss, '\n')
cat('Test accuracy:', score$mean_absolute_error, '\n')


