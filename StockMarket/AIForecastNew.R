library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
ResultTrain$label %>% sum
ResultTrain %>% nrow
#
Train <- function(O, L1N, L2N, L1D, L2D, Epoch, Batch) {
    model <- keras_model_sequential()
    model %>%
    layer_dense(units = L1N, activation = 'relu', input_shape = c(L)) %>%
    layer_dropout(rate = L1D) %>%
    layer_dense(units = L2N, activation = 'tanh') %>%
    layer_dropout(rate = L2D) %>%
    layer_dense(units = 1, activation = 'sigmoid')

    model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(lr = 0.0005),
  metrics = c('accuracy')
    )


    history <- model %>% fit(
  trainfeature, trainlabel,
  epochs = Epoch, batch_size = Batch,
  validation_split = 0.2
    )
    Accuracy <- model %>% evaluate(testfeature, testlabel) %>% .$acc
    # Wrong <- ((model %>% predict_classes(testfeature) - test$label) == 1) %>% sum
    Correct <- ((model %>% predict_classes(testfeature))) %>% sum
    AccuracyAll <- model %>% evaluate(allfeature, label) %>% .$acc
    # WrongAll <- ((model %>% predict_classes(allfeature) - label) == 1) %>% sum
    CorrectAll <- ((model %>% predict_classes(allfeature))) %>% sum

    LatestAccuracy <- model %>% evaluate(latestfeature, latestlabel) %>% .$acc
    # tibble(Accuracy, Wrong, Correct, AccuracyAll, WrongAll, CorrectAll, LatestAccuracy, model = list(model), history = list(history))
    #tibble(Accuracy, Wrong, Correct, AccuracyAll, WrongAll, CorrectAll)
    tibble(Accuracy, Correct, AccuracyAll, CorrectAll, LatestAccuracy, model = list(model), history)
    #tibble(model=list(model))
}
Model <- Train(0, 256, 128, 0.46, 0.3, 100, 10)
Model <- Train(0, 120, 240, 0.26, 0.3, 10, 10)
Model <- Train(0, 120, 240, 0.28, 0.3, 15, 10)
Model <- Train(0, 120, 240, 0.3, 0.1, 30, 30)

Model <- Train(0, 120, 240, 0.3, 0.2, 30, 10)
Model$model[[1]] %>% evaluate(latestfeature, latestlabel) %>% .$acc
Model$model[[1]] %>% predict_classes(latestfeature) %>% as.vector
latestlabel
Model$model[[1]] %>% evaluate(testfeature, testlabel) %>% .$acc
Model$model[[1]] %>% predict_classes(testfeature) %>% as.vector
testlabel %>% as.integer
Model$history %>% plot
(Model$model[[1]] %>% predict_classes(testfeature) %>% as.vector - testlabel + (Model$model[[1]] %>% predict_classes(testfeature) %>% as.vector)) %>% table
# -1   0   1   2 
#148 289 14 20

#CNN

library(keras)
model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 64, kernel_size = c(2, 2), activation = "tanh",
                input_shape = c(20, 10, 1), strides = c(2, 1)) %>%
                layer_max_pooling_2d(pool_size = c(1, 1)) %>%
                layer_conv_2d(filters = 32, kernel_size = c(2, 2), activation = "tanh", strides = c(2, 1)) %>%
                layer_max_pooling_2d(pool_size = c(1, 1)) %>%
               layer_conv_2d(filters = 16, kernel_size = c(2, 2), activation = "relu", strides = c(2, 1))
model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 8, activation = "tanh") %>%
    layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
   loss = 'binary_crossentropy',
   optimizer = optimizer_rmsprop(lr = 0.0005),
   metrics = c('accuracy')
   )

history <- model %>% fit(
  trainfeature, trainlabel,
  epochs = 40, batch_size = 5,
validation_split = 0.2
)


model %>% evaluate(latestfeature, latestlabel) %>% .$acc
model %>% predict_classes(latestfeature) %>% as.vector
latestlabel
model %>% evaluate(testfeature, testlabel) %>% .$acc
model %>% predict_classes(testfeature) %>% as.vector
testlabel
history %>% plot
testlabel %>% sum
model %>% predict_classes(testfeature) %>% as.vector %>% sum
(model %>% predict_classes(testfeature) %>% as.vector - testlabel + (model %>% predict_classes(testfeature) %>% as.vector)) %>% table

model %>% evaluate(trainfeature, trainlabel) %>% .$acc
saveRDS(dt, file = "dt.rds")
model %>% save_model_hdf5("Stock0050CNN_828.h5")
