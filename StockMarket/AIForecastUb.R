library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(RODBC)
library(lubridate)
library(keras)
Sys.setlocale("LC_ALL", "cht")

rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
}

GetFeature <- function(df) {
    df[, c(-1)] %>% map(rescale01) %>% unlist %>% as.vector
    #df[, c(2, 3)] %>% unlist %>% as.vector
}
GetLabel <- function(df, TF, GT, EP) {
    BuyPrice <- df$Μ絃基[1] - df$害禴基畉[1]
    MaxPrice <- df$Μ絃基[2:nrow(df)] %>% max
    #MinPrice <- df$Μ絃基[2:nrow(df)] %>% min
    Profit1 <- (MaxPrice - BuyPrice) / BuyPrice * 100
    #Profit2 <- (BuyPrice - MinPrice) / BuyPrice * 100
    ifelse(Profit1 >= (2 * TF + GT + EP), 1, 0)
    #if (Profit1 >= (2 * TF + GT + EP)) { 1 } else if(Profit2 >= (2 * TF + GT + EP)) { 2 } else { 0}
}

ConnectFeature <- function(df) {
    feature <- c()
    for (i in 1:nrow(df)) {
        feature <- c(feature, df$feature[[i]])
    }
    feature
}

TimeWindow <- 10
PredictWindow <- 5
TradeFarePercent <- 0.1425
GovernmentTaxPercent <- 0.1
ExpectedProfitPercent <- 1.1



Data <- Price[13:nrow(Price),] %>% left_join(InstitutionalInvestors)
Data <- Data %>% select(-禦秈肂1, - 芥肂1, - 禦芥畉肂1)
SampleNumber <- nrow(Data) - (TimeWindow + PredictWindow) + 1
Result <- tibble(feature = list(), label = numeric(), date = character())
for (i in 1:SampleNumber) {
    TW <- Data[i:(i + TimeWindow - 1),]
    PW <- Data[(i + TimeWindow):(i + TimeWindow + PredictWindow - 1),]
    Date <- Data[(i + TimeWindow),]$ら戳
    Feature <- list(list(GetFeature(TW)))
    Label <- GetLabel(PW, TradeFarePercent, GovernmentTaxPercent, ExpectedProfitPercent)
    Result[i, 1] <- Feature
    Result[i, 2] <- Label
    Result[i, 3] <- Date
}

ResultTrain <- Result[(nrow(Result) - 2047):nrow(Result),]
ResultTrain <- Result
ResultTest <- Result[(nrow(Result) - 30):nrow(Result),]
feature <- ResultTrain %>% ConnectFeature
latestfeature <- ResultTest %>% ConnectFeature
label <- ResultTrain$label
latestlabel <- ResultTest$label

#Testing and Training Data
newdata <- ResultTrain[1:2000,] %>% select(feature,label)

library(unbalanced)

flap <- function(feature, label) {
    tibble(fl = c(feature))
}
newdata <- newdata %>% mutate(all = map2(feature, label, flap)) %>% select(all) %>% unlist

newdata <- array_reshape(newdata, c(2000, 20, 10, 1))
newdata[1,,,]
newdata <- trainfeature
n <- ncol(newdata)
output <- newdata[, n] %>% as.factor
output <- trainlabel %>% as.factor
input <- newdata[, - n] %>% as.tibble
input <- newdata %>% as.tibble
data <- ubSMOTE(X = input, Y = output)
newData <- cbind(data$X, data$Y)



dt <- sort(sample(nrow(newData), nrow(newData) * .8))
train <- newData[dt,]
test <- newData[-dt,]


trainfeature <- train[, 1:200] %>% as.matrix
trainlabel <- train[, 201] %>% as.character
testfeature <- test[, 1:200] %>% as.matrix
testlabel <- test[, 201] %>% as.character





#Train Deep Learning




allfeature <- array_reshape(feature, c(nrow(ResultTrain), L))
latestfeature <- array_reshape(latestfeature, c(nrow(ResultTest), L))
# rescale


#trainlabel <- to_categorical(trainlabel, 2)
#testlabel <- to_categorical(testlabel, 2)
#alllabel <- to_categorical(label, 2)
#latestlabel <- to_categorical(latestlabel, 2)


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
    tibble(Accuracy, Correct, AccuracyAll, CorrectAll, LatestAccuracy, model = list(model),history=list(history))
    
    #tibble(model=list(model))
}



Model4 <- Train(0, 120, 240, 0.3, 0.26, 20, 10)
Model4$model[[1]] %>% predict_classes(latestfeature)
Model4$model[[1]] %>% evaluate(latestfeature, latestlabel) %>% .$acc
Model4$model[[1]] %>% predict_classes(latestfeature) %>% as.vector
Model4$model[[1]] %>% predict(latestfeature) %>% as.vector %>% round(2)
ResultTest$label
latestlabel
Model4$history[[1]] %>% plot
rep(10, 4)
ModelList <- tibble(Order = seq(1, 20, by = 1))

#ModelList <- ModelList %>% mutate(L2N = list(seq(10, 90, by = 20))) %>% unnest
#ModelList <- ModelList %>% mutate(L1D = list(seq(0.1, 0.9, by = 0.2))) %>% unnest

ModelList <- ModelList %>% mutate(L2D = list(seq(0.1, 0.5, by = 0.2))) %>% unnest
ModelList <- ModelList %>% mutate(Batch = list(seq(10, 30, by = 10))) %>% unnest
ModelList <- ModelList %>% mutate(train = pmap(list(120, 240, L1D, L2D, 30, Batch), Train))
ModelList <- ModelList %>% mutate(train = pmap(list(Order, 120, 240, 0.3, 0.26, 10, 10), Train))
ModelList <- ModelList %>% unnest
ModelList$LatestAccuracy %>% mean
ModelList$Accuracy %>% mean
ModelList %>% select(-model, - history) %>% View
ModelList$model[[1]] %>% evaluate(testfeature, testlabel) %>% .$acc
Model$model[[1]] %>% evaluate(testfeature, testlabel) %>% .$acc
plot(history)
ModelList %>% View

model %>% predict_classes(testfeature)
Accuracy <- model %>% evaluate(testfeature, testlabel) %>% .$acc

Wrong <- ((model %>% predict_classes(testfeature) - test$label) == 1) %>% sum
Correct <- ((model %>% predict_classes(testfeature))) %>% sum
Correct - 2 * Wrong

model %>% evaluate(allfeature, alllabel)
WrongAll <- ((model %>% predict_classes(allfeature) - label) == 1) %>% sum
CorrectAll <- ((model %>% predict_classes(allfeature))) %>% sum
CorrectAll - 2 * WrongAll
