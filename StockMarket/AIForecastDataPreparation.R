library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(unbalanced)

Train0050<-readRDS('Train0050.rds')

GetTrainData <- function(TrainRaw) {
    Data <- TrainRaw
    TimeWindow <- 10
    PredictWindow <- 5
    TradeFarePercent <- 0.1425
    GovernmentTaxPercent <- 0.1
    ExpectedProfitPercent <- 1.1

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
    Result

}
TrainData <- GetTrainData(Train0050 %>% select(-禦秈肂1, - 芥肂1, - 禦芥畉肂1))
TrainData$feature[[1]]
Result <- TrainData
ConnectFeature <- function(df) {
    feature <- c()
    for (i in 1:nrow(df)) {
        feature <- c(feature, df$feature[[i]])
    }
    feature
}

#ResultTrain <- Result[(nrow(Result) - 2047):(nrow(Result) - 31),]
ResultTrain <- Result[1:(nrow(Result) - 61),]
ResultTest <- Result[(nrow(Result) - 60):nrow(Result),]
feature <- ResultTrain %>% ConnectFeature
latestfeature <- ResultTest %>% ConnectFeature

label <- ResultTrain$label
latestlabel <- ResultTest$label

dt <- sort(sample(nrow(ResultTrain), nrow(ResultTrain) * .8))
train <- Result[dt,]
test <- Result[-dt,]
L <- Result$feature[[1]] %>% length

trainfeature <- train %>% ConnectFeature
trainlabel <- train$label
testfeature <- test %>% ConnectFeature
testlabel <- test$label

library(keras)

array_reshape(c(1:18),c(3,3,2))

trainfeature <- array_reshape(trainfeature, c(nrow(train), L))
testfeature <- array_reshape(testfeature, c(nrow(test), L))
allfeature <- array_reshape(feature, c(nrow(ResultTrain), L))
latestfeature <- array_reshape(latestfeature, c(nrow(ResultTest), L))






#unbalance data

newdata <- trainfeature
n <- ncol(newdata)
output <- newdata[, n] %>% as.factor
output <- trainlabel %>% as.factor
input <- newdata[, - n] %>% as.tibble
input <- newdata %>% as.tibble
data <- ubSMOTE(X = input, Y = output, perc.under = 400)
#?ubSMOTE
newData <- cbind(data$X, data$Y)
trainfeature <- newData[, 1:200] %>% as.matrix
trainlabel <- newData[, 201] %>% as.character



#CNN
testfeature <- array_reshape(testfeature, c(nrow(test), 20, 10, 1))
allfeature <- array_reshape(feature, c(nrow(ResultTrain), 20, 10, 1))
latestfeature <- array_reshape(latestfeature, c(nrow(ResultTest), 20, 10, 1))
trainfeature <- trainfeature %>% array_reshape(c(trainfeature %>% nrow, 20, 10, 1))
