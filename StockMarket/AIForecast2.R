library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(RODBC)
library(lubridate)

LOCAL <- odbcConnect("LOCAL")

#Get Stock Trade Information
Sys.setlocale("LC_ALL", "C")
Sys.setlocale("LC_ALL", "cht")

GetOneMonthStockPrice <- function(Stock, Year, Month) {
    Purl <- paste0("http://www.twse.com.tw/exchangeReport/STOCK_DAY?response=json&date=", Year, Month, "01&stockNo=", Stock, "")
    Price <- POST(Purl)
    Price <- Price %>% content(as = "text") %>% fromJSON
    PriceData <- Price$data %>% as.tibble
    names(PriceData) <- Price$fields
    PriceData
}
Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
Price <- tibble()
for (year in 2018) {
    for (month in Month) {
        print(paste0(year, "", month))
        Result <- GetOneMonthStockPrice("0050", year, month)
        Price <- rbind(Price, Result)
        Sys.sleep(sample(4:10, size = 1))
    }
}

CleanPrice <- function(df) {
    df$成交股數 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$成交金額 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$漲跌價差 %<>% gsub(pattern = "+", replacement = "") %>% gsub(pattern = " ", replacement = "") %>% as.numeric
    df$成交筆數 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$開盤價 %<>% as.numeric
    df$最高價 %<>% as.numeric
    df$最低價 %<>% as.numeric
    df$收盤價 %<>% as.numeric
    df
}

GetPriceDifference <- function(df) {
    for (i in 2:nrow(df)) {
        df$漲跌價差[i] <- df$收盤價[i] - df$收盤價[i - 1]
    }
    df
}
Price <- Price %>% CleanPrice %>% GetPriceDifference
Price <- Price %>% arrange(日期)

CleanInstitutional <- function(df) {
    df$買進金額 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$賣出金額 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$買賣差額 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df
}
GetData <- function(list, n) { list[[1]] %>% .[n] }
StringSplit <- function(str) { gsub(str, pattern = " ", replacement = "") %>% strsplit(split = "/") }
Schedule <- Price[, 1]
Schedule <- Schedule %>% mutate(日期2 = as.character(map(日期, ~ gsub(., pattern = " ", replacement = ""))))
Schedule <- Schedule %>% mutate(Date = map(日期2, StringSplit))
Schedule <- Schedule %>% mutate(year = as.numeric(map2(Date, 1, GetData)) + 1911, month = map2(Date, 2, GetData), day = map2(Date, 3, GetData)) %>% select(-Date) %>% unnest
Schedule <- Schedule %>% mutate(Date = paste0(year, month, day))


GetInstitutionalInvestors <- function(Date) {
    print(Date)
    Sys.sleep(sample(3:8, size = 1))
    Purl <- paste0("http://www.tse.com.tw/fund/BFI82U?response=json&dayDate=", Date, "&weekDate=20180528&monthDate=20180601&type=day")
    InstitutionalInvestors <- POST(Purl)
    InstitutionalInvestors <- InstitutionalInvestors %>% content(as = "text") %>% fromJSON
    InstitutionalInvestorsData <- InstitutionalInvestors$data %>% as.tibble
    names(InstitutionalInvestorsData) <- InstitutionalInvestors$fields

    InstitutionalInvestorsData <- InstitutionalInvestorsData %>% mutate(單位名稱 = as.character(map(單位名稱, ~ (strsplit(., split = "[(]") %>% unlist %>% .[1]))))
    InstitutionalInvestorsData <- InstitutionalInvestorsData %>% CleanInstitutional %>% group_by(單位名稱) %>% summarise(買進金額 = sum(買進金額), 賣出金額 = sum(賣出金額), 買賣差額 = sum(買賣差額))

    InstitutionalInvestorsData <- InstitutionalInvestorsData %>% filter(單位名稱 != '外資自營商')
    InstitutionalInvestorsData <- InstitutionalInvestorsData %>% group_by(單位名稱) %>% nest %>% arrange(單位名稱) %>% t %>% as.tibble %>% .[2,] %>% unnest
    InstitutionalInvestorsData %>% mutate(date = InstitutionalInvestors$params$dayDate)
}
InstitutionalInvestors1 <- Schedule[1:700,] %>% mutate(data = map(Date, GetInstitutionalInvestors))
InstitutionalInvestors2 <- Schedule[701:1400,] %>% mutate(data = map(Date, GetInstitutionalInvestors))
InstitutionalInvestors3 <- Schedule[1401:nrow(Schedule),] %>% mutate(data = map(Date, GetInstitutionalInvestors))
InstitutionalInvestors <- rbind(InstitutionalInvestors1, InstitutionalInvestors2, InstitutionalInvestors3)
InstitutionalInvestors <- InstitutionalInvestors %>% select(日期, data)
InstitutionalInvestors <- InstitutionalInvestors %>% unnest
InstitutionalInvestors <- InstitutionalInvestors %>% select(-date)
#Make Sample and Label

rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
}

GetFeature <- function(df) {
    featureA <- df[, c(-1, -10, -11, -12)] %>% map(rescale01) %>% unlist %>% as.vector
    featureB <- df[, c(10, 11, 12)] %>% unlist %>% as.vector
    c(featureA,featureB)
    #df[, c(2, 3)] %>% unlist %>% as.vector
}
GetLabel <- function(df, TF, GT, EP) {
    BuyPrice <- df$收盤價[1] - df$漲跌價差[1]
    MaxPrice <- df$收盤價[2:nrow(df)] %>% max
    #MinPrice <- df$收盤價[2:nrow(df)] %>% min
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
Data <- Data %>% select(-買進金額1, - 賣出金額1, - 買賣差額1)
SampleNumber <- nrow(Data) - (TimeWindow + PredictWindow) + 1
Result <- tibble(feature = list(), label = numeric(), date = character())
for (i in 1:SampleNumber) {
    TW <- Data[i:(i + TimeWindow - 1),]
    PW <- Data[(i + TimeWindow):(i + TimeWindow + PredictWindow - 1),]
    Date <- Data[(i + TimeWindow),]$日期
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

dt <- sort(sample(nrow(ResultTrain), nrow(ResultTrain) * .7))
train <- Result[dt,]
test <- Result[-dt,]
L <- Result$feature[[1]] %>% length

trainfeature <- train %>% ConnectFeature
trainlabel <- train$label
testfeature <- test %>% ConnectFeature
testlabel <- test$label




#Train Deep Learning
library(keras)



trainfeature <- array_reshape(trainfeature, c(nrow(train), L))
testfeature <- array_reshape(testfeature, c(nrow(test), L))
allfeature <- array_reshape(feature, c(nrow(ResultTrain), L))
latestfeature <- array_reshape(latestfeature, c(nrow(ResultTest), L))
# rescale


trainlabel <- to_categorical(trainlabel, 2)
testlabel <- to_categorical(testlabel, 2)
alllabel <- to_categorical(label, 2)
latestlabel <- to_categorical(latestlabel, 2)


Train <- function(O, L1N, L2N, L1D, L2D, Epoch, Batch) {
    model <- keras_model_sequential()
    model %>%
    layer_dense(units = L1N, activation = 'relu', input_shape = c(L)) %>%
    layer_dropout(rate = L1D) %>%
    layer_dense(units = L2N, activation = 'tanh') %>%
    layer_dropout(rate = L2D) %>%
    layer_dense(units = 2, activation = 'sigmoid')

    model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
    )


    history <- model %>% fit(
  trainfeature, trainlabel,
  epochs = Epoch, batch_size = Batch,
  validation_split = 0.2
    )
    Accuracy <- model %>% evaluate(testfeature, testlabel) %>% .$acc
    Wrong <- ((model %>% predict_classes(testfeature) - test$label) == 1) %>% sum
    Correct <- ((model %>% predict_classes(testfeature))) %>% sum
    AccuracyAll <- model %>% evaluate(allfeature, alllabel) %>% .$acc
    WrongAll <- ((model %>% predict_classes(allfeature) - label) == 1) %>% sum
    CorrectAll <- ((model %>% predict_classes(allfeature))) %>% sum

    LatestAccuracy <- model %>% evaluate(latestfeature, latestlabel) %>% .$acc
    tibble(Accuracy, Wrong, Correct, AccuracyAll, WrongAll, CorrectAll, LatestAccuracy, model = list(model), history = list(history))
    #tibble(Accuracy, Wrong, Correct, AccuracyAll, WrongAll, CorrectAll)
}
testlabel %>% sum
Model <- Train(512, 256, 0.4, 0.3, 20, 40)

Model4 <- Train(0, 120, 240, 0.3, 0.26, 15, 10)
Model4$model[[1]] %>% evaluate(latestfeature, latestlabel) %>% .$acc
Model4$model[[1]] %>% predict_classes(latestfeature)
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
ModelList <- ModelList %>% mutate(train = pmap(list(Order, 120, 240, 0.3, 0.26, 30, 10), Train))
ModelList <- ModelList %>% unnest
ModelList$LatestAccuracy %>% mean
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




#Make Prediction Today
library(TTR)
PredictModel <- Model4$model[[1]]


Sys.setlocale("LC_ALL", "C")
Sys.setlocale("LC_ALL", "cht")
May <- GetOneMonthStockPrice('0050', 2018, '05')
June <- GetOneMonthStockPrice('0050', 2018, '06')
PriceAction <- rbind(May, June) %>% arrange(日期) %>% CleanPrice %>% GetPriceDifference
DecisionFeature <- PriceAction[(nrow(PriceAction) - TimeWindow + 1):nrow(PriceAction),]

Schedule1 <- DecisionFeature[, 1]
Schedule1 <- Schedule1 %>% mutate(日期2 = as.character(map(日期, ~ gsub(., pattern = " ", replacement = ""))))
Schedule1 <- Schedule1 %>% mutate(Date = map(日期2, StringSplit))
Schedule1 <- Schedule1 %>% mutate(year = as.numeric(map2(Date, 1, GetData)) + 1911, month = map2(Date, 2, GetData), day = map2(Date, 3, GetData)) %>% select(-Date) %>% unnest
Schedule1 <- Schedule1 %>% mutate(Date = paste0(year, month, day))
Investor <- Schedule1 %>% mutate(data = map(Date, GetInstitutionalInvestors))
Investor <- Investor %>% select(日期, data) %>% unnest %>% select(-date)


DecisionFeature <- DecisionFeature %>% left_join(Investor) %>% select(-買進金額1, - 賣出金額1, - 買賣差額1)

KDNew <- (Price %>% select(-K, - FD, - D) %>% rbind(June) %>% CleanPrice %>% arrange(日期) %>% select(最高價, 最低價, 收盤價) %>% rename(High = 最高價, Low = 最低價, Close = 收盤價)) %>% stoch(., nFastK = 9)
DecisionFeature$K <- KDNew[(nrow(KDNew) - 9):nrow(KDNew), 1]
DecisionFeature$FD <- KDNew[(nrow(KDNew) - 9):nrow(KDNew), 2]
DecisionFeature$D <- KDNew[(nrow(KDNew) - 9):nrow(KDNew), 3]

DecisionFeature <- DecisionFeature %>% GetFeature
DecisionFeature <- array_reshape(DecisionFeature, c(1, L))

PredictModel %>% predict_classes(DecisionFeature)


 %>% evaluate(testfeature, testlabel)

#Optimization
feature %>% length
c(c(2, 3), c(4, 5))

(Result$label %>% sum) * ExpectedProfitPercent
Feature <- GetFeature(Test)
one[1, 1] <- list(list(ine))
aa <- one$feature[[1]]
bb <- aa
#0.6 514.8   0.1 120.5 0.3 361.5



GetProfit <- function(PredictWindow, ExpectedProfitPercent) {
    print(PredictWindow)
    TimeWindow <- 10
    TradeFarePercent <- 0.1425
    GovernmentTaxPercent <- 0.1

    SampleNumber <- nrow(Price) - (TimeWindow + PredictWindow) + 1

    Result <- tibble(feature = list(), label = numeric())
    for (i in 1:SampleNumber) {
        TW <- Price[i:(i + TimeWindow - 1),]
        PW <- Price[(i + TimeWindow):(i + TimeWindow + PredictWindow - 1),]
        Feature <- list(list(GetFeature(TW)))
        Label <- GetLabel(PW, TradeFarePercent, GovernmentTaxPercent, ExpectedProfitPercent)
        Result[i, 1] <- Feature
        Result[i, 2] <- Label
    }
    Times <- (Result$label %>% sum)
    Profit <- (Result$label %>% sum) * ExpectedProfitPercent
    tibble(Times, Profit)
}

OptimizationResult <- tibble(PW = 2:60)
EP <- list(seq(0.1, 3, by = 0.1))
OptimizationResult <- OptimizationResult %>% mutate(EP)
OptimizationResult <- OptimizationResult %>% unnest
OptimizationResult <- OptimizationResult %>% mutate(result = map2(PW, EP, GetProfit))
OptimizationResultUnnest <- OptimizationResult %>% unnest
OptimizationResultUnnest %>% filter(PW == 5) %>% View
Profit <- OptimizationResultUnnest %>% group_by(PW) %>% summarise(max(Profit))