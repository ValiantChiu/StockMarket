library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(randomForest)
library(unbalanced)
library(TTR)
Sys.setlocale("LC_ALL", "cht")

#Compared Deep Learning
model <- randomForest(x = trainfeature, y = trainlabel %>% as.factor, ntree = 300, importance = TRUE,
 proximity = TRUE)

prediction %>% as.character %>% as.numeric


prediction <- predict(model, totalnewfeature)
caret::confusionMatrix(prediction, totalnewlabel %>% as.factor)

prediction <- predict(model, testfeature)
caret::confusionMatrix(prediction, testlabel %>% as.factor)

#Tax
TradeFarePercent <- 0.1425
GovernmentTaxPercent <- 0.1

#0050
#PredictWindow <- 8
#ExpectedProfitPercent <- 1.1
PredictWindow <- 7
ExpectedProfitPercent <- 1.1

#0056
PredictWindow <- 10
ExpectedProfitPercent <- 1.3

PredictWindow <- 8
ExpectedProfitPercent <- 1.1
#0051
PredictWindow <- 8
ExpectedProfitPercent <- 1.1
PredictWindow <- 7
ExpectedProfitPercent <- 1.5



GetDataLabel <- function(df) {
    GetLabel <- function(df, TF, GT, EP) {
        BuyPrice <- df$收盤價[1] - df$漲跌價差[1]
        MaxPrice <- df$收盤價[2:nrow(df)] %>% max
        #MinPrice <- df$收盤價[2:nrow(df)] %>% min
        Profit1 <- (MaxPrice - BuyPrice) / BuyPrice * 100
        #Profit2 <- (BuyPrice - MinPrice) / BuyPrice * 100
        ifelse(Profit1 >= (2 * TF + GT + EP), 1, 0)
        #if (Profit1 >= (2 * TF + GT + EP)) { 1 } else if(Profit2 >= (2 * TF + GT + EP)) { 2 } else { 0}
    }
    LabelRange <- (nrow(df) - PredictWindow )
    AllLabel <- c()
    for (i in 1:LabelRange) {
        PW <- df[(i+1):(i + PredictWindow ),]
        AllLabel[i] <- GetLabel(PW, TradeFarePercent, GovernmentTaxPercent, ExpectedProfitPercent)
    }
    df[1:LabelRange,] %>% mutate(Label = AllLabel)
}
RFDataRaw <- Train0056 %>% select(-買進金額1, - 賣出金額1, - 買賣差額1) %>% rename(K = fastK, FD = fastD,D=slowD)
RFDataRaw <- Train0051 %>% filter(!is.na(最低價))
RFData <- RFDataRaw %>% GetDataLabel 
newdata <- RFData[1:(nrow(RFData)-10), -1]
n <- ncol(newdata)
output <- newdata[, n] %>% as.matrix %>% as.factor
input <- newdata[, - n] %>% as.tibble

data <- ubSMOTE(X = input, Y = output, perc.over = 200, k = 5, perc.under = 200, verbose = TRUE)
data <- ubSMOTE(X = input, Y = output, verbose = TRUE)

newData <- cbind(data$X, data$Y)



dt <- sort(sample(nrow(newData), nrow(newData) * 0.9))
train <- newData[dt,]
test <- newData[-dt,]

model <- randomForest(x = train[, - n], y = train[, n] %>% as.factor, ntree = 1000, importance = TRUE,
 proximity = TRUE)

Totalnewdata <- RFData[(nrow(RFData) - 10+1):nrow(RFData), -1]

prediction <- predict(model, Totalnewdata[, - n])
caret::confusionMatrix(prediction %>% as.character %>% as.factor, Totalnewdata[, n] %>% as.matrix %>% as.factor)

prediction <- predict(model, test[, - n])
caret::confusionMatrix(prediction %>% as.character %>% as.factor, test[, n] %>% as.matrix %>% as.factor)

#Make Prediction Today
GetThreeBigInvestorsInform <- function(Schedule1) {
    CleanInstitutional <- function(df) {
        df$買進金額 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$賣出金額 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$買賣差額 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df
    }
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

    GetData <- function(list, n) { list[[1]] %>% .[n] }
    StringSplit <- function(str) { gsub(str, pattern = " ", replacement = "") %>% strsplit(split = "/") }
    Schedule1 <- Schedule1 %>% mutate(日期2 = as.character(map(日期, ~ gsub(., pattern = " ", replacement = ""))))
    Schedule1 <- Schedule1 %>% mutate(Date = map(日期2, StringSplit))
    Schedule1 <- Schedule1 %>% mutate(year = as.numeric(map2(Date, 1, GetData)) + 1911, month = map2(Date, 2, GetData), day = map2(Date, 3, GetData)) %>% select(-Date) %>% unnest
    Schedule1 <- Schedule1 %>% mutate(Date = paste0(year, month, day))
    Investor <- Schedule1 %>% mutate(data = map(Date, GetInstitutionalInvestors))
    Investor <- Investor %>% select(日期, data) %>% unnest %>% select(-date)
    Investor
}
MonthIndex<-Sys.time() %>% month
if (nchar(MonthIndex) == 1) { M <- paste0("0", MonthIndex) } else { M <- MonthIndex }
#OneMonthPrice <- GetOneMonthStockPrice('0056', 2018, M)
OneMonthPrice <- GetOneMonthStockPrice('0050', 2018, M)

RFPrice <- RFDataRaw %>% select(日期, 成交股數, 成交金額, 開盤價, 最高價, 最低價, 收盤價, 漲跌價差, 成交筆數) %>% arrange(日期)
if (RFPrice$`日期`[nrow(RFPrice)] == OneMonthPrice$`日期`[nrow(OneMonthPrice)]) {
    'Stop'
} else {
    LatestPrice <- OneMonthPrice %>% filter(日期 > RFPrice$`日期`[nrow(RFPrice)])
    PriceAction <- RFPrice %>% rbind(LatestPrice) %>% arrange(日期) %>% CleanPrice %>% GetPriceDifference
    Schedule1 <- PriceAction[, 1] %>% filter(日期 > RFPrice$`日期`[nrow(RFPrice)])
    Investor <- Schedule1 %>% GetThreeBigInvestorsInform
    DecisionFeature <- PriceAction %>%  filter(日期 > RFPrice$`日期`[nrow(RFPrice)]) %>% left_join(Investor) %>% select(-買進金額1, - 賣出金額1, - 買賣差額1)
    KDNew <- (RFPrice %>% rbind(LatestPrice) %>% CleanPrice %>% arrange(日期) %>% select(最高價, 最低價, 收盤價) %>% rename(High = 最高價, Low = 最低價, Close = 收盤價)) %>% stoch(., nFastK = 9)
    DecisionFeature$K <- KDNew[nrow(KDNew), 1]
    DecisionFeature$FD <- KDNew[nrow(KDNew), 2]
    DecisionFeature$D <- KDNew[nrow(KDNew), 3]
    RFDataRaw <- RFDataRaw %>% rbind(DecisionFeature) 
}

predict(model, RFDataRaw[nrow(RFDataRaw),-1]) %>% as.character



#Buy
RFDataRaw[nrow(RFDataRaw), -1]$收盤價
#Sell
RFDataRaw[nrow(RFDataRaw), -1]$收盤價 * ((2 * TradeFarePercent + GovernmentTaxPercent + ExpectedProfitPercent) / 100 + 1)

RFDataRaw[nrow(RFDataRaw), ]