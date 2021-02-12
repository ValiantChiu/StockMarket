library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)

Sys.setlocale("LC_ALL", "cht")

Price201810 <- readRDS(file = "Price201810.rds")
Price201809 <- readRDS(file = "Price201809.rds")
Price0050 <- readRDS(file = "Price0050Raw.rds")
Price0056 <- readRDS(file = "Price0056Raw.rds")
PriceAll_201001_201808 <- readRDS(file = "PriceAll_201001_201808.rds")
Price0056 %>% arrange(desc(ら戳))

GetDistribution <- function(そ腹, df) {
    ProfitVariable <- 1
    print(そ腹)
    Feature <- function(df) {
        GetPriceDifference <- function(df) {
            for (i in 2:nrow(df)) {
                df$害禴基畉[i] <- df$Μ絃基[i] - df$Μ絃基[i - 1]
            }
            df
        }
        df <- df %>% GetPriceDifference
        Last <- df[1:(nrow(df) - 1),] %>%
        select(Θユ计, Θユ肂, 秨絃基, 程蔼基, 程基, Μ絃基, 害禴基畉, Θユ掸计,MA5,MA10,K,FD,D)
        names(Last) <- c("Θユ计l", "Θユ肂l", "秨絃基l", "程蔼基l", "程基l", "Μ絃基l", "害禴基畉l", "Θユ掸计l","MA5l","MA10l","Kl","FDl","Dl")
        New <- df[2:nrow(df),]
        DIFF <- cbind(New, Last) %>%
        mutate(Θユ计D = (Θユ计 - Θユ计l) / Θユ计l,
           Θユ肂D = (Θユ肂 - Θユ肂l) / Θユ肂l,
           秨絃基D = (秨絃基 - 秨絃基l) / 秨絃基l,
           程蔼基D = (程蔼基 - 程蔼基l) / 程蔼基l,
           程基D = (程基 - 程基l) / 程基l,
           Μ絃基D = (Μ絃基 - Μ絃基l) / Μ絃基l,
           害禴基畉D = (害禴基畉 - 害禴基畉l) / 害禴基畉l,
    Θユ掸计D = (Θユ掸计 - Θユ掸计l) / Θユ掸计l,
    MA5D = (MA5 - MA5l) / MA5l,
    MA510D = (MA5 - MA10) / MA10,
    MA10D = (MA10 - MA10l) / MA10l) %>%
        select(ら戳, Μ絃基, 程基, 秨絃基, Θユ计D, Θユ肂D, 秨絃基D, 程蔼基D, 程基D, Μ絃基D, 害禴基畉D, Θユ掸计D, MA5, MA10, MA5D, K, FD, D, MA510D, MA10D)
        DIFF
    }
    Price <- function(df) {
        Last <- df[1:(nrow(df) - 3),]
        Price <- df[3:(nrow(df) - 1),] %>% select(Μ絃基) %>% rename(nextprice = Μ絃基)
        LowPrice <- df[2:(nrow(df) - 2),] %>% select(程基) %>% rename(thresholdprice = 程基)
        FinalPrice <- df[4:nrow(df),] %>% select(秨絃基) %>% rename(finalprice = 秨絃基)
        Result <- cbind(Last, Price, LowPrice, FinalPrice) %>% as.tibble %>% mutate(priceD = (nextprice - Μ絃基) / Μ絃基)
        Result
    }
    df <- df %>% arrange(ら戳) %>% Feature %>% Price
    #result <- df %>% select(Μ絃基D, Θユ掸计D, priceD) %>% mutate(index = ifelse(priceD >= 0.01, "1", "0"))
    result <- df %>% mutate(index = ifelse(priceD >= (ProfitVariable + 0.1425 * 2 + 0.3) / 100, "1", "0"))
    result <- result %>%
    mutate(BuyIndex = ifelse(Μ絃基 >= thresholdprice, 1, 0), SellIndex = ifelse(priceD >= (ProfitVariable + 0.1425 * 2 + 0.3) / 100, 1, 0)) %>%
    mutate(FinalEarning = ((finalprice - Μ絃基) / Μ絃基) - (0.1425 * 2 + 0.3) / 100) %>% mutate(Profit = ProfitVariable / 100) %>% filter(complete.cases(.))
    GetFinalResult <- function(ら戳, BuyIndex, SellIndex, FinalEarning, Profit) {
        #print(ら戳)
        if (BuyIndex == 0) {
            0
        } else if (SellIndex == 0) {
            FinalEarning
        } else if (SellIndex != 0) {
            Profit
        }
    }
    result <- result %>% mutate(FinalResult = pmap(list(ら戳, BuyIndex, SellIndex, FinalEarning, Profit), GetFinalResult) %>% as.numeric)

    result
}

OnePrice <- PriceAll_201001_201808 %>% filter(そ腹 == 2020) %>% .$price %>% .[[1]] %>% arrange(ら戳)
OnePrice <- Price0056 %>% arrange(ら戳)
#Add KD

AddMVandKD <- function(OnePrice) {
    OnePrice <- OnePrice %>% arrange(ら戳)
    OnePrice <- OnePrice %>% filter(!is.na(Μ絃基)) %>% mutate(MA5 = SMA(Μ絃基, n = 5), MA10 = SMA(Μ絃基, n = 10))
    KD <- OnePrice %>% arrange(ら戳) %>% select(程蔼基, 程基, Μ絃基) %>% rename(High = 程蔼基, Low = 程基, Close = Μ絃基) %>% stoch(., nFastK = 9, nFastD = 9, nSlowD = 9)
    OnePrice$K <- KD[, 1]
    OnePrice$FD <- KD[, 2]
    OnePrice$D <- KD[, 3]
    #OnePriceToday <- OnePrice[2:nrow(OnePrice),]
    #MA5DYesterday <- OnePrice[1:nrow(OnePrice),] %>% .$MA5D
    #OnePriceToday[,'MA5DY']<-MA5DYesterday
    OnePrice
}
OneSample <- OnePrice %>% AddMVandKD

MAOne <- GetDistribution("", OneSample) %>% select(-Θユ计D, - Θユ肂D, - 秨絃基D, - 程蔼基D, - 程基D, - Μ絃基D, - 害禴基畉D, - Θユ掸计D)
GetMA5DY <- function(MAOne) {
    MAOneToday <- MAOne[2:nrow(MAOne),]
    MA5DYesterday <- MAOne$MA5D[1:(nrow(MAOne) - 1)]
    MAOneToday[, 'MA5DY'] <- MA5DYesterday
    MAOneToday
}
MAOneToday <- MAOne %>% GetMA5DY
#ETF =0.1/1000 others=0.3/100
GetNextSellPoint <- function(MAOne, MA5DPara, MA510DPara, index) {
    print(index)
    ProfitVariable <- 5
    #Price <- MAOne %>% select(-程基, - 秨絃基) %>% mutate(buy = ifelse(MA5 >= MA10 & MA5D >= MA5DPara & MA510D >= MA510DPara, 1, 0)) %>% arrange(ら戳)
    #Price <- MAOne %>% select(-程基, - 秨絃基) %>% mutate(buy = ifelse(MA5 >= MA10 & MA5D >= MA5DPara, 1, 0)) %>% arrange(ら戳)
    Price <- MAOne %>% select(-程基, - 秨絃基) %>% mutate(buy = ifelse(MA5D > MA5DY, 1, 0)) %>% arrange(ら戳)
    BuyPoint <- Price %>% filter(buy == 1 & BuyIndex == 1)
    BuyAll <- tibble()
    if (nrow(Price) > 0) {
        for (i in 1:nrow(BuyPoint)) {
            BuyOne <- BuyPoint[i,]
            BuyPrice <- BuyOne$`Μ絃基`[1]
            BuyDate <- BuyOne$`ら戳`[1]
            SellPoint <- Price %>% filter(ら戳 > BuyDate) %>% arrange(ら戳) %>% .[2:nrow(.),] %>% mutate(SellPoint = ifelse(((Μ絃基 - BuyPrice) / Μ絃基) > ((ProfitVariable + 0.1425 * 2 + 0.1) / 100), 1, 0)) %>% select(-finalprice) %>% filter(SellPoint == 1) %>% arrange(ら戳) %>% .[1,]
            SellPrice <- SellPoint$`Μ絃基`[1]
            SellDate <- SellPoint$`ら戳`[1]
            BuyOne <- BuyOne %>% mutate(SellDate, SellPrice)
            BuyAll <- rbind(BuyAll, BuyOne)
        }
    }
     BuyAll
}

BuyAll <- MAOneToday %>% GetNextSellPoint(., 0, 0, 0)

DealWithDate <- function(BuyAll) {
    if (nrow(BuyAll) > 0) {
        BuyAll %>% filter(complete.cases(.)) %>% mutate(BuyDate = str_split(ら戳, "/"), SellDate = str_split(SellDate, "/")) %>%
        mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
           mutate(years = as.integer(map(SellDate, ~ .[1])) + 1911,
           months = as.character(map(SellDate, ~ .[2])),
           days = as.character(map(SellDate, ~ .[3]))) %>%
           mutate(BuyDate = ymd(paste0(year, month, day)), SellDate = ymd(paste0(years, months, days))) %>%
           select(-year, - month, - day, - years, - months, - days) %>% mutate(TimeLength = (SellDate - BuyDate) %>% as.numeric) #%>% select(-ら戳, - nextprice, - finalprice, - priceD, - FinalEarning, - FinalResult, - SellIndex)
    } else {
        BuyAll
    }
    
  }
Result <- BuyAll %>% DealWithDate # %>% mutate(MA510D=(MA5-MA10)/MA10)
Result$TimeLength %>% mean
Result %>% View
Result %>% mutate(Year = year(BuyDate)) %>% filter(Year == 2016) %>% arrange(TimeLength) %>% filter(K<0.6)%>% View
GetFinalResult <- function(Result) {
    GetLabel <- function(TimeLength) {
        if (TimeLength <= 5) { "<=5" }
        else if (TimeLength > 5 & TimeLength < 10) { ">5 & <10" }
        else if (TimeLength >= 10 & TimeLength < 30) { ">=10 & <30" }
        else if (TimeLength >= 30 & TimeLength<180) { ">=30 & <180" } 
        else if (TimeLength >= 180) { ">=180"}
        }
    Result <- Result %>% mutate(Label = map(TimeLength, GetLabel) %>% as.character)
    Result %>% group_by(Label) %>% summarise(count = n())
}

Result %>% select(BuyDate, MA5, MA10, MA5D,MA10D ,MA510D, K, FD, D, TimeLength) %>% View

Result$MA5D %>% max
Result$MA5D %>% min
Result$MA510D %>% max
Result$MA510D %>% min
Result %>% View
#Prediction By Random Forest
library(randomForest)
library(unbalanced)
Result$TimeLength %>% summary
df <- Result %>% select(BuyDate, MA5, MA10, MA5D, MA10D, MA510D, MA5DY, K, FD, D, TimeLength) %>% mutate(Label = ifelse(TimeLength <= 90, 1, 0))

dt <- 1:(nrow(df)*0.8)
train <- df[dt,]
test <- df[-dt,] %>% .[complete.cases(.),]


n <- ncol(train)
output <- train$Label %>% as.character %>% as.factor

input <- train[, c(-1,-2,-3,- n,-11)] %>% as.tibble
data <- ubSMOTE(X = input, Y = output, perc.over = 200, k = 10, perc.under = 200, verbose = TRUE)
data <- ubSMOTE(X = input, Y = output,  verbose = TRUE)

newData <- cbind(data$X, data$Y) %>% .[complete.cases(.),]
newData[1,]
model <- randomForest(x = newData[, 1:7], y = (newData$`data$Y` %>% as.factor), ntree = 1688, importance = TRUE,
                      proximity = TRUE)
prediction <- predict(model, test[, c(-1, -2,-3,- n,-11)])
caret::confusionMatrix(prediction, test$Label %>% as.character %>% as.factor)
saveRDS(model, file = "Model0056_idea3.rds")
test$Label %>% as.character %>% as.factor
prediction %>% as.character %>% as.numeric

newData$`data$Y` %>% table

#Predict
Year <- 2018

GetStockPriceByMonth <- function(Stock,Month) {

    GetOneMonthStockPrice <- function(Stock, Year, Month) {
        Purl <- paste0("http://www.twse.com.tw/exchangeReport/STOCK_DAY?response=json&date=", Year, Month, "01&stockNo=", Stock, "")
        Price <- POST(Purl)
        Price <- Price %>% content(as = "text") %>% fromJSON
        PriceData <- Price$data %>% as.tibble
        names(PriceData) <- Price$fields
        PriceData
    }
    CleanPrice <- function(df) {
        df$Θユ计 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$Θユ肂 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$害禴基畉 %<>% gsub(pattern = "+", replacement = "") %>% gsub(pattern = " ", replacement = "") %>% as.numeric
        df$Θユ掸计 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$秨絃基 %<>% as.numeric
        df$程蔼基 %<>% as.numeric
        df$程基 %<>% as.numeric
        df$Μ絃基 %<>% as.numeric
        df
    }
    GetPriceDifference <- function(df) {
        for (i in 2:nrow(df)) {
            df$害禴基畉[i] <- df$Μ絃基[i] - df$Μ絃基[i - 1]
        }
        df
    }
    GetPrice <- function() {
        Year <- 2018:2018
        #Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
        Price <- tibble()
        for (year in Year) {
            #if (year == 2018) { Month <- c('11') }
            for (month in Month) {
                print(paste0(year, "", month))
                Result <- GetOneMonthStockPrice(Stock, year, month)
                Price <- rbind(Price, Result)
                Sys.sleep(sample(4:10, size = 1))
            }
        }
        Price
    }
    Price <- tibble()
    tryCatch(Price <- GetPrice(), error = function(e) { print(paste0(Stock, e)) }, finally = print("Hello"))
    if (nrow(Price) > 1) {
        Price %>% CleanPrice %>% GetPriceDifference %>% arrange(ら戳)
    } else {
        Price %>% CleanPrice
    }

}
Stock0050_201811 <- GetStockPriceByMonth("0056", "11")
Stock0050_201812 <- GetStockPriceByMonth("0056", "12")

newData <- rbind(Stock0050_201812, Stock0050_201811, Stock0050_201810) %>% arrange(ら戳)

df <- newData %>% AddMVandKD
GetMA5D10D <- function(df) {
    MA510Today <- df[2:nrow(df),]
    MA5Yesterday <- df$MA5[1:(nrow(df) - 1)]
    MA10Yesterday <- df$MA10[1:(nrow(df) - 1)]
    MA510Today[, 'MA5Yesterday'] <- MA5Yesterday
    MA510Today[, 'M105Yesterday'] <- MA10Yesterday

    MA510Today %>% mutate(MA5D = (MA5 - MA5Yesterday) / MA5Yesterday, MA10D = (MA10 - MA10Yesterday) / MA10Yesterday, MA510D = (MA5 - MA10) / MA10)
}
newDataFeature <- newData %>% AddMVandKD %>% GetMA5D10D %>% GetMA5DY %>% mutate(buy = ifelse(MA5D > MA5DY, 1, 0)) %>% filter(buy==1)
TodayDate <- df$`ら戳`[nrow(df)]


predictfeature <- newDataFeature %>% filter(complete.cases(.)) %>% filter(ら戳>=TodayDate) %>% select(MA5D, MA10D, MA510D, MA5DY, K, FD, D)

model <- readRDS(file = "Model0056_idea3.rds")
predict(model,predictfeature)



#Optimization
MA5DPara <- seq(0, 0.02, by = 0.004)
MA510DPara <- seq(0, 0.024, by = 0.004)

Parameters <- tibble(MA5DPara) %>% mutate(MA510DPara = list(MA510DPara)) %>% unnest %>% mutate(MAOne = list(MAOne),index=1:nrow(.))
ParametersResult1 <- Parameters %>% mutate(BuyAll = pmap(list(MAOne, MA5DPara, MA510DPara, index), GetNextSellPoint))

ParametersResult2 <- ParametersResult1 %>% mutate(Result = map(BuyAll, DealWithDate))
ParametersResult3 <- ParametersResult2 %>% mutate(nrown = map(Result, ~ nrow(.)) %>% as.numeric) %>% filter(nrown > 1) %>% mutate(FinalResult = map(Result, GetFinalResult))
check <- ParametersResult3 %>% select(MA5DPara, MA510DPara, FinalResult) %>% unnest
MAOne %>% filter(ら戳 >= '107/07/01') %>% select(-程基, - 秨絃基) %>% mutate(buy = ifelse(MA5 >= MA10 & MA5D >= 0, 1, 0)) %>% filter(buy == 1) %>% View
check %>% View

PriceAll_201001_201808$price[[1]] %>% arrange(desc(ら戳))

check <- PriceAll_201001_201808 %>% unnest %>% filter(ら戳 >= '106/08/20') %>% group_by(そ腹) %>% summarise(alltrade = sum(Θユ计))
check %>% arrange(desc(alltrade))

PriceAll_201001_201808 %>% filter(そ腹=='2303')