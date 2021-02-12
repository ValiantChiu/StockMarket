library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)
library(randomForest)
library(unbalanced)
Sys.setlocale("LC_ALL", "cht")

Year <- 2018
GetStockPriceByMonth <- function(Stock, Month) {

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

newData <- rbind(Stock0050_201812, Stock0050_201811) %>% arrange(ら戳)
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
GetMA5D10D <- function(df) {
    MA510Today <- df[2:nrow(df),]
    MA5Yesterday <- df$MA5[1:(nrow(df) - 1)]
    MA10Yesterday <- df$MA10[1:(nrow(df) - 1)]
    MA510Today[, 'MA5Yesterday'] <- MA5Yesterday
    MA510Today[, 'M105Yesterday'] <- MA10Yesterday

    MA510Today %>% mutate(MA5D = (MA5 - MA5Yesterday) / MA5Yesterday, MA10D = (MA10 - MA10Yesterday) / MA10Yesterday, MA510D = (MA5 - MA10) / MA10)
}
GetMA5DY <- function(MAOne) {
    MAOneToday <- MAOne[2:nrow(MAOne),]
    MA5DYesterday <- MAOne$MA5D[1:(nrow(MAOne) - 1)]
    MAOneToday[, 'MA5DY'] <- MA5DYesterday
    MAOneToday
}

newDataFeature <- newData %>% AddMVandKD %>% GetMA5D10D %>% GetMA5DY %>% mutate(buy = ifelse(MA5D > MA5DY, 1, 0)) %>% filter(buy == 1)
TodayDate <- newData$`ら戳`[nrow(newData)]

predictfeature <- newDataFeature %>% filter(complete.cases(.)) %>% filter(ら戳 >= TodayDate) %>% select(MA5D, MA10D, MA510D, MA5DY, K, FD, D)

model <- readRDS(file = "Model0056_idea3.rds")
predict(model, predictfeature)

newData %>% AddMVandKD %>% GetMA5D10D %>% GetMA5DY %>% filter(ら戳 >= TodayDate) %>% View
