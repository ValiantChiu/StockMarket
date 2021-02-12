library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)

Sys.setlocale("LC_ALL", "cht")

Price<-readRDS("Price005056_2010_2018.rds")
P <- Price$price[[1]]
DayList <- P %>% select(日期) %>% arrange(日期)
DayList <- DayList[5:(nrow(DayList) - 10),]
date <- DayList[2,] %>% .$日期
GetKData <- function(date) {
    P %>% filter(日期 <= date) %>% arrange(日期) %>% .[(nrow(.) - 4):nrow(.),]
}
P %>% filter(日期 <= date) %>% .[(nrow(.) - 4):nrow(.),] #%>% select(-成交筆數, - 漲跌價差, - 成交股數, - 成交金額)

KData <- DayList %>% mutate(KData = map(日期, GetKData))

CleanKData <- function(KData) {
    KData %>% arrange(日期) %>% select(-日期, - 成交股數, - 成交金額, - 漲跌價差, - 成交筆數)
}
KData <- KData %>% mutate(CleanedKData = map(KData, CleanKData))

GetSellPrice <- function(date) {
    P %>% filter(日期 > date) %>% .[10,] %>% select(收盤價) %>% .$收盤價
}

KData <- KData %>% mutate(SellPrice = map(日期, GetSellPrice) %>% as.numeric)
GetBuyPrice <- function(date) {
    P %>% filter(日期 == date) %>% .[1,] %>% select(收盤價) %>% .$收盤價
}
KData <- KData %>% mutate(BuyPrice = map(日期, GetBuyPrice) %>% as.numeric)
Normalization <- function(CleanedKData, BuyPrice) {
    (CleanedKData - BuyPrice) / BuyPrice
}

KData <- KData %>% mutate(NormalKData = map2(CleanedKData, BuyPrice, Normalization))

KData <- KData %>% mutate(Profit = (SellPrice - BuyPrice) / BuyPrice)

GetFeature <- function(NormalKData) {
    NormalKData %>% unlist %>% t %>% as.tibble
}

KData <- KData %>% mutate(Features = map(NormalKData, GetFeature))

KClusterData <- KData %>% select(-KData, - CleanedKData, - NormalKData) %>% unnest
library(cluster)
library(mclust)

fit_km1 <- kmeans(KClusterData[, 5:24], center = 16)

#fit_hc <- hclust(KClusterData[, 5:24])
fit_EM <- Mclust(KClusterData[, 5:24])

Result <- KClusterData %>% mutate(KCluster = fit_km1$cluster, EMCluster = fit_EM$classification) %>% select(日期, KCluster, EMCluster, Profit)
Result %>%  mutate(BuyDate = str_split(日期, "/")) %>%
            mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
        day = as.character(map(BuyDate, ~ .[3]))) %>% mutate(BuyDate = ymd(paste0(year, month, day))) %>%
         select(-year, - month, - day)


TbleauResult<-Result %>% group_by(KCluster) %>% summarise(profit = sum(Profit), times = n(), mp = max(Profit), mmp = min(Profit), medp = median(Profit), avg = mean(Profit)) 
write.csv(Result, file = 'K-Chart-Cluster.csv')

Result %>% filter(KCluster==13)