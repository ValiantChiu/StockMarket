library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)

Sys.setlocale("LC_ALL", "cht")
#Get Price
PriceAll_201001_201811 <- readRDS(file = "PriceAll_201001_201811.rds")
PriceAll_201001_201811_unnest <- PriceAll_201001_201811 %>% unnest
OnePrice <- PriceAll_201001_201811$data[[2]]
AddMVandKD <- function(OnePrice) {
    OnePrice <- OnePrice %>% arrange(ら戳)
    OnePrice <- OnePrice %>% filter(!is.na(Μ絃基)) %>% mutate(MA8 = SMA(Μ絃基, n = 8), MA21 = SMA(Μ絃基, n = 21), MA55 = SMA(Μ絃基, n = 55))
    KD <- OnePrice %>% arrange(ら戳) %>% select(程蔼基, 程基, Μ絃基) %>% rename(High = 程蔼基, Low = 程基, Close = Μ絃基) %>% stoch(., nFastK = 9, nFastD = 9, nSlowD = 9)
    OnePrice$K <- KD[, 1]
    OnePrice$FD <- KD[, 2]
    OnePrice$D <- KD[, 3]
    #OnePriceToday <- OnePrice[2:nrow(OnePrice),]
    #MA5DYesterday <- OnePrice[1:nrow(OnePrice),] %>% .$MA5D
    #OnePriceToday[,'MA5DY']<-MA5DYesterday
    OnePrice
}
MVKD <- OnePrice %>% AddMVandKD
GetMAD <- function(MAOne) {
    MAOne <- MAOne %>% filter(complete.cases(MAOne))
    MAOneToday <- MAOne[2:nrow(MAOne),]
    MA8Yesterday <- MAOne$MA8[1:(nrow(MAOne) - 1)]
    MA21Yesterday <- MAOne$MA21[1:(nrow(MAOne) - 1)]
    MA55Yesterday <- MAOne$MA55[1:(nrow(MAOne) - 1)]
    MAOneToday[, 'MA8Y'] <- MA8Yesterday
    MAOneToday[, 'MA21Y'] <- MA21Yesterday
    MAOneToday[, 'MA55Y'] <- MA55Yesterday
    MAOneToday %>% mutate(MA8D = MA8 - MA8Y, MA21D = MA21 - MA21Y, MA55D = MA55 - MA55Y)
}
MAD <- MVKD %>% GetMAD
GetMADD <- function(MAD) {
    MADToday <- MAD[2:(nrow(MAD)),]
    MA8DY <- MAD$MA8D[1:(nrow(MAD) - 1)]
    MA21DY <- MAD$MA21D[1:(nrow(MAD) - 1)]
    MA55DY <- MAD$MA55D[1:(nrow(MAD) - 1)]
    MADToday %>% mutate(MA8DY, MA21DY, MA55DY) %>% mutate(MA8DD = MA8D - MA8DY, MA21DD = MA21D - MA21DY, MA55DD = MA55D - MA55DY)
}
MADD <- MAD %>% GetMADD

GetThreeK <- function(MAD) {
    ThreeKToday <- MAD[3:nrow(MAD),]
    HKY <- MAD$`程蔼基`[2:(nrow(MAD) - 1)]
    HKY2 <- MAD$`程蔼基`[1:(nrow(MAD) - 2)]
    LKY <- MAD$`程基`[2:(nrow(MAD) - 1)]
    LKY2 <- MAD$`程基`[1:(nrow(MAD) - 2)]
    ThreeKToday %>% mutate(HKY, HKY2, LKY, LKY2)
}
ThreeK <- MADD %>% GetThreeK

Features <- ThreeK %>% select(ら戳, Μ絃基, MA8, MA21, MA55, MA8D, MA21D, MA55D, MA8DD, MA21DD, MA55DD)
Price <- Features %>% select(ら戳, Μ絃基)
Features <- Features %>% arrange(ら戳) %>% .[1:(nrow(Features)-10),]
GetLabel <- function(day,TPrice) {
    HPrice<-Price %>% filter(ら戳 > day) %>% arrange(ら戳) %>% .[1:10,] %>% .$Μ絃基 %>% max
    (HPrice - TPrice) / TPrice
}

Label <- Features %>% mutate(Label = pmap(list(ら戳, Μ絃基), GetLabel) %>% as.numeric)
GetFeature <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, MA8DD, MA21DD, MA55DD) {
    MAType <- 0
    MA8DT <- 0
    MA21DT <- 0
    MA55DT <- 0
    MA8DDT <- 0
    MA21DDT <- 0
    MA55DDT <- 0
    MA8DT <- ifelse(MA8D > 0, 1, 0)
    MA21DT<- ifelse(MA21D > 0, 1, 0)
    MA55DT<-ifelse(MA55D > 0, 1, 0)
    MA8DDT<-ifelse(MA8DD > 0, 1, 0)
    MA21DDT<-ifelse(MA21DD > 0, 1, 0)
    MA55DDT <- ifelse(MA55DD > 0, 1, 0)
    if (MA8 > MA21 & MA21 > MA55) {
        MAType <- 1
    } else if (MA8 > MA55 & MA55 > MA21) {
        MAType <- 2
    } else if (MA21 > MA8 & MA55 > MA55) {
        MAType <- 3
    } else if (MA21 > MA55 & MA55 > MA8) {
        MAType <- 4
    } else if (MA55 > MA21 & MA21 > MA8) {
        MAType <- 5
    } else if (MA55 > MA8 & MA8 > MA21) {
        MAType <- 6
    } else { MAType<-7 }
    tibble(MAType,MA8DT,MA8DDT,MA21DT,MA21DDT,MA55DT,MA55DDT)
}

LabelFeature <- Label %>% mutate(features = pmap(list(MA8, MA21, MA55, MA8D, MA21D, MA55D, MA8DD, MA21DD, MA55DD), GetFeature))


Result<-LabelFeature %>% select(ら戳, Μ絃基, features, Label) %>% unnest 
Result<-Result %>% mutate(BuyDate = str_split(ら戳, "/")) %>%
            mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
            mutate(Date = ymd(paste0(year, month, day)))
Result <- Result %>% select(-ら戳, - BuyDate, - year, -month, -day)
LabelFeature