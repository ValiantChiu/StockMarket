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
OnePrice <- PriceAll_201001_201811$data[[1]]
GetOneStockProfit <- function(OnePrice, BullUpperBound, BullLowerBound, BearUpperBound, BearLowerBound) {
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

    df <- ThreeK
    GetBullPoint <- function(df, UpperBound, LowerBound) {
        df %>% mutate(BullPoint = ifelse(MA8DD > LowerBound & MA8DD < UpperBound, 1, 0))
    }
    GetBearPoint <- function(df, UpperBound, LowerBound) {
        df %>% mutate(BearPoint = ifelse(MA8DD > LowerBound & MA8DD < UpperBound, 1, 0))
    }
    Bull <- df %>% GetBullPoint(., BullUpperBound, BullLowerBound)
    Bear <- Bull %>% GetBearPoint(., BearUpperBound, BearLowerBound)


    GetProfit <- function(Label) {
        Price <- Label
        BuyPoint <- Label %>% filter(BullPoint == 1) # %>% select(-SellLabel, - SellPrice)
        BuyAll <- tibble()
        if (nrow(Price) > 0) {
            for (i in 1:nrow(BuyPoint)) {
                BuyOne <- BuyPoint[i,]
                BuyPrice <- BuyOne$Μ絃基[1]
                BuyDate <- BuyOne$`ら戳`[1]
                SellPoint <- Price %>% filter(ら戳 > BuyDate) %>% filter(BearPoint == 1) %>% arrange(ら戳) %>% .[1,]
                SellPrice <- SellPoint$Μ絃基[1]
                SellDate <- SellPoint$`ら戳`[1]
                BuyOne <- BuyOne %>% mutate(SellDate, SellPrice)
                BuyAll <- rbind(BuyAll, BuyOne)
            }
        }
        BuyAll
    }
    Profit <- Bear %>% GetProfit
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
    Profit <- Profit %>% DealWithDate %>%
    select(ら戳, BuyDate, Μ絃基, SellDate, SellPrice, TimeLength) %>%
    mutate(Profit = (SellPrice - Μ絃基) / Μ絃基 / TimeLength)
    AVGProfit <- Profit %>% .$Profit %>% mean
    Times <- Profit %>% nrow
    AllProfit <- Profit %>% .$Profit %>% sum
    tibble(AVGProfit, Times, AllProfit, Result=list(Profit))
}


BullUpperBound <- seq(0.1, 0.6, 0.1)
BullLowerBound <- BullUpperBound - 0.1
BearUpperBound<-seq(-0.5,0,0.1)
BearLowerBound <- BearUpperBound - 0.1
BullP <- tibble(BullUpperBound, BullLowerBound)
BearP <- tibble(BearUpperBound, BearLowerBound)
Recipe <- BullP %>% mutate(BearP = list(BearP)) %>% unnest

Recipe <- Recipe %>% mutate(OneStock = list(OnePrice))
Result <- Recipe %>% mutate(Profit = pmap(list(OneStock, BullUpperBound, BullLowerBound, BearUpperBound, BearLowerBound), GetOneStockProfit))

Result %>% select(-OneStock) %>% unnest %>% filter(AllProfit == max(AllProfit)) %>% select(Result) %>% unnest %>% View

Result %>% select(-OneStock) %>% unnest %>% select(-Result) %>% View





