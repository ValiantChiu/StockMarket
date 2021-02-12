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
Price201812 <- readRDS(file = "Price201812.rds")
Price201901 <- readRDS(file = "Price201901.rds")

Price201812_unnest <- Price201812 %>% unnest
Price201901_unnest <- Price201901 %>% unnest
PriceAll_201001_201811_unnest <- PriceAll_201001_201811 %>% unnest
PriceAll_201001_201901 <- rbind(PriceAll_201001_201811_unnest, Price201812_unnest, Price201901_unnest) %>% group_by(そ腹, そ嘿) %>% nest

Price <- PriceAll_201001_201901 %>% filter(そ腹 == 1906) %>% .$data %>% .[[1]]

CleanDate <- function(df) {
    df %>% mutate(BuyDate = str_split(ら戳, "/")) %>%
            mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
            mutate(Date = ymd(paste0(year, month, day))) %>% select(-BuyDate, - year, - month, - day)
}

Price <- Price %>% CleanDate

GetMaxMinPricePoint <- function(df) {
    GetLabel <- function(Date, 程蔼基, PreviousPrice, NextPrice) {
        print(Date)
        if (程蔼基 > PreviousPrice & 程蔼基 >= NextPrice) {
            'High'} else if (程蔼基 <= PreviousPrice & 程蔼基 < NextPrice) {
            'Low' } else { 
            'NO'
        }
    }
    df <- df %>% arrange(Date) %>% filter(Θユ掸计!=0 & !is.na(Μ絃基))
    MainPrice <- df[2:(nrow(df) - 1),]
    PreviousPrice <- df[1:(nrow(df) - 2),]$程蔼基
    NextPrice <- df[3:(nrow(df)),]$程蔼基
    MainPrice <- MainPrice %>% mutate(PreviousPrice, NextPrice)
    MainPrice <- MainPrice %>% mutate(Lable = pmap(list(ら戳, 程蔼基, PreviousPrice, NextPrice), GetLabel) %>% as.character)
    LastPrice <- df[(nrow(df)),] %>% mutate(PreviousPrice = df[(nrow(df))-1,]$程蔼基, NextPrice = NA)
    LastPrice <- LastPrice %>% mutate(Lable = ifelse(程蔼基 >= PreviousPrice, 'High', 'Low'))
    rbind(MainPrice, LastPrice)
}
Price <- Price %>% GetMaxMinPricePoint
Price <- Price %>% arrange(Date) %>% mutate(index = 1:nrow(Price))


PricePeriod <- Price %>% filter(Date > '2018-01-01')

GetPreviousInformation <- function(df) {
    GetHighLowPointTopThree <- function(Time) {
        HighLowPoint <- Price %>% filter(Date < Time) %>% arrange(desc(Date)) %>% group_by(Lable) %>% nest %>% filter(Lable == 'Low' | Lable == 'High')
        GetTopThree <- function(Lable, data) {
            all <- data %>% arrange(desc(Date)) %>% .[1:3,] %>% select(Θユ计, 程蔼基, index) %>% mutate(sequence = paste0(Lable, 1:3))
            value <- all %>% select(Θユ计, sequence) %>% mutate(sequence = paste0('value', sequence)) %>% spread(key = sequence, value = Θユ计)
            price <- all %>% select(程蔼基, sequence) %>% mutate(sequence = paste0('price', sequence)) %>% spread(key = sequence, value = 程蔼基)
            index <- all %>% select(index, sequence) %>% mutate(sequence = paste0('index', sequence)) %>% spread(key = sequence, value = index)
            cbind(value, price, index) %>% as.tibble
        }
        HighLowPointTopThree <- HighLowPoint %>% mutate(LableData = map2(Lable, data, GetTopThree)) %>% select(LableData)
        cbind(HighLowPointTopThree$LableData[[1]], HighLowPointTopThree$LableData[[2]])
    }
    df %>% mutate(HighLowPoint = map(Date, GetHighLowPointTopThree))

}
PricePeriodHighLow <- PricePeriod %>% GetPreviousInformation %>% unnest

PricePeriodHighLow <- PricePeriodHighLow %>% mutate(Criterion1 = ifelse(程蔼基 > priceHigh1, 1, 0),
                             UpSlopeNow = (程蔼基 - priceLow1) / (index - indexLow1),
                             DownSlopePrevious = (priceLow1 - priceHigh1) / (indexLow1 - indexHigh1),
                             DownSlopePPrevious = (priceLow2 - priceHigh2) / (indexLow2 - indexHigh2),
                             TrendPriceSlope = (priceHigh1 - priceHigh2) / (indexHigh1 - indexHigh2),
                             TrendPriceSlope2 = (priceHigh1 - priceHigh3) / (indexHigh1 - indexHigh3)) %>%
                             mutate(TrendPrcieCriterion = priceHigh2 + (index - indexHigh2) * TrendPriceSlope,
                                    TrendPrcieCriterion2 = priceHigh3 + (index - indexHigh3) * TrendPriceSlope2) %>%
                                    mutate(TrendPrcieCriterion = min(TrendPrcieCriterion, TrendPrcieCriterion2))

PricePeriodHighLow <- PricePeriodHighLow %>% mutate(Criterion2 = ifelse(UpSlopeNow > abs(DownSlopePrevious), 1, 0),
                                                    Criterion3 = ifelse(DownSlopePrevious >= DownSlopePPrevious, 1, 0),
                                                    Criterion4 = ifelse(程蔼基 > TrendPrcieCriterion, 1, 0),
                                                    Criterion5 = ifelse(Θユ计 > valueLow1, 1, 0),
                                                    Criterion6 = ifelse(indexLow1 > indexHigh1, 1, 0),
                                                    Criterion7 = ifelse(UpSlopeNow > DownSlopePPrevious, 1, 0),
                                                    Criterion8 = ifelse(程蔼基 < priceHigh1,1,0))
PricePeriodHighLowAction <- PricePeriodHighLow %>% mutate(Action = Criterion2 * Criterion3 * Criterion4 * Criterion6 * Criterion8)
PricePeriodHighLowAction %>% arrange(desc(Date))
write.csv(PricePeriodHighLowAction, file = "Price1101.csv")


PricePeriodHighLowAction %>% filter(Date == '2018-10-12') %>% View




GetStockAction <- function(Stock, Price) {
    print(Stock)
    CleanDate <- function(df) {
        df %>% mutate(BuyDate = str_split(ら戳, "/")) %>%
            mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
            mutate(Date = ymd(paste0(year, month, day))) %>% select(-BuyDate, - year, - month, - day)
    }

    Price <- Price %>% CleanDate
    GetMaxMinPricePoint <- function(df) {
        GetLabel <- function(Date, 程蔼基, PreviousPrice, NextPrice) {
           # print(Date)
            if (程蔼基 >= PreviousPrice & 程蔼基 > NextPrice) {
                'High'
            } else if (程蔼基 <= PreviousPrice & 程蔼基 < NextPrice) {
                'Low'
            } else {
                'NO'
            }
        }
        df <- df %>% arrange(Date) %>% filter(Θユ掸计 != 0 & !is.na(Μ絃基))
        MainPrice <- df[2:(nrow(df) - 1),]
        PreviousPrice <- df[1:(nrow(df) - 2),]$程蔼基
        NextPrice <- df[3:(nrow(df)),]$程蔼基
        MainPrice <- MainPrice %>% mutate(PreviousPrice, NextPrice)
        MainPrice <- MainPrice %>% mutate(Lable = pmap(list(ら戳, 程蔼基, PreviousPrice, NextPrice), GetLabel) %>% as.character)
        LastPrice <- df[(nrow(df)),] %>% mutate(PreviousPrice = df[(nrow(df)) - 1,]$程蔼基, NextPrice = NA)
        LastPrice <- LastPrice %>% mutate(Lable = ifelse(程蔼基 >= PreviousPrice, 'High', 'Low'))
        rbind(MainPrice, LastPrice)
    }
  Price <- Price %>% GetMaxMinPricePoint
    Price <- Price %>% arrange(Date) %>% mutate(index = 1:nrow(Price))
    PricePeriod <- Price %>% filter(Date > '2019-01-07')

    GetPreviousInformation <- function(df) {
        GetHighLowPointTopThree <- function(Time) {
            HighLowPoint <- Price %>% filter(Date < Time) %>% arrange(desc(Date)) %>% group_by(Lable) %>% nest %>% filter(Lable == 'Low' | Lable == 'High')
            GetTopThree <- function(Lable, data) {
                all <- data %>% arrange(desc(Date)) %>% .[1:3,] %>% select(Θユ计, 程蔼基, index) %>% mutate(sequence = paste0(Lable, 1:3))
                value <- all %>% select(Θユ计, sequence) %>% mutate(sequence = paste0('value', sequence)) %>% spread(key = sequence, value = Θユ计)
                price <- all %>% select(程蔼基, sequence) %>% mutate(sequence = paste0('price', sequence)) %>% spread(key = sequence, value = 程蔼基)
                index <- all %>% select(index, sequence) %>% mutate(sequence = paste0('index', sequence)) %>% spread(key = sequence, value = index)
                cbind(value, price, index) %>% as.tibble
            }
            HighLowPointTopThree <- HighLowPoint %>% mutate(LableData = map2(Lable, data, GetTopThree)) %>% select(LableData)
            cbind(HighLowPointTopThree$LableData[[1]], HighLowPointTopThree$LableData[[2]])
        }
        df %>% mutate(HighLowPoint = map(Date, GetHighLowPointTopThree))

    }
    PricePeriodHighLow <- PricePeriod %>% GetPreviousInformation %>% unnest

    PricePeriodHighLow <- PricePeriodHighLow %>% mutate(Criterion1 = ifelse(程蔼基 > priceHigh1, 1, 0),
                             UpSlopeNow = (程蔼基 - priceLow1) / (index - indexLow1),
                             DownSlopePrevious = abs(priceLow1 - priceHigh1) / (indexLow1 - indexHigh1),
                             DownSlopePPrevious = abs(priceLow2 - priceHigh2) / (indexLow2 - indexHigh2),
                             TrendPriceSlope = (priceHigh1 - priceHigh2) / (indexHigh1 - indexHigh2),
                             TrendPriceSlope2 = (priceHigh1 - priceHigh3) / (indexHigh1 - indexHigh3)) %>%
                             mutate(TrendPrcieCriterion = priceHigh2 + (index - indexHigh2) * TrendPriceSlope,
                                    TrendPrcieCriterion2 = priceHigh3 + (index - indexHigh3) * TrendPriceSlope2) %>%
                                    mutate(TrendPrcieCriterion = min(TrendPrcieCriterion, TrendPrcieCriterion2))

    PricePeriodHighLow <- PricePeriodHighLow %>% mutate(Criterion2 = ifelse(UpSlopeNow > DownSlopePrevious, 1, 0),
                                                    Criterion3 = ifelse(DownSlopePrevious <= DownSlopePPrevious, 1, 0),
                                                    Criterion4 = ifelse(程蔼基 > TrendPrcieCriterion, 1, 0),
                                                    Criterion5 = ifelse(Θユ计 > valueLow1, 1, 0),
                                                    Criterion6 = ifelse(indexLow1 > indexHigh1, 1, 0),
                                                    Criterion7 = ifelse(UpSlopeNow > DownSlopePPrevious, 1, 0),
                                                    Criterion8 = ifelse(程蔼基 < priceHigh1, 1, 0))
    PricePeriodHighLowAction <- PricePeriodHighLow %>% mutate(Action = Criterion2 * Criterion3 * Criterion4 * Criterion6 * Criterion8)
    PricePeriodHighLowAction %>% arrange(desc(Date)) %>% .[1,]
}

PriceAll_201001_201901 %<>% mutate(nrown = map(data, ~ nrow(.)) %>% as.numeric)
Result <- PriceAll_201001_201901 %>% filter(nrown == 2231) %>% filter(そ腹 != '2498' & そ腹 != '3008' & そ腹 != '911619' & そ腹 != '911622' & そ腹 != '1418' & そ腹 != '1470' &
そ腹 != '910708' & そ腹 != '910861') %>% mutate(Result = map2(そ腹, data, GetStockAction))


Result %>% select(-data) %>% unnest %>% filter(Action == 1) %>% View

Result %>% select(-data) %>% unnest %>% View