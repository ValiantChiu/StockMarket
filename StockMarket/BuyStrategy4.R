library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)
Sys.setlocale("LC_ALL", "cht")
#GetRawData
PriceAll_201001_201811 <- readRDS(file = "PriceAll_201001_201811.rds")
Price201812 <- readRDS(file = "Price201812.rds")
Price201901 <- readRDS(file = "Price201901.rds")

Price201812 <- Price201812 %>% unnest
Price201901 <- Price201901 %>% unnest
PriceAll_201001_201811 <- PriceAll_201001_201811 %>% unnest

PriceAll_201001_201901 <- rbind(PriceAll_201001_201811, Price201812, Price201901) %>% group_by(そ腹, そ嘿) %>% nest


#Add Feature and Label

GetTodayYesterdayData <- function(df) {
    Today <- df[2:nrow(df),] %>% select(ら戳, Θユ计, Θユ肂, 秨絃基, 程蔼基, 程基, Μ絃基, 害禴基畉, Θユ掸计)
    Today <- Today %>% rename(ValueToday = Θユ肂, OpenPriceToday = 秨絃基, HighPriceToday = 程蔼基, LowPriceToday = 程基, ClosePriceToday=Μ絃基)
    Yesterday <- df[1:(nrow(df) - 1),] %>% select(Θユ肂, 秨絃基, 程蔼基, 程基, Μ絃基)
    names(Yesterday)<-c("ValueYesterday","OpenPriceYesterday","HighPriceYesterday","LowPriceYesterday","ClosePriceYesterday")
    cbind(Today, Yesterday) %>% as.tibble
}


GetFeature <- function(stock, df) {
    print(stock)
    Feature <- function(OpenPriceToday, OpenPriceYesterday, HighPriceToday, HighPriceYesterday, ClosePriceToday, ClosePriceYesterday, LowPriceToday, LowPriceYesterday, ValueToday, ValueYesterday) {
        OpenPriceF <- ifelse(OpenPriceToday > OpenPriceYesterday, '1', '0')
        HighPriceF <- ifelse(HighPriceToday > HighPriceYesterday, '1', '0')
        ClosePriceF <- ifelse(ClosePriceToday > ClosePriceYesterday, '1', '0')
        LowPriceF <- ifelse(LowPriceToday > LowPriceYesterday, '1', '0')
        ValueF <- ifelse(ValueToday > ValueYesterday, '1', '0')
        tibble(OpenPriceF, HighPriceF, ClosePriceF, LowPriceF, ValueF)
    }
    #df %>% mutate(result = pmap(list(OpenPriceToday, OpenPriceYesterday, HighPriceToday, HighPriceYesterday, ClosePriceToday, ClosePriceYesterday, LowPriceToday, LowPriceYesterday, ValueToday, ValueYesterday), Feature)) %>% unnest
    df %>% mutate(OpenPriceF = ifelse(OpenPriceToday > OpenPriceYesterday, '1', '0'),
                  HighPriceF = ifelse(HighPriceToday > HighPriceYesterday, '1', '0'),
                  ClosePriceF = ifelse(ClosePriceToday > ClosePriceYesterday, '1', '0'),
                  LowPriceF = ifelse(LowPriceToday > LowPriceYesterday, '1', '0'),
                  ValueF = ifelse(ValueToday > ValueYesterday, '1', '0'))
}


PriceAll_201001_201901 <- PriceAll_201001_201901 %>% mutate(Result = map( data, GetTodayYesterdayData))
PriceAll_Feature <- PriceAll_201001_201901 %>% mutate(Result = map2(そ腹, Result, GetFeature)) %>% select(-data)

PriceAll_Feature <- Price005056_2010_2018 %>% mutate(Result = map(price, GetTodayYesterdayData)) %>% mutate(Result = map2(そ腹, Result, GetFeature)) %>% select(-price)
 

GetLabel <- function(df) {
    GetProfit <- function(Price) {
        BuyPoint <- Price %>% filter(!is.na(OpenPriceToday))
        BuyAll <- tibble()
        if (nrow(Price) > 0) {
            for (i in 1:(nrow(BuyPoint)-11)) {
                BuyOne <- BuyPoint[i,]
                BuyPrice <- BuyOne$OpenPriceToday[1]
                BuyDate <- BuyOne$`ら戳`[1]
                

                SellPoint <- Price %>% filter(ら戳 > BuyDate) %>% arrange(ら戳) %>% .[1:10,] %>% filter(OpenPriceToday == max(OpenPriceToday))

                SellPrice <- SellPoint$OpenPriceToday[1]
                SellDate <- SellPoint$`ら戳`[1]
                BuyOne <- BuyOne %>% mutate(SellDate, SellPrice)
                BuyAll <- rbind(BuyAll, BuyOne)
            }
        }
        BuyAll
    }
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
    df %>% GetProfit %>% DealWithDate
}

df <- PriceAll_Feature$Result[[1]]
Result <- df %>% GetLabel
Result %<>% mutate(Feature = paste0(OpenPriceF, HighPriceF, ClosePriceF, LowPriceF, ValueF)) 
write.csv(Result,file='BuyStrategy4.csv')
Result %>% View