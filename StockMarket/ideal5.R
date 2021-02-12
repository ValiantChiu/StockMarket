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
con <- file('PriceAll_201001_201811.csv', encoding = "UTF-8")
write.csv(PriceAll_201001_201811_unnest,file=con)
OneSample <- PriceAll_201001_201811$data[[1]]
GetAbnormalData <- function(Price) {
    Price %>% mutate(Abnormal = ifelse((開盤價 - 收盤價) / 收盤價 > 0.07, 1, 0)) %>% filter(Abnormal == 1)
}
AbnormalData <- PriceAll_201001_201811 %>% mutate(AbnormalData = map(data, GetAbnormalData))
AbnormalData %>% select(公司代號, AbnormalData) %>% unnest %>% arrange(desc(日期)) %>% filter(公司代號 == 2023)


GetDailyExchangeReport <- function(Year, Month, Date) {
    Exurl <- paste0("http://www.tse.com.tw/exchangeReport/TWTASU?response=json&date=", Year, Month, Date)
    Exchange <- POST(Exurl)
    Exchange <- Exchange %>% content(as = "text") %>% fromJSON
    ExchangeData <- Exchange$data %>% as.tibble
    names(ExchangeData) <- Exchange$fields
    ExchangeData
}
Year <- 2018
Month <- 12
Date<-20
ExchangeReport<-GetDailyExchangeReport(Year, Month, Date)
ExchangeReport$`證券名稱`[1] %>% strsplit(., split = " ") %>% .[[1]] %>% .[1]

CleanExchangeReport <- function(df) {
    CleanName <- function(Name) {
        Name %>% strsplit(., split = " ") %>% .[[1]] %>% .[1]
    }
    df <- df %>% mutate(公司代號 = map(`證券名稱`, CleanName) %>% as.character) %>% select(-證券名稱)
    df$融券賣出成交數量 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$融券賣出成交金額 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$借券賣出成交數量 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$借券賣出成交金額 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df
}
ExchangeReport %>% CleanExchangeReport %>% filter(!complete.cases(.))
ExchangeReport %>% mutate(公司代號 = map(`證券名稱`, CleanName) %>% as.character) %>% select(-證券名稱) %>% filter(公司代號 == 1101)
ExchangeReport %>% mutate(公司代號 = map(`證券名稱`, CleanName) %>% as.character) %>% select(-證券名稱)

PriceAll_201001_201811_unnest %<>% filter(complete.cases(.)) %>% mutate(BuyDate = str_split(日期, "/")) %>%
        mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>% mutate(日期 = ymd(paste0(year, month, day))) %>% select(-year, - month, - day, -BuyDate)
         