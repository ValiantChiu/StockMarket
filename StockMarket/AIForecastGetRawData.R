library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)
Sys.setlocale("LC_ALL", "cht")

#GetPrice
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
GetStockPriceByYear <- function(Stock,Year) {
    GetOneMonthStockPrice <- function(Stock, Year, Month) {
        Purl <- paste0("http://www.twse.com.tw/exchangeReport/STOCK_DAY?response=json&date=", Year, Month, "01&stockNo=", Stock, "")
        Price <- POST(Purl)
        Price <- Price %>% content(as = "text") %>% fromJSON
        PriceData <- Price$data %>% as.tibble
        names(PriceData) <- Price$fields
        PriceData
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
    Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
    Price <- tibble()
    for (year in Year) {
        for (month in Month) {
            print(paste0(year, "", month))
            Result <- GetOneMonthStockPrice(Stock, year, month)
            Price <- rbind(Price, Result)
            Sys.sleep(sample(4:10, size = 1))
        }
    }
    Price %>% CleanPrice %>% GetPriceDifference %>% arrange(日期)
}

Price0050New <- GetStockPriceByYear("0050",2018)
Sys.time()
GetStockPriceByYear2("1101")
Sys.time()
Price0050 <- Price0050 %>% filter(日期 < '107/01/01') %>% rbind(Price0050New)
 saveRDS(Price0050, file = "Price0050Raw.rds")
Price0050 <- readRDS(file = "Price0050Raw.rds") #

#GetThreeBigInvestorsInform
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
Investor <- readRDS(file = "InvestorRaw.rds")#
InvestorNew <- Price0050 %>% select(日期) %>% filter(!(日期 %in% Investor$`日期`)) %>% GetThreeBigInvestorsInform
Investor <- rbind(Investor, InvestorNew)

saveRDS(Investor,file = "InvestorRaw.rds")

#GetTrainData

PriceInvestor0050 <- Price0050 %>% CleanPrice %>% GetPriceDifference %>% left_join(Investor) %>% arrange(日期)

KDNew <- (PriceInvestor0050 %>% filter(!is.na(開盤價)) %>% arrange(日期) %>% select(最高價, 最低價, 收盤價) %>% rename(High = 最高價, Low = 最低價, Close = 收盤價)) %>% stoch(., nFastK = 9)

Train0050 <- cbind(PriceInvestor0050 %>% filter(!is.na(開盤價)), KDNew) %>% as.tibble %>% .[13:nrow(.),]

saveRDS(Train0050, file = "Train0050.rds")
Train0050 <- readRDS(file = "Train0050.rds")

library(readr)
titanic <- read_csv("http://mops.twse.com.tw/server-java/t105sb02?firstin=true&step=10&filename=t163sb04_20180905_211542605.csv")
read_csv("http://mops.twse.com.tw/server-java/t105sb02?firstin=true&step=10&filename=t163sb04_20180905_211542605.csv")

library(curl)
url = "ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2015/11/PXD000299/"
h = new_handle(dirlistonly = TRUE)
 con = curl(url, "r", h)
 tbl = read.table(con, stringsAsFactors = TRUE, fill = TRUE)
 close(con)
head(tbl)

urls <- paste0(url, tbl[1:5, 1])
fls = basename(urls)
curl_fetch_disk(urls[1], fls[1])