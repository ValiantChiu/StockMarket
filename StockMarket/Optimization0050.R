library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)
Sys.setlocale("LC_ALL", "cht")
companylist <- readRDS(file = "companylist.rds") %>% as.tibble %>% filter(そ腹 != 'そ腹')

GetStockPriceByYear <- function(Stock) {

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
        Year <- 2018:2019
        Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
        Price <- tibble()
        for (year in Year) {
            if (year == 2019) { Month <- c('01', '02', '03', '04', '05', '06') }
            for (month in Month) {
                print(paste0(year, "", month))
                Result <- GetOneMonthStockPrice(Stock, year, month)
                Price <- rbind(Price, Result)
                Sys.sleep(sample(4:8, size = 1))
            }
        }
        Price
    }
    Price <- tibble()
    tryCatch(Price <- GetPrice(), error = function(e) { print(paste0(Stock, e)) }, finally = print("Hello"))
    if (nrow(Price) != 0) {
        Price %>% CleanPrice %>% GetPriceDifference %>% arrange(ら戳)
    } else {
        Price
    }

}
Price0050 <- GetStockPriceByYear('0050')


Price0050 %>% rename(High = 程蔼基, Low = 程基, Close = Μ絃基) %>% stoch(.[, c("High", "Low", "Close")], nFastK = 9)
Price0050
result <- Price0050 %>% rename(High = 程蔼基, Low = 程基, Close = Μ絃基)
result %>% arrange(desc(ら戳))
stoch(result[, c("High", "Low", "Close")], nFastK = 9)

DealWithDate <- function(Price) {
    Price %>% mutate(BuyDate = str_split(ら戳, "/")) %>%
        mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
           mutate(Date = ymd(paste0(year, month, day))) %>%
           select(-year, - month, - day, - ら戳, - BuyDate)
}
result %>% DealWithDate
? stoch