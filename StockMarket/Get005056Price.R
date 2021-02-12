library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)


Sys.setlocale("LC_ALL", "cht")
#Get Price
companylist <- tibble(そ腹=c('0050','0051'))
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
        Year <- 2010:2020
        Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
        Price <- tibble()
        for (year in Year) {
            #if (year == 2019) { Month <- c('01') }
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


Price005056_2010_2018 <- tibble()
for (i in 1:nrow(companylist)) {
    print(i)
    PriceOne <- companylist[i,] %>% mutate(price = map(そ腹, GetStockPriceByYear))
    #PriceAll <- rbind(PriceAll, PriceOne)
    Price005056_2010_2018 <- rbind(Price005056_2010_2018, PriceOne)
    saveRDS(Price005056_2010_2018, file = "Price005051_2010_2020.rds")
}

Price005056_2010_2018$price[[2]]


