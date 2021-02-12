library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
Sys.setlocale("LC_ALL", "cht")
companylist <- readRDS(file = "companylist.rds") %>% as.tibble %>% filter(公司代號!='公司代號')
DealWithDate <- function(Price) {
    Price %>% mutate(BuyDate = str_split(日期, "/")) %>%
        mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
           mutate(Date = ymd(paste0(year, month, day))) %>%
           select(-year, - month, - day, - 日期, - BuyDate)
}
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
    GetPrice <- function() {
        Year <- 2018:2020
        Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
        Price <- tibble()
        for (year in Year) {
            if (year == 2020) { Month <- c('01', '02', '03', '04', '05', '06','07','08','09','10','11') }
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
    tryCatch(Price <- GetPrice(), error = function(e) { print(paste0(Stock,e)) }, finally = print("Hello"))
    if (nrow(Price) != 0) {
        Price %>% CleanPrice %>% GetPriceDifference %>% arrange(日期)
    } else {
        Price
    }

}
Price4807 <- GetStockPriceByYear("4807")
saveRDS(Price0050, file = "Price00631L.rdata")
Price00631L <- readRDS(file = "Price00631L.rdata")
Price00631L %<>% DealWithDate
Price4807 %<>% DealWithDate
#PriceAll <- readRDS(file = "PriceAll.rds")
#Price201812 <- readRDS(file = "PriceAll_201812.rds")
#Price202012 <- readRDS(file = "PriceAll_202012.rds")
PriceAll <- tibble()
#companylist <- companylist %>% filter((公司代號 %in% EmptyCompanyList$公司代號))

for (i in 1:nrow(companylist)) {
    print(i)
    PriceOne <- companylist[i,] %>% mutate(price = map(公司代號, GetStockPriceByYear))
    PriceAll <- rbind(PriceAll, PriceOne)
    #Price2018091011 <- rbind(Price_all, PriceOne)
    #saveRDS(PriceAll, file = "PriceAll_2018091011.rds")
}
EmptyCompanyList <- PriceAll %>% mutate(nrowns = map(price, ~ nrow(.)) %>% as.numeric) %>% filter(nrowns == 0)
PriceAll <- PriceAll %>% filter(!(公司代號 %in% EmptyCompanyList$公司代號))
#companylist <- companylist %>% filter(公司代號 %in% EmptyCompanyList$公司代號)
Price2018091011 <- Price2018091011 %>% filter(!(公司代號 %in% EmptyCompanyList$公司代號))


Sys.time()
PriceOne <- companylist[10,] %>% mutate(price = map(公司代號, GetStockPriceByYear))
Sys.time()
PriceAll <- rbind(PriceAll, PriceOne)
#saveRDS(PriceAll, file = "PriceAll_202012.rds")
#Get Daily Price
Purl <- paste0("http://www.twse.com.tw/exchangeReport/TWT84U?response=json&date=20180801&selectType=ALLBUT0999&_=1535809647629")
Price <- POST(Purl)
Price <- Price %>% content(as = "text") %>% fromJSON
PriceData <- Price$data %>% as.tibble
names(PriceData) <- Price$fields
PriceData %>% .[, - c(3, 4, 5)] %>% filter(證券代號 %in% companylist$`公司代號`)

#Industry Category
#url view-source:http://mops.twse.com.tw/mops/web/ajax_t51sb01?=1&step=2&firstin=1&TYPEK=sii&code=02
Industry<-c("水泥工業",
              "食品工業",
              "塑膠工業",
              "紡織纖維",
              "電機機械",
              "電器電纜",
              "化學工業",
              "生技醫療業",
              "化學生技醫療",
              "玻璃陶瓷",
              "造紙工業",
              "鋼鐵工業",
              "橡膠工業",
              "汽車工業",
              "半導體業",
              "電腦及週邊設備業",
              "光電業",
              "通信網路業",
              "電子零組件業",
              "電子通路業",
              "資訊服務業",
              "其他電子業",
              "電子工業",
              "油電燃氣業",
              "建材營造",
              "航運業",
              "觀光事業",
              "金融保險業",
              "貿易百貨",
              "綜合企業",
              "其他",
              "存託憑證")
Index<-c(01,02,03,04,05,06,21,22,07,08,09,10,11,12,24,25,26,27,28,29,30,31,13,23,14,15,16,17,18,19,20,91)




#Combine
PriceAll_201001_201808 <- readRDS(file = "PriceAll_201001_201808.rds")
Price2018091011
PriceAll_201001_201808$`公司代號` %>% unique

PriceAll_201001_201808_unnest <- PriceAll_201001_201808 %>% unnest %>% unique
Price2018091011_unnest <- Price2018091011 %>% unnest %>% unique
PriceAll_201001_201811 <- rbind(PriceAll_201001_201808_unnest, Price2018091011_unnest) %>% group_by(公司代號, 公司名稱) %>% nest
saveRDS(PriceAll_201001_201811, file = "PriceAll_201001_201811.rds")

PriceAll_201001_201811$data[[1]] %>% View


#Data Deal 
Price00631L <- Price00631L %>% mutate(WeekNumber = epiweek(Date),Year=year(Date))
LastDayPrice <- Price00631L %>% group_by(Year, WeekNumber) %>% filter(Date == max(Date)) %>% arrange(desc(Date))

INDEX <- Price00631L %>% select(Year, WeekNumber) %>% unique %>% arrange(Year, WeekNumber) %>% mutate(INDEX = 1:nrow(.))

LastDayPrice %<>% select(收盤價)

LastWeekLastDayPrice <- LastDayPrice %>% left_join(INDEX) %>% mutate(INDEX = INDEX + 1) %>% ungroup %>% select(-Year, - WeekNumber) %>% rename(LastWeekLastDayPrice = 收盤價)
ThisWeekLastDayPrice <- LastDayPrice %>% left_join(INDEX) %>% ungroup %>% select(-Year, - WeekNumber) %>% rename(ThisWeekLastDayPrice = 收盤價)

Price00631L %<>% left_join(INDEX)

Price00631LS <- Price00631L %>% left_join(LastWeekLastDayPrice) %>% left_join(ThisWeekLastDayPrice)
Price00631LS %>% View
write.csv(Price00631LS, file = "Price00631L.csv")




#

Price_2010_2020 <- rbind(PriceAll, Price202012) %>% unnest

Price_2010_2020_nest <- Price_2010_2020 %>% group_by(公司代號, 公司名稱) %>% nest
deal_with_data <- function(data) {
    data %>% unique %>% DealWithDate
}
Price_2010_2020_nest <- Price_2010_2020_nest %>% mutate(new_data = map(data, deal_with_data))

Price_2010_2020_nest <- Price_2010_2020_nest %>% select(-data)

one_price <- Price_2010_2020_nest$new_data[[1]]

write.csv(one_price, file = 'price_sample.csv')

one_price %>% unique