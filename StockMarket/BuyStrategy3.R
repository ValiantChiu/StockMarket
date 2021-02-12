library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)

Sys.setlocale("LC_ALL", "cht")
#Get Price
companylist <- readRDS(file = "companylist.rds") %>% as.tibble %>% filter(!(公司代號 %in% c("2025", "2856", "3514" ,"3561" ,"4984" ,"6131", "6145", "6422")))
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
        Year <- 2019:2019
        #Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
        Price <- tibble()
        for (year in Year) {
            if (year == 2019) { Month <- c('02') }
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
        Price %>% CleanPrice %>% GetPriceDifference %>% arrange(日期)
    } else {
        Price
    }

}
Price201902 <- readRDS(file = "PriceAll_201902.rds")

companylist <- companylist %>% filter(!(公司代號 %in% Price201902$公司代號)) %>% filter(公司代號!='公司代號')
companylist %>% View
#Price201902 <- tibble()
for (i in 1:nrow(companylist)) {
    print(i)
    PriceOne <- companylist[i,] %>% mutate(price = map(公司代號, GetStockPriceByYear))
    #PriceAll <- rbind(PriceAll, PriceOne)
    Price201902 <- rbind(Price201902, PriceOne)
    saveRDS(Price201902, file = "Price201902.rds")
}
EmptyCompanyList <- Price201902 %>% mutate(nrowns = map(price, ~ nrow(.)) %>% as.numeric) %>% filter(nrowns == 0 & 公司名稱!="公司名稱")
#companylist <- companylist %>% filter(公司代號 %in% EmptyCompanyList$公司代號)
Price201902 <- Price201902 %>% filter(!(公司代號 %in% EmptyCompanyList$公司代號))
PriceAll_201001_201811 <- readRDS(file = "PriceAll_201001_201811.rds")
Price201812 <- readRDS(file = "Price201812.rds")
Price201901 <- readRDS(file = "Price201901.rds")

Price201812_unnest <- Price201812 %>% unnest
Price201901_unnest <- Price201901 %>% unnest
Price201902_unnest <- Price201902 %>% unnest
PriceAll_201001_201811_unnest <- PriceAll_201001_201811 %>% unnest

PriceAll_201001_201902 <- rbind(PriceAll_201001_201811_unnest, Price201812_unnest, Price201901_unnest, Price201902_unnest) %>% group_by(公司代號, 公司名稱) %>% nest


#Strategy
GetLastestLabel <- function(Price, index, Type) {
    #Price <- Price %>% filter(日期<'108/01/05')
    print(index)
    nrown <- 0
    if (nrow(Price) < 140) { nrown <- nrow(Price) } else { nrown <- 140 }
    Price <- Price[(nrow(Price) - nrown + 1):nrow(Price),] %>% filter(complete.cases(.))
    if (Type == 'Bull') {
        GetLabel <- function(BuySellPrice) {
            GetBuyLabel <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, 最高價, HKY, HKY2, MA8DD, MA21DD, MA55DD) {
                if (
                    (MA8D > MA21D) & (MA21D > 0) & MA55D > 0 & MA8DD > 0 & MA21DD > 0 & MA55DD > 0 #& (MA8 - MA21 > MA21 - MA55)
                    #(MA8D > MA21D) & (MA21D > 0)  & MA8DD > 0 & MA21DD > 0 & MA55DD > 0
                    ) {
                    1
                } else {
                    0
                }
            }
            BuySellPrice %>% mutate(BuyLabel = pmap(list(MA8, MA21, MA55, MA8D, MA21D, MA55D, 最高價, HKY, HKY2, MA8DD, MA21DD, MA55DD), GetBuyLabel) %>% as.character)
        }
    } else {
        GetLabel <- function(BuySellPrice) {
            GetBuyLabel <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, 最高價, HKY, HKY2, MA8DD, MA21DD, MA55DD) {
                if ((MA8 < MA21) & (MA21 < MA55) &
                    (MA8D < MA21D) & (MA21D < 0) & MA55DD < 0 & MA8DD < 0 #& (MA8 - MA21 > MA21 - MA55)
                    ) {
                    1
                } else {
                    0
                }
            }
            BuySellPrice %>% mutate(BuyLabel = pmap(list(MA8, MA21, MA55, MA8D, MA21D, MA55D, 最高價, HKY, HKY2, MA8DD, MA21DD, MA55DD), GetBuyLabel) %>% as.character)
        }
    }
    Result <- Price %>% arrange(日期) %>% AddMVandKD %>% GetMAD %>% GetMADD %>% GetThreeK %>% GetLabel %>% arrange(日期) %>% .[nrow(.),]
    if (Result$收盤價[1] > 50) { FT <- 0 } else { FT <- 1 }
    TradeAmount <- ifelse(Result$成交股數 / 1000 > 500, 1, 0)
    Label <- (Result$BuyLabel[1] %>% as.numeric) * FT
    tibble(list(Result), Label, TradeAmount)
}

AddMVandKD <- function(OnePrice) {
    OnePrice <- OnePrice %>% arrange(日期)
    OnePrice <- OnePrice %>% filter(!is.na(收盤價)) %>% mutate(MA8 = SMA(收盤價, n = 8), MA21 = SMA(收盤價, n = 21), MA55 = SMA(收盤價, n = 55))
    KD <- OnePrice %>% arrange(日期) %>% select(最高價, 最低價, 收盤價) %>% rename(High = 最高價, Low = 最低價, Close = 收盤價) %>% stoch(., nFastK = 9, nFastD = 9, nSlowD = 9)
    OnePrice$K <- KD[, 1]
    OnePrice$FD <- KD[, 2]
    OnePrice$D <- KD[, 3]
    #OnePriceToday <- OnePrice[2:nrow(OnePrice),]
    #MA5DYesterday <- OnePrice[1:nrow(OnePrice),] %>% .$MA5D
    #OnePriceToday[,'MA5DY']<-MA5DYesterday
    OnePrice
}

GetMAD <- function(MAOne) {
    MAOne <- MAOne %>% filter(complete.cases(MAOne))
    MAOneToday <- MAOne[2:nrow(MAOne),]
    MA8Yesterday <- MAOne$MA8[1:(nrow(MAOne) - 1)]
    MA21Yesterday <- MAOne$MA21[1:(nrow(MAOne) - 1)]
    MA55Yesterday <- MAOne$MA55[1:(nrow(MAOne) - 1)]
    MAOneToday[, 'MA8Y'] <- MA8Yesterday
    MAOneToday[, 'MA21Y'] <- MA21Yesterday
    MAOneToday[, 'MA55Y'] <- MA55Yesterday
    MAOneToday %>% mutate(MA8D = (MA8 - MA8Y) / MA8Y, MA21D = (MA21 - MA21Y) / MA21Y, MA55D = (MA55 - MA55Y) / MA55Y)
}

GetMADD <- function(MAD) {
    MADToday <- MAD[2:(nrow(MAD)),]
    MA8DY <- MAD$MA8D[1:(nrow(MAD) - 1)]
    MA21DY <- MAD$MA21D[1:(nrow(MAD) - 1)]
    MA55DY <- MAD$MA55D[1:(nrow(MAD) - 1)]
    MADToday %>% mutate(MA8DY, MA21DY, MA55DY) %>% mutate(MA8DD = (MA8D - MA8DY), MA21DD = (MA21D - MA21DY), MA55DD = MA55D - MA55DY)
}

GetThreeK <- function(MAD) {
    ThreeKToday <- MAD[3:nrow(MAD),]
    HKY <- MAD$`最高價`[2:(nrow(MAD) - 1)]
    HKY2 <- MAD$`最高價`[1:(nrow(MAD) - 2)]
    LKY <- MAD$`最低價`[2:(nrow(MAD) - 1)]
    LKY2 <- MAD$`最低價`[1:(nrow(MAD) - 2)]
    ThreeKToday %>% mutate(HKY, HKY2, LKY, LKY2)
}
BullResult <- PriceAll_201001_201902 %>% filter(公司代號 != '3008' & 公司代號 != '911619' & 公司代號 != '911622' & 公司代號 != '1418' & 公司代號 != '1470' &
公司代號 != '910708' & 公司代號 != '910861' & 公司代號 != '911608' & 公司代號 != '912398') %>% mutate(MH = pmap(list(data, 公司代號, "Bull"), GetLastestLabel)) #%>% mutate(MH = as.character(MH))
BullResult <- BullResult %>% select(-data) %>% unnest %>% filter(Label == '1' & TradeAmount == '1') %>% unnest %>% arrange(desc(MA8D)) %>% .[1:40,]
BullResult$`公司代號`

BearResult <- PriceAll_201001_201901 %>% filter(公司代號 != '3008' & 公司代號 != '911619' & 公司代號 != '911622' & 公司代號 != '1418' & 公司代號 != '1470' &
公司代號 != '910708' & 公司代號 != '910861' & 公司代號 != '911608') %>% mutate(MH = pmap(list(data, 公司代號, "Bear"), GetLastestLabel)) #%>% mutate(MH = as.character(MH))
BearResult <- BearResult %>% select(-data) %>% unnest %>% filter(Label == '1' & TradeAmount == '1') %>% unnest %>% arrange(desc(MA8D)) %>% .[1:40,]
BearResult$`公司代號`



#Test
resulttt$`公司代號`
resulttt$MA8D
resulttt$MA21D
PriceAll_201001_201901$data[[1]] %>% arrange(desc(日期))


