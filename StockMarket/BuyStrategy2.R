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
        Year <- 2018:2018
        #Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
        Price <- tibble()
        for (year in Year) {
            if (year == 2018) { Month <- c('12') }
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
Price201812 <- readRDS(file = "PriceAll_201812.rds")
companylist <- companylist %>% filter(!(そ腹 %in% Price201812$そ腹))
Price201812 <- tibble()
for (i in 1:nrow(companylist)) {
    print(i)
    PriceOne <- companylist[i,] %>% mutate(price = map(そ腹, GetStockPriceByYear))
    #PriceAll <- rbind(PriceAll, PriceOne)
    Price201812 <- rbind(Price201812, PriceOne)
    saveRDS(Price201812, file = "Price201812.rds")
}
EmptyCompanyList <- Price201812 %>% mutate(nrowns = map(price, ~ nrow(.)) %>% as.numeric) %>% filter(nrowns == 0)
#companylist <- companylist %>% filter(そ腹 %in% EmptyCompanyList$そ腹)
Price201812 <- Price201812 %>% filter(!(そ腹 %in% EmptyCompanyList$そ腹))
Price201812_unnest <- Price201812 %>% unnest
PriceAll_201001_201811_unnest <- PriceAll_201001_201811 %>% unnest 
PriceAll_201001_201812 <- rbind(PriceAll_201001_201811_unnest, Price201812_unnest) %>% group_by(そ腹, そ嘿) %>% nest


#Strategy
GetLastestLabel <- function(Price, index,Type) {
    print(index)
    nrown <- 0
    if (nrow(Price) < 140) { nrown <- nrow(Price) } else { nrown <- 140 }
    Price <- Price[(nrow(Price) - nrown + 1):nrow(Price),] %>% filter(complete.cases(.))
    if (Type == 'Bull') {
        GetLabel <- function(BuySellPrice) {
            GetBuyLabel <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD) {
                if ((MA8 > MA21) & (MA21 > MA55) &
                    (MA8D > MA21D) & (MA21D > 0) & MA55DD > 0 & MA8DD > 0 #& (MA8 - MA21 > MA21 - MA55)
                    ) {
                    1
                } else {
                    0
                }
            }
            BuySellPrice %>% mutate(BuyLabel = pmap(list(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD), GetBuyLabel) %>% as.character)
        }
    } else {
        GetLabel <- function(BuySellPrice) {
            GetBuyLabel <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD) {
                if ((MA8 < MA21) & (MA21 < MA55) &
                    (MA8D < MA21D) & (MA21D < 0) & MA55DD < 0 & MA8DD < 0 #& (MA8 - MA21 > MA21 - MA55)
                    ) {
                    1
                } else {
                    0
                }
            }
            BuySellPrice %>% mutate(BuyLabel = pmap(list(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD), GetBuyLabel) %>% as.character)
        }
    }
    Result <- Price %>% arrange(ら戳) %>% AddMVandKD %>% GetMAD %>% GetMADD %>% GetThreeK %>% GetLabel %>% arrange(ら戳) %>% .[nrow(.),]
    if (Result$Μ絃基[1] > 50) { FT <- 0 } else { FT <- 1 }
    TradeAmount <- ifelse(Result$Θユ计 / 1000 > 500, 1, 0)
    Label <- (Result$BuyLabel[1] %>% as.numeric) * FT
    tibble(TradeAmount, Label)
}

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

GetMADD <- function(MAD) {
    MADToday <- MAD[2:(nrow(MAD)),]
    MA8DY <- MAD$MA8D[1:(nrow(MAD) - 1)]
    MA21DY <- MAD$MA21D[1:(nrow(MAD) - 1)]
    MA55DY <- MAD$MA55D[1:(nrow(MAD) - 1)]
    MADToday %>% mutate(MA8DY, MA21DY, MA55DY) %>% mutate(MA8DD = MA8D - MA8DY, MA21DD = MA21D - MA21DY, MA55DD = MA55D - MA55DY)
}

GetThreeK <- function(MAD) {
    ThreeKToday <- MAD[3:nrow(MAD),]
    HKY <- MAD$`程蔼基`[2:(nrow(MAD) - 1)]
    HKY2 <- MAD$`程蔼基`[1:(nrow(MAD) - 2)]
    LKY <- MAD$`程基`[2:(nrow(MAD) - 1)]
    LKY2 <- MAD$`程基`[1:(nrow(MAD) - 2)]
    ThreeKToday %>% mutate(HKY, HKY2, LKY, LKY2)
}
Result <- PriceAll_201001_201811 %>% filter(そ腹 != '3008' & そ腹 != '911619' & そ腹 != '911622' & そ腹 != '1418' & そ腹 != '1470' &
そ腹 != '910708' & そ腹 != '910861') %>% mutate(MH = pmap(list(data, そ腹,"Bull"), GetLastestLabel)) #%>% mutate(MH = as.character(MH))

resulttt<-Result %>% select(-data) %>% unnest %>% filter(Label == '1' & TradeAmount == '1') %>% unnest %>% top_n(wt=MA21D,10)
resulttt$`そ腹`
resulttt$MA21D
PriceAll_201001_201811$data[[1]] %>% arrange(desc(ら戳))





#Historical Data

GetAllLabel <- function(Price, index, Type) {
    print(index)
    nrown <- 0
    if (nrow(Price) < 140) { nrown <- nrow(Price) } else { nrown <- 140 }
    Price <- Price %>% filter(complete.cases(.))
    if (Type == 'Bull') {
        GetLabel <- function(BuySellPrice) {
            GetBuyLabel <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD) {
                if ((MA8 > MA21) & (MA21 > MA55) &
                    (MA8D > MA21D) & (MA21D > 0) & MA55D > 0 & MA8DD > 0 #& (MA8 - MA21 > MA21 - MA55)
                    ) {
                    1
                } else {
                    0
                }
            }
            BuySellPrice %>% mutate(BuyLabel = pmap(list(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD), GetBuyLabel) %>% as.character)
        }
    } else {
        GetLabel <- function(BuySellPrice) {
            GetBuyLabel <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD) {
                if ((MA8 < MA21) & (MA21 < MA55) &
                    (MA8D < MA21D) & (MA21D < 0) & MA55D < 0 & MA8DD < 0 #& (MA8 - MA21 > MA21 - MA55)
                    ) {
                    1
                } else {
                    0
                }
            }
            BuySellPrice %>% mutate(BuyLabel = pmap(list(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD), GetBuyLabel) %>% as.character)
        }
    }
    Result <- Price %>% arrange(ら戳) %>% AddMVandKD %>% GetMAD %>% GetMADD %>% GetThreeK %>% GetLabel %>% arrange(ら戳) #%>% .[nrow(.),]
    #if (Result$Μ絃基[1] > 50) { FT <- 0 } else { FT <- 1 }
    #TradeAmount <- ifelse(Result$Θユ计 / 1000 > 500, 1, 0)
    #Label <- (Result$BuyLabel[1] %>% as.numeric) * FT
    #tibble(TradeAmount, Label)
    Result
}
OneSample<-PriceAll_201001_201811$data[[492]] %>% GetAllLabel(., "", "Bull") 
buylabel <- OneSample$BuyLabel
buyprice <- OneSample$`Μ絃基`
tradeamount <- OneSample$`Θユ计`
NDay <- 5
NDayPrice<-3
GetLastNDayIndex <- function(buylabel,buyprice, NDay) {
    buylabel <- buylabel %>% as.numeric
    NotHighestNDayIndex<-c()
    LastNDayIndex <- c()
    TradeIndex<-c()
    for (i in (NDay+1):length(buylabel)) {
        if (sum(buylabel[(i - NDay):(i - 1)]) == 0) {
            LastNDayIndex[i]<-1
        } else {
            LastNDayIndex[i]<-0
        }
    }
    for (i in (NDayPrice + 1):length(buyprice)) {
        if (max(buyprice[(i - NDayPrice):(i - 1)]) > buyprice[i]) {
            NotHighestNDayIndex[i] <- 1
        } else {
            NotHighestNDayIndex[i] <- 0
        }
    }
    for (i in (NDayPrice + 1):length(tradeamount)) {
        if (max(tradeamount[(i - NDayPrice):(i - 1)]) < tradeamount[i]) {
            TradeIndex[i] <- 1
        } else {
            TradeIndex[i] <- 0
        }
    }
}

Price <- OneSample %>% mutate(LastNDayIndex, NotHighestNDayIndex, TradeIndex) %>% select(ら戳, Μ絃基, Θユ计, BuyLabel, LastNDayIndex, NotHighestNDayIndex, TradeIndex) %>% filter(complete.cases(.))

GetNextSellPoint <- function(Price) {
    print(index)
    ProfitVariable <- 2
    #Price <- MAOne %>% select(-程基, - 秨絃基) %>% mutate(buy = ifelse(MA5 >= MA10 & MA5D >= MA5DPara & MA510D >= MA510DPara, 1, 0)) %>% arrange(ら戳)
    #Price <- MAOne %>% select(-程基, - 秨絃基) %>% mutate(buy = ifelse(MA5 >= MA10 & MA5D >= MA5DPara, 1, 0)) %>% arrange(ら戳)
    Price <- Price %>% arrange(ら戳)
    BuyPoint <- Price %>% filter(BuyLabel == 1 )
    BuyAll <- tibble()
    if (nrow(Price) > 0) {
        for (i in 1:nrow(BuyPoint)) {
            BuyOne <- BuyPoint[i,]
            BuyPrice <- BuyOne$`Μ絃基`[1]
            BuyDate <- BuyOne$`ら戳`[1]
            SellPoint <- Price %>% filter(ら戳 > BuyDate) %>% arrange(ら戳) %>% .[2:nrow(.),] %>% mutate(SellPoint = ifelse(((Μ絃基 - BuyPrice) / Μ絃基) > ((ProfitVariable + 0.1425 * 2 + 0.3) / 100), 1, 0)) %>% filter(SellPoint == 1) %>% arrange(ら戳) %>% .[1,]
            SellPrice <- SellPoint$`Μ絃基`[1]
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


BuyAll %>% filter(complete.cases(.)) %>% DealWithDate %>% View

BuyAll %>% filter(complete.cases(.)) %>% DealWithDate %>% group_by(LastNDayIndex, NotHighestNDayIndex, TradeIndex) %>% summarise(mDay = mean(TimeLength))
BuyAll %>% filter(complete.cases(.)) %>% DealWithDate %>% filter(LastNDayIndex==0 & NotHighestNDayIndex==1 & TradeIndex==1)

Price %>% View