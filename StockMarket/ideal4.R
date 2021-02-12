library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)

Sys.setlocale("LC_ALL", "cht")
PriceAll_201001_201808 <- readRDS(file = "PriceAll_201001_201808.rds")
PriceAll_201001_201811 <- readRDS(file = "PriceAll_201001_201811.rds")
Stock2485 <- PriceAll_201001_201808 %>% filter(そ腹 == 2485)

OnePrice<-Stock2485$price[[1]]
OnePrice %>% arrange(desc(ら戳))


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
MVKD<-OnePrice %>% AddMVandKD
GetMAD <- function(MAOne) {
    MAOne <- MAOne %>% filter(complete.cases(MAOne))
    MAOneToday <- MAOne[2:nrow(MAOne),]
    MA8Yesterday <- MAOne$MA8[1:(nrow(MAOne) - 1)]
    MA21Yesterday <- MAOne$MA21[1:(nrow(MAOne) - 1)]
    MA55Yesterday <- MAOne$MA55[1:(nrow(MAOne) - 1)]
    MAOneToday[, 'MA8Y'] <- MA8Yesterday
    MAOneToday[, 'MA21Y'] <- MA21Yesterday
    MAOneToday[, 'MA55Y'] <- MA55Yesterday
    MAOneToday %>% mutate(MA8D = MA8-MA8Y,MA21D=MA21-MA21Y,MA55D=MA55-MA55Y)
}
MAD <- MVKD %>% GetMAD
GetMADD <- function(MAD) {
    MADToday <- MAD[2:(nrow(MAD)),]
    MA8DY <- MAD$MA8D[1:(nrow(MAD) - 1)]
    MA21DY <- MAD$MA21D[1:(nrow(MAD) - 1)]
    MA55DY <- MAD$MA55D[1:(nrow(MAD) - 1)]
    MADToday %>% mutate(MA8DY, MA21DY, MA55DY) %>% mutate(MA8DD = MA8D - MA8DY, MA21DD = MA21D - MA21DY, MA55DD = MA55D- MA55DY)
}
MADD <- MAD %>% GetMADD

GetThreeK <- function(MAD) {
    ThreeKToday <- MAD[3:nrow(MAD),]
    HKY <- MAD$`程蔼基`[2:(nrow(MAD)-1)]
    HKY2 <- MAD$`程蔼基`[1:(nrow(MAD) - 2)]
    LKY <- MAD$`程基`[2:(nrow(MAD) - 1)]
    LKY2 <- MAD$`程基`[1:(nrow(MAD) - 2)]
    ThreeKToday %>% mutate(HKY, HKY2, LKY, LKY2)
}
ThreeK <- MADD %>% GetThreeK
GetBuySellPrice <- function(MAD) {
    PriceToday <- MAD[1:(nrow(MAD) - 1),]
    BuyPrice <- MAD$`秨絃基`[2:(nrow(MAD))]
    SellPrice <- MAD$`秨絃基`[2:(nrow(MAD))]
    PriceToday %>% mutate(BuyPrice, SellPrice)
}
BuySellPrice <- ThreeK %>% GetBuySellPrice

GetLabel <- function(BuySellPrice,MA55Parameter) {
    GetBuyLabel <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD) {
        if ((MA8 > MA21) & (MA21 > MA55) &
            (MA8D > MA21D) & (MA21D > MA55D) & MA55D > MA55Parameter & MA55 > 0 &
            程蔼基 > HKY & 程蔼基 > HKY2# &
           # MA8DD>0 &MA21DD>0 & MA55DD>0 
        ) {
            1
        } else {
            0
        }
    }
    GetSellLabel <- function(MA8D,MA21D,MA55D,程基,LKY,LKY2,MA8DD,MA21DD,MA55DD) {
        if (  MA21DD <= 0 & MA8DD <= 0 & (程基 < LKY & 程基 < LKY2)) { #MA8D <=0 & & (程基 < LKY & 程基 < LKY2) 
            1
        } else {
            0
        }
    }
    BuySellPrice %>% mutate(BuyLabel = pmap(list(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD), GetBuyLabel) %>% as.character, SellLabel = pmap(list(MA8D, MA21D, MA55D, 程基, LKY, LKY2, MA8DD, MA21DD, MA55DD), GetSellLabel) %>% as.character)
}
Label<-BuySellPrice %>% GetLabel(.,0) 
#Label %>% select(ら戳, BuyLabel, BuyPrice, SellLabel, SellPrice, MA8, MA21, MA55,MA8D,MA21D,MA55D,Μ絃基,程蔼基,程基,秨絃基) %>% View

GetProfit <- function(Label) {
    Price<-Label
    BuyPoint <- Label %>% filter(BuyLabel == 1 ) %>% select(-SellLabel, -SellPrice)
    BuyAll <- tibble()
    if (nrow(Price) > 0) {
        for (i in 1:nrow(BuyPoint)) {
            BuyOne <- BuyPoint[i,]
            BuyPrice <- BuyOne$BuyPrice[1]
            BuyDate <- BuyOne$`ら戳`[1]
            SellPoint <- Price %>% filter(ら戳 > BuyDate) %>% filter(SellLabel == 1) %>% arrange(ら戳) %>% .[1,]
            SellPrice <- SellPoint$SellPrice[1]
            SellDate <- SellPoint$`ら戳`[1]
            BuyOne <- BuyOne %>% mutate(SellDate, SellPrice)
            BuyAll <- rbind(BuyAll, BuyOne)
        }
    }
    BuyAll
}
Profit <- GetProfit(Label)
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
Profit <- Profit %>% DealWithDate
GetRealProfit <- function(Profit) {
    Profit <- Profit %>% mutate(BuyDate = as.character(BuyDate), SellDate = as.character(SellDate))
    RealProfit <- Profit %>% arrange(BuyDate) %>% .[1,]
    firstBuyPoint <- Profit %>% arrange(BuyDate) %>% .[1,]
    firstBuyDate <- firstBuyPoint$BuyDate[1] 
    selldate <- firstBuyPoint$SellDate[1] 
    repeat {
        OneRealProfit <- Profit %>% filter(BuyDate > selldate)
        if (nrow(OneRealProfit) == 0) {
            break
        }
        OneRealProfit <- OneRealProfit %>% arrange(ら戳) %>% .[1,]
        selldate <- OneRealProfit$SellDate[1]
        RealProfit <- rbind(RealProfit, OneRealProfit)
    }
    RealProfit
}
RealProfit <- Profit %>% GetRealProfit

RealProfit %>% mutate(Profit = (SellPrice - BuyPrice) / BuyPrice) %>% .$Profit %>% sum
RealProfit %>% mutate(Profit = (SellPrice - BuyPrice) / BuyPrice) %>% View
Profit %>% mutate(Profit = (SellPrice - BuyPrice) / BuyPrice) %>% .$Profit %>% sum
Profit %>% mutate(Profit = (SellPrice - BuyPrice) / BuyPrice) %>% filter(Profit > 0) %>% .$MA8D %>% summary
Profit %>% mutate(Profit = (SellPrice - BuyPrice) / BuyPrice) %>% filter(Profit < 0) %>% .$MA8D %>% summary

v <- c("Hello", "loop")
cnt <- 2

repeat {
    print(v)
    cnt <- cnt + 1

    if (cnt > 5) {
        break
    }
}

MAOne<-Label
GetNextSellPoint <- function(MAOne) {
    #print(index)
    ProfitVariable <- 1
    #Price <- MAOne %>% select(-程基, - 秨絃基) %>% mutate(buy = ifelse(MA5 >= MA10 & MA5D >= MA5DPara & MA510D >= MA510DPara, 1, 0)) %>% arrange(ら戳)
    #Price <- MAOne %>% select(-程基, - 秨絃基) %>% mutate(buy = ifelse(MA5 >= MA10 & MA5D >= MA5DPara, 1, 0)) %>% arrange(ら戳)
    Price <- MAOne  %>% arrange(ら戳)
    BuyPoint <- Price %>% filter(BuyLabel == 1) %>% select(-SellPrice)
    BuyAll <- tibble()
    if (nrow(Price) > 0) {
        for (i in 1:nrow(BuyPoint)) {
            BuyOne <- BuyPoint[i,]
            buyprice <- BuyOne$BuyPrice[1]
            BuyDate <- BuyOne$`ら戳`[1]
            SellPoint <- Price %>% filter(ら戳 > BuyDate) %>% arrange(ら戳) %>% .[2:nrow(.),] %>% mutate(SellPoint = ifelse(((SellPrice - buyprice) / SellPrice) > ((ProfitVariable + 0.1425 * 2 + 0.1) / 100), 1, 0)) %>% filter(SellPoint==1) %>% arrange(ら戳) %>% .[1,]
            SellPrice <- SellPoint$SellPrice[1]
            SellDate <- SellPoint$`ら戳`[1]
            BuyOne <- BuyOne %>% mutate(SellDate, SellPrice)
            BuyAll <- rbind(BuyAll, BuyOne)
        }
    }
    BuyAll
}
Result<- MAOne %>% GetNextSellPoint
Result %>% DealWithDate %>% GetRealProfit 


#Strategy 

GetLastestLabel <- function(Price, index) {
    print(index)
    nrown <- 0
    if (nrow(Price) < 140) { nrown <- nrow(Price) } else { nrown <- 140 }
    Price <- Price[(nrow(Price) - nrown + 1):nrow(Price),] %>% filter(complete.cases(.))
    GetLabel <- function(BuySellPrice) {
        GetBuyLabel <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD) {
            if ((MA8 > MA21) & (MA21 > MA55) &
                (MA8D > MA21D) & (MA21D > 0) & MA55D > 0 & MA55 > 0 & MA8DD > 0 #& (MA8 - MA21 > MA21 - MA55)
                ) {
                1
            } else {
                0
            }
        }

        BuySellPrice %>% mutate(BuyLabel = pmap(list(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD), GetBuyLabel) %>% as.character)
    }
    Result <- Price %>% arrange(ら戳) %>% AddMVandKD %>% GetMAD %>% GetMADD %>% GetThreeK %>% GetLabel %>% arrange(ら戳) %>% .[nrow(.),]
    if (Result$Μ絃基[1] > 50) { FT <- 0 } else { FT <- 1 }
    TradeAmount <- ifelse(Result$Θユ计 /1000 >500,1,0)
    Label <- (Result$BuyLabel[1] %>% as.numeric) * FT
    tibble(TradeAmount,Label)
}

Result <- PriceAll_201001_201811 %>% filter(そ腹 != '3008' & そ腹 != '911619' & そ腹 != '911622' & そ腹 != '1418' & そ腹 != '1470' &
そ腹 != '910708' & そ腹 != '910861') %>% mutate(MH = map2(data, そ腹, GetLastestLabel)) #%>% mutate(MH = as.character(MH))

Result %>% select(-data) %>% unnest %>% filter(Label == '1' &TradeAmount=='1' ) %>% .$そ腹

PriceAll_201001_201811$data[[1]]