library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)

Sys.setlocale("LC_ALL", "cht")
#HistoryData

GetPriceHistory <- function() {
    PriceAll_201001_201811 <- readRDS(file = "PriceAll_201001_201811.rds")
    Price201812 <- readRDS(file = "Price201812.rds")
    Price201901 <- readRDS(file = "Price201901.rds")

    Price201812_unnest <- Price201812 %>% unnest
    Price201901_unnest <- Price201901 %>% unnest
    PriceAll_201001_201811_unnest <- PriceAll_201001_201811 %>% unnest

    rbind(PriceAll_201001_201811_unnest, Price201812_unnest, Price201901_unnest) %>% group_by(そ腹, そ嘿) %>% nest

}
PriceHistory <- GetPriceHistory()
DAYList <- PriceHistory$data[[1]] %>% arrange(desc(ら戳)) %>% select(ら戳)
#BuyingList
GetBuyingList <- function(DAY) {
    GetLastestLabel <- function(Price, index, Type) {
        Price <- Price %>% filter(ら戳 <= DAY) %>% filter(!is.na(秨絃基))
        print(index)
        nrown <- 0
        if (nrow(Price) < 140) { nrown <- nrow(Price) } else { nrown <- 140 }
        if (nrow(Price) >= 59) {
            #nrown<-56
            Price <- Price[(nrow(Price) - nrown + 1):nrow(Price),] %>% filter(complete.cases(.))
            if (Type == 'Bull') {
                GetLabel <- function(BuySellPrice) {
                    GetBuyLabel <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, 程蔼基, HKY, HKY2, MA8DD, MA21DD, MA55DD) {
                        if (
                            (MA8D > MA21D) & (MA21D > 0) & MA55D > 0 & MA8DD > 0 & MA21DD > 0 & MA55DD > 0 #& (MA8 - MA21 > MA21 - MA55)
                            #(MA8D > MA21D) & (MA21D > 0)  & MA8DD > 0 & MA21DD > 0 & MA55DD > 0
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
            Result <- Price %>% arrange(ら戳) %>% AddMVandKD %>% GetMAD %>% GetMADD %>% GetThreeK %>% filter(!is.na(Μ絃基)) %>% GetLabel %>% arrange(ら戳) %>% .[nrow(.),]
            if (Result$Μ絃基[1] > 50) { FT <- 0 } else { FT <- 1 }
            TradeAmount <- ifelse(Result$Θユ计 / 1000 > 500, 1, 0)
            Label <- (Result$BuyLabel[1] %>% as.numeric) * FT
            tibble(list(Result), Label, TradeAmount)
        } else { tibble() }

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
        HKY <- MAD$`程蔼基`[2:(nrow(MAD) - 1)]
        HKY2 <- MAD$`程蔼基`[1:(nrow(MAD) - 2)]
        LKY <- MAD$`程基`[2:(nrow(MAD) - 1)]
        LKY2 <- MAD$`程基`[1:(nrow(MAD) - 2)]
        ThreeKToday %>% mutate(HKY, HKY2, LKY, LKY2)
    }
    BullResult <- PriceHistory %>% filter(そ腹 != '3008' & そ腹 != '911619' & そ腹 != '911622' & そ腹 != '1418' & そ腹 != '1470' &
    そ腹 != '910708' & そ腹 != '910861' & そ腹 != '911608') %>% mutate(MH = pmap(list(data, そ腹, "Bull"), GetLastestLabel)) #%>% mutate(MH = as.character(MH))
    BullResult <- BullResult %>% select(-data) %>% unnest %>% filter(Label == '1' & TradeAmount == '1') %>% unnest %>% arrange(desc(MA8D)) %>% .[1:5,]
    BullResult <- BullResult %>% filter(!is.na(そ腹))
    if (nrow(BullResult) > 0) {
        BullResult %>% select(そ腹, ら戳, Μ絃基) %>% filter(ら戳 == DAY)
    } else { tibble()}
}

#Initialize Trade History
DAY <- '107/01/02'
InitialBuyingList <- GetBuyingList(DAY)
TradeHistory <- tibble(Date = DAY, Cash = 500, Stock = list(tibble(そ腹 = '', ら戳 = '', Μ絃基 = 1.1, PriceCriterion = 1.1, BoughtPrice = 1.1, BoughtDay = '')[0,]), BuyingList = list(InitialBuyingList), SellingList = list(tibble()), BoughtList = list(tibble()), SoldList = list(tibble()),StockValue=0)



#Main

BuyingList <- TradeHistory$BuyingList[[nrow(TradeHistory)]]
Date <- DAYList %>% filter(ら戳 > TradeHistory$Date[[nrow(TradeHistory)]]) %>% arrange(ら戳) %>% .$ら戳 %>% .[1]
CashYesterday <- TradeHistory$Cash[[nrow(TradeHistory)]]
StockYesterday <- TradeHistory$Stock[[nrow(TradeHistory)]]
SellingList <- TradeHistory$SellingList[[nrow(TradeHistory)]]

BuyingListToday <- GetBuyingList(Date)

GetBoughtList <- function(CashYesterday, BuyingList) {
    if (nrow(BuyingList) > 0) {
        BuyingList %<>% mutate(PriceCriterion = cumsum(Μ絃基)) %>% filter(PriceCriterion < CashYesterday)
        GetTodayPrice <- function(Stock) {
            StockPrice <- PriceHistory %>% filter(そ腹 == Stock) %>% unnest %>% filter(ら戳 == Date)
            StockPrice$程基[1]
        }
        BuyingList %>% mutate(PriceToday = map(そ腹, GetTodayPrice) %>% as.numeric) %>% filter(PriceToday <= Μ絃基) %>% mutate(BoughtDay = Date) %>% rename(BoughtPrice = PriceToday) %>% mutate(BoughtPrice = Μ絃基)
    } else {
        tibble()
    }
}
BoughtList <- GetBoughtList(CashYesterday, BuyingList)                      



GetSoldList <- function(SellingList) {
    if (nrow(SellingList) > 0) {
        GetPrice <- function(Stock) {
            StockPrice <- PriceHistory %>% filter(そ腹 == Stock) %>% unnest %>% filter(ら戳 == Date)
            TodayPrice <- StockPrice$`程蔼基`[1]
            YesterdayPrice <- PriceHistory %>% filter(そ腹 == Stock) %>% unnest %>% filter(ら戳 < Date) %>% arrange(desc(ら戳)) %>% .$Μ絃基 %>% .[1]
            tibble(YesterdayPrice, TodayPrice)
        }
        SellingList %>% mutate(PriceToday = map(そ腹, GetPrice)) %>% select(-data) %>% unnest %>% filter(TodayPrice > YesterdayPrice) %>% mutate(SoldDay = Date) %>% rename(SoldPrice = YesterdayPrice)
    } else {
        tibble()
    }
}

SoldList <- GetSoldList(SellingList)


GetStock <- function(StockYesterday, BoughtList, SoldList) {
    if (nrow(SoldList)>0) {
        StockToday <- StockYesterday %>% left_join(SoldList %>% select(そ腹, ら戳) %>% mutate(SoldIndex = 1)) %>% filter(is.na(SoldIndex)) %>% select(-SoldIndex)
    } else {
        StockToday <- StockYesterday
    }
    StockToday <- rbind(StockToday, BoughtList)
    StockToday
}
StockListToday <- GetStock(StockYesterday, BoughtList, SoldList)
GetCash <- function(CashYesterday, BoughtList, SoldList) {
    SoldRevenue<-0
    BoughtCost <- BoughtList$BoughtPrice %>% sum
    if (nrow(SoldList) > 0) {
        SoldRevenue <- SoldList$SoldPrice %>% sum
    }
    CashYesterday - BoughtCost + SoldRevenue
}
CashToday <- GetCash(CashYesterday, BoughtList, SoldList)
GetStockValue <- function(StockListToday) {
    GetPresentValue <- function(data) {
        data %>% filter(ら戳 == Date) %>% .$Μ絃基 %>% .[1]
    }
    StockValueList <- StockListToday %>% left_join(PriceHistory) %>% mutate(PresentValue = map(data, GetPresentValue) %>% as.numeric)
    StockValueList$PresentValue %>% sum
}
StockValueToday <- GetStockValue(StockListToday)
GetSellingList <- function(StockYesterday) {
    if (nrow(StockYesterday) > 0) {
        SellingList <- StockYesterday %>% left_join(PriceHistory)
        GetSellingList <- function(BoughtPrice, data) {
            GetLossControlCriterion <- function() {
                ClosePriceToday <- data %>% filter(ら戳 == Date) %>% .$Μ絃基 %>% .[1]
                ProfitRatio <- (ClosePriceToday - BoughtPrice) / BoughtPrice
                ifelse(ProfitRatio > 0.1 | ProfitRatio < -0.05, 1, 0)
            }
            LossControlCriterion <- GetLossControlCriterion()
            GetThreeKCriterion <- function() {
                ThreeKPrice <- data %>% filter(ら戳 <= Date) %>% arrange(desc(ら戳)) %>% .[1:3,] %>% select(程基)
                ifelse(ThreeKPrice$`程基`[1] < ThreeKPrice$`程基`[2] & ThreeKPrice$`程基`[1] < ThreeKPrice$`程基`[3], 1, 0)
            }
            ThreeKCriterion <- GetLossControlCriterion()
            ifelse((LossControlCriterion + ThreeKCriterion) > 0, 1, 0)
        }
        SellingList %>% mutate(SellingSignal = map2(BoughtPrice, data, GetSellingList) %>% as.numeric) %>% filter(SellingSignal == 1)
    } else {
        tibble()
    }

}
SellingListToday <- GetSellingList(StockListToday)



TradeHistoryOne <- tibble(Date, Cash = CashToday, Stock = list(StockListToday), BuyingList = list(BuyingListToday), SellingList = list(SellingListToday), BoughtList = list(BoughtList), SoldList = list(SoldList), StockValue = StockValueToday)
rbind(TradeHistory, TradeHistoryOne) %>% arrange(Date)

TradeHistory <- rbind(TradeHistory, TradeHistoryOne) %>% arrange(Date)

#GetTradeHistory
GetTradeHistory <- function(TradeHistory) {
    BuyingList <- TradeHistory$BuyingList[[nrow(TradeHistory)]]
    Date <- DAYList %>% filter(ら戳 > TradeHistory$Date[[nrow(TradeHistory)]]) %>% arrange(ら戳) %>% .$ら戳 %>% .[1]
    CashYesterday <- TradeHistory$Cash[[nrow(TradeHistory)]]
    StockYesterday <- TradeHistory$Stock[[nrow(TradeHistory)]]
    SellingList <- TradeHistory$SellingList[[nrow(TradeHistory)]]

    BuyingListToday <- GetBuyingList(Date)

    GetBoughtList <- function(CashYesterday, BuyingList) {
        if (nrow(BuyingList) > 0) {
            BuyingList %<>% mutate(PriceCriterion = cumsum(Μ絃基)) %>% filter(PriceCriterion < CashYesterday)
            GetTodayPrice <- function(Stock) {
                StockPrice <- PriceHistory %>% filter(そ腹 == Stock) %>% unnest %>% filter(ら戳 == Date)
                StockPrice$程基[1]
            }
            BuyingList %>% mutate(PriceToday = map(そ腹, GetTodayPrice) %>% as.numeric) %>% filter(PriceToday <= Μ絃基) %>% mutate(BoughtDay = Date) %>% rename(BoughtPrice = PriceToday) %>% mutate(BoughtPrice = Μ絃基)
        } else {
            tibble()
        }
    }
    BoughtList <- GetBoughtList(CashYesterday, BuyingList)

    GetSellingList <- function(StockYesterday) {
        if (nrow(StockYesterday) > 0) {
            SellingList <- StockYesterday %>% left_join(PriceHistory)
            GetSellingList <- function(BoughtPrice, data) {
                GetLossControlCriterion <- function() {
                    ClosePriceToday <- data %>% filter(ら戳 == Date) %>% .$Μ絃基 %>% .[1]
                    ProfitRatio <- (ClosePriceToday - BoughtPrice) / BoughtPrice
                    ifelse(ProfitRatio > 0.1 | ProfitRatio < -0.05, 1, 0)
                }
                LossControlCriterion <- GetLossControlCriterion()
                GetThreeKCriterion <- function() {
                    ThreeKPrice <- data %>% filter(ら戳 <= Date) %>% arrange(desc(ら戳)) %>% .[1:3,] %>% select(程基)
                    ifelse(ThreeKPrice$`程基`[1] < ThreeKPrice$`程基`[2] & ThreeKPrice$`程基`[1] < ThreeKPrice$`程基`[3], 1, 0)
                }
                ThreeKCriterion <- GetLossControlCriterion()
                ifelse((LossControlCriterion + ThreeKCriterion) > 0, 1, 0)
            }
            SellingList %>% mutate(SellingSignal = map2(BoughtPrice, data, GetSellingList) %>% as.numeric) %>% filter(SellingSignal == 1)
        } else {
            tibble()
        }

    }
    SellingListToday <- GetSellingList(StockYesterday)

    GetSoldList <- function(SellingList) {
        if (nrow(SellingList) > 0) {
            GetPrice <- function(Stock) {
                StockPrice <- PriceHistory %>% filter(そ腹 == Stock) %>% unnest %>% filter(ら戳 == Date)
                TodayPrice <- StockPrice$`程蔼基`[1]
                YesterdayPrice <- PriceHistory %>% filter(そ腹 == Stock) %>% unnest %>% filter(ら戳 < Date) %>% arrange(desc(ら戳)) %>% .$Μ絃基 %>% .[1]
                tibble(YesterdayPrice, TodayPrice)
            }
            SellingList %>% mutate(PriceToday = map(そ腹, GetPrice)) %>% select(-data) %>% unnest %>% filter(TodayPrice > YesterdayPrice) %>% mutate(SoldDay = Date) %>% rename(SoldPrice = YesterdayPrice)
        } else {
            tibble()
        }
    }

    SoldList <- GetSoldList(SellingList)


    GetStock <- function(StockYesterday, BoughtList, SoldList) {
        StockToday <- rbind(StockYesterday, BoughtList)
        if (nrow(SoldList) > 0) {
            StockToday <- StockToday %>% filter(!(そ腹 %in% SoldList$`そ腹`))
        }
        StockToday
    }
    StockListToday <- GetStock(StockYesterday, BoughtList, SoldList)
    GetCash <- function(CashYesterday, BoughtList, SoldList) {
        SoldRevenue <- 0
        BoughtCost <- BoughtList$BoughtPrice %>% sum
        if (nrow(SoldList) > 0) {
            SoldRevenue <- SoldList$SoldPrice %>% sum
        }
        CashYesterday - BoughtCost + SoldRevenue
    }
    CashToday <- GetCash(CashYesterday, BoughtList, SoldList)
    GetStockValue <- function(StockListToday) {
        GetPresentValue <- function(data) {
            data %>% filter(ら戳 == Date) %>% .$Μ絃基
        }
        StockValueList <- StockListToday %>% left_join(PriceHistory) %>% mutate(PresentValue = map(data, GetPresentValue) %>% as.numeric)
        StockValueList$PresentValue %>% sum
    }
    StockValueToday <- GetStockValue(StockListToday)

    TradeHistoryOne <- tibble(Date, Cash = CashToday, Stock = list(StockListToday), BuyingList = list(BuyingListToday), SellingList = list(SellingListToday), BoughtList = list(BoughtList), SoldList = list(SoldList), StockValue = StockValueToday)
    rbind(TradeHistory, TradeHistoryOne) %>% arrange(Date)
}
GetTradeHistory(TradeHistory)
GetTradeHistory2 <- function(TradeHistory) {
    BuyingList <- TradeHistory$BuyingList[[nrow(TradeHistory)]]
    Date <- DAYList %>% filter(ら戳 > TradeHistory$Date[[nrow(TradeHistory)]]) %>% arrange(ら戳) %>% .$ら戳 %>% .[1]
    CashYesterday <- TradeHistory$Cash[[nrow(TradeHistory)]]
    StockYesterday <- TradeHistory$Stock[[nrow(TradeHistory)]]
    SellingList <- TradeHistory$SellingList[[nrow(TradeHistory)]]

    BuyingListToday <- GetBuyingList(Date)

    GetBoughtList <- function(CashYesterday, BuyingList) {
        if (nrow(BuyingList) > 0) {
            BuyingList %<>% mutate(PriceCriterion = cumsum(Μ絃基)) %>% filter(PriceCriterion < CashYesterday)
            GetTodayPrice <- function(Stock) {
                StockPrice <- PriceHistory %>% filter(そ腹 == Stock) %>% unnest %>% filter(ら戳 == Date)
                StockPrice$程基[1]
            }
            BuyingList %>% mutate(PriceToday = map(そ腹, GetTodayPrice) %>% as.numeric) %>% filter(PriceToday <= Μ絃基) %>% mutate(BoughtDay = Date) %>% rename(BoughtPrice = PriceToday) %>% mutate(BoughtPrice = Μ絃基)
        } else {
            tibble()
        }
    }
    BoughtList <- GetBoughtList(CashYesterday, BuyingList)

    GetSoldList <- function(SellingList) {
        if (nrow(SellingList) > 0) {
            GetPrice <- function(Stock) {
                StockPrice <- PriceHistory %>% filter(そ腹 == Stock) %>% unnest %>% filter(ら戳 == Date)
                TodayPrice <- StockPrice$`程蔼基`[1]
                YesterdayPrice <- PriceHistory %>% filter(そ腹 == Stock) %>% unnest %>% filter(ら戳 < Date) %>% arrange(desc(ら戳)) %>% .$Μ絃基 %>% .[1]
                tibble(YesterdayPrice, TodayPrice)
            }
            SellingList %>% mutate(PriceToday = map(そ腹, GetPrice)) %>% select(-data) %>% unnest %>% filter(TodayPrice > YesterdayPrice) %>% mutate(SoldDay = Date) %>% rename(SoldPrice = YesterdayPrice)
        } else {
            tibble()
        }
    }
    SoldList <- GetSoldList(SellingList)

    GetStock <- function(StockYesterday, BoughtList, SoldList) {
        if (nrow(SoldList) > 0) {
            StockToday <- StockYesterday %>% left_join(SoldList %>% select(そ腹, ら戳) %>% mutate(SoldIndex = 1)) %>% filter(is.na(SoldIndex)) %>% select(-SoldIndex)
        } else {
            StockToday <- StockYesterday
        }
        StockToday <- rbind(StockToday, BoughtList)
        StockToday
    }
    StockListToday <- GetStock(StockYesterday, BoughtList, SoldList)
    GetCash <- function(CashYesterday, BoughtList, SoldList) {
        SoldRevenue <- 0
        BoughtCost <- BoughtList$BoughtPrice %>% sum
        if (nrow(SoldList) > 0) {
            SoldRevenue <- SoldList$SoldPrice %>% sum
        }
        CashYesterday - BoughtCost + SoldRevenue
    }
    CashToday <- GetCash(CashYesterday, BoughtList, SoldList)
    GetStockValue <- function(StockListToday) {
        GetPresentValue <- function(data) {
            data %>% filter(ら戳 == Date) %>% .$Μ絃基 %>% .[1]
        }
        StockValueList <- StockListToday %>% left_join(PriceHistory) %>% mutate(PresentValue = map(data, GetPresentValue) %>% as.numeric)
        StockValueList$PresentValue %>% sum
    }
    StockValueToday <- GetStockValue(StockListToday)
    GetSellingList <- function(StockYesterday) {
        if (nrow(StockYesterday) > 0) {
            SellingList <- StockYesterday %>% left_join(PriceHistory)
            GetSellingList <- function(BoughtPrice, data) {
                GetLossControlCriterion <- function() {
                    ClosePriceToday <- data %>% filter(ら戳 == Date) %>% .$Μ絃基 %>% .[1]
                    ProfitRatio <- (ClosePriceToday - BoughtPrice) / BoughtPrice
                    ifelse(ProfitRatio > 0.01 | ProfitRatio < -0.03, 1, 0)
                }
                LossControlCriterion <- GetLossControlCriterion()
                GetThreeKCriterion <- function() {
                    ThreeKPrice <- data %>% filter(ら戳 <= Date) %>% arrange(desc(ら戳)) %>% .[1:3,] %>% select(程基)
                    ifelse(ThreeKPrice$`程基`[1] < ThreeKPrice$`程基`[2] & ThreeKPrice$`程基`[1] < ThreeKPrice$`程基`[3], 1, 0)
                }
                #ThreeKCriterion <- GetLossControlCriterion()
                ThreeKCriterion <-0
                ifelse((LossControlCriterion + ThreeKCriterion) > 0, 1, 0)
            }
            SellingList %>% mutate(SellingSignal = map2(BoughtPrice, data, GetSellingList) %>% as.numeric) %>% filter(SellingSignal == 1)
        } else {
            tibble()
        }

    }
    SellingListToday <- GetSellingList(StockListToday)



    TradeHistoryOne <- tibble(Date, Cash = CashToday, Stock = list(StockListToday), BuyingList = list(BuyingListToday), SellingList = list(SellingListToday), BoughtList = list(BoughtList), SoldList = list(SoldList), StockValue = StockValueToday)
    rbind(TradeHistory, TradeHistoryOne) %>% arrange(Date)
}


for (i in 1:200) {
    TradeHistory <- GetTradeHistory2(TradeHistory)
}

TradeHistory %>% View
TradeHistory %>% arrange(desc(Date))