library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)
Sys.setlocale("LC_ALL", "cht")
DealWithDate <- function(Price) {
    Price %>% mutate(BuyDate = str_split(ら戳, "/")) %>%
        mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
           mutate(Date = ymd(paste0(year, month, day))) %>%
           select(-year, - month, - day, - ら戳, - BuyDate)
}

Price00631L <- readRDS(file = "Price00631L.rdata")
Price00632R <- readRDS(file = "Price00632R.rdata")
Price0050 <- readRDS(file = "Price0050.rdata")

Price00631L %<>% DealWithDate
Price00632R %<>% DealWithDate
Price0050 %<>% DealWithDate


Price00631L <- Price00631L %>% arrange(Date)
Price00632R <- Price00632R %>% arrange(Date)
Price0050 <- Price0050 %>% arrange(Date)

Price <- Price00631L
Price <- Price00632R
Price <- Price0050

n<-9
GetKD <- function(Price,n) {
    GetPL1_N <- function(Price) {
        for (i in n:nrow(Price)) {
            PL1_N <- Price$`程基`[(i - n + 1):i] %>% min
            Price[i, 'PL1_N'] <- PL1_N
        }
        Price
    }
    GetPH1_N <- function(Price) {
        for (i in n:nrow(Price)) {
            PH1_N <- Price$`程蔼基`[(i - n + 1):i] %>% max
            Price[i, 'PH1_N'] <- PH1_N
        }
        Price
    }
    Price_HL <- Price %>% GetPL1_N %>% GetPH1_N
    Price_HL_RSV <- Price_HL %>% mutate(RSV = (Μ絃基 - PL1_N) / (PH1_N - PL1_N) * 100)
    GetKD <- function(Price_HL_RSV) {
        Price_HL_RSV[n - 1, 'K'] <- 50
        Price_HL_RSV[n - 1, 'D'] <- 50
        for (i in n:nrow(Price_HL_RSV)) {
            KN_1 <- Price_HL_RSV$K[i - 1]
            DN_1 <- Price_HL_RSV$D[i - 1]
            RSVN <- Price_HL_RSV$RSV[i]
            Price_HL_RSV[i, 'K'] <- RSVN / 3 + 2 / 3 * KN_1
            KN <- Price_HL_RSV$K[i]
            Price_HL_RSV[i, 'D'] <- KN / 3 + 2 / 3 * DN_1
        }
        Price_HL_RSV
    }
    Price_HL_RSV %>% GetKD 
}

PriceKD <- Price %>% GetKD(., 9)


PriceKD <- PriceKD %>% mutate(MA60 = SMA(Μ絃基, n = 60))
PriceKD <- PriceKD %>% filter(complete.cases(.))
GetMarketStatus <- function(PriceKD) {
    PriceKD <- PriceKD %>% mutate(LowLabel = ifelse(Μ絃基 < MA60, 'Low', 'High'))
    PriceKD[1,'StatusCount']<-1
    for (i in 2:nrow(PriceKD)) {
        LabelN <- PriceKD$LowLabel[i-1]
        LabelN1 <- PriceKD$LowLabel[i]
        StatusCountN<-PriceKD$StatusCount[i-1]
        if (LabelN == LabelN1) {
            StatusCountN1 <- StatusCountN + 1
            PriceKD[i, 'StatusCount'] <- StatusCountN1
        } else {
            StatusCountN1 <- 1
            PriceKD[i, 'StatusCount'] <- StatusCountN1
        }
    }
    PriceKD %>% mutate(MartketStatus = ifelse(LowLabel == 'Low' & StatusCount >= 10, 'Bear', ifelse(LowLabel == 'High' & StatusCount >= 10,'Bull','NoTrend')))
}
PriceKD <- PriceKD %>% GetMarketStatus
write.csv(PriceKD %>% filter(complete.cases(.)), file = 'P00631L.csv')
write.csv(PriceKD %>% filter(complete.cases(.)), file = 'P00632R.csv')
write.csv(PriceKD %>% filter(complete.cases(.)), file = 'P0050.csv')
PriceKD %>% View
# Long Strategy
LongStrategy <- PriceKD



Cash <- 400
KSC <- 80
KBC <- 30
BuyUnit <- 1
SellUnit<-1
LongStrategyResult <- function(LongStrategy, KSC, KBC, Cash, BuyUnit, SellUnit) {
    #Initialize
    LongStrategy[1, 'Long'] <- 0
    LongStrategy[1, 'Cash'] <- Cash
    for (i in 1:(nrow(LongStrategy) - 1)) {
        CashN <- LongStrategy$Cash[i]
        LongN <- LongStrategy$Long[i]
        LongN1 <- LongN
        CashN1 <- CashN
        KN <- LongStrategy$K[i]
        PCN <- LongStrategy$`Μ絃基`[i]
        PLN1 <- LongStrategy$`程基`[i + 1]
        PHN1 <- LongStrategy$`程蔼基`[i + 1]
        #Buy
        if (KN <= KBC & CashN >= BuyUnit* PCN) {
            if (PLN1 <= PCN) {
                CashN1 <- CashN - BuyUnit*PCN
                LongN1 <- LongN + BuyUnit
            }
        }
        #Sell
        if (KN >= KSC & (LongN %/% SellUnit) > 0) {
            if (PHN1>=PCN) {
                CashN1 <- CashN + SellUnit*PCN
                LongN1 <- LongN - SellUnit
            }
        } 
        LongStrategy[i + 1, 'Long'] <- LongN1
        LongStrategy[i + 1, 'Cash'] <- CashN1
    }
    LongStrategy %>% arrange(desc(Date)) %>% .[1,]
}

LongStrategy2016 <- PriceKD %>% filter(Date >= '2016-01-01' & Date < '2017-01-01') %>% arrange(Date)
LongStrategy2017 <- PriceKD %>% filter(Date >= '2017-01-01' & Date < '2018-01-01') %>% arrange(Date)
LongStrategy2018 <- PriceKD %>% filter(Date >= '2018-01-01' & Date < '2019-01-01') %>% arrange(Date)
LongStrategy2019 <- PriceKD %>% filter(Date >= '2019-01-01' ) %>% arrange(Date)
LongStrategy2016_2018 <- PriceKD %>% filter(Date >= '2016-01-01' & Date < '2019-01-01') %>% arrange(Date)


Verify <- LongStrategy %>% LongStrategyResult(., 80, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,]

Verify %>% mutate(Asset = Cash + Long * Μ絃基) %>% select(Long,Cash, Asset)
LongStrategy2016 %>% LongStrategyResult(., 80, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,] %>% mutate(Asset = Cash + Long * Μ絃基) %>% select(Long, Cash, Asset)
LongStrategy2017 %>% LongStrategyResult(., 80, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,] %>% mutate(Asset = Cash + Long * Μ絃基) %>% select(Long, Cash, Asset)
LongStrategy2018 %>% LongStrategyResult(., 80, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,] %>% mutate(Asset = Cash + Long * Μ絃基) %>% select(Long, Cash, Asset)
LongStrategy2016_2018 %>% LongStrategyResult(., 75, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,] %>% mutate(Asset = Cash + Long * Μ絃基) %>% select(Long, Cash, Asset)
PriceKD %>% filter(Date >= '2016-01-01') %>% LongStrategyResult(., 80, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,]
LongStrategy2016_2018 %>% LongStrategyResult(., 80, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,]


#Short Strategy
Cash <- 400
shorttable <- tibble(Date = '2019-01-01' %>% date, PS = 40, BankD = 50) %>% .[0,]

#Initialize
LongStrategy[1, 'Long'] <- 0
LongStrategy[1, 'Cash'] <- Cash
LongStrategy <- LongStrategy %>% mutate(ShortTable = list(shorttable))
UpdateShortTable <- function(shorttable) {
    if (nrow(shorttable) < 2) {
        shorttable[0,]
    } else {
        shorttable %>% arrange(Date) %>% .[2:nrow(.),]
    }
}
LongStrategy$ShortTable[[2]] <- shorttable2

shorttable2 <- tibble(Date = '2019-01-01' %>% date, PS = 40, BankD = 36)
shorttable3<- tibble(Date = '2019-01-03' %>% date, PS = 60, BankD = 54)
shorttablet<-rbind(shorttable2,shorttable3)
GetShortProfit(shorttablet,10,20)
GetShortN1(shorttablet, 1)
Result <- LongShortStrategyResult(LongStrategy, 75, 35, 400, 1, 1) %>% arrange(desc(Date))
check <- Result$ShortTable[[1]]

check$BankD %>% sum
LongShortStrategyResult <- function(LongStrategy, KSC, KBC, Cash, BuyUnit, SellUnit) {
    UpdateShortTableLong <- function(shorttable,BuyUnit) {
        if (nrow(shorttable) <= BuyUnit) {
            shorttable[0,]
        } else {
            shorttable %>% arrange(Date) %>% .[(BuyUnit+1):nrow(.),]
        }
    }
    UpdateShortTableShort<- function(shorttable, SellUnit,PCN,Date) {
        CreateShortRecord <- function(SellUnit, PCN, Date) {
            shortrecordtoday <- tibble()
            shortrecordtodayone <- tibble(Date, PS = PCN, BankD = PCN * 0.9)
            for (i in 1:SellUnit) {
                shortrecordtoday <- rbind(shortrecordtoday, shortrecordtodayone)
            }
            shortrecordtoday
        }
        shorttabletoday <- CreateShortRecord(SellUnit, PCN, Date)
        rbind(shorttable, shorttabletoday) %>% arrange(Date)
    }
    GetShortProfit <- function(shorttable, BuyUnit,PCN) {
        if (nrow(shorttable) <= BuyUnit) {
            BankD <- shorttable$BankD %>% sum
            Profit <- (shorttable$PS - PCN) %>% sum
        } else {
            shorttable <- shorttable %>% arrange(Date) %>% .[1:BuyUnit,]
            BankD <- shorttable$BankD %>% sum
            Profit <- (shorttable$PS - PCN) %>% sum
        }
        BankD+Profit
    }
    GetShortN1 <- function(shorttable, BuyUnit) {
        if (nrow(shorttable) <= BuyUnit) {
            0
        } else {
            shorttable %>% arrange(Date) %>% .[(BuyUnit + 1):nrow(.),] %>% nrow
        }
    }
    #Initialize
    shorttable <- tibble(Date = '2019-01-01' %>% date, PS = 40, BankD = 50) %>% .[0,]
    LongStrategy[1, 'Long'] <- 0
    LongStrategy[1, 'Cash'] <- Cash
    LongStrategy <- LongStrategy %>% mutate(ShortTable = list(shorttable))
    for (i in 1:(nrow(LongStrategy) - 1)) {
        DateN1 <- LongStrategy$Date[i+1]
        CashN <- LongStrategy$Cash[i]
        LongN <- LongStrategy$Long[i]
        shorttableN <- LongStrategy$ShortTable[[i]]
        shortN <- shorttableN %>% nrow
        LongN1 <- LongN
        CashN1 <- CashN
        shortN1<-shortN
        shorttableN1 <- shorttableN
        KN <- LongStrategy$K[i]
        PCN <- LongStrategy$`Μ絃基`[i]
        PLN1 <- LongStrategy$`程基`[i + 1]
        PHN1 <- LongStrategy$`程蔼基`[i + 1]
        #Long
        if (KN <= KBC ) {
            if (PLN1 <= PCN) {
                if (shortN > 0) {
                    shortN1 <- GetShortN1(shorttableN,BuyUnit)
                    CashN1 <- CashN + GetShortProfit(shorttableN, BuyUnit, PCN)
                    shorttableN1 <- UpdateShortTableLong(shorttableN, BuyUnit)
                } else if (CashN >= BuyUnit*PCN) {
                    CashN1 <- CashN -( BuyUnit * PCN)
                    LongN1 <- LongN + BuyUnit
                }
            }
        }
        #Short
        if (KN >= KSC ) {
            if (PHN1 >= PCN) {
                if (LongN > 0) {
                    if ((LongN %/% SellUnit) > 0) {
                        CashN1 <- CashN + SellUnit * PCN
                        LongN1 <- LongN - SellUnit
                    } else {
                        CashN1 <- CashN + LongN * PCN
                        LongN1 <- 0
                    }
                } else if(CashN>=SellUnit*PCN*0.9){
                    CashN1 <- CashN - SellUnit * PCN * 0.9
                    shortN1 <- shortN + SellUnit
                    shorttableN1 <- UpdateShortTableShort(shorttableN, SellUnit, PCN, DateN1)

                }

            }
        }
        LongStrategy[i + 1, 'Long'] <- LongN1
        LongStrategy[i + 1, 'Cash'] <- CashN1
        LongStrategy$ShortTable[[i+1]] <- shorttableN1
    }
    LongStrategy %>% arrange(desc(Date)) %>% .[1,]
}





#Add Market Status

LongStrategyMarketStatusResult <- function(LongStrategy, KSC, KBC, KSC_b, KBC_b, KSC_B, KBC_B,Index, Cash, BuyUnit, SellUnit) {
    print(Index)
    #Initialize
    LongStrategy[1, 'Long'] <- 0
    LongStrategy[1, 'Cash'] <- Cash
    for (i in 1:(nrow(LongStrategy) - 1)) {
        MarketStatus <- LongStrategy$MartketStatus[i]
        if (MarketStatus == 'Bull') {
            KSC <- KSC_B
            KBC <- KBC_B
        }
        else if (MarketStatus == 'Bear') {
            KSC <- KSC_b
            KBC <- KBC_b
        }
        CashN <- LongStrategy$Cash[i]
        LongN <- LongStrategy$Long[i]
        LongN1 <- LongN
        CashN1 <- CashN
        KN <- LongStrategy$K[i]
        PCN <- LongStrategy$`Μ絃基`[i]
        PLN1 <- LongStrategy$`程基`[i + 1]
        PHN1 <- LongStrategy$`程蔼基`[i + 1]
        #Buy
        if (KN <= KBC & CashN >= BuyUnit * PCN) {
            if (PLN1 <= PCN) {
                CashN1 <- CashN - BuyUnit * PCN
                LongN1 <- LongN + BuyUnit
            }
        }
        #Sell
        if (KN >= KSC & (LongN %/% SellUnit) > 0) {
            if (PHN1 >= PCN) {
                CashN1 <- CashN + SellUnit * PCN
                LongN1 <- LongN - SellUnit
            }
        }
        LongStrategy[i + 1, 'Long'] <- LongN1
        LongStrategy[i + 1, 'Cash'] <- CashN1
    }
    LongStrategy %>% arrange(desc(Date)) %>% .[1,]
}

#Test

ksc <- tibble(KSC = seq(60, 95, 5))
kbc <- tibble(KBC = seq(5, 55, 5))

ksc_b <- tibble(KSC_b = seq(60, 95, 5))
kbc_b <- tibble(KBC_b = seq(5, 55, 5))

ksc_B <- tibble(KSC_B = seq(60, 95, 5))
kbc_B <- tibble(KBC_B = seq(5, 55, 5))

TestTable <- ksc %>% mutate(KBC = list(kbc)) %>% unnest %>%
    mutate(KSC_b = list(ksc_b)) %>% unnest %>%
    mutate(KBC_b = list(kbc_b)) %>% unnest %>%
    mutate(KSC_B = list(ksc_B)) %>% unnest %>%
    mutate(KBC_B = list(kbc_B)) %>% unnest
TestTable <- TestTable %>% filter(KSC > KSC_b) %>% filter(KBC > KBC_b) %>% filter(KSC < KSC_B) %>% filter(KBC < KBC_B) %>% mutate(Index=1:nrow(.))
Result2017 <- TestTable[1:10,] %>% mutate(Result = pmap(list(list(LongStrategy2017), KSC, KBC, KSC_b, KBC_b, KSC_B, KBC_B, Index, 400, 1, 1), LongStrategyMarketStatusResult)) %>% unnest %>% mutate(YEAR = '2016')
Result2017 %>% mutate(Asset = Cash + Long * Μ絃基) %>% filter(Asset == max(Asset)) %>% select(Cash, Long, Asset) %>% View


ksc <- tibble(KSC = seq(60, 95, 5))
kbc <- tibble(KBC = seq(5, 55, 5))
TestTable <- ksc %>% mutate(KBC = list(kbc)) %>% unnest



Result2016 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2016), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2016')
Result2017 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2017), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2017')
Result2018 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2018), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2018')
Result2019 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2019), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2019')
Result2016_2018 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2016_2018), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2016_2018')
Result <- rbind(Result2016, Result2017, Result2018, Result2019, Result2016_2018)
write.csv(Result, file = 'tune0050.csv')



Result2016_NoMarket <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2017), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2017')
Result2016_NoMarket %>% mutate(Asset = Cash + Long * Μ絃基) %>% filter(Asset == max(Asset)) %>% select(Cash, Long, Asset)
