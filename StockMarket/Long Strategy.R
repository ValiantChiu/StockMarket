library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)

#Data Processing 
Sys.setlocale("LC_ALL", "cht")

Price00631L <- readRDS(file = "Price00631L.rdata")
Price00632R <- readRDS(file = "Price00632R.rdata")
Price0050 <- readRDS(file = "Price0050.rdata")
Price <- Price00631L

GetKD <- function(Price, n) {
    DealWithDate <- function(Price) {
        Price %>% mutate(BuyDate = str_split(ら戳, "/")) %>%
        mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
           mutate(Date = ymd(paste0(year, month, day))) %>%
           select(-year, - month, - day, - ら戳, - BuyDate)
    }
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
    Price <- Price %>% DealWithDate %>% arrange(Date)
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
    Price_HL_RSV %>% GetKD %>% arrange(Date)
}

PriceKD <- Price %>% GetKD(., 9)
PriceKD <- PriceKD %>% mutate(MA60 = SMA(Μ絃基, n = 60)) %>% filter(complete.cases(.))
GetMarketStatus <- function(PriceKD) {
    PriceKD <- PriceKD %>% mutate(LowLabel = ifelse(Μ絃基 < MA60, 'Low', 'High'))
    PriceKD[1, 'StatusCount'] <- 1
    for (i in 2:nrow(PriceKD)) {
        LabelN <- PriceKD$LowLabel[i - 1]
        LabelN1 <- PriceKD$LowLabel[i]
        StatusCountN <- PriceKD$StatusCount[i - 1]
        if (LabelN == LabelN1) {
            StatusCountN1 <- StatusCountN + 1
            PriceKD[i, 'StatusCount'] <- StatusCountN1
        } else {
            StatusCountN1 <- 1
            PriceKD[i, 'StatusCount'] <- StatusCountN1
        }
    }
    PriceKD %>% mutate(MartketStatus = ifelse(LowLabel == 'Low' & StatusCount >= 10, 'Bear', ifelse(LowLabel == 'High' & StatusCount >= 10, 'Bull', 'NoTrend')))
}
PriceKD <- PriceKD %>% GetMarketStatus

#Strategy

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

LongStrategyMarketStatusResult <- function(LongStrategy, KSC, KBC, KSC_b, KBC_b, KSC_B, KBC_B, Index, Cash, BuyUnit, SellUnit) {
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

LongStrategy2016 <- PriceKD %>% filter(Date >= '2016-01-01' & Date < '2017-01-01') %>% arrange(Date)
LongStrategy2017 <- PriceKD %>% filter(Date >= '2017-01-01' & Date < '2018-01-01') %>% arrange(Date)
LongStrategy2018 <- PriceKD %>% filter(Date >= '2018-01-01' & Date < '2019-01-01') %>% arrange(Date)
LongStrategy2019 <- PriceKD %>% filter(Date >= '2019-01-01') %>% arrange(Date)
LongStrategy2016_2018 <- PriceKD %>% filter(Date >= '2016-01-01' & Date < '2019-01-01') %>% arrange(Date)
#Test
LongStrategy2016 %>% LongStrategyResult(., 80, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,] %>% mutate(Asset = Cash + Long * Μ絃基) %>% select(Long, Cash, Asset)
LongStrategy2017 %>% LongStrategyResult(., 80, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,] %>% mutate(Asset = Cash + Long * Μ絃基) %>% select(Long, Cash, Asset)
LongStrategy2018 %>% LongStrategyResult(., 80, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,] %>% mutate(Asset = Cash + Long * Μ絃基) %>% select(Long, Cash, Asset)
LongStrategy2016_2018 %>% LongStrategyResult(., 80, 35, 400, 1, 1) %>% arrange(desc(Date)) %>% .[1,] %>% mutate(Asset = Cash + Long * Μ絃基) %>% select(Long, Cash, Asset)
#Tune
ksc <- tibble(KSC = seq(60, 95, 5))
kbc <- tibble(KBC = seq(5, 55, 5))
TestTable <- ksc %>% mutate(KBC = list(kbc)) %>% unnest



Result2016 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2016), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2016')
Result2017 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2017), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2017')
Result2018 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2018), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2018')
Result2019 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2019), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2019')
Result2016_2018 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2016_2018), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2016_2018')
Result <- rbind(Result2016, Result2017, Result2018, Result2019, Result2016_2018)

Result2016 %>% mutate(Asset = Cash + Long * Μ絃基) %>% arrange(desc(Asset)) %>% select(Asset)




#Add Market Status
#Tune
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
TestTable <- TestTable %>% filter(KSC > KSC_b) %>% filter(KBC > KBC_b) %>% filter(KSC < KSC_B) %>% filter(KBC < KBC_B) %>% mutate(Index = 1:nrow(.))
Result2016 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2017), KSC, KBC, KSC_b, KBC_b, KSC_B, KBC_B, Index, 400, 1, 1), LongStrategyMarketStatusResult)) %>% unnest %>% mutate(YEAR = '2016')
Result2017 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2017), KSC, KBC, KSC_b, KBC_b, KSC_B, KBC_B, Index, 400, 1, 1), LongStrategyMarketStatusResult)) %>% unnest %>% mutate(YEAR = '2017')
Result2018 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2017), KSC, KBC, KSC_b, KBC_b, KSC_B, KBC_B, Index, 400, 1, 1), LongStrategyMarketStatusResult)) %>% unnest %>% mutate(YEAR = '2018')
Result2019 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2017), KSC, KBC, KSC_b, KBC_b, KSC_B, KBC_B, Index, 400, 1, 1), LongStrategyMarketStatusResult)) %>% unnest %>% unnest %>% mutate(YEAR = '2019')
Result2016_2018 <- TestTable %>% mutate(Result = pmap(list(list(LongStrategy2016_2018), KSC, KBC, 400, 1, 1), LongStrategyResult)) %>% unnest %>% mutate(YEAR = '2016_2018')
Result <- rbind(Result2016, Result2017, Result2018, Result2019, Result2016_2018)


Result2016 %>% mutate(Asset = Cash + Long * Μ絃基) %>% select(KSC, KBC, KSC_b, KBC_b, KSC_B, KBC_B,Long, Cash, Asset) %>% arrange(desc(Asset))