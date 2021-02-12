library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)
Sys.setlocale("LC_ALL", "cht")

GetOneTradeVolume <- function(Year, Month, Day) {
    Vurl <- paste0("https://www.tpex.org.tw/web/stock/aftertrading/trading_volume/vol_rank_result.php?l=zh-tw&t=D&d=",Year,"/",Month,"/",Day,"")
    TradeVolume <- POST(Vurl)
    TradeVolume <- TradeVolume %>% content(as = "text") %>% fromJSON
    VolumeData <- TradeVolume$aaData %>% as.tibble
    names(VolumeData) <- c("rank", "stock", "name", "Volume")
    VolumeData
}

DealWithDate <- function(Price) {
    Price %>% mutate(BuyDate = str_split(日期, "/")) %>%
        mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
           mutate(Date = ymd(paste0(year, month, day))) %>%
           select(-year, - month, - day, - 日期, - BuyDate)
}
GetPrice <- function(Year, Month, Stock) {
    print(Stock)
    Purl <- paste0("https://www.tpex.org.tw/web/stock/aftertrading/daily_trading_info/st43_result.php?l=zh-tw&d=",Year,"/",Month,"/01&stkno=",Stock,"")
    Price <- POST(Purl)
    Price <- Price %>% content(as = "text") %>% fromJSON
    PriceData <- Price$aaData %>% as.tibble
    names(PriceData) <- c("日期", "成交仟股", "成交仟元", "開盤", "最高", "最低", "收盤", "漲跌", "筆數")
    Sys.sleep(sample(4:8, size = 1))
    PriceData
}

#成交量名單
#Three day large
Today <- GetOneTradeVolume(108, '12', '17') %>% mutate(rank = as.numeric(rank)) %>% filter(rank <= 20)
Yesterday <- GetOneTradeVolume(108, 12, '16') %>% mutate(rank = as.numeric(rank)) %>% filter(rank <= 20)
BeforeYesterday <- GetOneTradeVolume(108, 12, '13') %>% mutate(rank = as.numeric(rank)) %>% filter(rank <= 20)
TradeVolumeList1 <- Today %>% filter(stock %in% Yesterday$stock) %>% filter(stock %in% BeforeYesterday$stock)
#Today large and >2 yesterday
Today <- GetOneTradeVolume(108, '12', '17') %>% mutate(rank = as.numeric(rank)) %>% filter(rank <= 20)
Yesterday <- GetOneTradeVolume(108, 12, '16') %>% mutate(rank = as.numeric(rank)) %>% filter(rank <= 20) %>% select(stock, Volume) %>% rename(Volume_y=Volume)
TradeVolumeList2 <- Today %>% left_join(Yesterday) %>%
    filter(complete.cases(.)) %>%
    mutate(Volume = gsub(pattern = ",", replacement = "", Volume)) %>%
    mutate(Volume_y = gsub(pattern = ",", replacement = "", Volume_y)) %>%
    filter((as.numeric(Volume) / as.numeric(Volume_y)) >= 1.1)

TradeVolumeList <- rbind(TradeVolumeList1 %>%
    mutate(Volume = gsub(pattern = ",", replacement = "", Volume)), TradeVolumeList2 %>% select(-Volume_y)) %>% unique
#中期名單

PriceThisMonth<-TradeVolumeList %>% mutate(price = pmap(list(108,12,stock), GetPrice))
PriceLastMonth <- TradeVolumeList %>% mutate(price = pmap(list(108, 11, stock), GetPrice))
PriceBeforeLastMonth <- TradeVolumeList %>% mutate(price = pmap(list(108, "10", stock), GetPrice))
PriceBBeforeLastMonth <- TradeVolumeList %>% mutate(price = pmap(list(108, "09", stock), GetPrice))
Price <- rbind(PriceThisMonth %>% unnest, PriceLastMonth %>% unnest, PriceBeforeLastMonth %>% unnest, PriceBBeforeLastMonth %>% unnest)
Price <- Price %>% DealWithDate %>% group_by(rank, stock, name) %>% nest
GetMA <- function(price) {
    price %>% arrange(Date) %>% mutate(MA12 = SMA(as.numeric(收盤), n = 12), MA58 = SMA(as.numeric(收盤), n = 58)) %>% filter(Date == max(Date))
}
MAList <- Price %>% mutate(MA = map(data, GetMA)) %>% select(-data) %>% unnest %>% rename(O = 開盤, H = 最高, L = 最低, C = 收盤)
GetBullBear <- function(L, H, MA12, MA58) {
    type<-''
    if (L > MA12 & MA12 > MA58) { type<-"Bull" }
    if (H < MA12 & MA12 < MA58) { type <- "Bear" }
    type
}
MAList <- MAList %>% mutate(Type = pmap(list(L, H, MA12, MA58), GetBullBear) %>% as.character)


#Prcing
GetPricing <- function(O, H, L, C,Type) {
    M <- (H + L) / 2
    alpha <- ifelse(M < C, 1.618, 1.382)
    beta <- ifelse(M < C, 1.382, 1.618)
    St <- L + (H - L) * alpha
    Weak <- H - (H - L) * beta
    BuyP <- 0
    BuyPs <- 0
    SellP <- 0
    SellPs<-0
    if (Type == 'Bull') {
        if (Weak < C & C < M) {
            BuyP <- Weak
            SellP <- M
            SellPs<-0.98*Weak
        }
        if (M<C & C<St) {
            BuyP <- M
            SellP <- St
            SellPs <- Weak
        }
    } else {
        if (M < C & C < St) {
            BuyP <- M
            BuyPs<-1.02*St
            SellP <- St
        }
        if (Weak < C & C < M) {
            BuyP <- Weak
            BuyPs <- St
            SellP <- M
        }
    }
    tibble(M, St, Weak, BuyP, BuyPs, SellP, SellPs)
}
Pricing <- MAList %>% mutate(O = as.numeric(O), H = as.numeric(H), L = as.numeric(L), C = as.numeric(C)) %>% mutate(pricing = pmap(list(O, H, L, C, Type), GetPricing)) %>% unnest


#ATRandQuota
GetATR <- function(data) {
    oneprice <- data %>% arrange(desc(Date)) %>%
    rename(O = 開盤, H = 最高, L = 最低, C = 收盤) %>%
    mutate(H = as.numeric(H), L = as.numeric(L), C = as.numeric(C)) %>% select(Date, H, L, C)
    First <- oneprice[1:22,] %>% select(H, L)
    Last <- oneprice[2:23,] %>% select(C)
    All <- cbind(First, Last) %>% as.tibble
    ATR <- All %>% mutate(H_L = H - L, H_C = abs(L - C), L_C = abs(L - C)) %>% mutate(ATEone = pmap(list(H_L, H_C, L_C), ~ max(.)) %>% as.numeric)
    ATR$ATEone %>% mean
}
AllATR <- Price %>% mutate(ATR = map(data, GetATR)) %>% select(-data, - rank) %>% unnest %>% mutate(Quota = 10000 / ATR)

#Day Trade List
DayTradeList <- Pricing %>% left_join(AllATR) %>% filter(Type != '') %>% select(-rank, - 成交仟元, - 漲跌, - 筆數, - 成交仟股) %>% arrange(stock)
Result <- DayTradeList %>% select(-Date) %>% arrange(stock) %>% select(stock, name, BuyP, BuyPs, SellP, SellPs) %>% filter(stock %in% c( "5347","5483","6182",""))
Result
saveRDS(DayTradeList, file = 'DayTradeList1217.rds')
DayTradeList <- readRDS('DayTradeList1217.rds')
Result$BuyP
DayTradeList <- readRDS('DayTradeList1213.rds')
list1$BuyP[3]