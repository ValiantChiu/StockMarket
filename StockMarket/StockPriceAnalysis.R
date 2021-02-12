library(magrittr)
library(tidyverse)
library(lubridate)
Sys.setlocale("LC_ALL", "cht")

Price0050 <- readRDS(file = "Price0050Raw.rds") #
Price0050 %>% as.tibble

Price2439 <- GetStockPriceByYear(2439)

PredictWindow <- 10
TradeFarePercent <- 0.1425
GovernmentTaxPercent <- 0.1
ExpectedProfitPercent <- 1

Price <- Price0050 %>% mutate(pricevrate = (收盤價 / (收盤價 - 漲跌價差) - 1) * 100) %>% arrange(日期)

signal <- Price %>% filter(pricevrate >= 1 & pricevrate<2)
GetLabel <- function(price,date,window,criterion) {
    PriceWindow <- Price %>% filter(日期 > date) %>% arrange(日期) %>% .[2:window,]
    maxprice<-PriceWindow$收盤價 %>% max
    ifelse(((maxprice-price) / price)*100 > criterion, 1, 0)
}
result <- signal %>% mutate(result = pmap(list(收盤價, 日期, PredictWindow, (2 * TradeFarePercent + GovernmentTaxPercent + ExpectedProfitPercent)), GetLabel) %>% as.numeric)
output<-result$result %>% table %>% as.tibble 
names(output) <- c("type", "count")
output

GetResult <- function(sig, PW, EP, n) {
    print(n)
    GetLabel <- function(price, date, window, criterion) {
        PriceWindow <- Price %>% filter(日期 > date) %>% arrange(日期) %>% .[2:window,]
        maxprice <- PriceWindow$收盤價 %>% max
        ifelse(((maxprice - price) / price) * 100 > criterion, 1, 0)
    }
    signal <- Price %>% filter(pricevrate >= sig & pricevrate < (sig + 1))
    result <- signal %>% mutate(result = pmap(list(收盤價, 日期, PW, (2 * TradeFarePercent + GovernmentTaxPercent + EP)), GetLabel) %>% as.numeric)
    output <- result$result %>% table %>% as.tibble
    #names(output) <- c("type", "count")
    output
}


#Create Parameter
EP <- tibble(EP = seq(0.1, 5, by = 0.5))
PW <- tibble(PW = seq(2, 10, by = 1))
signal <- tibble(signal = seq(-10, 9, by = 1))
Parameter <- EP %>% mutate(PW = list(PW)) %>% unnest %>% mutate(signal = list(signal)) %>% unnest %>% mutate(n=1:nrow(.))
rrrresult <- Parameter %>% mutate(result = pmap(list(signal, PW, EP,n), GetResult))
check<-rrrresult %>% mutate(nrown = map(result, ~ nrow(.)) %>% as.numeric) %>% filter(nrown == 1) %>% unnest
check %>% filter(. == 1) %>% arrange(desc(EP)) %>% View
 saveRDS(rrrresult, file = "rrrresult.rds")