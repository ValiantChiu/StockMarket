library(tidyverse)
library(modelr)
library(magrittr)
library(quantmod)

companylist <- tibble(ㅍ쩻쩘많 = c('0056', '0050', '00632R', '00652', '00654R', '00634R', '00638R', '00752', '00737', '00876'))

#M ETF
companylist <- tibble(ㅍ쩻쩘많 = c('00763U', '00738U', '00715L', '00708L', '00693U', '00674R', '00673R', '00642U', '00635U'))
companylist <- tibble(ㅍ쩻쩘많 = c('00631L', '00653L', '00637L', '00753L', '00670L', '00852L', '00715L', '00708L','00693U'))
GetStockPrice <- function(companylist) {
    StockAll <- tibble()
    for (i in 1:nrow(companylist)) {
        print(i)
        stock <- paste0(companylist$ㅍ쩻쩘많[i], ".TW")
        tryCatch(getSymbols(stock), error = function(e) e, finally = 1)
        Date <- get(stock) %>% as.data.frame %>% row.names
        OneStock <- get(stock) %>% as.tibble
        names(OneStock) <- c("Open", "High", "Low", "ClosePrice", "Volume", "Adjusted")
        OneStock <- OneStock %>% mutate(day = Date)
        OneStock <- OneStock %>% mutate(ㅍ쩻쩘많 = companylist$ㅍ쩻쩘많[i]) # %>% filter(day == max(day))
        StockAll <- rbind(StockAll, OneStock)
        Sys.sleep(sample(1:3, size = 1))
    }
    return(StockAll)
}

st <- Sys.time()
StockPrice <- companylist %>% GetStockPrice
et <- Sys.time()

StockPrice %>% View
write.csv(StockPrice, file = 'Stock Price/ETF/M_ETF_Price.csv')

StockPrice %>% filter(ㅍ쩻쩘많=='00763U')

GetKD <- function(stock_price) {
    GetRSV <- function(C, D, n) {
        data <- stock_price %>% filter(day <= D) %>% arrange(desc(day)) %>% .[1:9,]
        H <- data$High %>% max
        L <- data$Low %>% min
        RSV <- (C - L) / (H - L) * 100
        RSV
    }
    stock_price_RSV <- stock_price %>% mutate(RSV = pmap(list(ClosePrice, day, 9), GetRSV) %>% as.numeric)
    stock_price_RSV <- stock_price_RSV %>% arrange(day)
    stock_price_RSV <- stock_price_RSV %>% filter(!is.na(RSV))
    k <- c()
    d <- c()
    for (i in 1:nrow(stock_price_RSV)) {
        #print(i)
        if (i == 1) {
            k[i] <- 50 * 2 / 3 + stock_price_RSV$RSV[i] / 3
            d[i] <- 50 * 2 / 3 + k[i] / 3
        } else {
            k[i] <- k[i - 1] * 2 / 3 + stock_price_RSV$RSV[i] / 3
            d[i] <- d[i - 1] * 2 / 3 + k[i] / 3
        }
    }
    stock_price_all <- stock_price_RSV %>% mutate(k, d)
    stock_price_all
}
stock_price_all <- GetKD(stock_price)


StockPrice %>% filter(ㅍ쩻쩘많 == '00763U') %>% GetKD %>% arrange(desc(day))


kd_data <- StockPrice %>% group_by(ㅍ쩻쩘많) %>% nest %>% mutate(KD = map(data, GetKD)) %>% select(-data) %>% unnest

write.csv(kd_data, file = 'Stock Price/ETF/M_ETF_Price.csv')