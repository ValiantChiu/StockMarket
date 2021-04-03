library(tidyverse)
library(modelr)
library(magrittr)
library(quantmod)
companylist <- FinacialReportResultAll %>% filter(year == 108)

GetStockPrice <- function(companylist) {
    StockAll <- tibble()
    for (i in 1:nrow(companylist)) {
        print(i)
        stock <- paste0(companylist$ㅍ쩻쩘많[i], ".TW")
        start_date <- Sys.Date() - 50 -1828
        end_date <- Sys.Date() + 1 #-40
        tryCatch(getSymbols(stock, from = start_date, to = end_date), error = function(e) e, finally = 1)
        Date <- get(stock) %>% as.data.frame %>% row.names
        OneStock <- get(stock) %>% as.tibble
        names(OneStock) <- c("Open", "High", "Low", "ClosePrice", "Volume", "Adjusted")
        OneStock <- OneStock %>% mutate(day = Date)
        OneStock <- OneStock %>% mutate(ㅍ쩻쩘많 = companylist$ㅍ쩻쩘많[i]) # %>% filter(day == max(day))
        StockAll <- rbind(StockAll, OneStock)
        #Sys.sleep(sample(3:6, size = 1))
    }
    return(StockAll)
}
st <- Sys.time()
StockPrice <- companylist %>% GetStockPrice
et <- Sys.time()
#saveRDS(StockPrice,file = 'Stock Price/Price_2016_20210219.rds')
ed <- '2021-03-08'
sd <- (ed %>% as.Date - 40) %>% as.character
eed <- (ed %>% as.Date + 80) %>% as.character
StockPrice_nest <- StockPrice %>% group_by(ㅍ쩻쩘많) %>% nest

GetMA <- function(ㅍ쩻쩘많, data) {
    print(ㅍ쩻쩘많)
    if (nrow(data %>% filter(complete.cases(.))) >= 28) {
        data %>% filter(complete.cases(.)) %>% arrange(day) %>% mutate(MA28 = SMA(as.numeric(ClosePrice), n = 28)) %>% filter(complete.cases(.))
    } else {
        tibble()
    }
}
result <- StockPrice_nest %>% mutate(MA_result = map2(ㅍ쩻쩘많, data, GetMA))
result %>% select(-data) %>% unnest %>% filter(MA28 >= Low & MA28 <= High) %>% arrange(desc(Volume)) %>% mutate(dff = (ClosePrice - Open) / Open) %>% arrange(desc(dff))
MA28 <- result %>% select(-data) %>% unnest

write.csv(MA28 %>% ungroup, file = 'Stock Price/MA28.csv')
write.csv(StockPrice , file = 'Stock Price/StockPrice.csv')


StockPrice %>% filter(ㅍ쩻쩘많 == '1409') %>% View
MA28 %>% filter(ㅍ쩻쩘많 == '8374') %>% arrange(desc(day))
