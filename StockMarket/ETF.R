library(tidyverse)
library(modelr)
library(magrittr)
library(quantmod)

companylist <- tibble(ㅍ쩻쩘많=c('0056','0050','00632R','00652','00654R','00634R','00638R','00752','00737','00876'))
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
write.csv(StockPrice,file ='Stock Price/ETF/ETF_Price.csv')