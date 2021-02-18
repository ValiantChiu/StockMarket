library(tidyverse)
library(modelr)
library(magrittr)
library(quantmod)

FinacialReportResultPricing <- FinacialReportResultAll %>% arrange(公司代號, year) %>% group_by(公司代號) %>% summarise(ROE_last = last(ROE), 本益比法mean = mean(本益比法), K值法mean = mean(K值法), 負債比例mean = mean(負債比例), 股利mean = mean(現金股利元股), 本益比法first = first(本益比法), 本益比法last = last(本益比法), K值法first = first(K值法), K值法last = last(K值法), 負債比例first = first(負債比例), 負債比例last = last(負債比例), 股利first = first(現金股利元股), 股利last = last(現金股利元股))

#Analysis
GetRateIndex <- function(df) {
    RateROE <- lm(ROE ~ year, data = df) %>% .$coefficients %>% .[2] %>% as.numeric
    tibble(RateROE)
}
GetAverageIndex <- function(df) {
    AverageROE <- df$ROE %>% mean
    StdROE <- df$ROE %>% sd
    tibble(AverageROE, StdROE)
}

FinacialReportResultAllNest <- FinacialReportResultAll %>% group_by(公司代號) %>% nest

FinacialReportResult <- FinacialReportResultAllNest %>% mutate(averageindex = map(data, GetAverageIndex), count = map(data, nrow))
FinacialReportResult <- FinacialReportResult %>% mutate(CoOERate = map(data, GetRateIndex))
FinacialReportResult <- FinacialReportResult %>% select(-data) %>% unnest
FinacialReportResult <- FinacialReportResult %>% left_join(FinacialReportResultPricing)

FinacialReportResultAll 
write.csv(FinacialReportResultAll, file = 'Aggregation/aggregation_info.csv')




#Get Stock Price

FinacialReportResult <- FinacialReportResult %>% mutate(weight = AverageROE * RateROE)
stocklist <- FinacialReportResult %>% filter(AverageROE > 0.2) %>% arrange(desc(weight)) %>% filter(count >= 5)
GetStockPrice <- function(companylist) {
    StockAll <- tibble()
    for (i in 1:nrow(companylist)) {
        print(i)
        stock <- paste0(companylist$公司代號[i], ".TW")
        start_date <- Sys.Date() - 20
        end_date <- Sys.Date()+1
        getSymbols(stock,from = start_date, to = end_date)
        Date <- get(stock) %>% as.data.frame %>% row.names
        OneStock <- get(stock) %>% as.tibble
        names(OneStock) <- c("Open", "High", "Low", "ClosePrice", "Volume", "Adjusted")
        OneStock <- OneStock %>% mutate(day = Date)
        OneStock <- OneStock %>% mutate(公司代號 = companylist$公司代號[i]) %>% filter(day == max(day))
        StockAll <- rbind(StockAll, OneStock)
        #Sys.sleep(sample(3:6, size = 1))
    }
    return(StockAll)
}
st <- Sys.time()
StockPrice <- FinacialReportResult %>% GetStockPrice
et <- Sys.time()


write.csv(FinacialReportResult %>% left_join(basis) %>% left_join(StockPrice), file = 'Aggregation/analysis_info.csv')