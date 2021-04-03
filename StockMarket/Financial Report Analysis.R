library(tidyverse)
library(modelr)
library(magrittr)
library(quantmod)



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

FinacialReportResultAllNest <- FinacialReportResultAll %>% group_by(產業類別, 公司簡稱, 公司代號) %>% nest

GetAvgLastIndex <- function(data, n) {
    GetIndex <- function(data, n) {
        fill_na <- function(column_name, y, column_value) {
            if (is.na(column_value)) {
                if (column_name == '本期綜合損益總額') { data %>% filter(complete.cases(.)) %>% filter(year < y) %>% top_n(1, year) %>% .$本期綜合損益總額 }
                if (column_name == '權益總額') { data %>% filter(complete.cases(.)) %>% filter(year < y) %>% top_n(1, year) %>% .$權益總額 }
                if (column_name == '基本每股盈餘元') { data %>% filter(complete.cases(.)) %>% filter(year < y) %>% top_n(1, year) %>% .$基本每股盈餘元 }
                if (column_name == 'ROE') { data %>% filter(complete.cases(.)) %>% filter(year < y) %>% top_n(1, year) %>% .$ROE }
                if (column_name == '負債比例') { data %>% filter(complete.cases(.)) %>% filter(year < y) %>% top_n(1, year) %>% .$負債比例 }
                if (column_name == '每股參考淨值') { data %>% filter(complete.cases(.)) %>% filter(year < y) %>% top_n(1, year) %>% .$每股參考淨值 }
                if (column_name == '現金股利元股') { data %>% filter(complete.cases(.)) %>% filter(year < y) %>% top_n(1, year) %>% .$現金股利元股 }
            } else {
                column_value
            }
        }
        data <- data %>% mutate(現金股利元股 = pmap(list('現金股利元股', year, 現金股利元股), fill_na) %>% as.numeric)
        result <- data %>% top_n(n, year) %>% select(-year) %>% mutate_all(mean) %>% .[1,]
        if (n != 1) {
            result <- result %>% rename_all(~paste0(., 'mean'))
        } else {
            result <- result %>% rename_all(~paste0(., 'last'))
        }
        result
    }
    GetIndex(data,n)
}


FinacialReportResult <- FinacialReportResultAllNest %>% mutate(averageindex = map(data, GetAverageIndex), count = map(data, nrow))
FinacialReportResult <- FinacialReportResult %>% mutate(CoOERate = map(data, GetRateIndex))
FinacialReportResult <- FinacialReportResult %>% mutate(avg = map2(data, 5, GetAvgLastIndex), last = map2(data, 1, GetAvgLastIndex))
FinacialReportResult <- FinacialReportResult %>% select(-data) %>% unnest
FinacialReportResult <- FinacialReportResult %>% mutate(K值法mean = 每股參考淨值mean * (ROEmean / 0.08),
                                K值法last = 每股參考淨值last * (ROElast / 0.08),
                                股利法mean = 現金股利元股mean / 0.06,
                                股利法last = 現金股利元股last / 0.06,
                                本益比法mean = 基本每股盈餘元mean * 14,
                                本益比法last = 基本每股盈餘元last * 14)




write.csv(FinacialReportResultAll, file = 'Aggregation/aggregation_info.csv')




#Get Stock Price

FinacialReportResult <- FinacialReportResult %>% mutate(weight = AverageROE * RateROE)
##stocklist <- FinacialReportResult %>% filter(AverageROE > 0.2) %>% arrange(desc(weight)) %>% filter(count >= 5)
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