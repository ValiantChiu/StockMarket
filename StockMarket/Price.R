library(jsonlite)
library(magrittr)
library(tidyverse)
library(rvest)
library(httr)
#List
StockListURL <- paste0("http://mops.twse.com.tw/mops/web/ajax_t163sb14?1&step=1&firstin=1&off=1&TYPEK=sii&year=105&season=04");

Source <- read_html(StockListURL, encoding = "UTF-8")
CompanyIndex <- Source %>% html_nodes(css = "td:nth-child(1)") %>% html_text;
CompanyName <- Source %>% html_nodes(css = "td:nth-child(2)") %>% html_text ;
StockList <- tibble(CompanyIndex, CompanyName) %>% unique

#Price
PriceSource <- "http://www.twse.com.tw/exchangeReport/STOCK_DAY_AVG?response=json&date=20160301&stockNo=0050&_=1514724793134"

GetOneCompanyPriceHistory <- function(companyIndex,startyear,endyear) {
    m <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
    Result <- tibble()
    for (year in startyear:endyear) {
        for (month in m) {
            source <- paste0("http://www.twse.com.tw/exchangeReport/STOCK_DAY_AVG?response=json&date=20", year, month, "01&stockNo=",companyIndex,"&_=1514724793134")
            print(source)
            result <- fromJSON(source) %>% .$data %>% as.data.frame %>% slice(1:(n() - 1))
            if (nrow(result) == 1) next
            Result <- rbind(Result, result)
           
            Sys.sleep(sample(2:5, size = 1));
        }
    }
    Result %<>% mutate(company = companyIndex)
    return(Result)
}

#Collect Data
for (company in StockList$CompanyIndex) {
    AllPriceHistory <- tibble()
    OnePriceHistory<-GetOneCompanyPriceHistory(company,17,17)
    AllPriceResult <- rbind(AllPriceResult, OnePriceHistory)
}



Result$V1 %<>% as.character
Result %>% filter(V1 > '106/01/31') %>% select(V2) %>% .[, 'V2'] %>% as.character %>% as.double %>% mean
result <- fromJSON(source)
result<-fromJSON(source) %>% .$data %>% as.data.frame   %>% slice(1:(n()-1)) 
Result<-Result[0,]
result$data %>% nrow
Result$V2 %>% as.character %>% as.double %>%  sd
Result$V2 %<>% as.character %>% as.double
Result0056$V2 %>% summary
i <- Result %>% as.character %>% as.double
ggplot(data = Result, mapping = aes(x = V2)) +
  geom_freqpoly(binwidth = 0.2)
result$data[result$data %>% nrow, 2]