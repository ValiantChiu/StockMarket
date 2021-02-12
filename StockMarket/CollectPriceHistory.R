library(jsonlite)
library(magrittr)
library(tidyverse)
library(rvest)
library(httr)
library(lubridate)
library(RODBC)

LOCAL <- odbcConnect("LOCAL")
#List
StockListURL <- paste0("http://mops.twse.com.tw/mops/web/ajax_t163sb14?1&step=1&firstin=1&off=1&TYPEK=sii&year=105&season=04");

Source <- read_html(StockListURL, encoding = "UTF-8")
CompanyIndex <- Source %>% html_nodes(css = "td:nth-child(1)") %>% html_text;
CompanyName <- Source %>% html_nodes(css = "td:nth-child(2)") %>% html_text;
StockList <- tibble(CompanyIndex, CompanyName) %>% unique

#Price
PriceSource <- "http://www.twse.com.tw/exchangeReport/STOCK_DAY_AVG?response=json&date=20160301&stockNo=0050&_=1514724793134"
"http://www.twse.com.tw/exchangeReport/STOCK_DAY_AVG?response=json&date=20100301&stockNo=0056&_=1514724793134"
GetOneCompanyPriceHistory <- function(companyIndex, startyear, endyear) {
    m <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
    Result <- tibble()
    for (year in startyear:endyear) {
        for (month in m) {
            source <- paste0("http://www.twse.com.tw/exchangeReport/STOCK_DAY_AVG?response=json&date=20", year, month, "01&stockNo=", companyIndex, "&_=1514724793134")
            print(paste(companyIndex,":",year," ",month))
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
AllPriceHistory <- tibble()
AllPriceHistory %>% filter(company == '2103') %>% .[90:100,]
AllPriceHistory$company %>% min
AllPriceHistory$company %>% max
StockList <- tibble(CompanyIndex, CompanyName) %>% unique %>% filter(CompanyIndex > '6504') #AllPriceHistory$company %>% max
for (company in StockList$CompanyIndex) {
    OnePriceHistory <- GetOneCompanyPriceHistory(company, 10, 17)
    AllPriceHistory <- rbind(AllPriceHistory, OnePriceHistory)
    closeAllConnections()
}

Fr6505To9958PriceHistory<-AllPriceHistory
sqlSave(LOCAL, ResultAll2, tablename = paste0("StockPriceResultAll2010_2017"), rownames = FALSE, append = TRUE)
Fr6505To9958PriceHistory2 <- LOCAL %>% sqlQuery("select * from [master].[dbo].[StockPriceResultAll2010_2017]") %>% as.tibble
ResultAll$datesell[1] - ResultAll$datebuy[1]

Fr6505To9958PriceHistory2$datebuy %>% ymd 
odbcCloseAll()
# data from DB

A0 <- LOCAL %>% sqlQuery("select * from [master].[dbo].[Untill2382PriceHistory]")
A1 <- LOCAL %>% sqlQuery("select * from [master].[dbo].[Untill2103PriceHistory]")
A2 <- LOCAL %>% sqlQuery("select * from [master].[dbo].[Untill2009PriceHistory]")
A <- LOCAL %>% sqlQuery("select * from [master].[dbo].[Fr2383To2712PriceHistory]")
B<- LOCAL %>% sqlQuery("select * from [master].[dbo].[Fr2722To2851PriceHistory]")
C <- LOCAL %>% sqlQuery("select * from [master].[dbo].[Fr2852To4977PriceHistory]")
E <- LOCAL %>% sqlQuery("select * from [master].[dbo].[Fr4984To6504PriceHistory]")
G <- LOCAL %>% sqlQuery("select * from [master].[dbo].[Fr6505To9958PriceHistory]")

StockPrice <- rbind(A0, A1, A2, A, B, C, E, G) %>% as.tibble %>% unique

#Post-Processing
StockPrice <- LOCAL %>% sqlQuery("select * from [master].[dbo].[StockPrice2010_2017] ") %>% as.tibble
StockPrice <- StockPrice %>%
    mutate(dates = str_split(date, "/")) %>%
    mutate(year = as.integer(map(dates, ~ .[1])) + 1911,
           month = as.integer(map(dates, ~ .[2])),
           day = as.integer(map(dates, ~ .[3])))
StockPrice<-StockPrice %>% select(company,price,year,month,day) %>% arrange(company,year,month,day)

StockPrice<-StockPrice %>% mutate(price=as.double( gsub(pattern = ",",replacement = "",as.character(price)))) %>% filter(!is.na(price))
StockPrice <- StockPrice %>% mutate(date = make_date(year, month, day)) %>% select(company, price, year, date) 
odbcCloseAll()
#Data Check
StockPrice$company %>% unique
StockPrice %>% filter(company > 2109) 
#Optimization profit for best buy and sell day within one year
ResultAll <- tibble()
Company <- StockPrice$company %>% unique 
for (c in Company) {
    Result <- tibble()
    OneStock <- StockPrice %>% filter(company == c)
    Year <- OneStock$year %>% unique
    for (y in Year) {
    OneStockYear <- OneStock %>% filter(year == y)
    OneResult <- GetOneYearResult(OneStockYear)
    if ((OneResult$datebuy %>% class) == "numeric") next;
    Result <- rbind(Result, OneResult)
    }
    Result <- Result %>% mutate(company = c)
    
    ResultAll<-rbind(ResultAll,Result)
}

ResultAll2 <- ResultAll %>% as.tibble
ResultAll2 <- ResultAll2 %>% mutate(datebuy = as.character(datebuy), datesell = as.character(datesell))
ResultAll2$company %>% unique

GetOneYearResult <- function(OneStockYear) {
    profitfinal <- 0
    pricebuy <- 0
    pricesell<-0
    datebuy <- 0
    datesell <- 0
    for (i in 1:nrow(OneStockYear)) {
        Buy <- OneStockYear[i,]
        buyprice <- Buy$price[1]
        Sell <- OneStockYear %>% filter(date > Buy$date) %>% filter(price == max(price))
        if(nrow(Sell)==0) next
        sellprice <- Sell$price[1]
        profit<-sellprice-buyprice
        if (profit > profitfinal) {
            profitfinal <- profit
            datebuy <- Buy$date
            datesell <- Sell$date[1]
            pricebuy <- buyprice
            pricesell<-sellprice
        } 
    }
    return(tibble(profitfinal, datebuy, datesell, pricebuy,pricesell))
}



#Industry Category
Industry <- read.csv(file = "C:/Users/user/AppData/Local/Temp/Industry.csv.utf8", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "")
Industry<-Industry %>% as.tibble
names(Industry) <- c("company", "companyC", "industry")
Industry <- Industry %>% map(~gsub(pattern = "'", replacement = "", .)) %>% as.tibble
sqlSave(LOCAL, Industry, tablename = paste0("IndustryCategory"), rownames = FALSE, append = FALSE)
