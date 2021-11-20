library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)



#Get Price

url_info <- "https://www.twse.com.tw/SBL/t13sa710?response=json&startDate=20210701&endDate=20211120&stockNo=0056&tradeType=&_=1637382047696"
POST(url_info) %>% content(as = "text") %>% fromJSON
companylist <- tibble(公司代號 = c('0050', '0051'))

stock_list <- c('5536', '8112', '0056', '3056', '6176', '4119', '00683L', '0050', '8482', '3032', '3090', '00876', '1558', '5469', '00737', '00771', '8941', '8390')

GetOneStockInfo <- function(stock) {
    url_info <- paste0("https://www.twse.com.tw/SBL/t13sa710?response=json&startDate=20180101&endDate=20211120&stockNo=", stock, "&tradeType=&_=1637382047696")
    info <- POST(url_info)
    info <- info %>% content(as = "text") %>% fromJSON
    infoData <- info$data %>% as.tibble
    if (nrow(infoData) != 0) { names(infoData) <- info$fields }
    
    infoData
}
stock<-'00771'
GetOneStockInfo(stock)
GetInfo <- function(stock_list) {
    all <- tibble()
    for (i in stock_list) {
        print(i)
        one <- GetOneStockInfo(i)
        all <- rbind(one, all)
        Sys.sleep(sample(3:8, size = 1))
    }
    return(all)
}


ALL <- GetInfo(stock_list)


