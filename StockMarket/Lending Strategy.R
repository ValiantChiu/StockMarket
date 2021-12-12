library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)

s<-read.csv('Aggregation/analysis_info_S.csv') %>% as.tibble
m <- read.csv('Aggregation/analysis_info_M.csv') %>% as.tibble


all_stock<- c(s$`公司代號`, m$`公司代號`)

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
stock_list <- all_stock
GetInfo <- function(stock_list) {
    all <- tibble()
    for (i in stock_list) {
        print(i)
        one <- GetOneStockInfo(i) %>% mutate(stock = i)
        all <- rbind(one, all)
        Sys.sleep(sample(3:10, size = 1))
    }
    return(all)
}
saveRDS(all,file = 'Lending Strategy/all.rds')
all <- readRDS('Lending Strategy/all.rds')


ALL <- GetInfo(all_stock)
ALL <- all
ClearDate <- function(day) {
    paste0(
((day %>% substr(0, 3) %>% as.numeric) + 1911) %>% as.character, "-",
day %>% substr(5, 6), "-",
day %>% substr(8, 9))
}
ALL <- ALL %>% mutate(day = map(成交日期, ClearDate) %>% as.character, return_day = map(約定還券日期, ClearDate) %>% as.character)
write.csv(ALL %>% left_join(rbind(s, m) %>% select(公司代號, 產業類別) %>% unique %>% rename(stock= 公司代號)), 'Lending Strategy/All.csv')

ALL %>% arrange(day)