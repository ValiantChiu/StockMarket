library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
Sys.setlocale("LC_ALL", "cht")

GetShortLongInfo <- function(PreviousPeriod) {
    GetOneDayInfo <- function(Date) {
        Purl <- paste0("https://www.twse.com.tw/exchangeReport/TWT93U?response=json&date=", Date, "")
        Info <- POST(Purl)
        Info <- Info %>% content(as = "text") %>% fromJSON
        InfoData <- Info$data %>% as.tibble
        groupInfo <- Info$groups %>% as.tibble
        fields <- Info$fields
        GetTitle <- function(groupInfo, fields) {
            Title <- c()
            for (i in 1:nrow(groupInfo)) {
                title <- groupInfo[i,]$title
                Start <- groupInfo[i,]$start + 1
                End <- groupInfo[i,]$span
                Title <- c(Title, fields[Start:(Start + End - 1)] %>% paste0("_", title))
            }
            c(fields[1:groupInfo[1,]$start], Title)
        }
        Title <- GetTitle(groupInfo, fields)
        InfoData <- InfoData[, 1:length(Title)]
        names(InfoData) <- Title
        InfoData %>% mutate_all(~gsub(., pattern = ",", replacement = "")) %>% mutate(Date)
    }
    ShortLongInfo <- tibble()
    for (i in 0:PreviousPeriod) {
        Date <- (Sys.time() %>% date - i) %>% format(., '%Y%m%d')
        shortlonginfo <- tibble()
        tryCatch(shortlonginfo <- GetOneDayInfo(Date), error = function(e) { print(paste0(e)) }, finally = print("Hello"))
        if (nrow(shortlonginfo) != 0) {
            ShortLongInfo <- rbind(ShortLongInfo, shortlonginfo)
        }
        Sys.sleep(sample(4:8, size = 1))
    }
    ShortLongInfo
}
ShortLongResult <- GetShortLongInfo(365 * 7)

#saveRDS(ShortLongResult, file = "ShortLongResult.rdata")
ShortLongResult <- readRDS(file = "ShortLongResult.rdata")
Short00631L <- ShortLongResult %>% filter(股票代號 == '00631L')
Short4807<-ShortInfo %>% filter(股票代號 == '4807') 

GetOneDayInfo <- function(Date) {
    Purl <- paste0("https://www.twse.com.tw/exchangeReport/TWT93U?response=json&date=", Date, "")
    Info <- POST(Purl)
    Info <- Info %>% content(as = "text") %>% fromJSON
    InfoData <- Info$data %>% as.tibble
    groupInfo <- Info$groups %>% as.tibble
    fields <- Info$fields
    GetTitle <- function(groupInfo, fields) {
        Title <- c()
        for (i in 1:nrow(groupInfo)) {
            title <- groupInfo[i,]$title
            Start <- groupInfo[i,]$start + 1
            End <- groupInfo[i,]$span
            Title <- c(Title, fields[Start:(Start + End - 1)] %>% paste0("_", title))
        }
        c(fields[1:groupInfo[1,]$start], Title)
    }
    Title <- GetTitle(groupInfo, fields)
    InfoData <- InfoData[, 1:length(Title)]
    names(InfoData) <- Title
    InfoData %>% mutate_all(~gsub(., pattern = ",", replacement = ""))
}
Date <- (Sys.time() %>% date - 365 * 7) %>% format(., '%Y%m%d')
GetOneDayInfo(Date) %>% names
0:1

GetShortLongInfo(2)

Short00631l<-ShortLongResult %>% filter(股票代號 == '00631L')
Price00631L
Short00631l %<>% mutate(Date = Date %>% ymd)

?lubridate
Short00631l <- PriceKD %>% left_join(Short00631l)
write.csv(Short00631l, file = 'Short00631l.csv')

Short00631L %>% arrange(desc(Date)) %>% select(Date)
Short4807 %<>% mutate(Date = Date %>% ymd)

Short4807 <- Price4807 %>% left_join(Short4807)

write.csv(Short4807, file = 'Short4807.csv')


