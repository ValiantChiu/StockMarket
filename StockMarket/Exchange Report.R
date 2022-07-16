library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)
library(parallel)
library(doParallel)

#read old data
all_data <- readRDS('exchangeReport/exchangeReport_20141122_20210205.rds')

#write.csv(all_data %>% unnest, file = 'exchangeReport/exchangeReport_20141122_20210205.csv')
#get exchange report

GetOneDayFinancing <- function(Date) {
    print(Date)
    Purl <- paste0("https://www.twse.com.tw/exchangeReport/MI_MARGN?response=json&date=", Date, "&selectType=0099P")
    Report <- POST(Purl)
    Report <- Report %>% content(as = "text") %>% fromJSON
    ReportData <- Report$data %>% as.tibble
    if (nrow(ReportData) != 0) {
        fields <- Report$fields
        groups <- Report$groups
        groups <- groups %>% arrange(start)
        GroupFields <- function(fields, groups) {
            head_title <- fields[1:groups$start[1]]
            financing_title <- fields[(groups$start[1] + 1):(groups$start[1] + groups$span[1])] %>% paste0("_", groups$title[1])
            short_title <- fields[(groups$start[2] + 1):(groups$start[2] + groups$span[2])] %>% paste0("_", groups$title[2])
            c(head_title, financing_title, short_title)
        }

        header <- GroupFields(fields, groups)
        ReportData <- ReportData[, 1:(groups$start[2] + groups$span[2])]
        names(ReportData) <- header
        GetNumericReport <- function(Report) {

            for (i in 1:nrow(Report)) {
                Report[i,] <- as.numeric(gsub(pattern = ",", replacement = "", Report[i,]))
            }
            return(Report)
        }
        ReportNumericData <- ReportData[, (groups$start[1] + 1):(groups$start[2] + groups$span[2])] %>% GetNumericReport %>% mutate_all(as.numeric)
        ReportKeyData <- ReportData[, 1:(groups$start[1])]
        Sys.sleep(sample(10:20, size = 1))
        cbind(ReportKeyData, ReportNumericData) %>% as.tibble
    } else {
        Sys.sleep(sample(10:16, size = 1))
        tibble()
    }

}

all_days <- tibble()
for (i in 0:3900) {
    day <- Sys.time() %>% date - i
    all_days <- rbind(all_days, tibble(day))
}
all_days <- all_days %>% mutate(date = format(day, '%Y%m%d')) 
all_days_new <- all_days %>% filter(day > all_data %>% arrange(desc(day)) %>% .$day %>% .[1]) %>% mutate(data = map(date, GetOneDayFinancing))


all_data <- rbind(all_data, all_days_new)


all_days %>% filter(day > all_data %>% arrange(desc(day)) %>% .$day %>% .[1])
all_data


