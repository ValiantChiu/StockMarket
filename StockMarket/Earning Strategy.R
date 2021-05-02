library(tidyverse)
library(modelr)
library(magrittr)
library(quantmod)
library(lubridate)


GetER <- function(file_dir) {
    deal_with_data <- function(data) {
        DealWithDate <- function(Price) {
            Price %>% mutate(BuyDate = str_split(日期, "/")) %>%
            mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
            mutate(Date = ymd(paste0(year, month, day))) %>%
            select(-year, - month, - day, - 日期, - BuyDate)
        }
        data %>% rename(日期 = 出表日期) %>% DealWithDate
    }
    file_list <- list.files(path = paste0("Monthly Revenue/", file_dir, ""), pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
    all_revenue_data <- tibble()
    for (file_name in file_list) {
        all_revenue_data <- rbind(all_revenue_data, read_csv(paste0("Monthly Revenue/", file_dir, "/", file_name, "")))
    }
    all_revenue_data %>% deal_with_data %>% group_by(公司代號) %>% filter(Date == min(Date)) %>% unique %>% ungroup

}
ER2<- GetER('110/3') 

start_date <- ER2 %>% filter(公司代號 == 6271) %>% select(Date) %>% .$Date
公司代號 <- 6271
n <- 15

GetStockPrice <- function(公司代號,start_date,n) {

    print(公司代號)
    stock <- paste0(公司代號, ".TW")
    start_date <- start_date + 1  #1828
    end_date <- start_date + 40 #-40
    tryCatch(getSymbols(stock, from = start_date, to = end_date), error = function(e) e, finally = 1)
    Date <- get(stock) %>% as.data.frame %>% row.names
    OneStock <- get(stock) %>% as.tibble
    names(OneStock) <- c("Open", "High", "Low", "ClosePrice", "Volume", "Adjusted")
    OneStock <- OneStock %>% mutate(day = Date)    
    OneStock <- OneStock %>% arrange(day)
    price_d <- OneStock[1,]$ClosePrice
    price_d_n <- OneStock[2:n,]$High %>% max
    
    #Sys.sleep(sample(1:1, size = 1))
    tibble(price_d, price_d_n,n)
}

ER2_1 <- ER2[1:500,] %>% mutate(label = pmap(list(公司代號, Date, 15), GetStockPrice)) 
ER2_2 <- ER2[501:700,] %>% mutate(label = pmap(list(公司代號, Date, 15), GetStockPrice))
ER2_3 <- ER2[701:850,] %>% mutate(label = pmap(list(公司代號, Date, 15), GetStockPrice))
ER2_4 <- ER2[851:944,] %>% mutate(label = pmap(list(公司代號, Date, 15), GetStockPrice))

ER2_all <- rbind(ER2_1, ER2_2, ER2_3, ER2_4) %>% unnest

write.csv(ER2_all,file ='ER/ER_3_2021.csv')