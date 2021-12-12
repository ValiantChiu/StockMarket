library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)


ClearDate <- function(day) {
    paste0(
    ((day %>% substr(0, 3) %>% as.numeric) + 1911) %>% as.character, "-",
    day %>% substr(5, 6), "-",
    day %>% substr(8, 9))
}

stock_price <- readRDS(file = "Reverse Strategy/R_Stock_Price.rds")
wrangle_data <- function(one_stock) {
    one_stock <- one_stock %>% mutate(day = map(日期, ClearDate) %>% as.character) %>% arrange(day)
    today <- one_stock %>% .[2:(nrow(one_stock)),]
    today <- today %>% mutate(yest_close = one_stock$`收盤價`[1:(nrow(one_stock)-1)])
    today
}
stock_price <- stock_price %>% mutate(new_data = map(price, wrangle_data)) %>% select(-price) %>% unnest



write.csv(stock_price, file = 'Reverse Strategy/r_strategy.csv')

by_year <- stock_price %>% mutate(YEAR = year(day)) %>% group_by(公司代號, YEAR) %>% filter(day == max(day) | day == min(day)) %>% select(公司代號, 收盤價, YEAR) %>% mutate(index = c('First', 'Second')) %>% pivot_wider(names_from = index, values_from = c(收盤價))
write.csv(by_year, file = 'Reverse Strategy/r_strategy_by_year.csv')