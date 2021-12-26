library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)
stock_price <- readRDS('00631L Strategy/stock_price.rds')

#KD Calculation
GetKD <- function(stock_price) {
    GetRSV <- function(C, D, n) {
        data <- stock_price %>% filter(day <= D) %>% arrange(desc(day)) %>% .[1:9,]
        H <- data$`程蔼基` %>% max
        L <- data$`程基` %>% min
        RSV <- (C - L) / (H - L) * 100
        RSV
    }
    stock_price_RSV <- stock_price %>% mutate(RSV = pmap(list(Μ絃基, day, 9), GetRSV) %>% as.numeric)
    stock_price_RSV <- stock_price_RSV %>% arrange(day)
    stock_price_RSV <- stock_price_RSV %>% filter(!is.na(RSV))
    k <- c()
    d <- c()
    for (i in 1:nrow(stock_price_RSV)) {
        #print(i)
        if (i == 1) {
            k[i] <- 50 * 2 / 3 + stock_price_RSV$RSV[i] / 3
            d[i] <- 50 * 2 / 3 + k[i] / 3
        } else {
            k[i] <- k[i - 1] * 2 / 3 + stock_price_RSV$RSV[i] / 3
            d[i] <- d[i - 1] * 2 / 3 + k[i] / 3
        }
    }
    stock_price_all <- stock_price_RSV %>% mutate(k, d)
}
stock_price_all <- GetKD(stock_price)
#write.csv(stock_price_all, file = '00631L Strategy/KD.csv')
#saveRDS(stock_price_all,file = '00631L Strategy/KD.rds' )

stock_price_all <- stock_price_all %>% rename(open = 秨絃基, highr = 程蔼基, low = 程基, close = Μ絃基) %>% select(-c(ら戳, yest_close, 害禴基畉, Θユ计, Θユ肂, Θユ掸计))

#tunning
k_buy_parameter <- 20
k_sell_parameter <- 80

GetActionHistory <- function(k_buy_parameter, k_sell_parameter) {
    buy_point <- stock_price_all %>% filter(k < k_buy_parameter)
    sell_point <- stock_price_all %>% filter(k > k_sell_parameter)
    GetBuySellInfo <- function(buy_time) {
        buy_info <- stock_price_all %>% filter(day > buy_time) %>% arrange(day) %>% .[1,] %>% select(-c(そ腹)) %>% rename_all(~paste0(., '_buy'))
        sell_info <- sell_point %>% filter(day > buy_time) %>% arrange(day) %>% .[1,] %>% select(-c(そ腹)) %>% rename_all(~paste0(., '_sell'))
        cbind(buy_info, sell_info) %>% as.tibble
    }
    buy_sell_info <- buy_point %>% mutate(buysellinfo = map(day, GetBuySellInfo)) %>% unnest
    buy_sell_info <- buy_sell_info %>% group_by(day_sell) %>% filter(day_buy == min(day_buy))


    profit_buy_info <- buy_sell_info %>% ungroup %>% filter(open_sell >= open_buy)
    non_profit_buy <- buy_sell_info %>% ungroup %>% filter(open_sell < open_buy) %>% select(そ腹, open, highr, low, close, day, RSV, k, d)

    GetNonProfitBuySellInfo <- function(buy_time) {
        buy_info <- stock_price_all %>% filter(day > buy_time) %>% arrange(day) %>% .[1,] %>% select(-c(そ腹)) %>% rename_all(~paste0(., '_buy'))
        buy_price <- buy_info$open_buy[1]
        sell_info <- sell_point %>% filter(day > buy_time) %>% arrange(day) %>% .[1,] %>% select(-c(そ腹)) %>% rename_all(~paste0(., '_sell'))
        sell_day <- sell_info$day_sell[1]
        sell_info <- stock_price_all %>% filter(day > sell_day & open > buy_price) %>% arrange(day) %>% .[1,] %>% select(-c(そ腹)) %>% rename_all(~paste0(., '_sell'))
        cbind(buy_info, sell_info) %>% as.tibble
    }
    non_profit_buy_info <- non_profit_buy %>% mutate(buysellinfo = map(day, GetNonProfitBuySellInfo)) %>% unnest
    buysellinfo <- rbind(profit_buy_info, non_profit_buy_info) %>% arrange(day)

    FilterDay <- function(buysellinfo) {
        filter_day <- tibble()
        for (i in 1:nrow(buysellinfo)) {
            buy_day <- buysellinfo$day_buy[i]
            sell_day <- buysellinfo$day_sell[i]
            one_filter_day <- buysellinfo %>% filter(day_buy > buy_day & day_sell < sell_day) %>% select(day_buy)
            filter_day <- rbind(filter_day, one_filter_day)
        }
        filter_day
    }
    filter_list <- FilterDay(buysellinfo)
    action_history <- buysellinfo %>% filter(!(day_buy %in% filter_list$day_buy))
    action_history <- action_history %>% mutate(profit = (open_sell - open_buy) / open_buy)
    action_history$profit %>% sum
}


write.csv(action_history, file = '00631L Strategy/buysellinfo.csv')

#Day Trade

SL <- 0.2 / 100
SP <- 0.3 / 100
GetTotalProfit <- function(SL, SP) {
    Tf <- (0.1425 + 0.1) / 100
    GetProfit <- function(O, H, L, C) {
        if (
            ((O - L) / O + Tf) >= SL
            ) {
            -SL
        }
        else if (
            ((H - O) / O - Tf) >= SP
            ) {
            SP
        } else {
            (C - O) / O - Tf
        }
    }
    if (T) {
        GetProfit <- function(O, H, L, C) {
            if (
                ((H - O) / O - Tf) >= SP
                ) {
                SP
            }
            else if (
                ((O - L) / O + Tf) >= SL
                ) {
                -SL
            } else {
                (C - O) / O - Tf
            }
        }
    }

    profit_result <- stock_price %>% mutate(profit = pmap(list(秨絃基, 程蔼基, 程基, Μ絃基), GetProfit) %>% as.numeric)
    profit_result$profit %>% sum
}


SL_ <- tibble(SL = seq(0.003, 0.03, by = 0.001))
SP_ <- tibble(SP = seq(0.003, 0.03, by = 0.001))
SL_SP_table <- SL_ %>% mutate(sp = list(SP_)) %>% unnest

SL_SP_table %>% mutate(Total_Porift = map2(SL, SP, GetTotalProfit) %>% as.numeric) %>% arrange(desc(Total_Porift))
