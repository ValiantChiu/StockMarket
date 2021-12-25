library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)
stock_price <- readRDS('00631L Strategy/stock_price.rds')


SL <- 0.2 / 100
SP <- 0.3 / 100
GetTotalProfit <- function(SL,SP) {
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


SL_<- tibble(SL = seq(0.003, 0.03, by = 0.001))
SP_ <- tibble(SP = seq(0.003, 0.03, by = 0.001))
SL_SP_table <- SL_ %>% mutate(sp = list(SP_)) %>% unnest

SL_SP_table %>% mutate(Total_Porift = map2(SL, SP, GetTotalProfit) %>% as.numeric) %>% arrange(desc(Total_Porift))




GetRSV <- function(C, D, n) {
    data <- stock_price %>% filter(day <= D) %>% arrange(desc(day)) %>% .[1:9,]
    H <- data$`程蔼基` %>% max
    L <- data$`程基` %>% min
    RSV <- (C - L) / (H - L) * 100
    RSV
}
stock_price_RSV <- stock_price %>% mutate(RSV = pmap(list(Μ絃基, day, 9), GetRSV) %>% as.numeric)

stock_price_RSV <- stock_price_RSV  %>% arrange(day)

stock_price_RSV <- stock_price_RSV %>% filter(!is.na(RSV))
k <- c()
d <- c()
for (i in 1:nrow(stock_price_RSV)) {
    print(i)
    if (i == 1) {
        k[i] <- 50 * 2 / 3 + stock_price_RSV$RSV[i] / 3
        d[i] <- 50 * 2 / 3 + k[i] / 3
    } else {
        k[i] <- k[i-1] * 2 / 3 + stock_price_RSV$RSV[i] / 3
        d[i] <- d[i-1] * 2 / 3 + k[i] / 3
    }
}
stock_price_all <- stock_price_RSV %>% mutate(k, d)
write.csv(stock_price_all, file = '00631L Strategy/KD.csv')
stock_price_all %>% View