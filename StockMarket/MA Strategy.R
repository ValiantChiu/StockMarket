library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)
PriceAll <- readRDS(file = "PriceAll_201001_201811.rds")
Price202012 <- readRDS(file = "PriceAll_202012.rds") %>% rename(data = price)


Price_2010_2020 <- rbind(PriceAll, Price202012) %>% unnest

Price_2010_2020_nest <- Price_2010_2020 %>% group_by(そ腹, そ嘿) %>% nest
deal_with_data <- function(data) {
    DealWithDate <- function(Price) {
        Price %>% mutate(BuyDate = str_split(ら戳, "/")) %>%
        mutate(year = as.integer(map(BuyDate, ~ .[1])) + 1911,
           month = as.character(map(BuyDate, ~ .[2])),
           day = as.character(map(BuyDate, ~ .[3]))) %>%
           mutate(Date = ymd(paste0(year, month, day))) %>%
           select(-year, - month, - day, - ら戳, - BuyDate)
    }
    data %>% unique %>% DealWithDate
}
Price_2010_2020_nest <- Price_2010_2020_nest %>% mutate(new_data = map(data, deal_with_data))

Price_2010_2020_nest <- Price_2010_2020_nest %>% select(-data)

one_price <- Price_2010_2020_nest$new_data[[1]]
GetMA <- function(price) {
    price %>% arrange(Date) %>% filter(!is.na(Μ絃基)) %>%
    mutate(MA5 = SMA(as.numeric(Μ絃基), n = 5), MA20 = SMA(as.numeric(Μ絃基), n = 20), MA50 = SMA(as.numeric(Μ絃基), n = 50)) %>%
    filter(complete.cases(.)) %>% mutate(diff_ratio_1 = abs((MA50 - MA5) / Μ絃基), diff_ratio_2 = abs((MA50 - MA20) / Μ絃基))
}

one_price %>% arrange(Date) %>% filter(!is.na(Μ絃基)) %>%
    mutate(MA5 = SMA(as.numeric(Μ絃基), n = 5), MA20 = SMA(as.numeric(Μ絃基), n = 20), MA50 = SMA(as.numeric(Μ絃基), n = 50)) %>%
    filter(complete.cases(.)) %>% mutate(diff_ratio_1 = abs((MA50 - MA5) / Μ絃基), diff_ratio_2 = abs((MA50 - MA20) / Μ絃基)) %>% filter(diff_ratio_1 <= 0.005 & diff_ratio_2 <= 0.005) %>% View


Price_2010_2020_result <- Price_2010_2020_nest %>% mutate(MA_result = map(new_data, GetMA))

Price_2010_2020_result <- Price_2010_2020_result %>% select(-new_data) %>% unnest

write.csv(Price_2010_2020_result, file = 'MA_Strategy.csv')



Price <- Price202012 %>% unnest

Price %>% filter(Μ絃基 < 5)


Price_2010_2020_nest 