library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)
library(TTR)

data <- readRDS("Price005051_2010_2020.rds") 
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
Price <- data$price[[1]] %>% deal_with_data %>% arrange(Date)

KDNew <- (Price %>% arrange((Date)) %>% select(程蔼基, 程基, Μ絃基) %>% rename(High = 程蔼基, Low = 程基, Close = Μ絃基))  %>% stoch(., nFastK = 9, nFastD = 3, nSlowD = 3, maType = list(list(SMA), list(EMA, wilder = TRUE), list(SMA))) #%>% stoch(., nFastK = 9)

Price <- Price %>% mutate(fastK = KDNew[, 1], fastD = KDNew[, 2], slowD = KDNew[, 3])
Price <- Price %>% filter(complete.cases(.))
data <- Price
Get_Signal <- function(data, k_criterion,D_criterion = 0) {
    data %>% filter(fastK <= k_criterion)
}
Get_Signal(Price, 0.2) %>% arrange(desc(Date))