library(tidyverse)
library(modelr)
library(magrittr)
library(readxl)

Revenue <- read_excel("Price Estimation/Revenue.xlsx")
#saveRDS(Dividend_s, file = 'Price Estimation/Dividend_s.rds')
#saveRDS(Dividend, file = 'Price Estimation/Dividend.rds')
Dividend_s <- readRDS('Price Estimation/Dividend_s.rds')
Dividend <- readRDS('Price Estimation/Dividend.rds')
analysis_info_M <- read.csv("Aggregation/analysis_info_M.csv") %>% as.tibble
analysis_info_S <- read.csv("Aggregation/analysis_info_S.csv") %>% as.tibble
price <- rbind(analysis_info_M, analysis_info_S) %>% select(公司代號, ClosePrice)

Dividend <-rbind(Dividend, Dividend_s)


last_record <- Revenue %>% mutate(Stock = as.character(Stock)) %>% left_join(Dividend, by = c('Stock' = '公司代號', 'year_t' = 'year')) %>% filter(year_w %in% c(2020, 2021))

last_record <- last_record %>% group_by(Stock) %>% filter(n() == 2)
last_record <- last_record %>% nest


EstimatePrice <- function(data) {
    data <- data %>% arrange(year_w)
    price_d<- (data$Revenue[2] / data$Revenue[1]) * data$`現金股利元股`[1]/0.06
    price_d
}

last_record <- last_record %>% mutate(price_d = map(data, EstimatePrice) %>% as.numeric)

price_estimation <- last_record %>% left_join(price %>% mutate(公司代號 = as.character(公司代號)), by = c('Stock' = '公司代號')) %>% select(-data)

write.csv(price_estimation,file ='Price Estimation/price_estimation.csv')