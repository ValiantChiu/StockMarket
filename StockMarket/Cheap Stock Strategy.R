library(tidyverse)
library(modelr)
library(magrittr)

stock_price <- readRDS('Stock Price/Price_2011_20210902.rds') %>% as.tibble

#decision Variable
lb <- 5
p_percent <- 0.4
ub <- lb * (1 + p_percent)


#data wrangle

stock_list <- stock_price %>% filter(ClosePrice < lb) %>% select(ㅍ쩻쩘많) %>% unique
target_stock <- stock_price %>% filter(ㅍ쩻쩘많 %in% stock_list$`ㅍ쩻쩘많`)
target_stock_nest <- target_stock %>% group_by(ㅍ쩻쩘많) %>% nest

#


one_stock <- target_stock_nest %>% filter(ㅍ쩻쩘많 == 3051) %>% .$data %>% .[[1]]
one_stock <- one_stock %>% select(day, ClosePrice)

price <- one_stock %>% arrange(day) %>% rename(day_r = day, ClosePrice_r = ClosePrice)
r_price <- price[1:nrow(price) - 1,]
r_1_price <- price[2:nrow(price),] %>% rename_all(~paste0(., '_1'))
price_info <- cbind(r_price, r_1_price) %>% as.tibble %>% select(-ClosePrice_r)




one_stock_label <- one_stock %>%
    mutate(type = ifelse(ClosePrice <= lb | ClosePrice >= ub, 1, 0)) %>%
    filter(type != 0) %>%
    mutate(type = ifelse(ClosePrice <= lb, 0, 1))


fake_day <- tibble(day = '2010-01-01',ClosePrice=100,type = 1)
one_stock_label <- one_stock_label %>% select(day, ClosePrice, type)
one_stock_label <- rbind(fake_day, one_stock_label) %>% arrange(day)
l_one_stock_label <- one_stock_label[1:nrow(one_stock_label) - 1,] %>% rename_all(~paste0(., '_l'))
r_one_stock_label <- one_stock_label[2:nrow(one_stock_label),] %>% rename_all(~paste0(., '_r'))
l_r_one_stock_label <- cbind(l_one_stock_label, r_one_stock_label) %>% as.tibble %>% filter(type_l != type_r)

first_in_time <- l_r_one_stock_label %>% filter(type_r == 0) %>% .$day_r %>% min
r_one_stock_label_in_out <- l_r_one_stock_label %>% filter(day_r >= first_in_time) %>% select(day_r, ClosePrice_r, type_r)
r_one_stock_label_in_out <- r_one_stock_label_in_out %>% left_join(price_info) %>% select(day_r_1, ClosePrice_r_1, type_r)

r_one_stock_label_in <- r_one_stock_label_in_out[1:nrow(r_one_stock_label_in_out) - 1,]
r_one_stock_label_out <- r_one_stock_label_in_out[2:nrow(r_one_stock_label_in_out),] %>% rename_all(~paste0(., '_r'))

cbind(r_one_stock_label_in, r_one_stock_label_out) %>% filter(type_r==0)