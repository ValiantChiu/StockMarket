library(tidyverse)
library(lubridate)
TXData <- read.csv("TXF20110101~20191130(CrazyIndicator.pixnet.net).csv") %>% as.tibble %>% mutate(Date = as.character(Date), Time = as.character(Time)) %>% mutate(time = paste(Date, Time)) %>% mutate(hrs = hour(time))
TXData_day <- TXData %>% filter(hrs >= 8 & hrs <= 13)

TXData_day <- TXData_day %>% filter(Date == '2019/11/27' | Date == '2019/11/26' | Date == '2019/11/25')
TXData_day <- TXData_day %>% mutate(k_interval = floor_date((time %>% as.POSIXct) - 46 * 60, unit = "30 minutes"))
TXData_day <- TXData_day %>% mutate(k_hour = as.character(hms::as.hms(k_interval))) %>% filter(k_hour == '08:00:00' | k_hour == '12:30:00') 
kmax_kmin <- TXData_day %>% group_by(k_interval) %>% summarise(k_max = max(High), k_min = min(Low))
kclose <- TXData_day %>% group_by(k_interval) %>% filter(time == max(time)) %>% select(time, Close, k_interval, k_hour) %>% ungroup

yesterday <- kclose %>% left_join(kmax_kmin) %>% select(time, k_interval, k_hour, k_max, k_min, Close) %>% .[1:(nrow(.)-1),]
today <- kclose %>% left_join(kmax_kmin) %>% select(time, k_interval, k_hour, k_max, k_min, Close) %>% .[2:nrow(.),]
cbind(yesterday,today)