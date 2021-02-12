


write.csv(PriceAll_201001_201811.rds)

PriceData <- readRDS(file = "PriceAll_201001_201811.rds")

PriceAll<-PriceData %>% unnest%>% DealWithDate
ShortLongResult %<>% mutate(Date = Date %>% ymd)

ShortAll <- ShortLongResult %>% rename(公司代號 = 股票代號) %>% left_join(PriceAll)

write.csv(ShortAll, file = 'ShortAll.csv')

ShortAll <- read.csv(file = "ShortAll.csv")

ShortAll <- ShortAll %>% as.tibble %>% filter(complete.cases(.))