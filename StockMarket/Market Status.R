library(lubridate)
library(tidyverse)
library(RODBC)
library(modelr)

LOCAL <- odbcConnect("LOCAL")
#Data Input
StockBestBuySell <- LOCAL %>% sqlQuery("Select * from [master].[dbo].[StockPriceResultAll2010_2017]") %>% as.tibble
StockBestBuySell<- StockBestBuySell %>% mutate(datebuy=ymd(datebuy),datesell=ymd(datesell))
StockBestBuySell <- StockBestBuySell %>% mutate(period = as.integer(datesell - datebuy),ROI=100*(profitfinal/pricebuy))

Industry <- read.csv(file = "C:/Users/user/AppData/Local/Temp/Industry.csv.utf8", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "")
Industry <- Industry %>% as.tibble
names(Industry) <- c("company", "companyC", "industry")
Industry <- Industry %>% map(~gsub(pattern = "'", replacement = "", .)) %>% as.tibble

StockBestBuySell <- StockBestBuySell %>% mutate(company=as.character(company))
StockBestBuySell <- StockBestBuySell %>% left_join(Industry)

# According to Indsutry Category to get best buy and best sell information within one year
IndustryCategory<-StockBestBuySell %>% filter(!is.na(industry)) %>% .$industry %>% unique
Result <- list()
i<-1
for (indus in IndustryCategory) {
    buy<-StockBestBuySell %>% filter(industry == indus) %>% .$datebuy %>% month %>% table
    sell <- StockBestBuySell %>% filter(industry == indus) %>% .$datesell %>% month %>% table
    Result[[i]] <- list(buy/sum(buy), sell/sum(sell))
    i<-i+1
}
Result[[1]][[1]] %>% plot


#Frequent High Return Stock 
ROIHighStock <- StockBestBuySell %>% filter(profitfinal / pricebuy > 0.5) %>% group_by(company,industry) %>% summarise(times = n()) %>% filter(times > 6)
StockBestBuySell %>% filter(company == 8105)
ROIHighStock$company[1]
# Description Statistic
StockBestBuySell %>% filter(company == ROIHighStock$company[1])
StockBestBuySell %>% filter(company == 4414)
StockBestBuySellSummary <- StockBestBuySell %>% group_by(company, companyC, industry) %>% summarise(ROIstd = sd(ROI), ROIMean = mean(ROI), periodstd = sd(period), periodmean = mean(period), times = n())
StockBestBuySellSummary <- StockBestBuySellSummary %>% arrange(desc(ROIMean)) %>% filter(ROIstd != Inf)
StockBestBuySellSummary %>% filter(times == 8) %>% ggplot() + geom_point(mapping = aes(x = ROIMean, y = ROIstd, color = industry))
StockBestBuySellSummary %>% filter(company==4952)

StockBestBuySellSummary %>% filter(times == 8) %>% filter(ROIMean > 200 & ROIMean<300)
StockBestBuySellSummary %>% filter(times == 8) %>% filter(ROIMean - 3 * ROIstd > 0) %>% ggplot() + geom_point(mapping = aes(x = ROIMean, y = ROIstd, color = industry))

look<-StockBestBuySellSummary %>% filter(times == 8) %>% filter(ROIMean - 3 * ROIstd > 0) %>% arrange(desc(ROIMean)) 

StockBestBuySellSummary %>% ungroup %>% select(-companyC, - industry) %>% as.data.frame

#Economic Index & stock Price
rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
}

LagIndex <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Market Status/20180401/Lagging20180401105517.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "")
PresentIndex <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Market Status/20180401/Present20180401105455.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "")
LeadIndex <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Market Status/20180401/Lead20180401105210.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "")

names(LagIndex) <- c("Date", "Value", "YOY") 
LagIndex <- LagIndex %>% .[,c(1,2,3)] %>% mutate(type = "Lag")
LagIndex$Date <- LagIndex$Date %>% paste0("-15") %>% ymd
LagIndex <- LagIndex %>% filter(Date > '2012-01-01')
LagIndex <- LagIndex %>% mutate(YOY = rescale01(YOY))


names(PresentIndex) <- c("Date", "Value", "YOY")
PresentIndex <- PresentIndex %>% .[, c(1, 2, 3)] %>% mutate(type = "Present")
PresentIndex$Date <- PresentIndex$Date %>% paste0("-15") %>% ymd
PresentIndex <- PresentIndex %>% filter(Date > '2012-01-01')
PresentIndex <- PresentIndex %>% mutate(YOY = rescale01(YOY))


names(LeadIndex) <- c("Date", "Value", "YOY")
LeadIndex <- LeadIndex %>% .[, c(1, 2, 3)] %>% mutate(type = "Lead")
LeadIndex$Date <- LeadIndex$Date %>% paste0("-15") %>% ymd
LeadIndex <- LeadIndex %>% filter(Date > '2012-01-01')
LeadIndex <- LeadIndex %>% mutate(YOY = rescale01(YOY))


AllIndex<-rbind(LagIndex,PresentIndex,LeadIndex)

#AllIndex$Date <- AllIndex$Date %>% paste0("-15") %>% ymd
AllIndex<-AllIndex %>% select(Date,YOY,type)
AllIndex %>% filter(Date > '2015-01-01') %>% ggplot() + geom_line(mapping = aes(x = Date, y = YOY, colour = type))
AllIndex %>% ggplot(mapping = aes(x = Date, y = YOY, colour = type)) + geom_line() + geom_point(size = 4)
#Bind the price with industry
Industry$company<-as.integer(Industry$company)
StockPrice <- StockPrice %>% left_join(Industry) #%>% filter(date>='2012-01-01')
StockPriceSumAll <- StockPrice %>% group_by(industry,date) %>% summarise(price = mean(price))
StockPriceSum <- StockPriceSumAll %>% ungroup %>% filter(industry == "¥ú¹q·~") %>% select(date, price)
StockPriceSumRescale <- StockPriceSum %>% filter(date < '2018-01-01') %>% mutate(price = rescale01(price)) #%>% ggplot() + geom_line(mapping = aes(x = date, y = price))
StockPriceSumRescale <- StockPriceSumRescale %>% mutate(type = "Stock")
names(StockPriceSumRescale)<-c("Date","Ratio","type")
names(AllIndex) <- c("Date", "Ratio", "type")

Result <- rbind(StockPriceSumRescale, AllIndex)
Result %>% ggplot() + geom_line(mapping = aes(x = Date, y = Ratio, colour = type))


#Model Analysis
StockPrice %>%
    ggplot(aes(date, price, group = company)) +
    geom_line(alpha = 1 / 3)

by_company <- StockPrice %>%
    group_by(company, industry) %>%
    nest()
company_model <- function(df) {
    lm(price ~ date, data = df)
}

by_company <- by_company %>%
    mutate(model = map(data, company_model))

by_company <- by_company %>%
    mutate(
    resids = map2(data, model, add_residuals)
  )

resids <- unnest(by_company, resids)

resids %>%
    ggplot(aes(date, resid)) +
    geom_line(aes(group = company), alpha = 1 / 3) +
    geom_smooth(se = FALSE)

resids %>% filter(industry!='?????????') %>%
    ggplot(aes(date, resid, group = company)) +
    geom_line(alpha = 1 / 3) +
    facet_wrap(~industry)

resids %>% 
  group_by(industry) %>% 
  summarise(minresid=min(resid),maxresid=max(resid),meanresid=mean(resid)) %>% 
  arrange(abs(meanresid)) %>% View()
by_company<-by_company %>% mutate(predict=map2(data,model,add_predictions))
by_company %>% filter(industry=='????????????' & company=='3413')%>% unnest(predict,resids) %>% ggplot()+geom_line(mapping = aes(x=date,y=pred,group=company))
by_company %>% filter(industry=='????????????' & company=='3413')%>% unnest(predict,resids) %>% ggplot()+geom_line(mapping = aes(x=date,y=resid,group=company))  
by_company %>% filter(industry=='????????????')

Industry$industry %>% unique

GrowthPrice<-by_company %>% filter(industry=='????????????' ) %>% mutate(glance = map(model, broom::tidy)) %>% unnest(glance) %>%  filter(term=='date' & estimate>0)
GrowthPrice %>% arrange(desc(estimate))

StockPrice$price %>% max
StockPrice %>% filter(date>'2017-09-01') %>%  View