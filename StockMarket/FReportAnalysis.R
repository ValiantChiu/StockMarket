library(RODBC)
library(tidyverse)
library(modelr)
library(quantmod)
Sys.setlocale("LC_ALL", "C")
Sys.setlocale("LC_ALL", "cht")
LOCAL <- odbcConnect("LOCAL")
CleanData <- function(df) {
    df %>% map(~as.numeric(gsub(pattern = ",", replacement = "", .))) %>% as.tibble
}


#Data Preparation 
IS1Sql <- "Select * from [master].[dbo].[IS1]"
IS2Sql <- "Select * from [master].[dbo].[IS2]"
IS3Sql <- "Select * from [master].[dbo].[IS3]"
IS4Sql <- "Select * from [master].[dbo].[IS4]"
IS5Sql <- "Select * from [master].[dbo].[IS5]"
IS6Sql <- "Select * from [master].[dbo].[IS6]"

BS1Sql <- "Select * from [master].[dbo].[BS1]"
BS2Sql <- "Select * from [master].[dbo].[BS2]"
BS3Sql <- "Select * from [master].[dbo].[BS3]"
BS4Sql <- "Select * from [master].[dbo].[BS4]"
BS5Sql <- "Select * from [master].[dbo].[BS5]"
BS6Sql <- "Select * from [master].[dbo].[BS6]"

IS1 <- LOCAL %>% sqlQuery(IS1Sql) %>% as.tibble
IS2 <- LOCAL %>% sqlQuery(IS2Sql) %>% as.tibble
IS3 <- LOCAL %>% sqlQuery(IS3Sql) %>% as.tibble
IS4 <- LOCAL %>% sqlQuery(IS4Sql) %>% as.tibble
IS5 <- LOCAL %>% sqlQuery(IS5Sql) %>% as.tibble
IS6 <- LOCAL %>% sqlQuery(IS6Sql) %>% as.tibble

BS1 <- LOCAL %>% sqlQuery(BS1Sql) %>% as.tibble
BS2 <- LOCAL %>% sqlQuery(BS2Sql) %>% as.tibble
BS3 <- LOCAL %>% sqlQuery(BS3Sql) %>% as.tibble
BS4 <- LOCAL %>% sqlQuery(BS4Sql) %>% as.tibble
BS5 <- LOCAL %>% sqlQuery(BS5Sql) %>% as.tibble
BS6 <- LOCAL %>% sqlQuery(BS6Sql) %>% as.tibble


BS1<-BS1 %>% select("公司代號", "現金及約當現金", "應收款項淨額", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值", "year", "season") %>% CleanData
BS2 <- BS2 %>% select("公司代號", "流動資產", "非流動資產", "資產合計", "流動負債", "負債合計", "股本", "權益合計", "每股參考淨值", "year", "season") %>% CleanData
BS3 <- BS3 %>% select("公司代號", "流動資產", "非流動資產", "資產總額", "流動負債", "負債總額", "股本", "權益總額", "每股參考淨值", "year", "season") %>% CleanData
BS4 <- BS4 %>% select("公司代號", "現金及約當現金", "應收款項淨額", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值", "year", "season") %>% CleanData
BS5 <- BS5 %>% select("公司代號", "現金及約當現金", "應收款項", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值", "year", "season") %>% CleanData
BS6 <- BS6 %>% select("公司代號", "流動資產", "非流動資產", "資產總額", "流動負債", "負債總額", "股本", "權益總額", "每股參考淨值", "year", "season") %>% CleanData

IS1 <- IS1 %>% select("公司代號", "利息淨收益", "利息以外淨損益", "營業費用", "繼續營業單位本期稅後淨利淨損", "其他綜合損益稅後", "本期綜合損益總額稅後", "基本每股盈餘元", "year", "season") %>% CleanData
IS2 <- IS2 %>% select("公司代號", "收益", "支出及費用", "營業外損益", "本期其他綜合損益稅後淨額", "本期綜合損益總額", "基本每股盈餘元", "year", "season") %>% CleanData
IS3 <- IS3 %>% select("公司代號", "營業收入", "營業成本", "營業費用", "營業外收入及支出", "繼續營業單位本期淨利淨損", "其他綜合損益淨額", "本期綜合損益總額", "基本每股盈餘元", "year", "season") %>% CleanData
IS4 <- IS4 %>% select("公司代號", "利息淨收益", "利息以外淨收益", "營業費用", "本期稅後淨利淨損", "本期其他綜合損益稅後淨額", "本期綜合損益總額", "基本每股盈餘元", "year", "season") %>% CleanData
IS5 <- IS5 %>% select("公司代號", "營業收入", "營業成本", "營業費用", "營業外收入及支出", "本期淨利淨損", "其他綜合損益稅後淨額", "本期綜合損益總額", "基本每股盈餘元", "year", "season") %>% CleanData
IS6 <- IS6 %>% select("公司代號", "收入", "支出", "本期淨利淨損", "其他綜合損益", "本期綜合損益總額", "基本每股盈餘元", "year","season") %>% CleanData


CFSql<-"select * from [master].[dbo].[CF_All] "
CF <- LOCAL %>% sqlQuery(CFSql) %>% CleanData %>% unique
CF <- CF %>% mutate(自由現金流 = 營業活動之淨現金流入流出 + 投資活動之淨現金流入流出 + 籌資活動之淨現金流入流出) %>% rename(公司代號 = company)

#Aggregate

FinacialReport1 <- BS1 %>% inner_join(IS1) %>% inner_join(CF)
FinacialReport1Result <- FinacialReport1 %>% mutate(CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額稅後 / 權益總額,
                           CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額稅後 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year, CoOE, ROE, CfOE, 本益比法, K值法, 負債比例, 權益總額)

FinacialReport2 <- BS2 %>% inner_join(IS2) %>% inner_join(CF)
FinacialReport2Result <- FinacialReport2 %>% mutate(CoOE = 營業活動之淨現金流入流出 / 權益合計,
                           ROE = 本期綜合損益總額 / 權益合計,
                           CfOE = 自由現金流 / 權益合計,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益合計 / 0.08),
                           負債比例 = 負債合計 / 資產合計,
                           權益總額 = 權益合計) %>% select(公司代號, year, CoOE, ROE, CfOE, 本益比法, K值法, 負債比例, 權益總額)


FinacialReport3 <- BS3 %>% inner_join(IS3) %>% inner_join(CF)
FinacialReport3Result<-FinacialReport3 %>% mutate(CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
                           CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year, CoOE, ROE, CfOE, 本益比法, K值法, 負債比例, 權益總額)

FinacialReport4 <- BS4 %>% inner_join(IS4) %>% inner_join(CF)
FinacialReport4Result <- FinacialReport4 %>% mutate(CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
                           CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year, CoOE, ROE, CfOE, 本益比法, K值法, 負債比例, 權益總額)


FinacialReport5 <- BS5 %>% inner_join(IS5) %>% inner_join(CF)
FinacialReport5Result <- FinacialReport5 %>% mutate(CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
                           CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year, CoOE, ROE, CfOE, 本益比法, K值法, 負債比例, 權益總額)
FinacialReport6 <- BS6 %>% inner_join(IS6) %>% inner_join(CF)
FinacialReport6Result <- FinacialReport6 %>% mutate(CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
                           CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year, CoOE, ROE, CfOE, 本益比法, K值法, 負債比例, 權益總額)

FinacialReportResultAll <- rbind(FinacialReport1Result, FinacialReport2Result, FinacialReport3Result, FinacialReport4Result, FinacialReport5Result, FinacialReport6Result)

FinacialReportResultAll %>% arrange(公司代號)
FinacialReportResultAll %>% arrange(desc(ROE))
sqlSave(LOCAL, FinacialReportResultAll, tablename = "FinacialReportResultAll", rownames = FALSE, append = TRUE)

FinacialReport3 %>% filter(公司代號==2939) %>% View
#post preparation
GetRateIndex <- function(df) {
    RateCoOE <- lm(CoOE ~ year, data = df) %>% .$coefficients %>% .[2] %>% as.numeric
    RateROE <- lm(ROE ~ year, data = df) %>% .$coefficients %>% .[2] %>% as.numeric
    RateCfOE <- lm(CfOE ~ year, data = df) %>% .$coefficients %>% .[2] %>% as.numeric
    #RateDividend <- lm(股東配發s盈餘分配之現金股利元股 ~ year, data = df) %>% .$coefficients %>% .[2] %>% as.numeric
    tibble(RateCoOE, RateROE, RateCfOE)
    #tibble(RateCoOE, RateROE, RateCfOE, RateDividend)
}
GetAverageIndex <- function(df) {
    AverageCoOE <- df$CoOE %>% mean
    AverageROE <- df$ROE %>% mean
    AverageCfOE <- df$CfOE %>% mean
    #AverageDividend <- df$股東配發s盈餘分配之現金股利元股 %>% mean
    StdCoOE <- df$CoOE %>% sd
    StdROE <- df$ROE %>% sd
    StdCfOE <- df$CfOE %>% sd
    #StdDividend <- df$股東配發s盈餘分配之現金股利元股 %>% sd
    #tibble(AverageCoOE, AverageROE, AverageCfOE, AverageDividend, StdCoOE, StdROE, StdCfOE, StdDividend)
    tibble(AverageCoOE, AverageROE, AverageCfOE, StdCoOE, StdROE, StdCfOE)
}

FinancialReport <- paste0("select * from [master].[dbo].[FinacialReportResultAll]")
DividendAll <- LOCAL %>% sqlQuery("select * from [master].[dbo].[DividendAll]") %>% as.tibble
DividendAll %>% select(公司代號, 股東配發s盈餘分配之現金股利元股, year) %>% View
Dividend <- DividendAll %>% select(公司代號, 股東配發s盈餘分配之現金股利元股, year) %>% mutate(公司代號 = as.double(as.character(公司代號)))

Dividend$`股東配發s盈餘分配之現金股利元股` <- Dividend$`股東配發s盈餘分配之現金股利元股`  %>%  as.character %>% as.numeric
FinacialReportResultAll <- LOCAL %>% sqlQuery(FinancialReport) %>% as.tibble %>% arrange(desc(year))
FinacialReportResultAll <- FinacialReportResultAll %>% left_join(Dividend) %>% filter(!is.na(股東配發s盈餘分配之現金股利元股))
FinacialReportResultPricing <- FinacialReportResultAll %>% group_by(公司代號) %>% summarise(本益比法mean = mean(本益比法), K值法mean = mean(K值法), 負債比例mean = mean(負債比例), 股利mean = mean(股東配發s盈餘分配之現金股利元股), 本益比法first = first(本益比法), K值法first = first(K值法), 負債比例first = first(負債比例), 股利first = first(股東配發s盈餘分配之現金股利元股))
#FinacialReportResultPricing <- FinacialReportResultAll %>% group_by(公司代號) %>% summarise(本益比法mean = mean(本益比法), K值法mean = mean(K值法), 負債比例mean = mean(負債比例), 本益比法first = first(本益比法), K值法first = first(K值法), 負債比例first = first(負債比例))
warnings()
FinacialReportResultAllNest <- FinacialReportResultAll %>% group_by(公司代號) %>% nest
FinacialReportResult %>% filter(公司代號 == 2020)
FinacialReportResultAll %>% filter(公司代號 == 3040)
FinacialReportResult <- FinacialReportResultAllNest %>% mutate(averageindex = map(data, GetAverageIndex),count=map(data,nrow))
FinacialReportResult <- FinacialReportResult %>% mutate(CoOERate = map(data, GetRateIndex))
FinacialReportResult <- FinacialReportResult %>% select(-data) %>% unnest
FinacialReportResult <- FinacialReportResult %>% left_join(FinacialReportResultPricing)


FinacialReportResult <- FinacialReportResult %>% mutate(weight=AverageROE*RateROE)
stocklist<-FinacialReportResult %>% filter(AverageCfOE > 0 & AverageROE > 0.15 & RateROE > 0) %>% arrange(desc(weight))
stocklist %>% View
saveRDS(stocklist, file = "GoodStockList.rds")
stocklist <- readRDS(file = "GoodStockList.rds")

FinacialReportResult %>% filter(公司代號==6128) %>% View

FinacialReportResult %>% filter(AverageROE >= 0.2 ) %>% ggplot() + geom_point(mapping = aes(x = RateROE, y = AverageROE, color = 公司代號))
stocklist <- FinacialReportResult %>% filter(AverageROE >= 0.2 & RateROE > 0) %>% arrange(desc(RateROE)) #%>% select(公司代號, AverageROE, RateROE)
stocklist <- FinacialReportResult %>% filter(AverageCfOE > 0 & AverageROE > 0.20 & count == 5 & 公司代號!=1256) %>% arrange(desc(AverageROE)) %>% .[1:20,]
GetStockPrice <- function(companylist) {
    StockAll <- tibble()
    Sys.sleep(sample(3:6, size = 1))

    for (i in 1:nrow(companylist)) {
        stock <- paste0(companylist$公司代號[i], ".TW")
        getSymbols(stock)
        OneStock <- get(stock) %>% last %>% as.tibble %>% .[, c(4, 5)]
        names(OneStock) <- c("ClosePrice", "Volume")
        OneStock <- OneStock %>% mutate(公司代號 = companylist$公司代號[i])
        StockAll <- rbind(StockAll, OneStock)
    }
    return(StockAll)
}
getSymbols("0050.TW")
Price<-GetStockPrice(stocklist)
stocklist <- stocklist %>% left_join(Price)
stocklist <- stocklist %>% mutate(ExpectedPrice = (本益比法mean + 本益比法first) / 2) %>% mutate(ExpectedReturn = (ExpectedPrice - ClosePrice) / ClosePrice)
stocklist <- stocklist %>% arrange(desc(ExpectedReturn)) %>% select(-本益比法mean, - K值法mean, - 負債比例mean, - 股利mean, - 本益比法first, - K值法first)


stocklist %>% filter(本益比法first>ClosePrice) %>% View
Result<-stocklist %>% mutate(DRatio = AverageDividend / ClosePrice, DRatiof = 股利first/ClosePrice) %>%  arrange(desc(DRatiof)) 
Result %>% arrange(desc(DRatiof)) %>% select(公司代號, count, 本益比法mean, K值法mean, 負債比例mean, ClosePrice, DRatiof, DRatio, AverageROE) %>% filter(ClosePrice < 本益比法mean)

Dividend %>% filter(公司代號 == '1536')


#Dividend Analysis
FinacialReportResultAll %>% left_join(Dividend) %>% mutate(DRatio = 股東配發s盈餘分配之現金股利元股 / 權益總額) %>% arrange(desc(DRatio)) %>% filter(公司代號 == 4952)
FinacialReportResultAll %>% left_join(Dividend) %>% mutate(DRatio = 股東配發s盈餘分配之現金股利元股 / 權益總額) %>% arrange(desc(DRatio)) %>% group_by(公司代號) %>% summarise(MDRatio = mean(DRatio)) %>% arrange(desc(MDRatio))


get('0050.TW')

#手續費 0.1425% * 0.65 ETF 0.1%
0.1425*0.65