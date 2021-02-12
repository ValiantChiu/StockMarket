library(RODBC)
library(tidyverse)
library(modelr)
library(magrittr)
library(quantmod)
Sys.setlocale("LC_ALL", "C")
Sys.setlocale("LC_ALL", "cht")

CleanData <- function(df) {
    df %>% map(~as.numeric(gsub(pattern = ",", replacement = "", .))) %>% as.tibble
}


#Data Preparation 
BS1 <- BS[[1]] %>% select("公司代號", "現金及約當現金", "應收款項－淨額", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值", "year", "season") %>% CleanData
BS2 <- BS[[2]] %>% select("公司代號", "流動資產", "非流動資產", "資產合計", "流動負債", "負債合計", "股本", "權益合計", "每股參考淨值", "year", "season") %>% CleanData
BS3 <- BS[[3]] %>% select("公司代號", "流動資產", "非流動資產", "資產總額", "流動負債", "負債總額", "股本", "權益總額", "每股參考淨值", "year", "season") %>% CleanData
BS4 <- BS[[4]] %>% select("公司代號", "現金及約當現金", "應收款項－淨額", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值", "year", "season") %>% CleanData
BS5 <- BS[[5]] %>% select("公司代號", "現金及約當現金", "應收款項", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值", "year", "season") %>% CleanData
BS6 <- BS[[6]] %>% select("公司代號", "流動資產", "非流動資產", "資產總額", "流動負債", "負債總額", "股本", "權益總額", "每股參考淨值", "year", "season") %>% CleanData

IS1 <- IS[[1]] %>% select("公司代號", "利息淨收益", "利息以外淨損益", "營業費用", "繼續營業單位本期稅後淨利淨損", "其他綜合損益稅後", "本期綜合損益總額稅後", "基本每股盈餘元", "year", "season") %>% CleanData
IS2 <- IS[[2]] %>% select("公司代號", "收益", "支出及費用", "營業外損益", "本期其他綜合損益稅後淨額", "本期綜合損益總額", "基本每股盈餘元", "year", "season") %>% CleanData
IS3 <- IS[[3]] %>% select("公司代號", "營業收入", "營業成本", "營業費用", "營業外收入及支出", "繼續營業單位本期淨利淨損", "其他綜合損益淨額", "本期綜合損益總額", "基本每股盈餘元", "year", "season") %>% CleanData
IS4 <- IS[[4]] %>% select("公司代號", "利息淨收益", "利息以外淨收益", "營業費用", "本期稅後淨利淨損", "本期其他綜合損益稅後淨額", "本期綜合損益總額", "基本每股盈餘元", "year", "season") %>% CleanData
IS5 <- IS[[5]] %>% select("公司代號", "營業收入", "營業成本", "營業費用", "營業外收入及支出", "本期淨利淨損", "其他綜合損益稅後淨額", "本期綜合損益總額", "基本每股盈餘元", "year", "season") %>% CleanData
IS6 <- IS[[6]] %>% select("公司代號", "收入", "支出", "本期淨利淨損", "其他綜合損益", "本期綜合損益總額", "基本每股盈餘元", "year", "season") %>% CleanData


#Aggregate

FinacialReport1 <- BS1 %>% inner_join(IS1) %>% filter(權益總額>0)
FinacialReport1Result <- FinacialReport1 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額稅後 / 權益總額,
                           #CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額稅後 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year, ROE, 本益比法, K值法, 負債比例,  權益總額)

FinacialReport2 <- BS2 %>% inner_join(IS2) %>% filter(權益合計 > 0)
FinacialReport2Result <- FinacialReport2 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益合計,
                           ROE = 本期綜合損益總額 / 權益合計,
                           #CfOE = 自由現金流 / 權益合計,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益合計 / 0.08),
                           負債比例 = 負債合計 / 資產合計,
                           權益總額 = 權益合計) %>% select(公司代號, year,  ROE,  本益比法, K值法, 負債比例, 權益總額)

FinacialReport3 <- BS3 %>% inner_join(IS3) %>% filter(權益總額 > 0)
FinacialReport3Result <- FinacialReport3 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
                           #CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year,  ROE,  本益比法, K值法, 負債比例, 權益總額)

FinacialReport4 <- BS4 %>% inner_join(IS4) %>% filter(權益總額 > 0)
FinacialReport4Result <- FinacialReport4 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
                           #CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year,  ROE,  本益比法, K值法, 負債比例, 權益總額)

FinacialReport5 <- BS5 %>% inner_join(IS5) %>% filter(權益總額 > 0)
FinacialReport5Result <- FinacialReport5 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
                           #CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year,  ROE,  本益比法, K值法, 負債比例, 權益總額)
FinacialReport6 <- BS6 %>% inner_join(IS6) %>% filter(權益總額 > 0)
FinacialReport6Result <- FinacialReport6 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
                           #CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year,  ROE,  本益比法, K值法, 負債比例, 權益總額)

FinacialReportResultAll <- rbind(FinacialReport1Result, FinacialReport2Result, FinacialReport3Result, FinacialReport4Result, FinacialReport5Result, FinacialReport6Result)

#post preparation
CleanDividendData <- function(df, Year) {
    df <- df[,1:10]
    names(df) <- c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股")#, "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股")#, "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "普通股每股面額")
    df <- df %>% select(公司代號名稱, 股東配發s盈餘分配之現金股利元股)# ,股東配發s股東配發之現金股利總金額元) %>% mutate(股東配發s股東配發之現金股利總金額元 = as.character(股東配發s股東配發之現金股利總金額元))
    GetCompany <- function(string) {
        strsplit(as.character(string), split = "-") %>% .[[1]] %>% .[1] %>% trimws

    }
    df <- df %>% mutate(公司代號 = as.character(map(公司代號名稱, GetCompany))) %>% select(-公司代號名稱)
    df <- df %>% mutate(股東配發s盈餘分配之現金股利元股 = 股東配發s盈餘分配之現金股利元股 %>% as.character %>% as.numeric) 
    df %>% filter(!is.na(股東配發s盈餘分配之現金股利元股)) %>% mutate(year = Year) %>% unique
}
check<- read.csv(file = "C:/Users/ACER/Downloads/t163sb04_20210212_113221965.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend101 <- read.csv(file = "D:/Investment Plan/StockMarket/Dividend/Dividend101.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend102 <- read.csv(file = "D:/Investment Plan/StockMarket/Dividend/Dividend102.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend103 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend103.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend104 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend104.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend105 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend105.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend106 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend106.csv.utf8", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend101 %<>% CleanDividendData(., 101)
Dividend102 %<>% CleanDividendData(., 102)
Dividend103 %<>% CleanDividendData(., 103)
Dividend104 %<>% CleanDividendData(., 104)
Dividend105 %<>% CleanDividendData(., 105)
Dividend106 %<>% CleanDividendData(., 106)
DivideAll <- rbind(Dividend101, Dividend102, Dividend103, Dividend104, Dividend105, Dividend106)

FinacialReportResultAll <- FinacialReportResultAll %>% mutate(公司代號 = as.character(公司代號)) %>% left_join(DivideAll) %>% filter(!is.na(股東配發s盈餘分配之現金股利元股))

FinacialReportResultPricing <- FinacialReportResultAll %>% group_by(公司代號) %>% summarise(本益比法mean = mean(本益比法), K值法mean = mean(K值法), 負債比例mean = mean(負債比例), 股利mean = mean(股東配發s盈餘分配之現金股利元股), 本益比法first = first(本益比法), K值法first = first(K值法), 負債比例first = first(負債比例), 股利first = first(股東配發s盈餘分配之現金股利元股))
FinacialReportResultPricing <- FinacialReportResultAll %>% group_by(公司代號) %>% summarise(本益比法mean = mean(本益比法), K值法mean = mean(K值法), 負債比例mean = mean(負債比例), 本益比法first = last(本益比法), K值法first = last(K值法), 負債比例first = last(負債比例))

FinacialReportResultAll %>% filter(公司代號==9945)
#Analysis
GetRateIndex <- function(df) {

    RateROE <- lm(ROE ~ year, data = df) %>% .$coefficients %>% .[2] %>% as.numeric

    tibble(RateROE)
}
GetAverageIndex <- function(df) {
    AverageROE <- df$ROE %>% mean
    StdROE <- df$ROE %>% sd
    tibble(AverageROE, StdROE)
}

FinacialReportResultAllNest <- FinacialReportResultAll %>% group_by(公司代號) %>% nest

FinacialReportResult <- FinacialReportResultAllNest %>% mutate(averageindex = map(data, GetAverageIndex), count = map(data, nrow))
FinacialReportResult <- FinacialReportResult %>% mutate(CoOERate = map(data, GetRateIndex))
FinacialReportResult <- FinacialReportResult %>% select(-data) %>% unnest
FinacialReportResult <- FinacialReportResult %>% left_join(FinacialReportResultPricing)


##Stock list and price
FinacialReportResult <- FinacialReportResult %>% mutate(weight = AverageROE * RateROE)
stocklist <- FinacialReportResult %>% filter(AverageROE > 0.2 ) %>% arrange(desc(weight)) %>% filter(count>=5)
FinacialReportResult %>% filter(AverageROE > 0.2) %>% arrange(desc(AverageROE))
GetStockPrice <- function(companylist) {
    StockAll <- tibble()
    for (i in 1:nrow(companylist)) {
        stock <- paste0(companylist$公司代號[i], ".TW")
        getSymbols(stock)
        Date <- get(stock) %>% as.data.frame %>% row.names
        OneStock <- get(stock) %>% as.tibble
        names(OneStock) <- c("Open", "High", "Low", "ClosePrice", "Volume", "Adjusted")
        OneStock <- OneStock %>% mutate(day = Date)
        OneStock <- OneStock %>% mutate(公司代號 = companylist$公司代號[i]) %>% filter(day == max(day))
        StockAll <- rbind(StockAll, OneStock)
        Sys.sleep(sample(3:6, size = 1))
    }
    return(StockAll)
}
StockPrice <- stocklist %>% GetStockPrice
stocklist[1:4,] %>% GetStockPrice
sprice <- StockPrice %>% select(ClosePrice, day, 公司代號)
stocklist <- stocklist %>% left_join(sprice)
stocklist %>% View
FinacialReportResultAll %>% filter(公司代號 == 2475)
IS3 %>% filter(公司代號 == 2475)
BS3 %>% filter(公司代號 == 2475)
FinacialReportResult %>% filter(公司代號 == 6128) %>% View

FinacialReportResult %>% filter(AverageROE >= 0.2) %>% ggplot() + geom_point(mapping = aes(x = RateROE, y = AverageROE, color = 公司代號))
stocklist <- FinacialReportResult %>% filter(AverageROE >= 0.2 & RateROE > 0) %>% arrange(desc(RateROE)) #%>% select(公司代號, AverageROE, RateROE)
stocklist <- FinacialReportResult %>% filter(AverageCfOE > 0 & AverageROE > 0.20 & count == 5 & 公司代號 != 1256) %>% arrange(desc(AverageROE)) %>% .[1:20,]

#Dividend Analysis
FinacialReportResultAll %>% left_join(Dividend) %>% mutate(DRatio = 股東配發s盈餘分配之現金股利元股 / 權益總額) %>% arrange(desc(DRatio)) %>% filter(公司代號 == 4952)
FinacialReportResultAll %>% left_join(Dividend) %>% mutate(DRatio = 股東配發s盈餘分配之現金股利元股 / 權益總額) %>% arrange(desc(DRatio)) %>% group_by(公司代號) %>% summarise(MDRatio = mean(DRatio)) %>% arrange(desc(MDRatio))


#手續費 0.1425% * 0.65 ETF 0.1%
0.1425 * 0.65