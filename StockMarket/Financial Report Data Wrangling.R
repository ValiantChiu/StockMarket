library(tidyverse)
library(modelr)
library(magrittr)
end_year <- 110
#library(quantmod)
CleanData <- function(df) {
    df %>% map(~as.numeric(gsub(pattern = ",", replacement = "", .))) %>% as.tibble
}
SelectColumn <- function(Df, Type, Category) {
    if (Type == "Balance Sheet") {
        if (Category == 1) { Df <- Df %>% select("公司代號", "現金及約當現金", "應收款項－淨額", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值") }
        if (Category == 2) { Df <- Df %>% select("公司代號", "流動資產", "非流動資產", "資產合計", "流動負債", "負債合計", "股本", "權益合計", "每股參考淨值") }
        if (Category == 3) { Df <- Df %>% select("公司代號", "流動資產", "非流動資產", "資產總額", "流動負債", "負債總額", "股本", "權益總額", "每股參考淨值") }
        if (Category == 4) { Df <- Df %>% select("公司代號", "現金及約當現金", "應收款項－淨額", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值") }
        if (Category == 5) { Df <- Df %>% select("公司代號", "現金及約當現金", "應收款項", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值") }
        if (Category == 6) { Df <- Df %>% select("公司代號", "流動資產", "非流動資產", "資產總額", "流動負債", "負債總額", "股本", "權益總額", "每股參考淨值") }
        } else {
            if (Category == 1) { Df <- Df %>% select("公司代號", "利息淨收益", "利息以外淨損益", "營業費用", "繼續營業單位本期稅後淨利淨損", "其他綜合損益稅後", "本期綜合損益總額稅後", "基本每股盈餘元") }
            if (Category == 2) { Df <- Df %>% select("公司代號", "收益", "支出及費用", "營業外損益", "本期其他綜合損益稅後淨額", "本期綜合損益總額", "基本每股盈餘元") }
            if (Category == 3) { Df <- Df %>% select("公司代號", "營業收入", "營業成本", "營業費用", "營業外收入及支出", "繼續營業單位本期淨利淨損", "其他綜合損益淨額", "本期綜合損益總額", "基本每股盈餘元") }
            if (Category == 4) { Df <- Df %>% select("公司代號", "利息淨收益", "利息以外淨收益", "營業費用", "本期稅後淨利淨損", "本期其他綜合損益稅後淨額", "本期綜合損益總額", "基本每股盈餘元") }
            if (Category == 5) { Df <- Df %>% select("公司代號", "營業收入", "營業成本", "營業費用", "營業外收入及支出", "本期淨利淨損", "其他綜合損益稅後淨額", "本期綜合損益總額", "基本每股盈餘元") }
            if (Category == 6) { Df <- Df %>% select("公司代號", "收入", "支出", "本期淨利淨損", "其他綜合損益", "本期綜合損益總額", "基本每股盈餘元") }

        }


    return(Df)
}
#year <- 108
#number <- 2
#type <- "Balance Sheet"
#type <- "Income Statement"
GetFinancialReport <- function(year_range, number, type) {
    GetOneFinancialReport <- function(year, number, type) {
        DealWithBSHeader <- function(header, number) {
            if (number == 3 | number == 5) {
                if ("資產總計" %in% (header %>% unlist)) {
                    header <- header %>% rename(資產總額 = 資產總計, 權益總額 = 權益總計, 負債總額 = 負債總計)
                }
                header 
               } else if (number == 6) {
                    if ("資產總計" %in% (header %>% unlist)) {
                        header <- header %>% rename(資產總額 = 資產總計)
                    }
                   if ("負債總計" %in% (header %>% unlist)) {
                       header <- header %>% rename(負債總額 = 負債總計)
                   }
                header
               } else if (number == 2) {
                    if ("資產總計" %in% (header %>% unlist)) {
                        header <- header %>% rename(資產合計 = 資產總計, 權益合計 = 權益總計, 負債合計 = 負債總計)
                    }
                   header
               } else {
                   header
               }
        }
        DATA <- read.csv(paste0("D:/StockMarket/StockMarket/", type, "/", year, "/", number, ".csv"), header = FALSE) %>% as.tibble %>% mutate_all(as.character)
        header <- DATA[1,] %>% mutate_all(funs(gsub(pattern = "）|（", "", .)))
        names(header) <- header
        if (type == 'Balance Sheet') { header <-DealWithBSHeader(header,number)}
        content <- DATA[-1,]
        names(content) <- names(header)
        content %>% SelectColumn(., type, number) %>% CleanData %>% mutate(year)
    }
    Report <- tibble()
    for (i in year_range) {
        print(i)
        One_Report <- GetOneFinancialReport(i, number, type)
        Report <- rbind(Report, One_Report)
    }
    Report
}

#Income Statement
year_range <- 102:end_year
IS1 <- GetFinancialReport(year_range, 1, "Income Statement")
IS2 <- GetFinancialReport(year_range, 2, "Income Statement")
IS3 <- GetFinancialReport(year_range, 3, "Income Statement")
IS4 <- GetFinancialReport(year_range, 4, "Income Statement")
IS5 <- GetFinancialReport(year_range, 5, "Income Statement")
IS6 <- GetFinancialReport(year_range, 6, "Income Statement")

#Balance Statement
BS1 <- GetFinancialReport(year_range, 1, "Balance Sheet")
BS2 <- GetFinancialReport(year_range, 2, "Balance Sheet")
BS3 <- GetFinancialReport(year_range, 3, "Balance Sheet")
BS4 <- GetFinancialReport(year_range, 4, "Balance Sheet")
BS5 <- GetFinancialReport(year_range, 5, "Balance Sheet")
BS6 <- GetFinancialReport(year_range, 6, "Balance Sheet")

#Dividend
GetDividendReport <- function(year_range) {
    GetOneDividendReport <- function(year) {
        DATA <- read.csv(paste0("D:/StockMarket/StockMarket/Dividend/Dividend", year, ".csv"), header = FALSE) %>% as.tibble %>% .[, - ncol(.)]
        header <- DATA[1,] %>% mutate_all(funs(gsub(pattern = "）|（", "", .)))
        names(header) <- header
        content <- DATA[-1,]
        names(content) <- names(header)
        content <- content %>% select(公司代號名稱, `股東配發-盈餘分配之現金股利(元/股)`) %>% mutate_all(as.character)
        GetCompany <- function(string) {
            strsplit(as.character(string), split = "-") %>% .[[1]] %>% .[1] %>% trimws

        }
        content <- content %>% mutate(公司代號 = as.character(map(公司代號名稱, GetCompany))) %>% select(-公司代號名稱)
        content <- content %>% mutate(現金股利元股 = `股東配發-盈餘分配之現金股利(元/股)` %>% as.character %>% as.numeric) %>% select(-`股東配發-盈餘分配之現金股利(元/股)`)
        content %>% filter(!is.na(現金股利元股)) %>% group_by(公司代號) %>% summarise(現金股利元股 = sum(現金股利元股)) %>% mutate(year)
    }
    Report <- tibble()
    for (i in year_range) {
        print(i)
        One_Report <- GetOneDividendReport(i)
        Report <- rbind(Report, One_Report)
    }
    Report
}
Dividend <- GetDividendReport(100:end_year)
#Basis Information
basis <- read.csv("D:/StockMarket/StockMarket/Information/basis.csv", header = FALSE) %>% as.tibble
header <- basis[1,] %>% mutate_all(funs(gsub(pattern = "）|（", "", .)))
names(header) <- header
content <- basis[-1,]
names(content) <- names(header)
basis <- content %>% select(公司代號, 產業類別, 公司簡稱) %>% mutate_all(as.character)




#Aggregate 
FinacialReport1 <- BS1 %>% inner_join(IS1) %>% filter(權益總額 > 0)
FinacialReport1Result <- FinacialReport1 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額稅後 / 權益總額,
#CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額稅後 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額,
                           本期綜合損益總額 = 本期綜合損益總額稅後) %>% select(公司代號, year, 本期綜合損益總額, 權益總額, 基本每股盈餘元, ROE, 負債比例, 每股參考淨值)

FinacialReport2 <- BS2 %>% inner_join(IS2) %>% filter(權益合計 > 0)
FinacialReport2Result <- FinacialReport2 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益合計,
                           ROE = 本期綜合損益總額 / 權益合計,
#CfOE = 自由現金流 / 權益合計,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益合計 / 0.08),
                           負債比例 = 負債合計 / 資產合計,
                           權益總額 = 權益合計) %>% select(公司代號, year, 本期綜合損益總額, 權益總額, 基本每股盈餘元, ROE, 負債比例, 每股參考淨值)

FinacialReport3 <- BS3 %>% inner_join(IS3) %>% filter(權益總額 > 0)
FinacialReport3Result <- FinacialReport3 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
#CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year, 本期綜合損益總額, 權益總額, 基本每股盈餘元, ROE, 負債比例, 每股參考淨值)


FinacialReport4 <- BS4 %>% inner_join(IS4) %>% filter(權益總額 > 0)
FinacialReport4Result <- FinacialReport4 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
#CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year, 本期綜合損益總額, 權益總額, 基本每股盈餘元, ROE, 負債比例, 每股參考淨值)

FinacialReport5 <- BS5 %>% inner_join(IS5) %>% filter(權益總額 > 0)
FinacialReport5Result <- FinacialReport5 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
#CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year, 本期綜合損益總額, 權益總額, 基本每股盈餘元, ROE, 負債比例, 每股參考淨值)
FinacialReport6 <- BS6 %>% inner_join(IS6) %>% filter(權益總額 > 0)
FinacialReport6Result <- FinacialReport6 %>% mutate(#CoOE = 營業活動之淨現金流入流出 / 權益總額,
                           ROE = 本期綜合損益總額 / 權益總額,
#CfOE = 自由現金流 / 權益總額,
                           本益比法 = 14 * 基本每股盈餘元,
                           K值法 = 每股參考淨值 * (本期綜合損益總額 / 權益總額 / 0.08),
                           負債比例 = 負債總額 / 資產總額) %>% select(公司代號, year, 本期綜合損益總額, 權益總額, 基本每股盈餘元, ROE, 負債比例, 每股參考淨值)
FinacialReportResultAll <- rbind(FinacialReport1Result, FinacialReport2Result, FinacialReport3Result, FinacialReport4Result, FinacialReport5Result, FinacialReport6Result)
FinacialReportResultAll <- FinacialReportResultAll %>% mutate(公司代號 = as.character(公司代號)) %>% left_join(Dividend) #%>% filter(!is.na(現金股利元股))
FinacialReportResultAll %<>% left_join(basis)

stock_type <- 'm'
#
#write.csv(FinacialReportResultAll,file = 'sample.csv')