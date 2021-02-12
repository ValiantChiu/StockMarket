library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(RODBC)
library(lubridate)

#LOCAL <- odbcConnect("LOCAL")
#Sys.setlocale("LC_ALL", "cht")


GetFinReport <- function(Url,Year,Season) {
    ResultAll <- tibble(r1 = list(), r2 = list(), r3 = list(), r4 = list(), r5 = list(), r6 = list(), year = integer(), season = integer())
    year <- Year
    season <- Season
    url <- Url
    for (y in Year) {
        for (s in Season) {
            postData <- paste0("encodeURIComponent=1&step=1&firstin=1&off=1&isQuery=Y&TYPEK=sii&year=", y, "&season=", s, "")
            result <- POST(url, body = postData)
            html <- result %>% content(as = "text") %>% read_html()
            result <- html %>% html_table()

            ResultOne <- tibble(r1 = result[2], r2 = result[3], r3 = result[4], r4 = result[5], r5 = result[6], r6 = result[7])
            ResultOne <- ResultOne %>% mutate(year = y, season = s)
            ResultAll <- rbind(ResultAll, ResultOne)

        }
        print(y)
        Sys.sleep(sample(2:5, size = 1));
    }
    return(ResultAll)
}
SelectColumn <- function(Df, Type, Category) {
  
        if (Type == "BS") {
            if (Category == 1) { Df <- Df %>% select("公司代號", "現金及約當現金", "應收款項－淨額", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值") }
            if (Category == 2) { Df <- Df %>% select("公司代號", "流動資產", "非流動資產", "資產合計", "流動負債", "負債合計", "股本", "權益合計", "每股參考淨值") }
            if (Category == 3) { Df <- Df %>% select("公司代號", "流動資產", "非流動資產", "資產總額", "流動負債", "負債總額", "股本", "權益總額", "每股參考淨值") }
            if (Category == 4) { Df <- Df %>% select("公司代號", "現金及約當現金", "應收款項－淨額", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值") }
            if (Category == 5) { Df <- Df %>% select("公司代號", "現金及約當現金", "應收款項", "資產總額", "負債總額", "股本", "權益總額", "每股參考淨值") }
            if (Category == 6) { Df <- Df %>% select("公司代號", "流動資產", "非流動資產", "資產總額", "流動負債", "負債總額", "股本", "權益總額", "每股參考淨值") }
            } else {
                if (Category == 1) { Df <- Df %>% select("公司代號", "利息淨收益", "利息以外淨損益", "營業費用", "繼續營業單位本期稅後淨利（淨損）", "其他綜合損益（稅後）", "本期綜合損益總額（稅後）", "基本每股盈餘（元）") }
                if (Category == 2) { Df <- Df %>% select("公司代號", "收益", "支出及費用","營業外損益", "本期其他綜合損益（稅後淨額）","本期綜合損益總額", "基本每股盈餘（元）") }
                if (Category == 3) { Df <- Df %>% select("公司代號", "營業收入", "營業成本", "營業費用","營業外收入及支出", "繼續營業單位本期淨利（淨損）","其他綜合損益（淨額）", "本期綜合損益總額", "基本每股盈餘（元）") }
                if (Category == 4) { Df <- Df %>% select("公司代號", "利息淨收益", "利息以外淨收益","營業費用", "本期稅後淨利（淨損）","本期其他綜合損益（稅後淨額）" ,"本期綜合損益總額", "基本每股盈餘（元）") }
                if (Category == 5) { Df <- Df %>% select("公司代號", "營業收入", "營業成本", "營業費用","營業外收入及支出" ,"本期淨利（淨損）", "其他綜合損益（稅後淨額）","本期綜合損益總額", "基本每股盈餘（元）") }
                if (Category == 6) { Df <- Df %>% select("公司代號", "收入", "支出", "本期淨利（淨損）", "其他綜合損益","本期綜合損益總額", "基本每股盈餘（元）") }

            }
 
    
    return(Df)
}
SelectCommonColumn <- function(Df, Name, Inde, Type) {
    if (Type == 'IS') {
        Df %>% select(Name)
    } else {
        if (Inde == 3) {
            if ("資產總計" %in% names(Df)) {
                Df %>% select(Name, "資產總計", "權益總計", "負債總計") %>% rename(`<U+8CC7><U+7522><U+7E3D><U+984D>` = `<U+8CC7><U+7522><U+7E3D><U+8A08>`,
        `<U+8CA0><U+50B5><U+7E3D><U+984D>` = `<U+8CA0><U+50B5><U+7E3D><U+8A08>`,
        `<U+6B0A><U+76CA><U+7E3D><U+984D>` = `<U+6B0A><U+76CA><U+7E3D><U+8A08>`)
            } else if ("資產總額" %in% names(Df)) {
                Df %>% select(Name, "資產總額", "權益總額", "負債總額")
            }
        } else if (Inde == 5) {
            if ("資產總計" %in% names(Df)) {
                Df %>% select(Name, "資產總計", "權益總計", "負債總計") %>% rename(`<U+8CC7><U+7522><U+7E3D><U+984D>` = `<U+8CC7><U+7522><U+7E3D><U+8A08>`,
        `<U+8CA0><U+50B5><U+7E3D><U+984D>` = `<U+8CA0><U+50B5><U+7E3D><U+8A08>`,
        `<U+6B0A><U+76CA><U+7E3D><U+984D>` = `<U+6B0A><U+76CA><U+7E3D><U+8A08>`)
            } else if ("資產總額" %in% names(Df)) {
                Df %>% select(Name, "資產總額", "權益總額", "負債總額")
            }
        } else if (Inde == 6) {
            if ("資產總計" %in% names(Df)) {
                Df %>% select(Name, "資產總計") %>% rename(`<U+8CC7><U+7522><U+7E3D><U+984D>` = `<U+8CC7><U+7522><U+7E3D><U+8A08>`)
            } else if ("資產總額" %in% names(Df)) {
                Df %>% select(Name, "資產總額")
            }
        } else {
            Df %>% as_tibble %>% select(Name)
        }
    }
    
}
AddTime <- function(Df,Year,Season) {
        Df %>% mutate(year = Year, season = Season)
}

GetCommonColumn <- function(Df) {
    common <- Df[[1]] %>% names
    if (length(Df) > 1) {
        for (i in 2:(length(Df))) {
            if (is.null(Df[[i]])) next;
            #if ("資產總計" %in% names(Df[[i]])) { Df[[i]] <- Df[[i]] %>% rename(`<U+8CC7><U+7522><U+7E3D><U+984D>` = `<U+8CC7><U+7522><U+7E3D><U+8A08>`) }
            common <- common %>% intersect(names(Df[[i]]))
        }
    }
    common
}

Rbind <- function(Df) {
    Result <- Df[[1]]
    if (length(Df) > 1) {
        for (i in 2:length(Df)) {
            Result <- rbind(Result, Df[[i]])
        }
    }
    Result
}

AggregateReport <- function(Df, Type) {

    Df <- Df %>% GetCommonName
    Df <- Df %>% mutate(inde = 1) %>% mutate(r1 = pmap(list(r1, r1name, inde,Type), SelectCommonColumn))
    Df <- Df %>% mutate(inde = 2) %>% mutate(r2 = pmap(list(r2, r2name, inde,Type), SelectCommonColumn))
    Df <- Df %>% mutate(inde = 3) %>% mutate(r3 = pmap(list(r3, r3name, inde,Type), SelectCommonColumn))
    Df <- Df %>% mutate(inde = 4) %>% mutate(r4 = pmap(list(r4, r4name, inde,Type), SelectCommonColumn))
    Df <- Df %>% mutate(inde = 5) %>% mutate(r5 = pmap(list(r5, r5name, inde,Type), SelectCommonColumn))
    Df <- Df %>% mutate(inde = 6) %>% mutate(r6 = pmap(list(r6, r6name, inde,Type), SelectCommonColumn))

    Df <- Df %>% mutate(r1 = pmap(list(r1, year, season), AddTime))
    Df <- Df %>% mutate(r2 = pmap(list(r2, year, season), AddTime))
    Df <- Df %>% mutate(r3 = pmap(list(r3, year, season), AddTime))
    Df <- Df %>% mutate(r4 = pmap(list(r4, year, season), AddTime))
    Df <- Df %>% mutate(r5 = pmap(list(r5, year, season), AddTime))
    Df <- Df %>% mutate(r6 = pmap(list(r6, year, season), AddTime))
    Df <- Df %>% .[, 1:6]
    r1 <- Df$r1 %>% Rbind
    names(r1) <- names(r1) %>% gsub(pattern = "（|）", replacement = "")
    r2 <- Df$r2 %>% Rbind
    names(r2) <- names(r2) %>% gsub(pattern = "（|）", replacement = "")
    r3 <- Df$r3 %>% Rbind
    names(r3) <- names(r3) %>% gsub(pattern = "（|）", replacement = "")
    r4 <- Df$r4 %>% Rbind
    names(r4) <- names(r4) %>% gsub(pattern = "（|）", replacement = "")
    r5 <- Df$r5 %>% Rbind
    names(r5) <- names(r5) %>% gsub(pattern = "（|）", replacement = "")
    r6 <- Df$r6 %>% Rbind
    names(r6) <- names(r6) %>% gsub(pattern = "（|）", replacement = "")
    list(r1,r2,r3,r4,r5,r6)
}
SaveReport <- function(Df,Type) {
    if (Type == 'IS') {
        ISName<-c("IS1","IS2","IS3","IS4","IS5","IS6")
        for (i in 1:length(Df)) {
            sqlSave(LOCAL, Df[[i]], tablename = ISName[i], rownames = FALSE, append = TRUE)
        }
    } else {
        BSName <- c("BS1", "BS2", "BS3", "BS4", "BS5", "BS6")
        for (i in 1:length(Df)) {
            sqlSave(LOCAL, Df[[i]], tablename = BSName[i], rownames = FALSE, append = TRUE)
        }
    }
}

GetCommonName <- function(Df) {
    ColumnName <- Df %>% map(~GetCommonColumn(.))
    namecolumn <- c("r1name", "r2name", "r3name", "r4name", "r5name", "r6name")
    for (i in 1:6) {
        Df[, namecolumn[i]] <- tibble(list(ColumnName[[i]]))
    }
    Df
}


GetNumericReport <- function(Report) {

    for (i in 1:nrow(Report)) { 
        Report[i,] <- as.numeric( gsub(pattern = ",", replacement = "", Report[i,]))
    }
    return(Report)
}


#Income Statement
Sys.setlocale("LC_ALL", "C")
ISUrl <- "https://mops.twse.com.tw/mops/web/ajax_t163sb04"
ISALL <- GetFinReport(ISUrl, c(107:107), c(4))
IS <- ISALL %>% AggregateReport(.,"IS")
Sys.setlocale("LC_ALL", "cht")
#SaveReport(IS,"IS")
#Balance Sheet
Sys.setlocale("LC_ALL", "C")
BSUrl<-"https://mops.twse.com.tw/mops/web/ajax_t163sb05"
BSAll <- GetFinReport(BSUrl, c(107:107), c(4))
BS <- BSAll %>% AggregateReport(.,"BS")
Sys.setlocale("LC_ALL", "cht")
#SaveReport(BS, "BS")

#debug
BS[[5]] %>% names
BSAll$r6[[6]] %>% .[1,] %>% names %>% select(`<U+8CC7><U+7522><U+7E3D><U+984D>`) %>% as.tibble %>% names
"資產總計" %in% (BSAll$r3[[2]] %>% .[1,] %>% names)
BSAll$r6[[5]] %>% .[1,] %>% names %>% rename(`<U+8CC7><U+7522><U+7E3D><U+984D>` = `<U+8CC7><U+7522><U+7E3D><U+8A08>`) %>% as.tibble %>% names
Df <- BSAll$r3
DfAll <- BSAll
#WholeList
CLUrl <- "http://mops.twse.com.tw/mops/web/ajax_t51sb01"
postData <- "encodeURIComponent=1&step=1&firstin=1&TYPEK=sii&code="
result <- POST(CLUrl, body = postData)
html <- result %>% content(as = "text") %>% read_html()
result <- html %>% html_table()
result <- result[[2]]
#companylist<-result %>% as.tibble %>% select(公司代號, 公司名稱)
CompanyList <- result[, 1] %>% setdiff("公司代號")
saveRDS(companylist,file = "companylist.rds")
#Cash Flow
GetOneCashFlow <- function(Company, Year, Season) {
    CFurl <- "http://mops.twse.com.tw/mops/web/ajax_t164sb05"
    #postData <- paste0("encodeURIComponent=1&step=1&firstin=1&off=1&keyword4=&code1=&TYPEK2=&checkbtn=&queryName=co_id&inpuType=co_id&TYPEK=all&isnew=false&co_id=", Company, "&year=", Year, "&season=0", Season, "")
    postData <- paste0("encodeURIComponent=1&id=&key=&TYPEK=sii&step=2&year=",Year,"&season=",Season,"&co_id=",Company,"&firstin=1")
    #encodeURIComponent=1&id=&key=&TYPEK=sii&step=2&year=106&season=4&co_id=2801&firstin=1
    result <- POST(CFurl, body = postData)
    html <- result %>% content(as = "text") %>% read_html()
    result <- html %>% html_table(fill = T)
    if (length(result) < 2) {
        if (result[[1]] %>% grepl(pattern = "Overrun")) {
            return(Company)
        } else {
            return(0)
        }
    } else {
        result <- result %>% .[[2]]
        result <- result[4:nrow(result), 1:2]
        NAME <- result[, 1]
        result <- result %>% as.matrix
        colnames(result) <- NULL
        result <- result %>% t %>% as.data.frame
        names(result) <- NAME
        result <- result[2,]
        names(result) <- names(result) %>% gsub(pattern = "　", replacement = "") %>% trimws()
        #result <- result %>% select("營業活動之淨現金流入（流出）", "投資活動之淨現金流入（流出）", "籌資活動之淨現金流入（流出）") #, "發放現金股利")
        result <- result %>% .[, c("營業活動之淨現金流入（流出）", "投資活動之淨現金流入（流出）", "籌資活動之淨現金流入（流出）")]
        result <- result %>% mutate(company = Company, year = Year, season = Season)
        return(result)
    }
}

CFAll <- data.frame()
LossCompany <- vector()
CompanyList <- CompanyList[CompanyList >= 3043]
CompanyList <- CompanyList[69:78]

for (company in CompanyList) {
    for (year in c(106)) {
        ResultOne<-0
        print(company)
        Sys.sleep(sample(3:6, size = 1))
        tryCatch(ResultOne <- GetOneCashFlow(company, year, 4), error = function(e) e)
        if (length(ResultOne) != 6) { LossCompany <- union(LossCompany, ResultOne); next; }
        CFAll <- rbind(CFAll, ResultOne)
        print(paste(company, " ", year))    
    }
}
 
Sys.setlocale("LC_ALL", "C")


CFAll1 <- CFAll
names(CFAll1) <- names(CFAll1) %>% gsub(pattern = "（|）", replacement = "")
Sys.setlocale("LC_ALL", "cht")
sqlSave(LOCAL, CFAll1, tablename = "CF_All", rownames = FALSE, append = TRUE)
CFResult %>% nrow
sql <- "SELECT * FROM [master].[dbo].[CF_All]"
CFResult <- LOCAL %>% sqlQuery(sql) %>% unique
CFResult %>% nrow
CFRcount<-CFResult %>% group_by(company) %>% summarise(count=n())
passcompany <- CFRcount %>% filter(count ==4)
passcompany$company

CompanyList <- CompanyList %>% setdiff(passcompany$company)

CFResult %>% filter(company == 1235)
CFAll1 %>% filter(company == 1235)

GetOneCashFlow(1235, 104, 4)


sqlSave(LOCAL, CFResult, tablename = "CF_All", rownames = FALSE, append = TRUE)

CFResult %>% View
# Diviedend 
CFurl <- "http://mops.twse.com.tw/server-java/t05st09sub"
postData <- "step=1&TYPEK=sii&YEAR=102&first="
postData <- paste0("step=1&TYPEK=sii&YEAR=102&first=")
#encodeURIComponent=1&id=&key=&TYPEK=sii&step=2&year=106&season=4&co_id=2801&firstin=1
result <- POST(CFurl, body = postData,add_headers("Referer" = "http://mops.twse.com.tw/mops/web/t05st09_new"))
html <- result %>% content(as = "text") %>% read_html()
result <- html %>% html_table()
? POST

url = "http://mops.twse.com.tw/t05st09sub?=step=1&TYPEK=sii&YEAR=102&first="
url %>% GET(add_headers("Referer" = "http://mops.twse.com.tw/mops/web/t05st09_new"))

names(Dividend101) <- c("公司代號名稱","資料來源","期別","董事會決議通過股利分派日","股東會日期","期初未分配盈餘or待彌補虧損元","本期淨利淨損元","可分配盈餘元","分配後期末未分配盈餘元","股東配發s盈餘分配之現金股利元股","股東配發s法定盈餘公積and資本公積發放之現金元股","股東配發s股東配發之現金股利總金額元","股東配發s盈餘轉增資配股元股","股東配發s法定盈餘公積and資本公積轉增資配股元股","股東配發s股東配股總股數股","董監酬勞元","員工紅利s現金紅利金額元","員工紅利s股票紅利金額元","員工紅利s股票紅利股數股","員工紅利s股票紅利股數佔盈餘轉增資之比例","有無全數分派員工股票紅利而股東未配發股票股利之情事","股東會對於員工紅利及董監酬勞之決議情形與原董事會通過擬議內容之差異原因及合理性","普通股每股面額") 

Dividend101 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend101.csv.utf8", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend102 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend102.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend103 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend103.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend104 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend104.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend105 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend105.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
Dividend106 <- read.csv(file = "C:/Users/user/Documents/Investment Plan/StockMarket/Dividend/Dividend106.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "") %>% as.tibble
names(Dividend101) <- c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股", "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股", "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "董監酬勞元", "員工紅利s現金紅利金額元", "員工紅利s股票紅利金額元", "員工紅利s股票紅利股數股", "員工紅利s股票紅利股數佔盈餘轉增資之比例", "有無全數分派員工股票紅利而股東未配發股票股利之情事", "股東會對於員工紅利及董監酬勞之決議情形與原董事會通過擬議內容之差異原因及合理性", "普通股每股面額")
names(Dividend102) <- c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股", "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股", "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "董監酬勞元", "員工紅利s現金紅利金額元", "員工紅利s股票紅利金額元", "員工紅利s股票紅利股數股", "員工紅利s股票紅利股數佔盈餘轉增資之比例", "有無全數分派員工股票紅利而股東未配發股票股利之情事", "股東會對於員工紅利及董監酬勞之決議情形與原董事會通過擬議內容之差異原因及合理性", "普通股每股面額")
names(Dividend103) <- c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股", "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股", "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "董監酬勞元", "員工紅利s現金紅利金額元", "員工紅利s股票紅利金額元", "員工紅利s股票紅利股數股", "員工紅利s股票紅利股數佔盈餘轉增資之比例", "有無全數分派員工股票紅利而股東未配發股票股利之情事", "股東會對於員工紅利及董監酬勞之決議情形與原董事會通過擬議內容之差異原因及合理性", "普通股每股面額")
names(Dividend104) <- c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股", "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股", "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "普通股每股面額")
names(Dividend105) <- c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股", "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股", "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "普通股每股面額")
names(Dividend106) <- c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股", "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股", "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "普通股每股面額")

Dividend101 <- Dividend101 %>% .[, c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股", "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股", "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "普通股每股面額")]
Dividend102 <- Dividend102 %>% .[, c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股", "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股", "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "普通股每股面額")]
Dividend103 <- Dividend103 %>% .[, c("公司代號名稱", "資料來源", "期別", "董事會決議通過股利分派日", "股東會日期", "期初未分配盈餘or待彌補虧損元", "本期淨利淨損元", "可分配盈餘元", "分配後期末未分配盈餘元", "股東配發s盈餘分配之現金股利元股", "股東配發s法定盈餘公積and資本公積發放之現金元股", "股東配發s股東配發之現金股利總金額元", "股東配發s盈餘轉增資配股元股", "股東配發s法定盈餘公積and資本公積轉增資配股元股", "股東配發s股東配股總股數股", "普通股每股面額")]
Dividend101 <- Dividend101 %>% mutate(year = 101)
Dividend102 <- Dividend102 %>% mutate(year = 102)
Dividend103 <- Dividend103 %>% mutate(year = 103)
Dividend104 <- Dividend104 %>% mutate(year = 104)
Dividend105 <- Dividend105 %>% mutate(year = 105)
Dividend106 <- Dividend106 %>% mutate(year = 106)
DividendAll<- Dividend106 %>% as.tibble
DividendAll <- rbind(Dividend101, Dividend102, Dividend103, Dividend104, Dividend105, Dividend106) %>% as.tibble
DividendAll <- DividendAll %>% filter(董事會決議通過股利分派日!="")
GetCompany <- function(string) {
    strsplit(as.character(string), split = "-") %>% .[[1]] %>% .[1] %>% trimws
}
DividendAll <- DividendAll %>% mutate(公司代號 = as.character(map(公司代號名稱, GetCompany)))
DividendAll <- DividendAll %>% filter(公司代號 != "公司代號名稱")
DividendAll <- DividendAll %>% select(-公司代號名稱)

sqlSave(LOCAL, DividendAll, tablename = "DividendAll", rownames = FALSE, append = TRUE)

#Collect Price
DividendAll <- LOCAL %>% sqlQuery("select * from [master].[dbo].[DividendAll]") %>% as.tibble
companylist <-DividendAll[1:10,"公司代號"] 
GetStockPrice(companylist)

GetStockPrice <- function(companylist) {
    StockAll <- tibble()
    for (i in 1:nrow(companylist)) {
        stock <- paste0(companylist$公司代號[i], ".TW")
        getSymbols(stock)
        Date <- get(stock) %>% as.data.frame %>% row.names
        OneStock <- get(stock) %>% as.tibble
        names(OneStock) <- c("Open","High","Low","ClosePrice", "Volume","Adjusted")
        OneStock <- OneStock %>% mutate(day=Date)
        OneStock <- OneStock %>% mutate(公司代號 = companylist$公司代號[i])
        StockAll <- rbind(StockAll, OneStock)
        Sys.sleep(sample(3:6, size = 1))
    }
    return(StockAll)
}
test<-get(stock) %>% as.data.frame %>% row.names
get(stock) %>% as.tibble %>% mutate(day = test)

get(stock) %>% row.names



#Price
Sys.setlocale("LC_ALL", "C")
Sys.setlocale("LC_ALL", "cht")
GetOneMonthStockPrice <- function(Stock, Year, Month) {
    Purl <- paste0("http://www.twse.com.tw/exchangeReport/STOCK_DAY?response=json&date=",Year,"0",Month,"01&stockNo=",Stock,"")
    Price <- POST(Purl)
    Price <- Price %>% content(as = "text") %>% fromJSON
    PriceData <- Price$data %>% as.tibble
    names(PriceData) <- Price$fields
    PriceData
}
GetOneMonthStockPrice("0050", 2018, )
GetOneMonthStockPrice("0056", 2018, 4)

Price <- tibble()

for (month in 1:12) {
    Result <- GetOneMonthStockPrice("0050", 2017, month)
    Price<-rbind(Price,Result)
    Sys.sleep(sample(3:6, size = 1))         
}
CleanPrice <- function(df) {
    df$成交股數 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$成交金額 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$漲跌價差 %<>% gsub(pattern = "+", replacement = "") %>% gsub(pattern = " ", replacement = "") %>% as.numeric
    df$成交筆數 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
    df$開盤價 %<>% as.numeric
    df$最高價 %<>% as.numeric
    df$最低價 %<>% as.numeric
    df$收盤價 %<>% as.numeric
    df
}
GetPriceDifference <- function(df) {
    for (i in 2:nrow(df)) {
        df$漲跌價差[i]<-df$收盤價[i]-df$收盤價[i-1]
    }
    df
}
Price<-Price %>% CleanPrice %>% GetPriceDifference 
?jsonlite
result <- html %>% html_table()
rvest



DividenAll <- DividendAll %>% select(公司代號, year, 股東配發s盈餘分配之現金股利元股) %>% filter(公司代號 != '愼㸴戼㹤愼㸵q愼㸵N戼㸸戼㸹愼㸶W戼㹡搼㸹') %>% mutate(股東配發s盈餘分配之現金股利元股 = as.numeric(as.character(股東配發s盈餘分配之現金股利元股)))

write.csv(DividenAll,file='DividenAll.csv')