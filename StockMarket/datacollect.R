library(rvest)
library(httr)
library(magrittr)
#library(sqldf)
library(ggplot2)
library(jsonlite)

#-----Common Function-----#
GetNumericReport <- function(Report) {

    for (i in 1:length(Report)) {
        for (j in 1:length(Report[[i]])) {
            Report[[i]][, j] <- as.numeric(gsub(pattern = ",", replacement = "", Report[[i]][, j]))
        }
    }
    return(Report)
}


#-------Source-------#
aa<-c(1,3,23,4)
aa %>% length()
#Source address
StockWeb <- "http://mops.twse.com.tw/mops/web/index"
url <- "http://mops.twse.com.tw/mops/web/ajax_t164sb03?1&step=1&firstin=1&off=1&keyword4=&code1=&TYPEK2=&checkbtn=&queryName=co_id&inpuType=co_id&TYPEK=all&isnew=true&co_id=2330&year=105&season="
#income statement
isURL <- "http://mops.twse.com.tw/mops/web/ajax_t163sb04?1&step=1&firstin=1&off=1&TYPEK=sii&year=105&season=02"
#balance Sheet
bsURL <- "http://mops.twse.com.tw/mops/web/ajax_t163sb05?1&step=1&firstin=1&off=1&TYPEK=sii&year=105&season=02"
#Review Result
reviewURL <- "http://mops.twse.com.tw/mops/web/ajax_t163sb14?1&step=1&firstin=1&off=1&TYPEK=sii&year=106&season=01"
#Industry EPS
epsURL <- "http://mops.twse.com.tw/mops/web/ajax_t163sb19?1&step=1&firstin=1&TYPEK=sii&code=01&year=105&season=02"
#Cash Flow
cfURL <- "http://mops.twse.com.tw/mops/web/ajax_t164sb05?=1&step=1&firstin=1&off=1&keyword4=&code1=&TYPEK2=&checkbtn=&queryName=co_id&inpuType=co_id&TYPEK=all&isnew=false&co_id=3167&year=102&season=01"
#------Review Report-------#
GetReviewReport <- function() {
    Result <- data.frame(company = 0, companyN = 0, comment = 0, date = 0, type = 0);
    Result <- Result[0,];
    for (i in 1:5) {
        #5
        for (j in 1:4) {
            #4
            year <- 101 + i;
            season <- j;
            date <- as.numeric(paste0(year, season));
            if (TRUE) {
                reviewURL <- paste0("http://mops.twse.com.tw/mops/web/ajax_t163sb14?1&step=1&firstin=1&off=1&TYPEK=sii&year=", year, "&season=0", season, "");
                Source <- read_html(reviewURL, encoding = "UTF-8");
                CompanyR <- Source %>% html_nodes(css = "td:nth-child(1)") %>% html_text;
                Length <- length(CompanyR);
                if (Length == 0) break;
                
                CompanyNR <- Source %>% html_nodes(css = "td:nth-child(2)") %>% html_text;
                CommentR <- Source %>% html_nodes(css = "td:nth-child(7)") %>% html_text;
                result <- data.frame(company = c(1:Length), companyN = c(1:Length), comment = c(1:Length), date = c(1:Length), type = c(1:Length));
                result$company <- CompanyR;
                result$companyN <- CompanyNR;
                result$comment <- CommentR;
                result$date <- date;
                if (j == 4) { result$type <- 'Year'; } else { result$type <- 'Season'; }
                Result <- rbind(Result, result);
                Sys.sleep(sample(2:5, size = 1));
            }
        }
    }
    Result$comment <- trimws(Result$comment);
    #Result <- Result[Result$comment == '無保留意見',];
    return(Result)
}
ReviewReport <- GetReviewReport()

ReviewReport %>% select(company,companyN) %>%  unique %>% nrow
ReviewReport$companyN %>% unique
#----Industry Category------#
#Source http://data.gov.tw/node/18419
`Industry` <- read.csv(file = "C:/Users/ChiuKees/AppData/Local/Temp/Industry.csv.utf8", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ",", dec = ".", quote = "\"", comment.char = "")
Industry <- `Industry`
head(Industry)

#----Balance Sheet-----------#
bsURL <- "http://mops.twse.com.tw/mops/web/ajax_t163sb05?1&step=1&firstin=1&off=1&TYPEK=sii&year=105&season=04"
Source <- read_html(bsURL, encoding = "UTF-8");
All <- Source %>% html_nodes(css = ".odd td , .hasBorder:nth-child(2) , .even td , th") %>% html_text;
Index <- grep(pattern = "[0-9]{4}", All)
d<-All[Index]
d
d[1]
length(All)

GetIndex <- function(All) {
    Index <- grep(pattern = "[0-9]{4}", All)
    Result <- data.frame(RowS = c(1:length(Index)), RowD = c(1:length(Index)), RowE = c(1:length(Index)), ColS = c(1:length(Index)), ColE = c(1:length(Index)));
    Result$RowS <- Index;
    for (i in 1:length(Index)) {
        if (i == length(Index)) {
            Result$RowD[i] = Result$RowD[i - 1];
        } else {
            Result$RowD[i] = Result$RowS[i + 1] - Result$RowS[i];
        }
    }

    for (i in 1:length(Index)) {
        if (i == length(Index) | i == 1) {
            Result$RowE[i] = Result$RowS[i] + Result$RowD[i] - 1;
            Result$ColS[i] = 0;
            Result$ColE[i] = 0;
        } else {
            if (Result$RowD[i] == Result$RowD[i + 1] | Result$RowD[i] == Result$RowD[i - 1]) {
                Result$RowE[i] = Result$RowS[i] + Result$RowD[i] - 1;
                Result$ColS[i] = 0;
                Result$ColE[i] = 0;
            } else {
                Result$RowE[i] = Result$RowS[i] + Result$RowD[i] - Result$RowD[i + 1] - 1;
                Result$ColS[i] = Result$RowE[i] + 1;
                Result$ColE[i] = Result$RowS[i + 1] - 1;
            }
        }
    }
    return(Result)
}

GetbsRaw <- function(IndexD, All) {
    inde <- 1;
    listresult <- list();
    for (i in 1:nrow(IndexD)) {
        if (i == 1) {
            HED <- All[c(1:IndexD$RowD[i])];
            listresult[[i]] <- data.frame(matrix(ncol = length(HED), nrow = 0));
            colnames(listresult[[i]]) <- HED;
            listresult[[i]] <- rbind(listresult[[i]][1,], All[c(IndexD$RowS[i]:IndexD$RowE[i])]);
        } else {
            if (IndexD$ColS[i] != 0) {
                inde <- inde + 1;
                listresult[[inde - 1]] <- rbind(listresult[[inde - 1]], All[c(IndexD$RowS[i]:IndexD$RowE[i])]);
                HED <- All[c(IndexD$ColS[i]:IndexD$ColE[i])];
                listresult[[inde]] <- data.frame(matrix(ncol = length(HED), nrow = 0));
                colnames(listresult[[inde]]) <- HED;
                listresult[[inde]] <- rbind(listresult[[inde]][1,], rep(NA, length(HED)));
                next;
            }
            listresult[[inde]] <- rbind(listresult[[inde]], All[c(IndexD$RowS[i]:IndexD$RowE[i])]);
        }
    }
    return(listresult)
}

ClearbsRaw <- function(bsraw, date) {
    for (i in 1:length(bsraw)) {
        bsraw[[i]] <- bsraw[[i]][complete.cases(bsraw[[i]]),];
        bsraw[[i]][, "date"] <- date;
    }
    return(bsraw)
}


Getbsreport <- function() {
    Parameter <- list(
    c("公司代號","現金及約當現金", "應收款項－淨額", "資產總額", "負債總額", "股本","權益總額", "每股參考淨值", "date"),
    c("公司代號","流動資產", "非流動資產", "資產合計","流動負債" ,"負債合計", "股本","權益合計" ,"每股參考淨值", "date"),
    c("公司代號","流動資產", "非流動資產", "資產總額","流動負債" ,"負債總額", "股本","權益總額" ,"每股參考淨值", "date"),
    c("公司代號","現金及約當現金", "應收款項－淨額", "資產總額", "負債總額", "股本","權益總額" ,"每股參考淨值", "date"),
    c("公司代號","現金及約當現金", "應收款項", "資產總額", "負債總額","股本","權益總額" ,"每股參考淨值", "date"),
    c("公司代號","流動資產", "非流動資產", "資產總額","流動負債" ,"負債總額", "股本","權益總額" ,"每股參考淨值", "date"))
    listall <- list();
    dateIndex <- 0;
    for (i in 1:6) {
        listall[[i]] <- data.frame();
    }

    for (i in 1:1) {
        #5
        s <- 4;
        if (i == 5) s <- 1;
        for (j in 1:1) {
            #s
            year <- 101 + i;
            season <- j;
            date <- as.numeric(paste0(year, season));
            dateIndex <- dateIndex + 1;
            if (TRUE) {
                bsURL <- paste0("http://mops.twse.com.tw/mops/web/ajax_t163sb05?1&step=1&firstin=1&off=1&TYPEK=sii&year=", year, "&season=0", season, "");
                Source <- read_html(bsURL, encoding = "UTF-8");
                All <- Source %>% html_nodes(css = ".odd td , .hasBorder:nth-child(2) , .even td , th") %>% html_text;
                if (length(All) == 0) break;

                listseason <- All %>% GetIndex() %>% GetbsRaw(All) %>% ClearbsRaw(dateIndex);
                for (k in 1:length(listseason)) { #length(listseason)
                    listall[[k]] <- rbind(listall[[k]], listseason[[k]][,Parameter[[k]]])
                }
                Sys.sleep(sample(1:2, size = 1));
            }
        }
    }
    return(listall)
}

bsreport <- Getbsreport()%>% GetNumericReport() ;
bsreport2 <- Getbsreport();



#----Income Statement-----------#

isURL <- "http://mops.twse.com.tw/mops/web/ajax_t163sb04?1&step=1&firstin=1&off=1&TYPEK=sii&year=105&season=02"

GetIndex <- function(All) {
    Index <- grep(pattern = "[0-9]{4}", All)
    Result <- data.frame(RowS = c(1:length(Index)), RowD = c(1:length(Index)), RowE = c(1:length(Index)), ColS = c(1:length(Index)), ColE = c(1:length(Index)));
    Result$RowS <- Index;
    for (i in 1:length(Index)) {
        if (i == length(Index)) {
            Result$RowD[i] = Result$RowD[i - 1];
        } else {
            Result$RowD[i] = Result$RowS[i + 1] - Result$RowS[i];
        }
    }

    for (i in 1:length(Index)) {
        if (i == length(Index) | i == 1) {
            Result$RowE[i] = Result$RowS[i] + Result$RowD[i] - 1;
            Result$ColS[i] = 0;
            Result$ColE[i] = 0;
        } else {
            if (Result$RowD[i] == Result$RowD[i + 1] | Result$RowD[i] == Result$RowD[i - 1]) {
                Result$RowE[i] = Result$RowS[i] + Result$RowD[i] - 1;
                Result$ColS[i] = 0;
                Result$ColE[i] = 0;
            } else {
                Result$RowE[i] = Result$RowS[i] + Result$RowD[i] - Result$RowD[i + 1] - 1;
                Result$ColS[i] = Result$RowE[i] + 1;
                Result$ColE[i] = Result$RowS[i + 1] - 1;
            }
        }
    }
    return(Result)
}

GetisRaw <- function(IndexD, All) {
    inde <- 1;
    listresult <- list();
    for (i in 1:nrow(IndexD)) {
        if (i == 1) {
            HED <- All[c(1:IndexD$RowD[i])];
            listresult[[i]] <- data.frame(matrix(ncol = length(HED), nrow = 0));
            colnames(listresult[[i]]) <- HED;
            listresult[[i]] <- rbind(listresult[[i]][1,], All[c(IndexD$RowS[i]:IndexD$RowE[i])]);
        } else {
            if (IndexD$ColS[i] != 0) {
                inde <- inde + 1;
                listresult[[inde - 1]] <- rbind(listresult[[inde - 1]], All[c(IndexD$RowS[i]:IndexD$RowE[i])]);
                HED <- All[c(IndexD$ColS[i]:IndexD$ColE[i])];
                listresult[[inde]] <- data.frame(matrix(ncol = length(HED), nrow = 0));
                colnames(listresult[[inde]]) <- HED;
                listresult[[inde]] <- rbind(listresult[[inde]][1,], rep(NA, length(HED)));
                next;
            }
            listresult[[inde]] <- rbind(listresult[[inde]], All[c(IndexD$RowS[i]:IndexD$RowE[i])]);
        }
    }
    return(listresult)
}

ClearisRaw <- function(israw, date) {
    for (i in 1:length(israw)) {
        israw[[i]] <- israw[[i]][complete.cases(israw[[i]]),];
        israw[[i]][, "date"] <- date;
    }
    return(israw)
}


Getisreport <- function() {
    Parameter <- list(
c("公司代號", "利息淨收益","利息以外淨損益", "營業費用", "繼續營業單位本期稅後淨利（淨損）","其他綜合損益（稅後）", "本期綜合損益總額（稅後）", "基本每股盈餘（元）", "date"),
c("公司代號", "收益", "支出及費用","營業外損益", "本期其他綜合損益（稅後淨額）","本期綜合損益總額", "基本每股盈餘（元）", "date"),
c("公司代號", "營業收入", "營業成本", "營業費用","營業外收入及支出", "繼續營業單位本期淨利（淨損）","其他綜合損益（淨額）", "本期綜合損益總額", "基本每股盈餘（元）", "date"),
c("公司代號", "利息淨收益", "利息以外淨收益","營業費用", "本期稅後淨利（淨損）","本期其他綜合損益（稅後淨額）" ,"本期綜合損益總額", "基本每股盈餘（元）", "date"),
c("公司代號", "營業收入", "營業成本", "營業費用","營業外收入及支出" ,"本期淨利（淨損）", "其他綜合損益（稅後淨額）","本期綜合損益總額", "基本每股盈餘（元）", "date"),
c("公司代號", "收入", "支出", "本期淨利（淨損）", "其他綜合損益","本期綜合損益總額", "基本每股盈餘（元）", "date")
    )
    listall <- list();
    dateIndex <- 0;
    for (i in 1:6) {
        listall[[i]] <- data.frame();
    }

    for (i in 1:1) {
        #5
        s <- 1;#4
        if (i == 5) s <- 1;
        for (j in 1:s) {
            #4
            year <- 101 + i;
            season <- j;
            date <- as.numeric(paste0(year, season));
            dateIndex <- dateIndex + 1;
            if (TRUE) {
                bsURL <- paste0("http://mops.twse.com.tw/mops/web/ajax_t163sb04?1&step=1&firstin=1&off=1&TYPEK=sii&year=", year, "&season=0", season, "");
                Source <- read_html(bsURL, encoding = "UTF-8");
                All <- Source %>% html_nodes(css = ".odd td , .hasBorder:nth-child(2) , .even td , th") %>% html_text;
                if (length(All) == 0) break;

                listseason <- All %>% GetIndex() %>% GetisRaw(All) %>% ClearisRaw(dateIndex);
                for (k in 1:length(listseason)) {
                    listall[[k]] <- rbind(listall[[k]], listseason[[k]][, Parameter[[k]]])
                }
                Sys.sleep(sample(2:5, size = 1));
            }
        }
    }
    return(listall)
}

isreport <- Getisreport() %>% GetNumericReport();
isreport2 <- Getisreport()

#---------Cash Flow-------------------#
cfURL <- "http://mops.twse.com.tw/mops/web/ajax_t164sb05?=1&step=1&firstin=1&off=1&keyword4=&code1=&TYPEK2=&checkbtn=&queryName=co_id&inpuType=co_id&TYPEK=all&isnew=false&co_id=2809&year=102&season=04"
cfURL <- RETRY("GET", cfURL,pause_base = 1,pause_cap = 60,quiet = FALSE)
Source <- read_html(cfURL, encoding = "UTF-8");
rowname <- Source %>% html_nodes(css = ".hasBorder td:nth-child(1)") %>% html_text;
amount <- Source %>% html_nodes(css = "td:nth-child(2)") %>% html_text;
?RETRY
#.hasBorder td

Parameter<-c("營業活動之淨現金流入（流出）","　投資活動之淨現金流入（流出）","　籌資活動之淨現金流入（流出）")
result<-data.frame()
result <- rbind(result, as.data.frame(t(amount)))
colnames(result) <- rowname;

result <- result[, Parameter]
result[, "date"] <- 1

Getcfreport <- function()
{
    Parameter <- c("營業活動之淨現金流入（流出）", "　投資活動之淨現金流入（流出）", "　籌資活動之淨現金流入（流出）","　發放現金股利")
    dateIndex <- 0;
    listall <- list();
    for (i in 1:1) {
        #5  
        closeAllConnections();
            year <- 101 + i;
          
            dateIndex <- dateIndex + 1;
            if (TRUE) {
                bsURL <- paste0("http://mops.twse.com.tw/mops/web/ajax_t163sb04?1&step=1&firstin=1&off=1&TYPEK=sii&year=", year, "&season=04");    
                tryCatch(Source <- read_html(bsURL, encoding = "UTF-8"),
                    error = function(e) { print("Allerror");
                        Sys.sleep(sample(10:10, size = 1));
                        closeAllConnections();
                        bsURL <- RETRY("GET", bsURL, times = 5, pause_base = 1, pause_cap = 60, quiet = FALSE);
                        Source <- read_html(bsURL, encoding = "UTF-8")
                    },
                    finally = print(year))
                All <- Source %>% html_nodes(css = ".odd td , .hasBorder:nth-child(2) , .even td , th") %>% html_text;
                if (length(All) == 0) break;
                Index <- grep(pattern = "[0-9]{4}", All);
                Company <- All[Index];
       
                for (company in Company) {
                    cfURL<-paste0("http://mops.twse.com.tw/mops/web/ajax_t164sb05?=1&id=&key=&TYPEK=sii&step=2&year=",year,"&season=4&co_id=",company,"&firstin=1")
                    tryCatch(Source <- read_html(cfURL, encoding = "UTF-8"),
                        error = function(e) {
                            print(Source)
                            closeAllConnections();
                            print(paste(company, "Companyerror"));
                            Sys.sleep(sample(10:10, size = 1));
                            RETRY("GET", cfURL, pause_base = 1, pause_cap = 60, quiet = FALSE);
                            Source <- read_html(cfURL, encoding = "UTF-8")
                        },
                        finally = print(company))
                    rowname <- Source %>% html_nodes(css = ".hasBorder td:nth-child(1)") %>% html_text;
                    amount <- Source %>% html_nodes(css = "td:nth-child(2)") %>% html_text;
                    result <- data.frame()
                    result <- rbind(result, as.data.frame(t(amount)))
                    colnames(result) <- rowname;
                    if (length(result) <= 2) {
                        Sys.sleep(sample(3:3, size = 1));
                        print(paste("length",Source));
                        next;
                    }
                    tryCatch(result <- result[, Parameter], error = function(e) { print(paste("DataEmptyError:", company));print("Select") }, finally = { print(length(result)) })
                    result[, "date"] <- dateIndex;
                    result[, "company"] <- company;
                    tryCatch(listall <- rbind(listall, result), error = function(e) { print("rbind")})
                    Sys.sleep(sample(3:4, size = 1));
                }
                Sys.sleep(sample(2:5, size = 1));
        }
    }
    return(listall)
}
CashFlow02 <- Getcfreport();

CashFlow05$`營業活動之淨現金流入（流出）` <- gsub(pattern = ",", replacement = "", CashFlow05$`營業活動之淨現金流入（流出）`) %>% as.numeric
c<-RETRY("GET","http://mops.twse.com.tw/mops/web/ajax_t164sb05?=1&step=1&firstin=1&off=1&keyword4=&code1=&TYPEK2=&checkbtn=&queryName=co_id&inpuType=co_id&TYPEK=all&isnew=false&co_id=4737&year=102&season=04")
class(c)

?RETRY

#---------Stock Price---------------#

GetSotckPrice <- function(Company) {
    priceall <- list();
bsURL <- paste0("http://mops.twse.com.tw/mops/web/ajax_t163sb04?1&step=1&firstin=1&off=1&TYPEK=sii&year=106&season=01");
Source <- read_html(bsURL, encoding = "UTF-8")
All <- Source %>% html_nodes(css = ".odd td , .hasBorder:nth-child(2) , .even td , th") %>% html_text;
Index <- grep(pattern = "[0-9]{4}", All);
#Company <- All[Index];
for (company in Company) {
    url <- paste0("http://www.twse.com.tw/exchangeReport/FMNPTK?response=json&stockNo=", company, "&_")
    tryCatch(result <- read_json(url) ,error = function(e) {print("error") },finally = print(company))
    price <- data.frame(matrix(ncol = length(result$fields), nrow = 1))
    if (length(result$fields) < 1) { print(result); next; }
    names(price) <- result$fields
    for (r in result$data) {
        price <- rbind(price, r)
    }
    price[, "公司"] <- company
    priceall <- rbind(priceall, price)
    Sys.sleep(sample(2:3, size = 1));
}
    return(priceall)
 }


StockPrice<-priceall[complete.cases(priceall),]


Companylist <- c(1232,
1476,
1537,
1707,
2207,
2231,
2330,
2408,
2707,
2912,
3008,
3045,
3130,
3450,
4807,
6409,
8114,
8422,
9910
)

ttt<-GetSotckPrice(Companylist)



showConnections(all = TRUE)
closeAllConnections()