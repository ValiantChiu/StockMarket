library(rvest)
library(httr)
library(magrittr)
#library(sqldf)
library(ggplot2)
library(jsonlite)
Parameter <- c("營業活動之淨現金流入（流出）", "　投資活動之淨現金流入（流出）", "　籌資活動之淨現金流入（流出）")
cfURL <- paste0("http://mops.twse.com.tw/mops/web/ajax_t164sb05?=1&id=&key=&TYPEK=sii&step=2&year=102&season=4&co_id=1218&firstin=1");
Source <- read_html(cfURL, encoding = "UTF-8")
rowname <- Source %>% html_nodes(css = ".hasBorder td:nth-child(1)") %>% html_text;
amount <- Source %>% html_nodes(css = "td:nth-child(2)") %>% html_text;
result <- data.frame()
result <- rbind(result, as.data.frame(t(amount)))
colnames(result) <- rowname;

result <- result[, Parameter]
result



bsURL <- paste0("http://mops.twse.com.tw/mops/web/ajax_t163sb04?1&step=1&firstin=1&off=1&TYPEK=sii&year=102&season=04");
tryCatch(Source <- read_html(bsURL, encoding = "UTF-8"), error = function(e) { Sys.sleep(sample(3:3, size = 1)); closeAllConnections(); RETRY("GET", bsURL, times = 5); Source <- read_html(bsURL, encoding = "UTF-8"); print("error") }, finally = print(year))
All <- Source %>% html_nodes(css = ".odd td , .hasBorder:nth-child(2) , .even td , th") %>% html_text;
Index <- grep(pattern = "[0-9]{4}", All);
Company <- All[Index];
sort(Company)

"http://mops.twse.com.tw/mops/web/ajax_t164sb05?=1&step=1&firstin=1&off=1&keyword4=&code1=&TYPEK2=&checkbtn=&queryName=co_id&inpuType=co_id&TYPEK=all&isnew=false&co_id=2867&year=103&season=04"
"http://mops.twse.com.tw/mops/web/ajax_t164sb05?=1&id=&key=&TYPEK=sii&step=2&year=103&season=4&co_id=2867&firstin=1"


dim(CashFlow05)
head(CashFlow05)

CashFlow0203 <- StockMarket_script_CashFlow0203[, c(2, 3, 4, 5, 6)]
CashFlow04 <- StockMarket_script_CashFlow04[, c(2, 3, 4, 5, 6)]
head(CashFlow04)
head(CashFlow0203)
names(CashFlow05) <- c("營業活動之淨現金流", "投資活動之淨現金流", "籌資活動之淨現金流", "Date", "Company")

CashFlow04$Date<-3
CashFlow05$Date <- 4

CashFlowAll <- rbind(CashFlow0203, CashFlow04, CashFlow05)

GoodCompany <- merge(StockMarket_test_CashFlowGoodCompany, StockMarket_script_CashFlowAll, by.x = "Company", by.y = "company")
names(GoodCompany)<-c("Company","count","row","營業活動之淨現金流","投資活動之淨現金流","籌資活動之淨現金流","date","自由現金流")
GoodCompany<-GoodCompany[,c("Company","營業活動之淨現金流","投資活動之淨現金流","籌資活動之淨現金流","date","自由現金流")]
names(GoodCompany) <- c("公司代號", "營業活動之淨現金流", "投資活動之淨現金流", "籌資活動之淨現金流", "date", "自由現金流")
cashflow<-GoodCompany
?merge