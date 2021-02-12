library(RODBC)
library(magrittr)

LOCAL<-odbcConnect("LOCAL")

StockMarket_bs_1 <- `StockMarket_script_bs_[[1]]`[, -1]



test <- merge(`StockMarket_script_bs_[[3]]`, Industry, by.x = "X.公司代號.", by.y = "X.公司代號.")
? merge
test2<-merge(test, `StockMarket_script_is_[[3]]`, by.x = c("X.公司代號.", "X.date."), by.y = c("X.公司代號.", "X.date."))

names(StockMarket_bs_1) <- c("公司代號", "現金及約當現金", "應收款項", "資產總額", "負債總額", "股本", "每股參考淨值", "日期")
sqlSave(LOCAL, StockMarket_bs_1, rownames = FALSE, append = TRUE)

StockMarket_script_CashFlowAll$date <- StockMarket_script_CashFlowAll$date * 4
StockMarket_script_CashFlowAll <- StockMarket_script_CashFlowAll[, -1]
names(StockMarket_script_CashFlowAll)<-c("營業活動之淨現金流","投資活動之淨現金流","籌資活動之淨現金流","date","company","自由現金流")
sqlSave(LOCAL, StockMarket_script_CashFlowAll,tablename = "Cash Flow", rownames = FALSE, append = TRUE)

tempt <- CashFlow02
CashFlow02$date <- 4
names(CashFlow02) <- c("營業活動之淨現金流", "投資活動之淨現金流", "籌資活動之淨現金流","發放現金股利", "date", "company")
CashFlow02$發放現金股利 <- gsub(pattern = ",", replacement = "", CashFlow02$發放現金股利) %>% as.numeric

sqlSave(LOCAL, CashFlow02, tablename = "CashFlow02", rownames = FALSE, append = TRUE)

Cash02 <- LOCAL %>% sqlQuery("SELECT *  FROM [master].[dbo].[CashFlow02]")
Cash03 <- LOCAL %>% sqlQuery("SELECT *  FROM [master].[dbo].[CashFlow03]")
Cash04 <- LOCAL %>% sqlQuery("SELECT *  FROM [master].[dbo].[CashFlow04]")
Cash05 <- LOCAL %>% sqlQuery("SELECT *  FROM [master].[dbo].[CashFlow05]")
CF <- rbind(Cash02, Cash03, Cash04, Cash05)
CF[,"自由現金流"]<-CF$`營業活動之淨現金流`+CF$`投資活動之淨現金流`
#---Qry---#
for (i in 1:6) {
QryBS <- paste0("Select * from [master].[dbo].[Balance Sheet_",i,"]")
QryIS <-paste0("Select * from [master].[dbo].[Income Statement_",i,"]")
QryCF<-"Select * from [master].[dbo].[Cash Flow]"
BS<-LOCAL %>% sqlQuery(QryBS)
IS <- LOCAL %>% sqlQuery(QryIS)
#CF <- LOCAL %>% sqlQuery(QryCF)
m1 <- merge(CF, BS, by.x = c("date", "company"), by.y = c("date", "公司代號"))
m2 <- merge(m1, IS, by.x = c("date", "company"), by.y = c("date", "公司代號"))
sqlSave(LOCAL, m2, tablename = paste0("Aggregate_",i,"_New"), rownames = FALSE, append = TRUE)
}
