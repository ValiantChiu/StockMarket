library(RODBC)
library(ggplot2)
library(magrittr)

LOCAL <- odbcConnect("LOCAL")



#----import data
Qry1<-"SELECT  *  ,[營業活動之淨現金流]/[權益總額] as CoOE
	  ,[本期綜合損益總額稅後]/[權益總額] as ROE
	  ,[自由現金流]/[權益總額] as CfOE
  FROM [master].[dbo].[Aggregate_1]"
Qry2 <- "SELECT  *
	  ,[營業活動之淨現金流]/[權益合計] as CoOE
	  ,[本期綜合損益總額]/[權益合計] as ROE
	  ,[自由現金流]/[權益合計] as CfOE
  FROM [master].[dbo].[Aggregate_2]"
Qry3 <- "SELECT *
	  ,[營業活動之淨現金流]/[權益總額] as CoOE
	  ,[本期綜合損益總額]/[權益總額] as ROE
	  ,[自由現金流]/[權益總額] as CfOE
  FROM [master].[dbo].[Aggregate_3]"
Qry4<- "SELECT *
	    ,[營業活動之淨現金流]/[權益總額] as CoOE
	  ,[本期綜合損益總額]/[權益總額] as ROE
	  ,[自由現金流]/[權益總額] as CfOE
  FROM [master].[dbo].[Aggregate_4]"
Qry5 <- "SELECT *
	     ,[營業活動之淨現金流]/[權益總額] as CoOE
	  ,[本期綜合損益總額]/[權益總額] as ROE
	  ,[自由現金流]/[權益總額] as CfOE
  FROM [master].[dbo].[Aggregate_5]"
Qry6 <- "SELECT *
	  ,[營業活動之淨現金流]/[權益總額] as CoOE
	  ,[本期綜合損益總額]/[權益總額] as ROE
	  ,[自由現金流]/[權益總額] as CfOE
  FROM [master].[dbo].[Aggregate_6]"

R1 <- LOCAL %>% sqlQuery(Qry1)
R2 <- LOCAL %>% sqlQuery(Qry2)
R3 <- LOCAL %>% sqlQuery(Qry3)
R4 <- LOCAL %>% sqlQuery(Qry4)
R5 <- LOCAL %>% sqlQuery(Qry5)
R6 <- LOCAL %>% sqlQuery(Qry6)
#library(sqldf)
#---------------Output png

#mypath<-file.path("C:","Users",)

DrawResult <- function(Sheet,Sheettype)
{
    for (Companytype in Sheet) {
        for (Company in unique(Companytype$公司代號)) {
            for (Colname in colnames(Companytype)) {
                if (Colname == "公司代號" || Colname == "date") next
                qplot(Companytype[Companytype$公司代號 == Company,]$date, Companytype[Companytype$公司代號 == Company,][, c(Colname)], geom = c("point", "line")) + ggtitle(paste(Company, " ", Colname)) + labs(x = "Date", y = Colname)
                ggsave(paste0("Company_", Company, "_", "Index_", Colname, ".png"), path = paste0("C:/Users/user/Investment Plan/StockMarket/",Sheettype))
                unlink(paste0("Company_", Company, "_", "Index_", Colname, ".png"))
            }
        }
    }
}

DrawResultCF <- function(Sheet, Sheettype) {
  
        for (Company in unique(Sheet$公司代號)) {
            for (Colname in colnames(Sheet)) {
                if (Colname == "公司代號" || Colname == "date") next
                qplot(Sheet[Sheet$公司代號 == Company,]$date, Sheet[Sheet$公司代號 == Company,][, c(Colname)], geom = c("point", "line")) + ggtitle(paste(Company, " ", Colname)) + labs(x = "Date", y = Colname)
                ggsave(paste0("Company_", Company, "_", "Index_", Colname, ".png"), path = paste0("C:/Users/user/Documents/StockMarket/", Sheettype))
                unlink(paste0("Company_", Company, "_", "Index_", Colname, ".png"))
            }
        }
    
}

DrawResultROE <- function(Sheet, Sheettype) {

    for (Company in unique(Sheet$company)) {
        for (Colname in colnames(Sheet)) {
            if (!(Colname == "CoOE" || Colname == "ROE" || Colname=="CfOE")) next
            qplot(Sheet[Sheet$company == Company,]$date, Sheet[Sheet$company == Company,][, c(Colname)], geom = c("point", "line")) + ggtitle(paste(Company, " ", Colname)) + labs(x = "Date", y = Colname)
            ggsave(paste0("Company_", Company, "_", "Index_", Colname, ".png"), path = paste0("C:/Users/user/Documents/Investment Plan/StockMarket/", Sheettype))
            unlink(paste0("Company_", Company, "_", "Index_", Colname, ".png"))
        }
    }
}

DrawResultROE(R1, "ROE")
DrawResultROE(R2, "ROE")
DrawResultROE(R3, "ROE")
DrawResultROE(R4, "ROE")
DrawResultROE(R5, "ROE")
DrawResultROE(R6, "ROE")
DrawResult(bsreport, "BalanceSheet")
DrawResult(isreport, "IncomeStatement")
DrawResultCF(cashflow,"CashFlow")
#C:/Users/user/Documents/StockMarket/BalanceSheet

isreport[[6]] <- isreport[[6]][isreport[[6]]$date%%4==0,]
1%%4




#-----Price evaluation

Qry1 <- "SELECT  date,company,[營業活動之淨現金流]/[權益總額] as CoOE
	  ,[本期綜合損益總額稅後]/[權益總額] as ROE
	  ,[自由現金流]/[權益總額] as CfOE
      ,-[發放現金股利]/[權益總額] as 股利密度
	  ,14*[基本每股盈餘元] as 本益比法
	  ,[每股參考淨值]*([本期綜合損益總額稅後]/[權益總額]/0.08) as K值法
      ,-[發放現金股利]/[權益總額] *[每股參考淨值]/0.06 as 股利法
      ,[負債總額]/[資產總額] as 負債比例
FROM [master].[dbo].[Aggregate_1_New]  "
Qry2 <- "SELECT   date,company,
	  [營業活動之淨現金流]/[權益合計] as CoOE
	  ,[本期綜合損益總額]/[權益合計] as ROE
	  ,[自由現金流]/[權益合計] as CfOE
      ,-[發放現金股利]/[權益合計] as 股利密度
	   ,14*[基本每股盈餘元] as 本益比法
	  ,[每股參考淨值]*([本期綜合損益總額]/[權益合計]/0.08) as K值法
      ,-[發放現金股利]/[權益合計] *[每股參考淨值]/0.06 as 股利法
      ,[負債總額]/[資產總額] as 負債比例
FROM [master].[dbo].[Aggregate_2_New] "

Qry3 <- "SELECT   date,company,
	  [營業活動之淨現金流]/[權益總額] as CoOE
	  ,[本期綜合損益總額]/[權益總額] as ROE
	  ,[自由現金流]/[權益總額] as CfOE
      ,-[發放現金股利]/[權益總額] as 股利密度
	  ,14*[基本每股盈餘元] as 本益比法
	  ,[每股參考淨值]*([本期綜合損益總額]/[權益總額]/0.08) as K值法
      ,-[發放現金股利]/[權益總額] *[每股參考淨值]/0.06 as 股利法
      ,[負債總額]/[資產總額] as 負債比例
FROM [master].[dbo].[Aggregate_3_New]"
Qry4 <- "SELECT   date,company,
	    [營業活動之淨現金流]/[權益總額] as CoOE
	  ,[本期綜合損益總額]/[權益總額] as ROE
	  ,[自由現金流]/[權益總額] as CfOE
      ,-[發放現金股利]/[權益總額] as 股利密度
	   ,14*[基本每股盈餘元] as 本益比法
	  ,[每股參考淨值]*([本期綜合損益總額]/[權益總額]/0.08) as K值法
      ,-[發放現金股利]/[權益總額] *[每股參考淨值]/0.06 as 股利法
      ,[負債總額]/[資產總額] as 負債比例
FROM [master].[dbo].[Aggregate_4_New] "
Qry5 <- "SELECT   date,company,
	     [營業活動之淨現金流]/[權益總額] as CoOE
	  ,[本期綜合損益總額]/[權益總額] as ROE
	  ,[自由現金流]/[權益總額] as CfOE
        ,-[發放現金股利]/[權益總額] as 股利密度
	   ,14*[基本每股盈餘元] as 本益比法
	  ,[每股參考淨值]*([本期綜合損益總額]/[權益總額]/0.08) as K值法
      ,-[發放現金股利]/[權益總額] *[每股參考淨值]/0.06 as 股利法
      ,[負債總額]/[資產總額] as 負債比例
FROM [master].[dbo].[Aggregate_5_New]"
Qry6 <- "SELECT   date,company,
	  [營業活動之淨現金流]/[權益總額] as CoOE
	  ,[本期綜合損益總額]/[權益總額] as ROE
	  ,[自由現金流]/[權益總額] as CfOE
        ,-[發放現金股利]/[權益總額] as 股利密度
	   ,14*[基本每股盈餘元] as 本益比法
	  ,[每股參考淨值]*([本期綜合損益總額]/[權益總額]/0.08) as K值法
     ,-[發放現金股利]/[權益總額] *[每股參考淨值]/0.06 as 股利法
      ,[負債總額]/[資產總額] as 負債比例
  FROM [master].[dbo].[Aggregate_6_New]"

R1 <- LOCAL %>% sqlQuery(Qry1)
R2 <- LOCAL %>% sqlQuery(Qry2)
R3 <- LOCAL %>% sqlQuery(Qry3)
R4 <- LOCAL %>% sqlQuery(Qry4)
R5 <- LOCAL %>% sqlQuery(Qry5)
R6 <- LOCAL %>% sqlQuery(Qry6)
All <- rbind(R1, R2, R3, R4, R5, R6)
All[All$company=='1906',]
plot(All[All$date == 16 & All$`負債比例` < 0.5,]$ROE, All[All$date == 16 & All$`負債比例` < 0.5,]$`負債比例`)
test<-(All$`股利密度`>0.15) %>% All[.,]
Big0.2 <- (All$ROE <= 0.2&All$ROE>0.15) %>% All[.,]
(All$company=="6201") %>% All[.,]
All <- All[All$date == 16,]
mean(c(67, 82, 47))
67 + 82 + 48
mean(((All$company == "6201") %>% All[.,])$本益比法)
mean(((All$company == "6201") %>% All[.,])$K值法)
(29.82 + 25.17063)/2