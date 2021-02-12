library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)


Sys.setlocale("LC_ALL", "cht")

Price201810 <- readRDS(file = "Price201810.rds")
Price201809 <- readRDS(file = "Price201809.rds")
PriceAll_201001_201808 <- readRDS(file = "PriceAll_201001_201808.rds")

GetDistribution <- function(公司代號, df) {
    ProfitVariable<-1
    print(公司代號)
    Feature <- function(df) {
        GetPriceDifference <- function(df) {
            for (i in 2:nrow(df)) {
                df$漲跌價差[i] <- df$收盤價[i] - df$收盤價[i - 1]
            }
            df
        }
        df <- df %>% GetPriceDifference
        Last <- df[1:(nrow(df) - 1),] %>%
        select(成交股數, 成交金額, 開盤價, 最高價, 最低價, 收盤價, 漲跌價差, 成交筆數)
        names(Last) <- c("成交股數l", "成交金額l", "開盤價l", "最高價l", "最低價l", "收盤價l", "漲跌價差l", "成交筆數l")
        New <- df[2:nrow(df),]
        DIFF <- cbind(New, Last) %>%
        mutate(成交股數D = (成交股數 - 成交股數l) / 成交股數,
           成交金額D = (成交金額 - 成交金額l) / 成交金額,
           開盤價D = (開盤價 - 開盤價l) / 開盤價,
           最高價D = (最高價 - 最高價l) / 最高價,
           最低價D = (最低價 - 最低價l) / 最低價,
           收盤價D = (收盤價 - 收盤價l) / 收盤價,
           漲跌價差D = (漲跌價差 - 漲跌價差l) / 漲跌價差,
    成交筆數D = (成交筆數 - 成交筆數l) / 成交筆數) %>%
        select(日期, 收盤價, 最低價,開盤價, 成交股數D, 成交金額D, 開盤價D, 最高價D, 最低價D, 收盤價D, 漲跌價差D, 成交筆數D)
        DIFF
    }
    Price <- function(df) {
        Last <- df[1:(nrow(df) - 3),]
        Price <- df[3:(nrow(df) - 1),] %>% select(收盤價) %>% rename(nextprice = 收盤價)
        LowPrice <- df[2:(nrow(df) - 2),] %>% select(最低價) %>% rename(thresholdprice = 最低價)
        FinalPrice <- df[4:nrow(df),] %>% select(開盤價) %>% rename(finalprice = 開盤價)
        Result <- cbind(Last, Price, LowPrice, FinalPrice) %>% as.tibble %>% mutate(priceD = (nextprice - 收盤價) / 收盤價)
        Result
    }
    df <- df %>% arrange(日期) %>% Feature %>% Price
    #result <- df %>% select(收盤價D, 成交筆數D, priceD) %>% mutate(index = ifelse(priceD >= 0.01, "1", "0"))
    result <- df %>% mutate(index = ifelse(priceD >= (ProfitVariable + 0.1425 * 2 + 0.3) / 100, "1", "0"))
    result <- result %>%
    mutate(BuyIndex = ifelse(收盤價 >= thresholdprice, 1, 0), SellIndex = ifelse(priceD >= (ProfitVariable + 0.1425 * 2 + 0.3) / 100, 1, 0)) %>%
    mutate(FinalEarning = ((finalprice - 收盤價) / 收盤價) - (0.1425 * 2 + 0.3) / 100) %>% mutate(Profit = ProfitVariable / 100) %>% filter(complete.cases(.))
    GetFinalResult <- function(日期, BuyIndex, SellIndex, FinalEarning, Profit) {
        #print(日期)
        if (BuyIndex == 0) {
            0
        } else if (SellIndex == 0) {
            FinalEarning
        } else if (SellIndex != 0) {
            Profit
        }
    }
    result <- result %>% mutate(FinalResult = pmap(list(日期, BuyIndex, SellIndex, FinalEarning, Profit), GetFinalResult) %>% as.numeric)

    result
}
GetMapDensity <- function(公司代號, ResultSome) {
    print(公司代號)
    GetMapInterval <- function() {
        CP <- seq(-0.1, 0.1, by = 0.025)
        CPS <- CP[1:(length(CP) - 1)]
        CPE <- CP[2:length(CP)]
        TV <- seq(-2, 1, by = 0.25)
        TVS <- TV[1:(length(TV) - 1)]
        TVE <- TV[2:length(TV)]
        CPMatrix <- tibble(CPS, CPE)
        TVMatrix <- tibble(TVS, TVE)
        CPMatrix %>% mutate(TV = list(TVMatrix)) %>% unnest
    }
    GetMapDensity <- function(CPS, CPE, TVS, TVE) {
        MapFiltered <- Map %>% filter(收盤價D >= CPS & 收盤價D < CPE & 成交金額D >= TVS & 成交金額D < TVE)
        Day <- MapFiltered %>% select(日期)
        MapFiltered0 <- MapFiltered %>% filter(BuyIndex == 1 & SellIndex==1) %>% nrow
        MapFiltered1 <- MapFiltered %>% filter(BuyIndex == 1 & SellIndex == 0) %>% nrow
        MapFiltered01 <- MapFiltered0 + MapFiltered1
        #tibble(Day = list(Day), MapFiltered0, MapFiltered1, MapFiltered01, density = MapFiltered1 / MapFiltered01)
        tibble(Day = list(Day), Profit = MapFiltered$FinalResult %>% sum, MapFiltered1, MapFiltered01, density = MapFiltered0 / MapFiltered01)
    }
    Map <- ResultSome #%>% select(日期, 成交金額D, 收盤價D, index)
    MapInterval <- GetMapInterval()
    Result <- MapInterval %>% mutate(result = pmap(list(CPS, CPE, TVS, TVE), GetMapDensity)) %>% unnest
    Result %>% filter(density >= 1)
}

ResultSome<-OneCase %>% GetDistribution("", .)
MapDensity <- ResultSome %>% GetMapDensity("",.)

ResultSome %>% filter(收盤價D >= -0.075 & 收盤價D < -0.05 & 成交金額D >= -0.25 & 成交金額D < 0) %>% .$FinalResult
ResultSome %>% filter(收盤價D >= 0.05 & 收盤價D < 0.075 & 成交金額D >= -1.25 & 成交金額D < -1) %>% .$日期
ResultSome %>% filter(日期 == "100/08/29") %>% View
OneCase %>% filter(日期 == "100/08/26" | 日期 == "100/08/29" | 日期 == "100/08/30" | 日期 == "100/08/31")


#Test
GetAction <- function(Feature, Recipe) {
    Getaction <- function(CPS, CPE, TVS, TVE) {
        MapFiltered <- Feature %>% filter(收盤價D >= CPS & 收盤價D < CPE & 成交金額D >= TVS & 成交金額D < TVE)
        #right <- MapFiltered$FinalResult %>% sum
        right <- MapFiltered %>%  select(FinalResult)

        #tibble(right)
        right
    }
    Feature <- Feature # %>% select(收盤價D, 成交金額D)
    Recipe %>% mutate(result = pmap(list(CPS, CPE, TVS, TVE), Getaction)) #%>% select(result) %>% unnest %>% .$right %>% sum

}
AllRecipe <- AllRecipe %>% mutate(nrown = map(Recipe, ~ nrow(.)) %>% as.numeric) %>% filter(nrown!=0)
GetAction(Feature, MapDensity) %>% select(-Day) %>% unnest
Price201809 <- Price201809 %>% rename(P201809=price)
TestSet <- AllRecipe %>% left_join(Price201809) %>% mutate(nrown = map(P201809, ~ nrow(.)) %>% as.numeric) %>% filter(nrown!=0)
TestSet <- TestSet %>% mutate(testfeatures = map2(公司代號, P201809, GetDistribution))

dealcount <- Price201809 %>% unnest %>% group_by(公司代號) %>% summarise(成交筆數 = sum(成交筆數)) %>% arrange(desc(成交筆數))
dealcount <- dealcount[1:10,]
result <- TestSet %>% mutate(testresult = map2(testfeatures, Recipe, GetAction))
result2<-result %>% select(公司代號, testresult) %>% unnest %>% select(-Day) %>% unnest %>% group_by(公司代號) %>% summarise(sp = sum(FinalResult)) 
result2 %>% View
result2 %>% filter(sp > 0) 
result2 %>% filter(sp == 0)
result2 %>% filter(sp < 0) 

dealcount %>% left_join(result2) %>% filter(!is.na(sp)) %>% .$sp %>% sum
result2$sp %>% sum
TestSet %>% select(Recipe) %>% View
#%>% .$sp %>% sum

#Get Recipe

Distribution <- PriceAll_201001_201808 %>% mutate(Distribution = map2(公司代號, price, GetDistribution))
saveRDS(Distribution, file = "Distribution.rds")
Distribution <- readRDS(file = "Distribution.rds")
AllRecipe <- Distribution %>% mutate(Recipe = map2(公司代號, Distribution, GetMapDensity))
saveRDS(AllRecipe, file = "AllRecipe.rds")


Distribution$Distribution[[1]]
AllRecipe$Recipe[[1]]

AllRecipe$Recipe[[1]] %>% .$Day
AllRecipe$Distribution[[1]] %>% filter(日期 == '100/07/13') %>% View
#Prediction
Sys.setlocale("LC_ALL", "cht")
companylist <- readRDS(file = "companylist.rds") %>% as.tibble %>% filter(公司代號 != '公司代號')
#Collect Price
GetStockPriceByMonth <- function(Stock) {

    GetOneMonthStockPrice <- function(Stock, Year, Month) {
        Purl <- paste0("http://www.twse.com.tw/exchangeReport/STOCK_DAY?response=json&date=", Year, Month, "01&stockNo=", Stock, "")
        Price <- POST(Purl)
        Price <- Price %>% content(as = "text") %>% fromJSON
        PriceData <- Price$data %>% as.tibble
        names(PriceData) <- Price$fields
        PriceData
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
            df$漲跌價差[i] <- df$收盤價[i] - df$收盤價[i - 1]
        }
        df
    }
    GetPrice <- function() {
        Year <- 2010:2018
        Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
        Price <- tibble()
        for (year in Year) {
            if (year == 2018) { Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11') }
            for (month in Month) {
                print(paste0(year, "", month))
                Result <- GetOneMonthStockPrice(Stock, year, month)
                Price <- rbind(Price, Result)
                Sys.sleep(sample(4:10, size = 1))
            }
        }
        Price
    }
    Price <- tibble()
    tryCatch(Price <- GetPrice(), error = function(e) { print(paste0(Stock, e)) }, finally = print("Hello"))
    if (nrow(Price) != 0) {
        Price %>% CleanPrice %>% GetPriceDifference %>% arrange(日期)
    } else {
        Price
    }

}
Price201811<-tibble()
for (i in 1:nrow(companylist)) {
    print(i)
    PriceOne <- companylist[i,] %>% mutate(price = map(公司代號, GetStockPriceByYear))
    #PriceAll <- rbind(PriceAll, PriceOne)
    Price201811 <- rbind(Price201810, PriceOne)
    saveRDS(Price201811, file = "Price201811.rds")
}
GetLastest <- function(df) {
    df %>% arrange(desc(日期)) %>% .[1:2,] %>% arrange(日期)
}
PriceInput <- Price201811 %>% mutate(input = map(price, GetLastest)) %>% select(-price)
GetFeature <- function(公司代號, df) {
    print(公司代號)
    Feature <- function(df) {
        GetPriceDifference <- function(df) {
            for (i in 2:nrow(df)) {
                df$漲跌價差[i] <- df$收盤價[i] - df$收盤價[i - 1]
            }
            df
        }
        df <- df %>% GetPriceDifference
        Last <- df[1:(nrow(df) - 1),] %>%
        select(成交股數, 成交金額, 開盤價, 最高價, 最低價, 收盤價, 漲跌價差, 成交筆數)
        names(Last) <- c("成交股數l", "成交金額l", "開盤價l", "最高價l", "最低價l", "收盤價l", "漲跌價差l", "成交筆數l")
        New <- df[2:nrow(df),]
        DIFF <- cbind(New, Last) %>%
        mutate(成交股數D = (成交股數 - 成交股數l) / 成交股數,
           成交金額D = (成交金額 - 成交金額l) / 成交金額,
           開盤價D = (開盤價 - 開盤價l) / 開盤價,
           最高價D = (最高價 - 最高價l) / 最高價,
           最低價D = (最低價 - 最低價l) / 最低價,
           收盤價D = (收盤價 - 收盤價l) / 收盤價,
           漲跌價差D = (漲跌價差 - 漲跌價差l) / 漲跌價差,
    成交筆數D = (成交筆數 - 成交筆數l) / 成交筆數) %>%
        select(日期, 收盤價, 成交股數D, 成交金額D, 開盤價D, 最高價D, 最低價D, 收盤價D, 漲跌價差D, 成交筆數D)
        DIFF
    }
    result <- df %>% arrange(日期) %>% Feature
    result %>% as.tibble
}
Feature <- PriceInput %>% mutate(nrown = map(input, ~ ncol(.)) %>% as.numeric) %>% filter(nrown == 9) %>% mutate(Feature = map2(公司代號, input, GetFeature))

FeatureTest <- Feature
PredictSource <- FeatureTest %>% left_join(AllRecipe %>% select(公司代號, Recipe))
PredictSource <- PredictSource %>% select(-nrown) %>% mutate(nrown = map(Recipe, ~ is.null(.)) %>% as.logical) %>% filter(!nrown)

GetAction <- function(Feature, Recipe) {
    Getaction <- function(CPS, CPE, TVS, TVE) {
        MapFiltered <- Feature %>% filter(收盤價D >= CPS & 收盤價D < CPE & 成交金額D >= TVS & 成交金額D < TVE)
        right <- MapFiltered %>% nrow
        tibble(right)
    }
    Feature <- Feature %>% select(收盤價D, 成交金額D)
    Recipe %>% mutate(result = pmap(list(CPS, CPE, TVS, TVE), Getaction)) %>% select(result) %>% unnest %>% .$right %>% sum

}
PredictionResult <- PredictSource %>% mutate(Action = map2(Feature, Recipe, GetAction) %>% as.numeric)





##Other Strategy(doesn't work)
OneCase <- PriceAll_201001_201808[2,]
Result <- OneCase %>% mutate(Result = map2(公司代號, price, GetDistribution))
#Result2 <- Result %>% mutate(MapD = map2(公司代號, Result, GetMapDensity))

check <- Result$Result[[1]]

df <- OneCase$price[[1]]# %>% .[1:6,]


Feature <- function(df) {
    GetPriceDifference <- function(df) {
        for (i in 2:nrow(df)) {
            df$漲跌價差[i] <- df$收盤價[i] - df$收盤價[i - 1]
        }
        df
    }
    df <- df %>% GetPriceDifference
    Last <- df[1:(nrow(df) - 1),] %>%
        select(成交股數, 成交金額, 開盤價, 最高價, 最低價, 收盤價, 漲跌價差, 成交筆數)
    names(Last) <- c("成交股數l", "成交金額l", "開盤價l", "最高價l", "最低價l", "收盤價l", "漲跌價差l", "成交筆數l")
    New <- df[2:nrow(df),]
    DIFF <- cbind(New, Last) %>%
        mutate(成交股數D = (成交股數 - 成交股數l) / 成交股數,
           成交金額D = (成交金額 - 成交金額l) / 成交金額,
           開盤價D = (開盤價 - 開盤價l) / 開盤價,
           最高價D = (最高價 - 最高價l) / 最高價,
           最低價D = (最低價 - 最低價l) / 最低價,
           收盤價D = (收盤價 - 收盤價l) / 收盤價,
           漲跌價差D = (漲跌價差 - 漲跌價差l) / 漲跌價差,
    成交筆數D = (成交筆數 - 成交筆數l) / 成交筆數) %>%
        select(日期, 收盤價, 最低價, 成交股數D, 成交金額D, 開盤價D, 最高價D, 最低價D, 收盤價D, 漲跌價差D, 成交筆數D)
    DIFF
}
Price <- function(df) {
    Last <- df[1:(nrow(df) - 3),]
    Price <- df[3:(nrow(df) - 1),] %>% select(收盤價) %>% rename(nextprice = 收盤價)
    LowPrice <- df[2:(nrow(df) - 2),] %>% select(最低價) %>% rename(thresholdprice = 最低價)
    FinalPrice <- df[4:nrow(df),] %>% select(最低價) %>% rename(finalprice = 最低價)
    Result <- cbind(Last, Price, LowPrice, FinalPrice) %>% as.tibble %>% mutate(priceD = (nextprice - 收盤價) / 收盤價)
    Result
}
df %<>% Feature %>% Price

df <- df %>% select(-最低價, - 成交股數D, - 成交金額D, - 開盤價D, - 最高價D, - 最低價D, - 收盤價D, - 漲跌價差D, - 成交筆數D) %>% filter(complete.cases(.))
ProfitVariable<-0.1
df <- df %>%
    mutate(BuyIndex = ifelse(收盤價 >= thresholdprice, 1, 0), SellIndex = ifelse(priceD >= (ProfitVariable + 0.1425 * 2 + 0.3) / 100, 1, 0)) %>%
    mutate(FinalEarning = ((finalprice - 收盤價) / 收盤價) - (0.1425 * 2 + 0.3) / 100) %>% mutate(Profit=ProfitVariable/100)
GetFinalResult <- function(日期, BuyIndex, SellIndex, FinalEarning, Profit) {
    print(日期)
    if (BuyIndex == 0) {
        0
    } else if (SellIndex==0) {
        FinalEarning
    } else if (SellIndex!=0) {
        Profit
    }
}
df <- df %>% mutate(FinalResult = pmap(list(日期,BuyIndex, SellIndex, FinalEarning, Profit), GetFinalResult) %>% as.numeric)
df$FinalEarning %>% sum


#
PriceAll_201001_201808 %>% filter(公司代號 == "0050")

Price0050<-readRDS(file = "Price0050raw.rds")
GetProfit <- function(公司代號, df, ProfitVariable) {
    Feature <- function(df) {
        GetPriceDifference <- function(df) {
            for (i in 2:nrow(df)) {
                df$漲跌價差[i] <- df$收盤價[i] - df$收盤價[i - 1]
            }
            df
        }
        df <- df %>% GetPriceDifference
        Last <- df[1:(nrow(df) - 1),] %>%
        select(成交股數, 成交金額, 開盤價, 最高價, 最低價, 收盤價, 漲跌價差, 成交筆數)
        names(Last) <- c("成交股數l", "成交金額l", "開盤價l", "最高價l", "最低價l", "收盤價l", "漲跌價差l", "成交筆數l")
        New <- df[2:nrow(df),]
        DIFF <- cbind(New, Last) %>%
        mutate(成交股數D = (成交股數 - 成交股數l) / 成交股數,
           成交金額D = (成交金額 - 成交金額l) / 成交金額,
           開盤價D = (開盤價 - 開盤價l) / 開盤價,
           最高價D = (最高價 - 最高價l) / 最高價,
           最低價D = (最低價 - 最低價l) / 最低價,
           收盤價D = (收盤價 - 收盤價l) / 收盤價,
           漲跌價差D = (漲跌價差 - 漲跌價差l) / 漲跌價差,
    成交筆數D = (成交筆數 - 成交筆數l) / 成交筆數) %>%
        select(日期, 收盤價, 最低價, 成交股數D, 成交金額D, 開盤價D, 最高價D, 最低價D, 收盤價D, 漲跌價差D, 成交筆數D)
        DIFF
    }
    Price <- function(df) {
        Last <- df[1:(nrow(df) - 3),]
        Price <- df[3:(nrow(df) - 1),] %>% select(收盤價) %>% rename(nextprice = 收盤價)
        LowPrice <- df[2:(nrow(df) - 2),] %>% select(最低價) %>% rename(thresholdprice = 最低價)
        FinalPrice <- df[4:nrow(df),] %>% select(最低價) %>% rename(finalprice = 最低價)
        Result <- cbind(Last, Price, LowPrice, FinalPrice) %>% as.tibble %>% mutate(priceD = (nextprice - 收盤價) / 收盤價)
        Result
    }
    df %<>% Feature %>% Price

    df <- df %>% select(-最低價, - 成交股數D, - 成交金額D, - 開盤價D, - 最高價D, - 最低價D, - 收盤價D, - 漲跌價差D, - 成交筆數D) %>% filter(complete.cases(.))
   
    df <- df %>%
    mutate(BuyIndex = ifelse(收盤價 >= thresholdprice, 1, 0), SellIndex = ifelse(priceD >= (ProfitVariable + 0.1425 * 2 + 0.1) / 100, 1, 0)) %>%
    mutate(FinalEarning = ((finalprice - 收盤價) / 收盤價) - (0.1425 * 2 + 0.1) / 100) %>% mutate(Profit = ProfitVariable / 100)
    #ETF =0.1/1000 others=0.3/100
    GetFinalResult <- function(日期, BuyIndex, SellIndex, FinalEarning, Profit) {
       # print(日期)
        if (BuyIndex == 0) {
            0
        } else if (SellIndex == 0) {
            FinalEarning
        } else if (SellIndex != 0) {
            Profit
        }
    }
    df <- df %>% mutate(FinalResult = pmap(list(日期, BuyIndex, SellIndex, FinalEarning, Profit), GetFinalResult) %>% as.numeric)
    df$FinalResult %>% sum
}
GetProfit("", Price0050, 7)

Experience<-tibble(ExpectedProfit = seq(3, 7, by = 0.1))
Experience <- Experience %>% mutate(Price = list(Price0050))
Result<-Experience %>% mutate(result = pmap(list("", Price, ExpectedProfit), GetProfit))
Result %>% mutate(result = as.numeric(result)) %>% select(-Price) %>% View