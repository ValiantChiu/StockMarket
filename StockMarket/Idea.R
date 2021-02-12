library(httr)
library(magrittr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)


Sys.setlocale("LC_ALL", "cht")
companylist <- readRDS(file = "companylist.rds") %>% as.tibble %>% filter(そ腹 != 'そ腹')
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
        df$Θユ计 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$Θユ肂 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$害禴基畉 %<>% gsub(pattern = "+", replacement = "") %>% gsub(pattern = " ", replacement = "") %>% as.numeric
        df$Θユ掸计 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$秨絃基 %<>% as.numeric
        df$程蔼基 %<>% as.numeric
        df$程基 %<>% as.numeric
        df$Μ絃基 %<>% as.numeric
        df
    }
    GetPriceDifference <- function(df) {
        for (i in 2:nrow(df)) {
            df$害禴基畉[i] <- df$Μ絃基[i] - df$Μ絃基[i - 1]
        }
        df
    }
    GetPrice <- function() {
        Year <- 2018:2018
        Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
        Price <- tibble()
        for (year in Year) {
            if (year == 2018) { Month <- c( '08') }
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
        Price %>% CleanPrice %>% GetPriceDifference %>% arrange(ら戳)
    } else {
        Price
    }

}
PriceAug <- readRDS(file = "PriceAug.rds")

for (i in 919:nrow(companylist)) {
    print(i)
    PriceOne <- companylist[i,] %>% mutate(price = map(そ腹, GetStockPriceByYear))
    PriceAug <- rbind(PriceAug, PriceOne)
    saveRDS(PriceAug, file = "PriceAug.rds")
}

Price201810 <- readRDS(file = "Price201810.rds")
Price201809 <- readRDS(file = "Price201809.rds")
PriceAll_201001_201808 <- readRDS(file = "PriceAll_201001_201808.rds")
Result<-PriceAll_201001_201808 %>% mutate(Result = map2(そ腹, price, GetDistribution))
Result2 <- Result %>% mutate(MapD = map2(そ腹,Result, GetMapDensity))
#Analysis
TimeWindow <- 10
PredictWindow <- 5
TradeFarePercent <- 0.1425
GovernmentTaxPercent <- 0.1
ExpectedProfitPercent <- 1.1
Price0050 <- readRDS(file = "Price0050Raw.rds")
PriceAll <- readRDS(file = "PriceAll.rds")
PriceOne <- PriceAll %>% filter(そ腹 == '2301')
PriceAll %>% select(そ腹) %>% View
GetDistribution <- function(そ腹, df) {
    print(そ腹)
    Feature <- function(df) {
        GetPriceDifference <- function(df) {
            for (i in 2:nrow(df)) {
                df$害禴基畉[i] <- df$Μ絃基[i] - df$Μ絃基[i - 1]
            }
            df
        }
        df <- df %>% GetPriceDifference
        Last <- df[1:(nrow(df) - 1),] %>%
        select(Θユ计, Θユ肂, 秨絃基, 程蔼基, 程基, Μ絃基, 害禴基畉, Θユ掸计)
        names(Last) <- c("Θユ计l", "Θユ肂l", "秨絃基l", "程蔼基l", "程基l", "Μ絃基l", "害禴基畉l", "Θユ掸计l")
        New <- df[2:nrow(df),]
        DIFF <- cbind(New, Last) %>%
        mutate(Θユ计D = (Θユ计 - Θユ计l) / Θユ计,
           Θユ肂D = (Θユ肂 - Θユ肂l) / Θユ肂,
           秨絃基D = (秨絃基 - 秨絃基l) / 秨絃基,
           程蔼基D = (程蔼基 - 程蔼基l) / 程蔼基,
           程基D = (程基 - 程基l) / 程基,
           Μ絃基D = (Μ絃基 - Μ絃基l) / Μ絃基,
           害禴基畉D = (害禴基畉 - 害禴基畉l) / 害禴基畉,
    Θユ掸计D = (Θユ掸计 - Θユ掸计l) / Θユ掸计) %>%
        select(ら戳, Μ絃基, Θユ计D, Θユ肂D, 秨絃基D, 程蔼基D, 程基D, Μ絃基D, 害禴基畉D, Θユ掸计D)
        DIFF
    }
    Price <- function(df) {
        Last <- df[2:(nrow(df) - 2),]
        Price <- df[4:nrow(df),] %>% select(Μ絃基) %>% rename(nextprice = Μ絃基)
        Result <- cbind(Last, Price) %>% as.tibble %>% mutate(priceD = (nextprice - Μ絃基) / Μ絃基)
        Result
    }
    df <- df %>% arrange(ら戳) %>% Feature %>% Price
    #result <- df %>% select(Μ絃基D, Θユ掸计D, priceD) %>% mutate(index = ifelse(priceD >= 0.01, "1", "0"))
    result <- df %>% mutate(index = ifelse(priceD >= (1.1 + 0.1425*2+0.3)/100, "1", "0"))

    result
}

ResultSome <- Price0050 %>% GetDistribution("0050", .) #%>% filter(index == 1)

Result <- PriceOne %>% mutate(nrown = map(price, ~ nrow(.)) %>% as.numeric) %>% filter(nrown != 0) %>% mutate(Result = map2(そ腹, price, GetDistribution))

ResultSome <- Result %>% select(-price) %>% unnest %>% filter(!is.na(index)) %>% .[,c(-1,-2,-3)]# %>% filter(index==1)
ggplot(data = ResultSome) + geom_point(mapping = aes(x = Θユ肂D, y = Μ絃基D, color = index), position = "jitter", size = 1)# + coord_cartesian(xlim = c(-0.05, 0.05), ylim = c(-0.05, 0.05))


#pairs(iris[1:4], main = "Edgar Anderson's Iris Data", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])
#pairs(ResultSome[3:10], pch = 25, bg = c("red", "blue")[unclass(ResultSome$index %>% as.factor)])



#GetRecipe
Result <- PriceAll %>% mutate(nrown = map(price, ~ nrow(.)) %>% as.numeric) %>% filter(nrown != 0) %>% mutate(Result = map2(そ腹, price, GetDistribution)) %>% select(-price)

GetMapDensity <- function(そ腹, ResultSome) {
    print(そ腹)
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
        MapFiltered <- Map %>% filter(Μ絃基D >= CPS & Μ絃基D < CPE & Θユ肂D >= TVS & Θユ肂D < TVE)
        Day <- MapFiltered %>% select(ら戳)
        MapFiltered0 <- MapFiltered %>% filter(index == 0) %>% nrow
        MapFiltered1 <- MapFiltered %>% filter(index == 1) %>% nrow
        MapFiltered01 <- MapFiltered0 + MapFiltered1
        tibble(Day=list(Day), MapFiltered0, MapFiltered1, MapFiltered01, density = MapFiltered1 / MapFiltered01)
    }
    Map <- ResultSome %>% select(ら戳, Θユ肂D, Μ絃基D, index)
    MapInterval <- GetMapInterval()
    Result <- MapInterval %>% mutate(result = pmap(list(CPS, CPE, TVS, TVE), GetMapDensity)) %>% unnest
    Result %>% filter(density>=0.6)
}
Result <- GetMapDensity(ResultSome)
AllRecipe<-Result %>% mutate(Recipe = map(Result,GetMapDensity))

AllRecipe %>% select(Recipe) %>% unnest %>% select(Day) %>% unnest %>% View

#Input
Sys.setlocale("LC_ALL", "cht")
companylist <- readRDS(file = "companylist.rds") %>% as.tibble %>% filter(そ腹 != 'そ腹')
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
        df$Θユ计 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$Θユ肂 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$害禴基畉 %<>% gsub(pattern = "+", replacement = "") %>% gsub(pattern = " ", replacement = "") %>% as.numeric
        df$Θユ掸计 %<>% gsub(pattern = ",", replacement = "") %>% as.numeric
        df$秨絃基 %<>% as.numeric
        df$程蔼基 %<>% as.numeric
        df$程基 %<>% as.numeric
        df$Μ絃基 %<>% as.numeric
        df
    }
    GetPriceDifference <- function(df) {
        for (i in 2:nrow(df)) {
            df$害禴基畉[i] <- df$Μ絃基[i] - df$Μ絃基[i - 1]
        }
        df
    }
    GetPrice <- function() {
        Year <- 2018:2018
        Month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
        Price <- tibble()
        for (year in Year) {
            if (year == 2018) { Month <- c('08') }
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
        Price %>% CleanPrice %>% GetPriceDifference %>% arrange(ら戳)
    } else {
        Price
    }

}
PriceAug <- readRDS(file = "PriceAug.rds")
df<-PriceAug$price[[1]]
GetLastest <- function(df) {
    df %>% arrange(desc(ら戳)) %>% .[1:2,] %>% arrange(ら戳)
}
PriceInput <- PriceAug %>% mutate(input = map(price, GetLastest)) %>% select(-price)
GetFeature <- function(そ腹, df) {
    print(そ腹)
    Feature <- function(df) {
        GetPriceDifference <- function(df) {
            for (i in 2:nrow(df)) {
                df$害禴基畉[i] <- df$Μ絃基[i] - df$Μ絃基[i - 1]
            }
            df
        }
        df <- df %>% GetPriceDifference
        Last <- df[1:(nrow(df) - 1),] %>%
        select(Θユ计, Θユ肂, 秨絃基, 程蔼基, 程基, Μ絃基, 害禴基畉, Θユ掸计)
        names(Last) <- c("Θユ计l", "Θユ肂l", "秨絃基l", "程蔼基l", "程基l", "Μ絃基l", "害禴基畉l", "Θユ掸计l")
        New <- df[2:nrow(df),]
        DIFF <- cbind(New, Last) %>%
        mutate(Θユ计D = (Θユ计 - Θユ计l) / Θユ计,
           Θユ肂D = (Θユ肂 - Θユ肂l) / Θユ肂,
           秨絃基D = (秨絃基 - 秨絃基l) / 秨絃基,
           程蔼基D = (程蔼基 - 程蔼基l) / 程蔼基,
           程基D = (程基 - 程基l) / 程基,
           Μ絃基D = (Μ絃基 - Μ絃基l) / Μ絃基,
           害禴基畉D = (害禴基畉 - 害禴基畉l) / 害禴基畉,
    Θユ掸计D = (Θユ掸计 - Θユ掸计l) / Θユ掸计) %>%
        select(ら戳, Μ絃基, Θユ计D, Θユ肂D, 秨絃基D, 程蔼基D, 程基D, Μ絃基D, 害禴基畉D, Θユ掸计D)
        DIFF
    }
    result <- df %>% arrange(ら戳) %>% Feature 
    result %>% as.tibble
}

Feature<-PriceInput %>% mutate(nrown = map(input, ~ ncol(.)) %>% as.numeric) %>% filter(nrown == 9) %>% mutate(Feature = map2(そ腹, input, GetFeature))

#Prediction
FeatureTest <- Feature
PredictSource <- FeatureTest %>% left_join(AllRecipe %>% select(そ腹, Recipe))
PredictSource <- PredictSource %>% select(-nrown) %>% mutate(nrown = map(Recipe, ~ is.null(.) )%>% as.logical) %>% filter(!nrown )


GetAction <- function(Feature, Recipe) {
    Getaction <- function(CPS, CPE, TVS, TVE) {
        MapFiltered <- Feature %>% filter(Μ絃基D >= CPS & Μ絃基D < CPE & Θユ肂D >= TVS & Θユ肂D < TVE)
        right <- MapFiltered %>% nrow
        tibble(right)
    }
    Feature <- Feature %>% select(Μ絃基D, Θユ肂D)
    Recipe %>% mutate(result = pmap(list(CPS, CPE, TVS, TVE), Getaction)) %>% select(result) %>% unnest %>% .$right %>% sum

}
PredictionResult <- PredictSource %>% mutate(Action = map2(Feature, Recipe, GetAction) %>% as.numeric)

PredictionResult %>% filter(Action==1)
PredictionResult 
PriceAug %>% filter(そ腹 == 1477) %>% unnest %>% arrange(desc(ら戳))
OneFeature <- PredictSource$Feature[[1]] %>% select(Μ絃基D, Θユ肂D)
Rec <- PredictSource$Recipe[[1]]
GetAction(OneFeature,Rec)
GetMapDensity <- function(CPS, CPE, TVS, TVE) {
    MapFiltered <- OneFeature %>% filter(Μ絃基D >= CPS & Μ絃基D < CPE & Θユ肂D >= TVS & Θユ肂D < TVE)
    right <- MapFiltered  %>% nrow
    tibble(right)
}
Rec %>% mutate(result = pmap(list(CPS, CPE, TVS, TVE), GetMapDensity)) %>% select(result) %>% unnest %>% .$right %>% sum

#
library(mclust)
Result$Day
Result %>% View
ResultSome$`Θユ肂D` %>% min
den <- densityMclust(ResultSome %>% filter(index == 0) %>% select(Θユ肂D, 秨絃基D))
plot(den, ResultSome %>% filter(index == 0) %>% select(Θユ肂D, 秨絃基D),nlevels=55)
2

ResultSome %>% filter(Θユ肂D < 0 & 秨絃基D < -0.025 ) %>% .$index %>% table
284/(284+717)
ResultSome %>% filter(Θユ肂D < 0 & 秨絃基D > 0) %>% .$index %>% table
310/(310+824)
#RF
library(unbalanced)
library(randomForest)

dt <- sort(sample(nrow(ResultSome), nrow(ResultSome) * .8))
dt<-1:1900
train <- ResultSome[dt,]
test <- ResultSome[-dt,] %>% .[complete.cases(.),]


n <- ncol(train)
output <- train$index %>% as.factor

input <- train[, c(-1,-2,-9,-11,-12,- n)] %>% as.tibble
data <- ubSMOTE(X = input, Y = output, perc.over = 200, k = 5, perc.under = 200, verbose = TRUE)

newData <- cbind(data$X, data$Y) %>% .[complete.cases(.),]

model <- randomForest(x = newData[, 1:6], y = (newData$`data$Y` %>% as.factor), ntree = 500, importance = TRUE,
                      proximity = TRUE)
prediction <- predict(model, test[, c(-1, -2, -9, -11, -12, - n)])
caret::confusionMatrix(prediction, test$index %>% as.factor)

test[, c(-1, -2, -9, -11, -12, - n)] %>% View

newData$`data$Y` %>% as.factor  %>% length
newData[, 1:6] %>% nrow
sapply(newData$`data$Y` %>% as.factor, function(x) sum(is.na(x)))
? randomForest

33 / (93 + 33)

10 / 33
50 / (118 + 57 + 35 + 15)
15/(57+15)