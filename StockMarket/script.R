
trainfeature %>% nrow
trainlabel %>% as.integer %>% sum
Price0050 %>% arrange(desc(日期))
Price0050 %>% filter(日期 < '107/01/01') %>% arrange(desc(日期))

totalnew <- ResultTrain[2001:nrow(ResultTrain),]
#totalnew <- ResultTrain
totalnewfeature <- totalnew %>% ConnectFeature %>% array_reshape(., c(nrow(totalnew), L))
totalnewlabel <- totalnew$label

Model4$model[[1]] %>% evaluate(totalnewfeature, totalnewlabel)
prediction<-Model4$model[[1]] %>% predict_classes(totalnewfeature) %>% as.vector
caret::confusionMatrix(prediction %>% as.character %>% as.factor, totalnewlabel %>% as.factor)
totalnewlabel
Model4$history[[1]] %>% plot

? randomForest

Data %>% arrange(desc(日期))

RFPrice <- Price %>% select(-K, - FD, - D) %>% rbind(June, July) %>% CleanPrice %>% arrange(日期)
RFPrice[1,] %>% rbind(June[1,]) %>% CleanPrice %>%  arrange(日期)
RFPrice %>% View

RFDataRaw %>% arrange(desc(日期))

Price[1,]
RFDataRaw[1,]


RFDataRaw %>% View
RFDataRaw %>%  arrange(desc(日期))
Totalnewdata[1, - n]
feature

OptimizationResultUnnest %>% View
OptimizationResultUnnest %>% group_by(PW) %>% filter(Profit == max(Profit)) %>% View
RFData$Label %>% sum

RFData[1:20,] %>% select(收盤價, Label)

output %>% as.character %>% as.numeric %>% sum
nrow(newdata)

RFDataRaw[nrow(RFDataRaw), ]
Train0056 %>% View
Price0051 %>% View

S3 %>% filter(日期 == "103/03/03")
GetThreeBigInvestorsInform(S3 %>% filter(日期 == "103/03/03"))

PriceInvestor0051 %>% View
Train0051 %>% View
OneMonthPrice %>% filter(日期 > RFPrice$`日期`[nrow(RFPrice)])

mnist <- dataset_mnist()

mnist$test[[1]]

ResultTrain$feature[[1]]

nrow(newdata)


"2018-09-02 09:24:53 CST"
937 * 13 / 60 / 24


PriceAll %>% mutate(nrow = map(price, ~ nrow(.)) %>% as.numeric) %>% filter(nrow != 0) %>% arrange(nrow)


AllRecipe$Recipe[[1]]

check <- TestSet %>% filter(公司代號 == 3037) %>% select(Recipe, P201809)
checkrecipe<-check$Recipe[[1]]
checkprice <- check$P201809[[1]]


result %>% filter(公司代號 == '1262') %>% select(testresult) %>% unnest %>% select(-Day) %>% unnest

result %>% filter(公司代號 == '1262') %>% select(testfeatures) %>% unnest %>% filter(收盤價D >= -0.05 & 收盤價D < -0.025 & 成交金額D >= -1.75 & 成交金額D < -1.5)

TestSet %>% filter(公司代號 == '1262') %>% select(testfeatures) %>% unnest %>% filter(收盤價D >= -0.05 & 收盤價D < -0.025 & 成交金額D >= -1.75 & 成交金額D < -1.5)

TestSet %>% filter(公司代號 == '1262') %>% select(testfeatures) %>% unnest %>% filter(日期 == "107/09/10")
Result %>% arrange(desc(TimeLength)) %>% View
OneSample %>% arrange(desc(日期))
Result %>% filter(日期 > '107/09/01') %>% View
Result %>% filter(MA5D > -0.000001*75 & MA5DY<0.00001) %>% .$TimeLength %>% mean
SEQ <- c()
MTime <- c()
Times<-c()
for (i in 1:20000) {
    print(paste0("Seq:", i))
    SEQ[i]<-i
    MTime[i] <- Result %>% filter(MA5D > -0.000001 *i ) %>% .$TimeLength %>% max
    Times[i] <- Result %>% filter(MA5D > -0.000001 *  i) %>% nrow
}
tibble(SEQ, MTime, Times) %>% filter(MTime <= 300) %>% filter(Times==max(Times))
tibble(SEQ, MTime, Times) %>% View
Result %>% arrange(desc(MA5D)) %>% .$MA5D %>% .[1]

Result$MA5D %>% min

Result %>% select(MA5D, MA5DY,MA5,MA10, TimeLength) %>% filter( MA5>MA10)
Result %>% select(MA5D, MA5DY, MA5, MA10, TimeLength) %>% filter(TimeLength > 90)

Result$MA5DY %>% max
Result$MA5DY %>% min

seq(-0.028, -0.001, by = 0.001)
seq(0, 0.012, by = 0.001)

Parameter <- tibble(LowerBound = seq(-0.028, -0.001, by = 0.001)) %>% mutate(HigherBound = list(tibble(HigherBound=seq(0, 0.012, by = 0.001))))
Parameter <- Parameter %>% unnest

OPTIMIZE <- function(LowerBound, HigherBound) {
    Worse <- Result %>% filter(MA5DY >= LowerBound & MA5DY <= HigherBound) %>% .$TimeLength %>% max
    AVG <- Result %>% filter(MA5DY >= LowerBound & MA5DY <= HigherBound) %>% .$TimeLength %>% mean
    Chance <- Result %>% filter(MA5DY >= LowerBound & MA5DY <= HigherBound) %>% nrow
    tibble(Worse,AVG,Chance)
}

FResult <- Parameter %>% mutate(result = map2(LowerBound, HigherBound, OPTIMIZE))
FResult %>% unnest %>% View

Result$MA10D %>% max

Result %>% filter(TimeLength > 90) %>% View

OneSample <- PriceAll_201001_201811$data[[1]]
OneSample %>% arrange(日期) %>% AddMVandKD %>% GetMAD %>% GetMADD %>% GetThreeK %>% GetLabel %>% select(日期,BuyLabel,收盤價) %>% View
GetLastestLabel <- function(Price, index) {
    print(index)
    nrown<-0
    if (nrow(Price)<140) { nrown<-nrow(Price) } else { nrown<-140}
    Price <- Price[(nrow(Price)-nrown+1):nrow(Price),] %>% filter(complete.cases(.))
    GetLabel <- function(BuySellPrice) {
        GetBuyLabel <- function(MA8, MA21, MA55, MA8D, MA21D, MA55D, 最高價, HKY, HKY2, MA8DD, MA21DD, MA55DD) {
            if ((MA8 > MA21) & (MA21 > MA55) &
                (MA8D > 0) & (MA21D > 0) & MA55D > 0 & MA55 > 0
                ) {
                1
            } else {
                0
            }
        }

        BuySellPrice %>% mutate(BuyLabel = pmap(list(MA8, MA21, MA55, MA8D, MA21D, MA55D, 最高價, HKY, HKY2, MA8DD, MA21DD, MA55DD), GetBuyLabel) %>% as.character)
    }
    Price %>% arrange(日期) %>% AddMVandKD %>% GetMAD %>% GetMADD %>% GetThreeK %>% GetLabel %>% arrange(日期) %>% .[nrow(.),] %>% .$BuyLabel

}

Result <- PriceAll_201001_201811 %>% filter(公司代號 != '3008' & 公司代號 != '911619' & 公司代號 != '911622' & 公司代號 != '1418' & 公司代號 != '1470' &
公司代號 != '910708' & 公司代號 != '910861') %>% mutate(MH = map2(data, 公司代號, GetLastestLabel))

Result %>% mutate(MH = as.character(MH)) %>% filter(MH!=0)

check <- PriceAll_201001_201811 %>% filter(公司代號 == '3530')
Price <- check$data[[1]]


PriceHistory$data[[1]] %>% arrange(desc(日期)) %>% select(日期) %>% .$日期


DAY <- '108/01/22'
Stock <- 1101
Date <- '108/01/23'
DATE <- '108/01/24'
DAte <- '108/01/25'
Date <- DAte
CashYesterday <- 200

StockYesterday <- BoughtList

PriceHistory %>% filter(公司代號 == 6412) %>% unnest %>% filter(日期 > '108/01/23')

data <- SellingList$data[[1]]
BoughtPrice<-41.6
SellingList

PriceHistory %>% filter(公司代號 == 4960) %>% unnest %>% filter(日期 <= DAY) %>% View

BoughtList[0,] %>% .$BoughtPrice %>% sum
GetPrice <- function(Stock, Date) {
    StockPrice <- PriceHistory %>% filter(公司代號 == 1101) %>% unnest %>% filter(日期 == Date)
    TodayPrice <- StockPrice$`最高價`[1]
    YesterdayPrice <- PriceHistory %>% filter(公司代號 == 1101) %>% unnest %>% filter(日期 < Date) %>% arrange(desc(日期)) %>% .$收盤價 %>% .[1]
    tibble(YesterdayPrice, TodayPrice)
}
SellingList %>% mutate(PriceToday = map2(公司代號, DAte, GetPrice)) %>% select(-data) %>% unnest %>% filter(TodayPrice > YesterdayPrice) %>% mutate(SoldDay = Date) %>% rename(SoldPrice = YesterdayPrice)

DAY <- '101/01/11'

TradeHistory$SellingList[[14]]


Price<-PriceHistory %>% filter(公司代號 == 4960) %>% unnest %>% filter(日期 <= DAY) %>% filter(!is.na(開盤價))

Price %>% arrange(日期) %>% AddMVandKD %>% GetMAD %>% GetMADD %>% GetThreeK

TradeHistory$Stock[[7]]

StockValueList$data[[1]] %>% filter(日期 == Date) %>% .$收盤價

TradeHistory %>% select(Date, Cash, StockValue) %>% mutate(ASSET = Cash + StockValue) %>% View
TradeHistory$Date[5]
TradeHistory$BuyingList[[5]]

TradeHistory$Stock[[78]] %>% .$收盤價 %>% sum

DAYList %>% View


TradeHistory$Stock[[6]]
TradeHistory$Stock[[7]]
TradeHistory$SellingList[[6]]
TradeHistory$SoldList[[7]]


StockYesterday <- TradeHistory$Stock[[6]]
BoughtList <- TradeHistory$BoughtList[[7]]
SoldList <- TradeHistory$SoldList[[7]]

StockToday %>% View

SoldList %>% View

TradeHistory$SellingList[[4]]
StockYesterday <- TradeHistory$Stock[[3]]
BoughtList <- TradeHistory$BoughtList[[5]]
SoldList <- TradeHistory$SoldList[[5]]

StockYesterday %>% filter(公司代號 == 1617)

check <- TradeHistory[15:22,] %>% mutate(ASSET = Cash + StockValue)

check$Stock[[7]]
check$SoldList[[8]] %>% .$SoldPrice %>% sum
check$BoughtList[[8]] %>% .$BoughtPrice %>% sum

25.4 + 107.57 - 17.3

check$Cash

StockYesterday <- check$Stock[[7]] 
SoldList <- check$SoldList[[8]]
SellingList <- check$SellingList[[7]]
Date<-'107/01/30'
GetSellingList(StockYesterday)
GetSellingList <- function(StockYesterday) {
    if (nrow(StockYesterday) > 0) {
        SellingList <- StockYesterday %>% left_join(PriceHistory)
        GetSellingList <- function(BoughtPrice, data) {
            GetLossControlCriterion <- function() {
                ClosePriceToday <- data %>% filter(日期 == Date) %>% .$收盤價 %>% .[1]
                ProfitRatio <- (ClosePriceToday - BoughtPrice) / BoughtPrice
                ifelse(ProfitRatio > 0.1 | ProfitRatio < -0.05, 1, 0)
            }
            LossControlCriterion <- GetLossControlCriterion()
            GetThreeKCriterion <- function() {
                ThreeKPrice <- data %>% filter(日期 <= Date) %>% arrange(desc(日期)) %>% .[1:3,] %>% select(最低價)
                ifelse(ThreeKPrice$`最低價`[1] < ThreeKPrice$`最低價`[2] & ThreeKPrice$`最低價`[1] < ThreeKPrice$`最低價`[3], 1, 0)
            }
            ThreeKCriterion <- GetLossControlCriterion()
            ifelse((LossControlCriterion + ThreeKCriterion) > 0, 1, 0)
        }
        SellingList %>% mutate(SellingSignal = map2(BoughtPrice, data, GetSellingList) %>% as.numeric)# %>% filter(SellingSignal == 1)
    } else {
        tibble()
    }

}

StockYesterday %>% View
SoldList %>% View

StockYesterday %>% left_join(SoldList %>% select(公司代號, 日期) %>% mutate(SoldIndex = 1)) %>% filter(is.na(SoldIndex)) %>% select(-SoldIndex)


check<-TradeHistory[24:26,] %>% mutate(ASSET = Cash + StockValue)
check$BoughtList[[3]]
StockListYesterday <- check$Stock[[2]]
StockListToday <- check$Stock[[3]]
Date <- '107/02/05'
StockListYesterday %>% left_join(PriceHistory) %>% mutate(PresentValue = map(data, GetPresentValue) %>% as.numeric) %>% select(-data) %>% View
Date <- '107/02/06'
StockListToday %>% left_join(PriceHistory) %>% mutate(PresentValue = map(data, GetPresentValue) %>% as.numeric) %>% select(-data) %>% View

