
library(magrittr)
library(e1071)

labelsvm <- trainlabel %>% as.factor
labelsvmtest <- testlabel %>% as.factor

model = svm(trainfeature, labelsvm, kernel = "linear", gamma = if (is.vector(trainfeature)) 1 else 1 / ncol(trainfeature))
pred = predict(model, testfeature) # Predict the data by the built model
PResult <- table(pred, labelsvmtest) # Accuracy calculation
PResult %<>% as.data.frame
Accuracy <- (1 - sum(PResult[PResult$pred != PResult$labelsvmtest,]$Freq) / sum(PResult$Freq)) * 100
Accuracy

(pred %>% as.tibble %>% .$value =='1') %>% sum




data(iris)
plot(iris$Petal.Length, iris$Petal.Width, pch = 21, bg = c("red", "black", "grey")[unclass(iris$Species)], main = "Iris Data")
legend(1, 2.5, c("setosa", "versicolor", "virginica"), col = c("red", "black", "gray"), lty = 1)
iris %>% summary
#First Method
x = iris[, -5] %>% as.matrix # features matrix
iris[, -5] %>% as.matrix
y = iris[, 5] # label
model = svm(x, y, kernel = "radial", gamma = if (is.vector(x)) 1 else 1 / ncol(x))

#Second Method
model = svm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, kernel = "radial")
model = svm(Species ~ ., data = iris)

#Use Model to predict and calculate the accuracy
pred = predict(model, x) # Predict the data by the built model
PResult <- table(pred, y) # Accuracy calculation
PResult %<>% as.data.frame
Accuracy <- (1 - sum(PResult[PResult$pred != PResult$y,]$Freq) / sum(PResult$Freq)) * 100
#Model Visualization
plot(model, iris, Petal.Width ~ Petal.Length, fill = FALSE, symbolPalette = c("red", "black", "grey"), svSymbol = "+")
legend(1, 2.5, c("setosa", "versicolor", "virginica"), col = c("red", "black", "gray"), lty = 1) #Add Legend



#Choose the best Model
attach(iris)
x = subset(iris, select = -Species) # get features matrix
y = Species # Get label
type = c("C-classification", "nu-classification", "one-classification") # The classification type
kernel = c("linear", "polynomial", "radial", "sigmoid") #Kernel function
pred = array(0, dim = c(150, 3, 4)) #Prediction Result Matrix 150¡A3¡A4
accuracy = matrix(0, 3, 4) #Accuracy Matrix 3¡A4
yy = as.integer(y) #For accuracy calculation  1¡A2¡A3

for (i in 1:3)
    #type level
    {
    for (j in 1:4)
        #kernel level
        {
        pred[, i, j] <- predict(svm(x, y, type = type[i], kernel = kernel[j]), x) #i type, j kernel
        if (i > 2) {
            accuracy[i, j] <- sum(pred[, i, j] != 1)
        }
        else {
            accuracy[i, j] <- sum(pred[, i, j] != yy)
        }
    }
}

dimnames(accuracy) = list(type, kernel) #Add type(row) and kernel(column) names
table(pred[, 1, 3], y) #
pred[, 1, 3]

###Improve Model
wts = c(1, 1, 1) # Weight of each label 1¡G1¡G1
names(wts) = c("setosa", "versicolor", "virginica")
model1 = svm(x, y, class.weights = wts)
pred1 = predict(model1, x)
table(pred1, y) #Result
wts = c(1, 100, 100) # Weight of each label 1¡G100¡G100
names(wts) = c("setosa", "versicolor", "virginica")
model2 = svm(x, y, class.weights = wts)
pred2 = predict(model2, x)
table(pred2, y) #Result
wts = c(1, 500, 500) # Weight of each label 1¡G500¡G500
names(wts) = c("setosa", "versicolor", "virginica")
model3 = svm(x, y, class.weights = wts) #Result
pred3 = predict(model3, x)
table(pred3, y) ##Result



#SVM2
SVMData<-Data[11:(nrow(Data)-4),]
SVMData[nrow(SVMData),]
SampleNumber <- nrow(Data) - (TimeWindow + PredictWindow) + 1
Result <- tibble(feature = list(), label = numeric(), date = character())
for (i in 1:SampleNumber) {
    TW <- Data[i:(i + TimeWindow - 1),]
    PW <- Data[(i + TimeWindow):(i + TimeWindow + PredictWindow - 1),]
    Date <- Data[(i + TimeWindow),]$¤é´Á
    Feature <- list(list(GetFeature(TW)))
    Label <- GetLabel(PW, TradeFarePercent, GovernmentTaxPercent, ExpectedProfitPercent)
    Result[i, 1] <- Feature
    Result[i, 2] <- Label
    Result[i, 3] <- Date
}

SVMData$label<-Result$label
SVMData <- SVMData[, -1]
SVMData$label %<>% as.factor
dt <- sort(sample(nrow(SVMData), nrow(SVMData) * .7))
train <- SVMData[dt,]
test <- SVMData[-dt,]
L <- Result$feature[[1]] %>% length

trainfeature <- train %>% ConnectFeature
trainlabel <- train$label
testfeature <- test %>% ConnectFeature
testlabel <- test$label


latestfeature <- SVMData[(nrow(SVMData)-30):nrow(SVMData),c(1:20)]
latestlabel <- SVMData[(nrow(SVMData) - 30):nrow(SVMData),]$label

train[,c(1:20)]

model = svm(train[, c(1:20)], train[, 21], kernel = "linear",, type = "nu-classification" ,gamma = if (is.vector(trainfeature)) 1 else 1 / ncol(trainfeature))
pred = predict(model, latestfeature) # Predict the data by the built model
PResult <- table(pred, latestlabel) # Accuracy calculation
PResult %<>% as.data.frame
Accuracy <- (1 - sum(PResult[PResult$pred != PResult$latestlabel,]$Freq) / sum(PResult$Freq)) * 100
Accuracy

(pred %>% as.tibble %>% .$value == '1') %>% sum

pred %>% as.vector %>% as.factor
latestlabel
