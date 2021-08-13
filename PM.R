
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(quantmod)) install.packages("quantmod", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(TTR)) install.packages("TTR", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(quantmod)
library(caret)
library(corrplot)
library(pROC)
library(ggplot2)


#Scrape data from yahoo finance (STOCK & INDEX)
getSymbols("AMZN",from="2015-08-01",to="2020-12-05")

#Graph capturing this years amazon's price chart (also shows bollinger bands, volume traded and MACD)
Graph <- AMZN%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2020')

#Determine price change for stock and classify them as up or down
price <- AMZN$AMZN.Close - AMZN$AMZN.Open
class <- ifelse(price > 0, 'UP','DOWN')

#add serial number to data
AMZN$id <-  1:nrow(AMZN)
AMZN <- AMZN[, c(7, 1, 2, 3, 4,5,6)]

#Computing various technical indexes
#F.I indicator
forceindex <- (AMZN$AMZN.Close - AMZN$AMZN.Open) * AMZN$AMZN.Vol 

  
# Buy & Sell signal Indicators (Williams %R and ROC)
Will5 <- WPR(AMZN[,c("AMZN.High","AMZN.Low","AMZN.Close")], n = 5)
Will10 <- WPR(AMZN[,c("AMZN.High","AMZN.Low","AMZN.Close")], n = 10)


RSI5  <- RSI(AMZN$AMZN.Close, n = 5,maType="WMA")
RSI10 <- RSI(AMZN$AMZN.Close, n = 10,maType="WMA") 

#Other momentum indicators
ROC5 <- ROC(AMZN$AMZN.Close, n = 5,type ="discrete")*100
ROC10 <- ROC(AMZN$AMZN.Close, n = 10,type ="discrete")*100 

# Volatility signal Indicator (ATR)
ATR5 = ATR(AMZN[,c("AMZN.High","AMZN.Low","AMZN.Close")], n = 5, maType="WMA")[,2]
ATR10 = ATR(AMZN[,c("AMZN.High","AMZN.Low","AMZN.Close")], n = 10, maType="WMA")[,2]

#combing all
df = data.frame(class,forceindex,Will5,Will10,RSI5,RSI10,ROC5,ROC10,ATR5,ATR10)
df <- na.omit(df)
ddf <- as.tibble(df)
ddf <- ddf %>% rename(Class = AMZN.Close, Forceindex = AMZN.Close.1, WillR5 = AMZN.Close.2, willR10 = AMZN.Close.3, RSI_5 = rsi, RSI_10 = rsi.1, ROC_5 = AMZN.Close.4, ROC_10 = AMZN.Close.5, ATR5 = atr, ATR10 = atr.1)


#Visualizing data by viewing correlation
summary(ddf)
correlation <- cor(ddf[,c(2:10)])
corrplot(correlation)


#create train and test data
set.seed(5)
test_index <- createDataPartition(ddf$Class,times=1,p=0.5,list=FALSE)
test <- ddf[test_index,]
train <- ddf[-test_index,]


#Trying Classification Algorithms
#KNN
set.seed(5)
train_knn_cv <- train(Class ~. , method = "knn", 
                      data = train,  preProc=c("range"))

KNN <- confusionMatrix(data = predict(train_knn_cv, test), 
                reference = as.factor(test$Class))$overall["Accuracy"]

# QDA and LDA
set.seed(5)
train_lda<- train(Class~., data=train, method="lda")

lda <- confusionMatrix(data = predict(train_lda, test), 
                reference = as.factor(test$Class))$overall["Accuracy"]

train_qda <- train(Class~., data=train, method="qda")

qda <- confusionMatrix(data = predict(train_qda, test), 
                reference = as.factor(test$Class))$overall["Accuracy"]

results <- c(KNN = KNN, LDA = lda, QDA = qda)

#tuning the parameters
set.seed(5)
grid = expand.grid(k=seq(1,100,1))

train_knn_cv <- train(Class ~. , method = "knn", tuneGrid = grid,
                      data = train,  preProc=c("range"))
KNN <- confusionMatrix(data = predict(train_knn_cv, test), 
                       reference = as.factor(test$Class))$overall["Accuracy"]
KNN


