# LOAD
library(readr)
library(nnet)
library(caret)
library(NeuralNetTools)
source("C:/Users/billl/Documents/School/Babson/Fall 22 Classes/Machine Learning/in class/R files/BabsonAnalytics.R")
df = read.csv("~/School/Babson/Fall 22 Classes/Machine Learning/Data/UniversalBank.csv")
View(df)
# MANAGE
df$ID = NULL
df$ZIP.Code = NULL
df$Family = as.factor(df$Family)
df$Securities.Account = as.factor(df$Securities.Account)
df$CD.Account = as.factor(df$CD.Account)
df$Online = as.factor(df$Online)
df$CreditCard = as.factor(df$CreditCard)
df$Personal.Loan = as.factor(df$Personal.Loan)
# PARTITION
standardizer = preProcess(df, method=c("center","scale"))
df = predict(standardizer, df)
# max value of income
max(df$Income)
# BUILD
trainingCases = sample(nrow(df),round(0.6*nrow(df)))
set.seed(1234) 
train = df[trainingCases,]
test = df[-trainingCases,]
model = nnet(Personal.Loan ~ ., data=train, size = 4)
# PREDICT
par(mar = numeric(4))
plotnet(model,pad_x = .5)
predNum = predict(model, test)
# EVALUATE
table(pred,test$Personal.Loan)
error_rate = sum(pred != test$Personal.Loan)/nrow(test)
error_bench = benchmarkErrorRate(train$Personal.Loan, test$Personal.Loan)



