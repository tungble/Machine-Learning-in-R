# LOAD
df = read.csv("~/School/Babson/Fall 22 Classes/Machine Learning/Data/BostonHousing.csv")
View(df)
library(rpart)
library(rpart.plot)
source("C:/Users/billl/Documents/School/Babson/Fall 22 Classes/Machine Learning/in class/R files/BabsonAnalytics.R")

# MANAGE
df$MEDV = NULL
df$CHAS = as.factor(df$CHAS)
df$RAD = as.factor(df$RAD)
df$ISHIGHVAL = as.logical(df$ISHIGHVAL)
# PARTITION
N = nrow(df) # Find number of total rows
set.seed(1234)
training_size = round(N*0.6) # Find the size of training (round)
training_cases =  sample(N, training_size) # Find rows for training (random)
train = df[training_cases, ] # Create training (t_c rows, all cols.)
test = df[-training_cases, ] # Create test (exclude t_c rows)
# BUILD
model = rpart(ISHIGHVAL ~., data=train)
rpart.plot(model)

# PREDICT
predictions = predict(model,test)>0.5
# EVALUATE
observations = test$ISHIGHVAL
table(predictions, observations)
error = sum(predictions != observations)/nrow(test)
error_bench = benchmarkErrorRate(train$ISHIGHVAL, test$ISHIGHVAL)

# BAG - side by side
library(randomForest)
model_rf = randomForest(ISHIGHVAL ~., data=train, ntree=1500)
pred_rf = predict(model_rf, test) > 0.5
pred_rf[1:10]
error_rf = sum(pred_rf != test$ISHIGHVAL)/nrow(test)

library(gbm)
model_boost = gbm(ISHIGHVAL ~., data=train, n.trees=5000, cv.folds=4)
# model complexity figure 
# as mc increases, train_error decreses, cv_error decreases then increases
best_size = gbm.perf(model_boost, method="cv")
pred_boost = predict(model_boost, test, best_size, type="response") > 0.5
pred_boost[1:10]
# self-made table for boost
observations = test$ISHIGHVAL
table(pred_boost, observations)
# Error rate for boost
error_boost = sum(pred_boost != test$ISHIGHVAL)/nrow(test)
# STACKING
pred_boost_full = predict(model_boost, df, best_size, type="response")
pred_rf_full = predict(model_rf, df)

df_stacked = cbind(df,pred_boost_full, pred_rf_full)

df_stacked$ISHIGHVAL = as.factor(df$ISHIGHVAL)

train_stacked = df_stacked[training_cases, ]
test_stacked = df_stacked[-training_cases, ]

stacked = glm(ISHIGHVAL ~ ., data = train_stacked, family=binomial)

pred_stacked = predict(stacked, test_stacked, type="response")
pred_stacked = (pred_stacked > 0.5)

error_stacked = sum(pred_stacked != test$ISHIGHVAL)/nrow(test)

