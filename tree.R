# LOAD
df <- read.csv("~/School/Babson/Fall 22 Classes/Machine Learning/Data/ToyotaCorolla.csv")

# MANAGE
df$Model = NULL
df$Fuel_Type = as.factor(df$Fuel_Type)
df$Met_Color = as.logical(df$Met_Color)
df$Automatic = as.factor(df$Automatic)

# PARTITION
N = nrow(df) # Find number of total rows
training_size = round(N*0.6) # Find the size of training (round)
set.seed(1234)
training_cases =  sample(N, training_size) # Find rows for training (random)
train = df[training_cases, ] # Create training (t_c rows, all cols.)
test = df[-training_cases, ] # Create test (exclude t_c rows)

# BUILD
library(rpart)
rpart.control() #to find default ms & mb
# stopping rules, overfit model
stopping_rules = rpart.control(minsplit=1,minbucket = 1, cp=-1)
# model w stopping rules
model = rpart(Price ~., data=train, control=stopping_rules)
# Model w/o stopping rules -- model = rpart(Price ~., data=train)
library(rpart.plot)
# -- rpart.plot(model)

source("C:/Users/billl/Documents/School/Babson/Fall 22 Classes/Machine Learning/in class/R files/BabsonAnalytics.R")
# -- 
model = easyPrune(model)

# PREDICTIONS
predictions = predict(model, test)

# EVALUATE
observations = test$Price
predictions_bench = mean(train$Price)
errors_bench = observations - predictions_bench
mape_bench = mean(abs(errors_bench/observations)) # MAPE_bench 

observations = test$Price
errors = observations - predictions
mape = mean(abs(errors/observations)) # MAPE = mean absolute percentage error (8.6% off) 


