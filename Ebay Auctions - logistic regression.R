# LOAD
df <- read.csv("~/School/Babson/Fall 22 Classes/Machine Learning/Data/eBayAuctions.csv")
source("C:/Users/billl/Documents/School/Babson/Fall 22 Classes/Machine Learning/in class/R files/BabsonAnalytics.R")

# MANAGE
df$ClosePrice = NULL
df$Competitive = as.logical(df$Competitive)
df$Category = as.factor(df$Category)
df$EndDay = as.factor(df$EndDay)
df$Currency = as.factor(df$Currency)

# PARTITION
N = nrow(df) # Find number of total rows
training_size = round(N*0.6) # Find the size of training (round)
set.seed(1234)
training_cases =  sample(N, training_size) # Find rows for training (random)
train = df[training_cases, ] # Create training (t_c rows, all cols.)
test = df[-training_cases, ] # Create test (exclude t_c rows)

# BUILD
model = glm(Competitive ~., data=train, family=binomial)
step(model) 
model = step(model)
summary(model)
# On avg, if all else equal, duration +1 -> odds of competitiveness changes by multiple of e^(-1.035e-01) <-> 0.9
# On avg, if all else equal, CurrencyGBP +1 -> odds of comp. changes by multiple of e^(1.429e+00) <-> 4.17

# PREDICT
predictions = predict(model, test, type="response")
predictions_tf = (predictions > 0.5) # cutoff is "bar" for true.

# EVALUATE
observations = test$Competitive
table(predictions_tf, observations)
error_rate = sum(predictions_tf != observations)/nrow(test)
error_bench = benchmarkErrorRate(train$Competitive, test$Competitive)
error_rate/error_bench
# Recall/sensitivty - correctly predicted Ts / T obs.
# spec. - correctly predicted Fs / F obs.
# Precision - correctly predicted Ts / total predicted Ts
sensitivity = 301/(301+112)

# More F obs.
specificity = 202/(202+174) #= 0.54 at 0.5 c.o
341/(341+35) #=0.91 at 0.75 c.o
precision = 301/(301+174)
# As cut-off dec, the test Sens inc and Spec dec
ROCChart(observations, predictions) # AUC = .68 @0.5
# Cut-off inc, move from top right to bot left
# lowest c.o, Se=1, Sp=0
# highest c.o, Se=0, Sp=1
liftChart(observations, predictions)
