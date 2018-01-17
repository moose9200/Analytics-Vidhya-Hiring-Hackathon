rm(list = ls())
setwd("C:/Users/Moose/Desktop/AVhire")

library(data.table)
df = fread("train.csv", header = T , stringsAsFactors = FALSE, check.names = FALSE,na.strings=c("","NA"))
df = df[,2:8]


library(splitstackshape)

df= cSplit(df, splitCols = "datetime", sep = " ", direction = "wide", drop = FALSE)

df = df[,-c("datetime","datetime_1")]

df$temperature = as.numeric(df$temperature)
df$pressure = as.numeric(df$pressure)
df$var1 = as.numeric(df$var1)
df$var2 = as.factor(df$var2)



fun <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

df$electricity_consumption=fun(df$electricity_consumption)
df$pressure=fun(df$pressure)
df$windspeed = fun(df$windspeed)
df$var1 = fun(df$var1)
df$temperature = fun(df$temperature)

labels <- df$electricity_consumption

df = df[,-c("electricity_consumption")]


df =  model.matrix(~.+0,data = df )

#library(h2o)
#h2o.init()


library(xgboost)
library(caret)
library(data.table)

dtrain <- xgb.DMatrix(data = df,label = labels) 


params <- list(booster = "gbtree", objective = "reg:linear", 
               eta=0.3, max_depth=10)


xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, 
                 showsd = T, stratified = T, print_every_n= 10, early_stop_round = 20, maximize = F)



xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 91, 
                   print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = RMSE)


xgb.importance(xgb1)






test= fread("test.csv", header = T , stringsAsFactors = FALSE, check.names = FALSE,na.strings=c("","NA"))
id = test[,c("ID")]

test = test[,2:7]


test= cSplit(test, splitCols = "datetime", sep = " ", direction = "wide", drop = FALSE)


test = test[,-c("datetime","datetime_1")]

test$temperature = as.numeric(test$temperature)
test$pressure = as.numeric(test$pressure)
test$var1 = as.numeric(test$var1)
test$var2 = as.factor(test$var2)

test$pressure=fun(test$pressure)
test$windspeed = fun(test$windspeed)
test$var1 = fun(test$var1)
test$temperature = fun(test$temperature)


test =  model.matrix(~.+0,data = test )


dtest <- xgb.DMatrix(data = test) 

pred <-predict(xgb1, dtest)

pred = as.data.frame(pred)

output = cbind(id,pred)

names(output)[2] = "electricity_consumption"
output$electricity_consumption=as.integer(output$electricity_consumption)

write.csv(output,"h20dl_wuthoutouylirs.csv",row.names = F)

