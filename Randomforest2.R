credit = read.csv("credit.csv")

head(credit)
set.seed(1234)

ids = sample(nrow(credit), nrow(credit)*0.8)
train = credit[ids,]
test = credit[-ids,]

head(train)

### 
library(randomForest)
library(caret)

### use randomForest 

creditrf = randomForest( default ~ . , data = train )

test$pred = predict(creditrf, newdata = test)

table(test$default, test$pred)

## parameter tuning 

creditrf = randomForest( default ~ . , data = train, ntree = 15, mtry = 5, max_depth = 8 )

test$pred = predict(creditrf, newdata = test)

table(test$default, test$pred)

### more paremeters 

creditrf1 = randomForest( default ~ . , data = train, ntree = 15, mtry = 5, max_depth = 8, nodesize = 12)

test$pred = predict(creditrf1, newdata = test)

table(test$default, test$pred)

## caret package 

library(caret)
library(randomForest)
library(mlbench)
library(e1071)

control = trainControl(method="repeatedcv", number=3, search = "grid") 
set.seed(1238)

tune = expand.grid(.mtry=c(4,5,6))

gridrf = train( default ~ . , data = train, method = "rf", 
                metric="Accuracy", tuneGrid=tune, trControl=control )

print(gridrf, showSD = T)


### More parameters in grid search 

tune = expand.grid(.mtry=c(5,6,7), .Max_depth= c(8,9))
tune

## custom paramerters for RandomForest 
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry","Max_depth"), class = rep("numeric", 2), label = c("mtry","Max_depth"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, Max_depth=param$Max_depth, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


## 
### 

gridrf= train(default ~., data=train, method=customRF, metric="Accuracy", tuneGrid=tune, trControl=control)
print(gridrf,showSD = T)



test$pred = predict(gridrf, newdata = test)

table(test$default, test$pred)
