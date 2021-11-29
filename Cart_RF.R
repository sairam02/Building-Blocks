library(ISLR)
data("Default")
head(Default)
Default = Default[ 1:1000, ]

library(ggplot2)
col = c("green","blue")

ggplot( Default, aes(income, balance, shape = default, color = student), colors("green","blue")) +
  geom_point()

library("rpart")
dtree = rpart( default ~ ., data=Default)
plot(dtree)
text(dtree)

library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)
fancyRpartPlot(dtree)

setwd("D:/AP/baging and  RF")

credit = read.csv("credit.csv")

head(credit)

prop.table(table(credit$default))

set.seed(1324)

idx = sample( nrow(credit), nrow(credit)*0.85) 

train = credit[ idx, ]
test = credit[ -idx, ]

dtree1 = rpart(default ~ ., data=train, method="class")

fancyRpartPlot(dtree1)

table(train$checking_balance)

dtree1

test$pred = predict(dtree1, newdata = test, type="class")
trainpred = predict(dtree1, newdata = train, type = "class")

#### confusion matrix 

table( test$default, test$pred) ## 
#### confusion matrix on train data 
table(train$default, trainpred)

(89+13)/150 ## 0.68
(563+132)/850 ## 0.81

printcp(dtree1)


##accuracy test data

(96+15)/150


#### 

printcp(dtree1)

tree2 = prune( dtree1, cp =0.011905)

printcp(tree2)

fancyRpartPlot(tree2)

### performance of pruned tree 
prune(dtree1, )

test$pred2 = predict( tree2 , newdata = test , type="class")

table( test$default, test$pred2)

(99+14)/150

trainpred2 = predict(tree2, newdata=train, type="class")

table(train$default, trainpred2)

(565+122)/850

tree3 = prune(tree2, cp =0.018520 )

printcp(tree3)

fancyRpartPlot(tree3)

test$pred = predict(tree3, newdata=test, type="class")

table(test$default, test$pred)

(94+11)/150

trainpred = predict(tree3, newdata = train, type="class")
 
table(train$default, trainpred)

(560+101)/850

 
###  CART for Regression 

setwd("D:/AP/UC1")

insurance = read.csv("insurance.csv")

### 

head(insurance)
## check for normal distribution( target variable)
hist(insurance$charges)
### Check for multicollinearity
cor( insurance$age, insurance$bmi)

### create train and test splits 

ids = sample(nrow(insurance), nrow(insurance)*0.8)
train = insurance[ids, ]
test = insurance[ -ids, ]


### CART for regression 
library(rpart)
tree1 = rpart( charges ~ . , data=train,  method = "anova")


fancyRpartPlot(tree1)
### predictions and assesment 

test$pred = predict( tree1, newdata = test)



### error caclculation 

test$err_sum  = (test$charges - test$pred) ** 2

sqrt(mean(test$err_sum))

test$abserr = abs(test$charges - test$pred)
test$pererr = (test$abserr/test$charges)*100
mean(test$pererr)

#### randomforest for classification
library(randomForest)
setwd("D:/AP/baging and  RF")

credit = read.csv("credit.csv")

head(credit)
colnames(credit)

prop.table(table(credit$default))

set.seed(1324)

idx = sample( nrow(credit), nrow(credit)*0.85) 

train = credit[ idx, ]
test = credit[ -idx, ]

rftree = randomForest( default ~ ., data = train)
names(rftree)
rftree$ntree ## default number of trees are 500
?randomForest

## change ntree 

rftree = randomForest( default ~ ., data = train, ntree = 30, mtry = 5, max_depth = 6,  nodesize = 8, classwt = c(0.75, 0.25) )


### model performance on test dataset 

test$pred = predict(rftree, newdata = test)
table(test$default, test$pred)
## variable importance of randomforest 
varImp(rftree)
varImpPlot(rftree)

#### random forest for regression 
## recreate the train and test dataset from insurance dataframe
rf_regression = randomForest( charges ~ . , data=train, ntree = 50, mtry = 6, max_depth = 10) # , ntree = 25, mtry = 6) #, mtry = 6)

test$pred_rf = predict( rf_regression, newdata = test)


test$err_sum  = (test$charges - test$pred_rf) ** 2

sqrt(mean(test$err_sum))

test$abserr = abs(test$charges - test$pred)
test$pererr = (test$abserr/test$charges)*100
mean(test$pererr)

varImpPlot(rf_regression)

## Gridsearch and cross validation 

library(caret)
library(randomForest)
library(mlbench)

?trainControl

control = trainControl(method="repeatedcv", number=5 , search = "grid") 
set.seed(1238)

## Parameter search using grid 

tune = expand.grid(.mtry=c(4,5,6))
?train
rf_gridsearch = train(default ~., data=train, method="rf", metric="Accuracy", tuneGrid=tune, trControl=control)

print(rf_gridsearch, showSD = T)

pred = predict(rf_gridsearch, newdata = test)

#### MOdel performance 

table(test$default, pred)

### More parameters in grid search 

tune = expand.grid(.mtry=c(4,5,6), .ntree=c(25,30,35), .Max_depth= c(8,9))
tune

## custom paramerters for RandomForest 
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree","Max_depth"), class = rep("numeric", 3), label = c("mtry", "ntree","Max_depth"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

### 

rf_gridsearch = train(default ~., data=train, method=customRF, metric="Accuracy", tuneGrid=tune, trControl=control)
print(rf_gridsearch,showSD = T)
plot(rf_gridsearch)
rf_gridsearch$results

names(rf_gridsearch)

summary(rf_gridsearch)

78/(78+5)
78/(78+22)

2*0.78*0.94/(0.94+0.78)
## performance of the model by caret RF
pred_rs = predict(rf_gridsearch, test)

table(test$default, pred_rs)

