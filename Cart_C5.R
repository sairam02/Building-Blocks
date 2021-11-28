library(ISLR)
data("Default")


head(Default)

Default = Default[ 1:1000, ]


library(ggplot2)

ggplot( Default, aes(income, balance, shape = student, color = default)) +
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

#### confusion matrix 

table( test$default, test$pred)

printcp(dtree1)

##accuracy test data

(89+13)/150


#### confusion matrix on train data 

summary(dtree1)

trainpred = predict(dtree1, newdata = train, type="class")

table(train$default, trainpred)

(563+132)/850

#### 

printcp(dtree1)

tree2 = prune( dtree1, cp =0.0158)

printcp(tree2)

fancyRpartPlot(tree2)

### performance of pruned tree 

test$pred2 = predict( tree2 , newdata = test , type="class")

table( test$default, test$pred2)

(94+15)/150

trainpred2 = predict(tree2, newdata=train, type="class")

table(train$default, trainpred2)

(550+130)/850

tree3 = prune(tree2, cp =0.018520 )

printcp(tree3)

fancyRpartPlot(tree3)

test$pred = predict(tree3, newdata=test, type="class")

table(test$default, test$pred)

(94+11)/150
 
 ### credit default prediction using c5.0 
 
 library(C50)
 
 ?C5.0
head(credit)
 
 target = credit$default
 
 table(target)
 
 credit$default = NULL
 
 set.seed(1234)

 ### create index to break into train and test sets
 idx = sample(nrow(credit),  nrow(credit)*0.8)  
### trains set ( both input and target)
 train_x = credit[ idx, ]
 train_y= target[idx]
### test sets( input and target)
 test_x = credit[-idx,]
 test_y = target[-idx]

 ### model bulding udsing c5.0 
 ## C5.0 usex (x,y ) approach not the formulae 
 treec50 = C5.0(train_x, train_y )
 
summary(treec50) 

### check the accuracy on test dataset 

pred = predict(treec50, test_x )

head(pred)

### tabulate the confusion matrix 

table(test_y, pred )

(118+21)/200

testerror = 1-(/200)
testerror
### Accuracy and error( misclassification rate)
(109+35)/200

p = 35/(35+24)
r = 35/(35+33)

2*p*r/(p+r)
### compare the train error and test error 

train_pred = predict(treec50, train_x)

table(train_y, train_pred)

trainerror = 1-( (5+148)/800)

accuracy = (539+181)/800

accuracy

### Precision 
p = 35/(35+24)
p 

### Recall 
r = 35/(35+33)
r
### f-1 score 
f_1 = 2 * p * r /( p + r )

controlled_tree = C5.0(x = train_x, train_y, control = C5.0Control(noGlobalPruning = TRUE,earlyStopping = TRUE) )


contrl_pred = predict(controlled_tree, test_x) 


table(test_y,contrl_pred )

cntrl_train_pred = predict(controlled_tree, train_x)

table(train_y, cntrl_train_pred )
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


#### random forest for regression 
names(train)
rf_regression = randomForest( charges ~ . , data=train, ntree = 25, mtry =4 ) # , ntree = 25, mtry = 6) #, mtry = 6)

test$pred_rf = predict( rf_regression, newdata = test)


test$err_sum  = (test$charges - test$pred_rf) ** 2

sqrt(mean(test$err_sum))
