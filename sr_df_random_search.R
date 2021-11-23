library(randomForest)
library(mlbench)
library(caret)

#loading dataset
data("Sonar")
dataset  <- Sonar
x <- dataset[,1:60]
y <- dataset[,61]

#creating model with default parameters
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Class~., data = dataset,method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

#random search
control <- trainControl(method = "repeatedcv", number=10,repeats = 3,search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(Class~., data = dataset, method="rf",metric=metric,tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
