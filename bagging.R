
setwd("D:/Data Science - Course/KNN")


# Classification: Breast Cancer data

library(ipred)
wcbd = read.csv("wisc_bc_data.csv")

head(wcbd)

set.seed( 1234)


ids = sample( nrow(wcbd), nrow(wcbd)*0.8)

train = wcbd[ ids, ]
test = wcbd[ -ids,]

mod = bagging(diagnosis ~  . -id , data=train, coob=TRUE)


names(mod)


test$pred = predict(mod, newdata=test, type="class")

pred

table( test$diagnosis, test$pred)

40/45

f1 = 2 * 0.89*1.0/(1.0+0.89)
