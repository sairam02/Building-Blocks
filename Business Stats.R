
a = c( 10,15,16,21,22,25,26,31,32,33,34,35,36,37,38, 44,46,48,49,50,55,60)

### descriptive statistics 
summary(a)

## Measures of spread 
var(a) ### variance 
sd(a) ## satandard deviation 

### distribution 
hist(a, breaks = 10)
hist(a)
sd(a)
mean(a)

## shape of the distibution using desity function 

k = dnorm(a, mean= 34.68, sd =13.43 ) ## density of a normal disrtibution 


k
## 
plot(a,k) # ,type="l")

?dnorm

### propability of <=x  

pnorm( 50, mean = 34.6, sd = 13.4 )

### probability of having a salary of atleast 30L when mean is 25L and Sd = 5L 

pnorm( 3000000, mean = 2500000, sd=500000)

### probability( Percentage of emp) having a salary b/w 32L and 30 L 

pnorm( 3200000, mean = 2500000, sd=500000) - pnorm( 3000000, mean = 2500000, sd=500000)

### probability( Percentage of emp) having a salary b/w 30L and 25 L 

pnorm( 3000000, mean = 2500000, sd=500000) - pnorm( 2500000, mean = 2500000, sd=500000)

### propability of having a salary of gt 40L 

1 - pnorm( 4000000, mean = 2500000, sd=500000)

#### qnorm is the opposite of pnorm 

qnorm( 0.3, mean=2500000, sd = 500000)

qnorm(0.841, mean = 2500000, sd = 500000)

qnorm(0.25, mean=2500000, sd = 500000)

qnorm(0.75, mean = 2500000, sd = 500000)

qnorm(0.5, mean = 2500000, sd = 500000)

qnorm( 0.95, mean=2500000, sd=500000)

qnorm( 0.997, mean=2500000, sd=500000)
### Hypothesis testing 


### zscores using scale function 

b = scale(a)



summary(b)
sd(b)

b 

boys = c( )


girls = c( 7, 8, 9, 9, 6, 7, 8, 4, 5, 4, 7, 5, 5, 5, 9, 4,9,5,7, 9,9,9,9,9,9,9)
length(girls)
boys = c( 5,6,7,5,6,7,8,4,9,6,5,7,8,6,7,5,8,4,4,8 ,3,4,3,4,3,4)
length(boys)

### Ha : mean(girls) >> mean(bos)
### h0 : mean(girls) <= mean(boys)

mean(girls)
mean(boys)



t.test( girls, boys, alternative ="greater")

t.test( boys, girls , alternative =  "two.sided")


t.test(girls, mu = 6, alternative = "two.sided")

t.test(boys, mu=6)



a = rnorm( 100, 1000, 1)

a 
t.test( a, mu=1000, alternative = "greater")

x = c( 1,2,3,4,5)
y = c( 11, 22, 34, 41, 50)

plot(x,y)

cor(x,y)

z = c( 55, 44, 39, 21, 15)
cor(x,z)


plot(x,y, type="l")

plot(x,z, type="l")

#### chisquare test 

setwd("C:/Users/SAIRAM YERRAMSETTI/Documents/data sets")
credit = read.csv("creditcard.csv")
str(df1)

head(credit)

table(credit$default, credit$phone)

chisq.test(table(credit$default, credit$phone))

### Anova exampe 
 a= c( 24, 26, 31, 27, 8)
 b = c( 29, 31,30, 36,33)
 c = c( 29, 27, 34, 26, 23)

 df = data.frame(a,b,c)
 
fit =  aov( c ~ a+b, data = df)


summary(fit)

fit2 = lm( c ~ a+b , data = df)

summary(fit2)


 
