library(MASS)
library(leaps)
library(car)

#read the dataset
data <- read.csv("dataset.csv", header=TRUE)
data 

hist(data$X1)

mx1 <- mean(data$X1)
mx1

vx1 <- var(data$X1)
vx1

summary(data$X1)

hist(data$X2)

mx2 <- mean(data$X2)
mx2

vx2 <- var(data$X2)
vx2

summary(data$X2)

hist(data$X3)

mx3 <- mean(data$X3)
mx3

vx3 <- var(data$X3)
vx3

summary(data$X3)

hist(data$X4)

mx4 <- mean(data$X4)
mx4

vx4 <- var(data$X4)
vx4

summary(data$X4)

hist(data$X5)

mx5 <- mean(data$X5)
mx5

vx5 <- var(data$X5)
vx5

summary(data$X5)

cor_data <- cor(data)

cor_data

############################################# 

y = data$Y
x = data$X1

yfit = lm(y~x)

summary(yfit)

(summary(yfit)$sigma)**2

plot(y~x,col="blue")
abline(yfit,col="red")

e = resid(yfit)

qqnorm(e,ylab="Standardized Residuals",xlab="Normal Scores",main="QQ PLOT") 
qqline(e)

plot(x,e)

yhat = predict(yfit)

plot(yhat,y)

hist(e)

plot(e)

plot(yfit)

####################### using x1, x1^2

quad.yfit = lm(y~poly(x,2,raw=T))

summary(quad.yfit)

(summary(quad.yfit)$sigma)**2

#plot(y~poly(x,2,raw=T),col="blue")
abline(quad.yfit,col="gray")


err = resid(quad.yfit)

plot(err)


##################################### using x1, x2, x3, x5

y = data$Y
x1 = data$X1
x2 = data$X2
x3 = data$X3
x4 = data$X4
x5 = data$X5

multi.yfit <- lm(y~x1+x2+x3+x5)

summary(multi.yfit)

(summary(multi.yfit)$sigma)**2

plot(y~x1+x2+x3+x5,col="blue")
abline(multi.yfit,col="red")

error = resid(multi.yfit)

qqnorm(error,ylab="Standardized Residuals",xlab="Normal Scores",main="QQ PLOT") 
qqline(error)

multi.yhat = predict(multi.yfit)

plot(multi.yhat,y)

hist(error)

plot(error)

plot(x1,e)
plot(x2,e)
plot(x3,e)

plot(multi.yfit)

###################################################  using X1, X2 and X5

mul.yfit <- lm(y~x1+x2+x5)

summary(mul.yfit)

(summary(mul.yfit)$sigma)**2

plot(y~x1+x2+x5,col="blue")
abline(multi.yfit,col="red")

er = resid(mul.yfit)

qqnorm(er,ylab="Standardized Residuals",xlab="Normal Scores",main="QQ PLOT") 
qqline(er)

mul.yhat = predict(mul.yfit)

plot(mul.yhat,y)

hist(er)

plot(er)

plot(mul.yfit)


############################################################ using X1 and X2 only 

m.yfit <- lm(y~x1+x2)

summary(m.yfit)

(summary(m.yfit)$sigma)**2

plot(y~x1+x2,col="blue")
abline(m.yfit,col="red")

err = resid(m.yfit)

qqnorm(err,ylab="Standardized Residuals",xlab="Normal Scores",main="QQ PLOT") 
qqline(err)

m.yhat = predict(m.yfit)

plot(m.yhat,y)

hist(err)

plot(err)

plot(m.yfit)

#################################################################

#we could add variables X3 and X4 and observe that the R-sq value doesn't improve

leaps<-regsubsets(y~x1+x2+x3+x5,data=data,nbest=1)  
plot(leaps)
subsets(leaps, statistic="rsq")


##################################################

mmmmyfit <- lm(y~x1+x2+x3+x5)

summary(mmmmyfit)

##################################################

mmyfit <- lm(y~x1+x2+x3+x4+x5)

summary(mmyfit)

##################################################




