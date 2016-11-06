install.packages("TTR")
install.packages("hydroGOF")
install.packages("tseries")
install.packages("forecast")

library("TTR")
library("hydroGOF")
library("tseries")
library("forecast")

####################################################################

#Q1

data <- read.csv("dataset.csv",header=TRUE)
#data

train = data[16:1509,] #removed tail, updated training set entry beginning from index : 16
test = data[1510:2000,] #updated test set entry beginning from index : 1510 

#length(train)
#length(test)

plot.ts(train,col="lightblue")

root_mean_square_error <- c(1:25)

for(i in 1:25){
  sma <- SimpleMovingAverage(train, i)
  root_mean_square_error[i] <- rmse(train[i+1:1494],sma[i+1:1494])
}

SimpleMovingAverage <- function(train,m) {

avg=0
ma_fit <- rep(times=m,0)


for(j in (m+1):length(train)){
   	for(i in 1:m){ 
     		avg = avg + train[j-i]
   	   }
    ma_fit = append(ma_fit, avg/m)
    avg = 0
  }
  return(ma_fit)
}

plot(root_mean_square_error)

#########################################################

#minimum for m = 2

sma_2 <- SimpleMovingAverage(train,2)

root_mean_squa_err_2 <- rmse(train[3:1494],sma_2[3:1494])

root_mean_squa_err_2

#######################################################

sma_3 <- SimpleMovingAverage(train,3)

root_mean_squa_err_3 <- rmse(train[4:1494],sma_3[4:1494])

root_mean_squa_err_3

#######################################################

sma_1 <- SimpleMovingAverage(train,1)

root_mean_squa_err_1 <- rmse(train[2:1494],sma_1[2:1494])

root_mean_squa_err_1

plot.ts(train,col="blue")
lines(sma_2,col="red")

##########################################################

#Q2

rmse_e <- c(1:11)

n<-1494

predicted_val <- c(1,1494)

k<-1

s <- seq(0, 1, by = 0.1)

for(i in s) {
for (j in 2:1494) {
  predicted_val[j] <- (i* train[j-1]) + ((1-i)* predicted_val[j-1])
}
error<-(train[2:1494] - predicted_val[2:1494])
sum_square_error<-sum(error^2)
rmse_e[k]<-sqrt(sum_square_error/n)
k<-k+1
}

rmse_e[1:11] #lowest value for alpha equal to 0.8

plot(rmse_e) 



n<-1494

predicted_val <- c(1,1494)

i<-0.8

for (j in 2:1494) {
  predicted_val[j] <- (i* train[j-1]) + ((1-i)* predicted_val[j-1])
}

plot.ts(train,col="red")
lines(predicted_val,col="blue")


######################################################

#Q3

pacf(train)

#arima_fit <- auto.arima(train,d=0,max.q=0) #to check the p value

arima_fit <- auto.arima(train,d=0,max.q=0,max.p=3) #p-value of 3 is obtained since the pacf has 3 values beyond the blue lines

arima_fit$coef

#length(fitted(arima_fit))

rmse(train,fitted(arima_fit))

plot.ts(train,col="red")
lines(fitted(arima_fit),col="blue")


#######################################################

#Q4

sma_test <- SimpleMovingAverage(test, 2)
root_mean_sq_er <- rmse(test[3:491],sma_test[3:491])

root_mean_sq_er

##### 20.73625

n<-491

predicted_val <- c(1,491)

i<-0.8

for (j in 2:491) {
  predicted_val[j] <- (i* test[j-1]) + ((1-i)* predicted_val[j-1])
}
error<-(test[2:491] - predicted_val[2:491])
sum_square_error<-sum(error^2)
rmse_e<-sqrt(sum_square_error/n)

rmse_e

##### 20.66266

arima_fit <- auto.arima(test,d=0,max.q=0,max.p=3)

rmse(test,fitted(arima_fit))

##### 19.13613

#################################################################################










