data <- read.csv(file.choose())
ts1 <- ts(data,start=c(1999,10),frequency = 3)
ts1
plot(ts1)
decomposed_data <- decompose(ts1, type="mult")
plot(decomposed_data)


#test & train! 
library(TSstudio)

data_split <- ts_split(ts1, sample.out = 15) 
data_train <- data_split$train
data_test <- data_split$test

library(tseries)
adf.test(ts1)
#good, we passed
plot(ts1)
plot(diff(ts1,lag=12))
plot(ts1)
plot(diff(ts1))

#second, we use the PACF to get the AR order (p). We aim for where the PACF cuts off. Note that we have to use the differenced dataset identified above
pacf(diff(diff(ts1, lag=12)))

#third, we use the ACF to determine q - we interpret it the same way as the PACF. Note that we have to use the differenced dataset identified above
acf(diff(diff(ts1, lag=12)))


#Naive forecast
library(forecast)
data_basic_forecast <- naive(data_train, h=12) #set h to the size of your test set!
autoplot(data_basic_forecast) + autolayer(data_test)
accuracy(data_basic_forecast,data_test)


#so, we're looking at an ARIMA(2,1,2) model.

arima_1 <- Arima(ts1, order=c(2,1,2), seasonal=c(0,1,0))
arima_1_forecast <- forecast(arima_1, h=15)
plot(arima_1_forecast)
checkresiduals(arima_1_forecast)


#let's try auto-arima

arima_2 <- auto.arima(ts1)
arima_2_forecast <- forecast(arima_2)
plot(arima_2_forecast)
summary(arima_2)

