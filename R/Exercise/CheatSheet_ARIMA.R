#Exponential Smoothing
library(forecast)

#let's use air passengers again:
airpass <- AirPassengers
plot(airpass)
decomposed_airpass <- decompose(airpass, type="mult")
plot(decomposed_airpass)

#test & train! 
library(TSstudio)

airpass_split <- ts_split(airpass, sample.out = 15) 

airpass_train <- airpass_split$train
airpass_test <- airpass_split$test

#ARIMA has three parameters - p,d,q. We refer to an ARIMA model as ARIMA(p,d,q)(P,D,Q), sometimes with a constant. The lower case parameters represent non-seasonal effects and the capital letters, seasonal effects
#before we even get started, we need to make sure our data is stationary. The aughmented-dicky-fuller test can help us here - it has a null hypothesis that your data has a unit root - the idea that a change in time changes the shape of the data
library(tseries)
adf.test(airpass)
#good, we passed
plot(airpass)
#but there is prety obviously a trend - so we must be stationary + trend
plot(diff(airpass))
#much better w/ a first difference
#there is also seasonality, so we should remove that - it looks like a 12 period seasonality

plot(diff(diff(airpass, lag=12)))

#OK, on to ARIMA
#First, determine d - if the data is stationary with no trend, we set d=0. If it is trendy, we take the first difference until we get a stationary output. d equals the number of differences
plot(airpass)
#obviously not stationary
plot(diff(airpass))
#much better - let's go with a d of 1

#second, we use the PACF to get the AR order (p). We aim for where the PACF cuts off. Note that we have to use the differenced dataset identified above
pacf(diff(diff(airpass, lag=12)))
#note the cutoff after 2 - set p to 2

#third, we use the ACF to determine q - we interpret it the same way as the PACF. Note that we have to use the differenced dataset identified above
acf(diff(diff(airpass, lag=12)))
#note the cutoff after 2 - set q to 2

#so, we're looking at an ARIMA(2,1,2) model.

arima_1 <- Arima(airpass, order=c(2,1,2), seasonal=c(0,1,0))
arima_1_forecast <- forecast(arima_1, h=15)
plot(arima_1_forecast)

checkresiduals(arima_1_forecast)
#we obviously missed something here! Look at that trend. 

#let's try auto-arima

arima_2 <- auto.arima(airpass)
arima_2_forecast <- forecast(arima_2)
plot(arima_2_forecast)
summary(arima_2)

