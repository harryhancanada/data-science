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

#let's build a simple exponential smoothing model
?ses

exp_forecast <- ses(airpass_train, h=15)

autoplot(exp_forecast) + autolayer(airpass_test)
#Note - this looks the same as a naive forecast, but is slightly different! 
#simple exponential smoothing is only suitable for timeseries without a trend 

#let's upgrade and try Holt's method
trend_forecast <- holt(airpass_train, h=15)
autoplot(trend_forecast) + autolayer(airpass_test)
#better - we got the trend piece here. 

#another upgrade - holt-winters!
trendseas_forecast <- hw(airpass_train, h=15)
autoplot(trendseas_forecast) + autolayer(airpass_test)
#getting much closer. 



