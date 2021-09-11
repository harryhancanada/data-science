#Intro to Time Series

airpass <- AirPassengers

#Visualization of Time Series
#Let's start with a simple plot of the raw data
plot(airpass)
#Note the obvious upward trend and seasonality

#Let's try to break out the 4 pieces of time series data we spoke about
decomposed_airpass <- decompose(airpass, type="mult")
plot(decomposed_airpass)
#note that this only works on time series data. If you instead have a dataframe, try:
#df_ts <- ts(df$variable, start = YYYY, end = YYYY, freq = #periods)  # Specify start and end year, measurement frequency (monthly = 12)

#each piece of this decomposition can be calculated manually. For example, use the ma() function to try and reproduce the trend!
ma_12 <- ma(airpass, 12) #set to 12 for a 12 period moving average, etc.

autoplot(airpass) + autolayer(ma_12)


#Before we go too far, let's split out train and test so we can see our forecast accuracy
library(TSstudio)

airpass_split <- ts_split(airpass, sample.out = 15) #use sample.out to determine the number of observations for test

airpass_train <- airpass_split$train
airpass_test <- airpass_split$test

#plot to verify - looks good!
plot(airpass_train)
plot(airpass_test)

#Naive forecast
library(forecast)

airpass_basic_forecast <- naive(airpass_train, h=15) #set h to the size of your test set!
#note there are other simple tools - seasonal naive, mean
#Let's graph it
autoplot(airpass_basic_forecast) + autolayer(airpass_test)

#and check the accuracy
accuracy(airpass_basic_forecast, airpass_test)




