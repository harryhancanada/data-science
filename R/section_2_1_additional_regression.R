rm(list = ls())

# set.seed(1) # set seed to reproduce results below
x <- rnorm(50)
y <- 1*x + rnorm(50)
fit <- lm(y ~ x) # fit linear model

# we can get summary for our model (residuals, coefficients, R-squared, etc.)
summary(fit)

# get all properties that we can extract from the fit object
names(fit)

# also we can extract necessary information from lm object using one of two forms below
fit$residuals # get residuals from fitted model
resid(fit)

fit$coefficients # get model coefficients
coef(fit)

fit$fitted.values # get fitted values from the model
fitted(fit)

# plot our points and then with abline function add fitted line
plot(x, y)
abline(fit)

# get all 4 diagnostic plots for fitted model
par(mfrow = c(2, 2)) # make 2x2 grid for our plots
plot(fit) # extract all 4 plots

# exclude intercept from our model by adding 0
fitWithoutIntercept <- lm(y ~ 0 + x)

fitData <- lm(mpg ~ cyl, data = mtcars)
summary(fitData)



# set.seed(1) # set seed to reproduce results below
x <- c(1,2,3,4)
y <- c(2,6,6,10)
fit <- lm(y ~ x) # fit linear model
summary(fit)
fit$coefficients
fit$residuals # get residuals from fitted model




x <- ts(11:15) + rnorm(5)
fit <- lm(x ~ time(x))


x <- rnorm(5)
y <- x + rnorm(5)
lm(y ~ x - intercept)
lm(y ~ x - 0)


x <- c(1,2,3,4,5)
y <- c(1.1,4.5,5.8,8.1,10.2)
fit <- lm(y ~ x) # fit linear model
summary(fit)
fit$coefficients
fit$residuals # get residuals from fitted model
fit$fitted.values # get fitted values from the model
fitted(fit)
plot(fit$residuals)

diff(y)
plot(diff(y))
plot(resid(fit), type="o", main="detrended")
plot(diff(y), type="o", main="first difference")
acf(resid(fit), 10, main="detrended")
acf(diff(y), 10, main="difference")


whiteNoise = ts(rnorm(n=100, sd=100))
timeSequence = time(whiteNoise)
beta0 = 1
beta1 = 2
trend = beta0 + beta1 * timeSequence
trendedNoise = trend + whiteNoise
myFit = lm(trendedNoise ~ timeSequence)

#extract trend
y = trendedNoise - myFit$residuals
plot(y)
