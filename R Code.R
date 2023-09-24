########## German Inflation Rates Project -------------------------------------------------
# Data Extracted from 'https://www.statbureau.org/en/germany/inflation-tables'
library(lmtest)
library(tseries)
library(forecast)

mydata = scan()

plot.ts(mydata)

germaninfl = ts(mydata, start = 2008, frequency = 12)

plot(germaninfl)

# Seasonal Decomposition ------------------------------------------------------------------
decompose(germaninfl)

plot(decompose(germaninfl))

# Using the stl method
plot(stl(germaninfl, s.window = 7))

## Stationarity, Autocorrelation, ACF and PACF tests ---------------------------------------

# Stationarity - Dickey-Fuller Test
adf.test(mydata)

# Autocorrelation - Durbin-Watson Test
length(mydata)
dwtest(mydata[-118] ~ mydata[-1])

# Normality - Shapico Wilk Test
shapiro.test(germaninfl)

# ACF and PACF test 
plot(acf(germaninfl, lag.max = 20))
plot(pacf(germaninfl, lag.max = 20))


## Seasonal Arima (package forecast) ------------------------------------------------------
germaninflarima = auto.arima(germaninfl, 
                             stepwise = T, 
                             approximation = F, 
                             trace = T)

# Forecast
forec = forecast(germaninflarima)
plot(forec)

checkresiduals(forec)
## Exponential Smoothing with ets --------------------------------------------------------

# Auto gemerated
ets(germaninfl)

# Forecast plot
germaninflets = ets(germaninfl)

plot(forecast(germaninflets, h = 12))

# Comparison with seasonal Holt Winters model
plot(hw(germaninfl, h = 12))

## Cross Validation of 2 models ---------------------------------------------------------
germaninflets = ets(germaninfl)
germaninflarima = auto.arima(germaninfl, 
                             stepwise = T, 
                             approximation = F, 
                             trace = T)

forecastets = function(x, h) {
  forecast(ets(x), h = h)
}

forecastarima = function(x, h) {
  forecast(auto.arima(x), stepwise = T, approximation = F, h=h)
}

etserror = tsCV(germaninfl, forecastets, h=1)
arimaerror = tsCV(germaninfl, forecastarima, h=1)

mean(etserror^2, na.rm=TRUE)
mean(arimaerror^2, na.rm=TRUE)
