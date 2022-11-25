########## DATA LOAD ##########
library(quantmod)
library(xts)
library(fUnitRoots)
library(forecast)
library(tseries)

portfolio = c("BTC-USD","ETH-USD","LTC-USD","XRP-USD","ADA-USD")
getSymbols(portfolio, src="yahoo", from="2020-01-01")
tail(`BTC-USD`)

chartSeries(`BTC-USD`)
barChart(`BTC-USD`,theme='white.mono',bar.type='hlc')

chartSeries(`ETH-USD`)
barChart(`BTC-USD`,theme='white.mono',bar.type='hlc')

chartSeries(`LTC-USD`)
barChart(`BTC-USD`,theme='white.mono',bar.type='hlc')

chartSeries(`XRP-USD`)
barChart(`BTC-USD`,theme='white.mono',bar.type='hlc')

chartSeries(`ADA-USD`)
barChart(`BTC-USD`,theme='white.mono',bar.type='hlc')

########## DATA PROCESS ##########

btc_ts = ts(log(`BTC-USD`[, 4]), frequency = 12)
plot(btc_ts)

adf.test(btc_ts)

btc_tscomponents <- decompose(btc_ts)
plot(btc_tscomponents, col = "red")

btc_tsdiff1 <- diff(btc_ts, differences=1)
plot.ts(btc_tsdiff1, col = "red")

adf.test(btc_tsdiff1)

Box.test(btc_tsdiff1, lag=12, type="Ljung")
acf(btc_tsdiff1)
pacf(btc_tsdiff1)   


########## MODELING ##########

# auto.arima()
btc_arima = arima(btc_ts,order=c(1,1,1),seasonal=list(order=c(5,1,0),period=12))
btc_arima
btc_tsforecasts <- predict(btc_arima)
plot(btc_arima)


arima_res = btc_arima$residuals
plot.ts(arima_res)

Box.test(arima_res,lag=12,type="Ljung")
pv=1-pchisq(14.578,11)
pv


