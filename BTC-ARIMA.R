########## DATA LOAD ##########
library(quantmod)
library(xts)
library(fUnitRoots)
library(forecast)
library(tseries)

#portfolio = c("BTC-USD","ETH-USD","LTC-USD","XRP-USD","ADA-USD")
portfolio = c("BTC-USD")
getSymbols(portfolio, src="yahoo", from="2016-01-01")
tail(`BTC-USD`)

chartSeries(`BTC-USD`)
barChart(`BTC-USD`,theme='white.mono',bar.type='hlc')
# 
# chartSeries(`ETH-USD`)
# barChart(`BTC-USD`,theme='white.mono',bar.type='hlc')
# 
# chartSeries(`LTC-USD`)
# barChart(`BTC-USD`,theme='white.mono',bar.type='hlc')
# 
# chartSeries(`XRP-USD`)
# barChart(`BTC-USD`,theme='white.mono',bar.type='hlc')
# 
# chartSeries(`ADA-USD`)
# barChart(`BTC-USD`,theme='white.mono',bar.type='hlc')


########## DATA EXPLORE ##########

chartSeries(`BTC-USD`)
logret <- diff(log(`BTC-USD`[, 6]))
chartSeries(
  logret, type="l", TA=NULL, 
  name="Log Returns",
  theme="white", major.ticks="years", minor.ticks=FALSE)

x <- coredata(logret)
hist(x, main="Log Returns", xlab="", ylab="")

qqnorm(x, main="Log Returns")
qqline(x, col="red")

fBasics::basicStats(x)

tmp.1 <- density(x, na.rm=TRUE)
tmp.x <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), 
             length.out=100)
tmp.y <- dnorm(tmp.x, mean(x, na.rm=TRUE), 
               sd(x, na.rm=TRUE))
tmp.ra <- range(c(tmp.1$y, tmp.y), na.rm=TRUE)
plot(tmp.1, main="Log Return", 
     ylim=tmp.ra)
lines(tmp.x, tmp.y, lwd=2, col="red")
legend("topleft", lwd=c(1,2), 
       col=c("black", "red"),
       legend=c("Kernel density Est.", 
                "Parametric normal density est."))

t.test(x)
tmp <- fBasics::basicStats(x)["Skewness", 1]; tmp
tmp/sqrt(6/length(x))

tmp <- fBasics::basicStats(x)["Kurtosis", 1]; tmp
tmp/sqrt(24/length(x))

fBasics::normalTest(x, method="jb")

########## DATA PROCESS ##########

periodicity(`BTC-USD`)

btc_weekly <- to.period(`BTC-USD`, "weeks")
periodicity(btc_weekly)

btc_monthly <- to.period(`BTC-USD`, "months")
periodicity(btc_monthly)

allReturns(`BTC-USD`)

btc_logrtn_daily <-dailyReturn(`BTC-USD`, type="log")
btc_logrtn_weekly <- weeklyReturn(`BTC-USD`, type="log")
btc_logrtn_monthly <- monthlyReturn(`BTC-USD`, type="log")

chartSeries(
  btc_logrtn_daily, type="l", TA=NULL, 
  name="Log Returns",
  theme="white", major.ticks="years", minor.ticks=FALSE)

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
acf(btc_tsdiff1, lag=36)
pacf(btc_tsdiff1, lag=36)   
unitrootTest(btc_tsdiff1,lags=1,type=c("c"))

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


