########## DATA LOAD ##########
library(quantmod)
library(xts)
library(fUnitRoots)
library(forecast)
library(tseries)
library(TSA)
#portfolio = c("BTC-USD","ETH-USD","LTC-USD","XRP-USD","ADA-USD")
portfolio = c("BTC-USD")
getSymbols(portfolio, src="yahoo", from="2000-01-01")
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

btc_daily <- to.period(`BTC-USD`, "days")
attr(btc_daily, 'frequency') <- 7
periodicity(btc_daily)

btc_weekly <- to.period(`BTC-USD`, "weeks")
attr(btc_weekly, 'frequency') <- 365.25/7
periodicity(btc_weekly)

btc_monthly <- to.period(`BTC-USD`, "months")
attr(btc_monthly, 'frequency') <- 12
periodicity(btc_monthly)

allReturns(`BTC-USD`)

# btc_logrtn_daily <-dailyReturn(`BTC-USD`, type="log")
# btc_logrtn_weekly <- weeklyReturn(`BTC-USD`, type="log")
# btc_logrtn_monthly <- monthlyReturn(`BTC-USD`, type="log")

btc_logrtn_daily <- Delt(Cl(`BTC-USD`),type='log') # same as diff(log(`BTC-USD`[, 4]))
btc_logrtn_weekly <- Delt(Cl(btc_weekly),type='log')
btc_logrtn_monthly <- Delt(Cl(btc_monthly),type='log')


chartSeries(
  btc_logrtn_daily, type="l", TA=NULL, 
  name="Daily Log Returns",
  theme="white", major.ticks="years", minor.ticks=FALSE)

plot(btc_logrtn_daily)
plot(logret)

########## DATA PROCESS ##########

decompose.xts <-
  function (x, type = c("additive", "multiplicative"), filter = NULL) 
  {
    dts <- decompose(as.ts(x), type, filter)
    dts$x <- .xts(dts$x, .index(x))
    dts$seasonal <- .xts(dts$seasonal, .index(x))
    dts$trend <- .xts(dts$trend, .index(x))
    dts$random <- .xts(dts$random, .index(x))
    
    with(dts,
         structure(list(x = x, seasonal = seasonal, trend = trend,
                        random = if (type == "additive") x - seasonal - trend else x/seasonal/trend, 
                        figure = figure, type = type), class = "decomposed.xts"))
  }

plot.decomposed.xts <-
  function(x, ...)
  {
    xx <- x$x
    if (is.null(xx))
      xx <- with(x,
                 if (type == "additive") random + trend + seasonal
                 else random * trend * seasonal)
    p <- cbind(observed = xx, trend = x$trend, seasonal = x$seasonal, random = x$random)
    plot(p, main = paste("Decomposition of", x$type, "time series"), multi.panel = 4,
         yaxis.same = FALSE, major.ticks = "years", grid.ticks.on = "years", ...)
  }


########## MOTHLY MODELING ##########

log_btc_monthly <-log(Cl(btc_monthly))
plot(log_btc_monthly)
adf.test(log_btc_monthly) # test stationary
Box.test(diff1_log_btc_monthly, lag=48, type="Ljung") # test white noise

btc_tscomponents <- decompose(as.ts(log_btc_monthly))
plot(btc_tscomponents, col = "red")

dex <- decompose.xts(log_btc_monthly)
plot(dex)

# ordinary difference on price i.e. log return
diff1_log_btc_monthly <- diff(log_btc_monthly, differences=1) # same as btc_logrtn_monthly
diff1_log_btc_monthly <- na.omit(diff1_log_btc_monthly)
plot(diff1_log_btc_monthly, col = "red", main="ordinary differentiation on log btc price monthly")

adf.test(diff1_log_btc_monthly) # test non-stationary, p-value < 0.05, is stationary
Box.test(diff1_log_btc_monthly, lag=48, type="Ljung") # test white noise, p-value > 0.05, is white noise
acf(ts(diff1_log_btc_monthly), main='Ordinary Diff on Monthly Log Return', lag=60) # a significant lag at 12 & 23
pacf(ts(diff1_log_btc_monthly),main='Ordinary Diff on Monthly Log Return', lag=60) # a significant lag at 13 & 23

# conclusion: monthly return is white noise

# # seasonal difference on price - seems incorrect
# sdiff1_log_btc_monthly <- diff(log_btc_monthly, differences=12) 
# sdiff1_log_btc_monthly <- na.omit(sdiff1_log_btc_monthly)
# plot(sdiff1_log_btc_monthly, col = "red", main="seasonal differentiation on log btc price monthly")
# 
# adf.test(sdiff1_log_btc_monthly) # test non-stationary, p < 0.05, stationary
# Box.test(sdiff1_log_btc_monthly, lag=48, type="Ljung") # test white noise, p < 0.05 not white noise
# acf(ts(sdiff1_log_btc_monthly), main='Seasonal Diff on Monthly Log Price', lag=60)
# pacf(ts(sdiff1_log_btc_monthly),main='Seasonal Diff on Monthly Log Price', lag=60)

resm <- ar(diff1_log_btc_monthly, method="ols"); resm
plot(as.numeric(names(resm$aic)), resm$aic, type="h",
     xlab="k", ylab="AIC")

monthly_AR_model = arima(log_btc_monthly,order=c(1,1,0),seasonal=list(order=c(0,1,0),period=12))
monthly_AR_model

jointTest= Box.test(monthly_AR_model$residuals, lag=12, type="Ljung")
jointTest

pv=1-pchisq(jointTest$statistic[1],11) #Compute p-value using 8 degrees of freedom
names(pv) <- 'pv'
pv

forecast::checkresiduals(monthly_AR_model)
tsdiag(monthly_AR_model)

# predict
monthly_AR_model <- arima(log_btc_monthly[1:95],order=c(1,1,0),seasonal=list(order=c(0,1,0),period=12))
monthly_AR_pred <- predict(monthly_AR_model, n.ahead=4)
result = cbind(Observed=round(c(log_btc_monthly[96:99]), 4), 
               Predict=round(c(monthly_AR_pred$pred), 4), 
               SE=round(c(monthly_AR_pred$se), 4))

plot(log_btc_monthly[90:99],type="l",lty=1)
lines(result[,2],col="red", lwd=1, lty=2, type="b", pch=2)
lines(result[,2] - 2*result[,3], 
      lwd=1, lty=3, type="l")
lines(result[,2] + 2*result[,3], 
      col="green", lwd=1, lty=3, type="l")



########## WEEKLY MODELING ##########

log_btc_weekly <-log(Cl(btc_weekly))
plot(log_btc_weekly)
adf.test(log_btc_weekly)

btc_tscomponents <- decompose(as.ts(log_btc_weekly))
plot(btc_tscomponents, col = "red")

dex <- decompose.xts(log_btc_weekly)
plot(dex)

diff1_log_btc_weekly <- diff(log_btc_weekly, differences=1) # same as btc_logrtn_monthly
diff1_log_btc_weekly <- na.omit(diff1_log_btc_weekly)
plot(diff1_log_btc_weekly, col = "red")

adf.test(diff1_log_btc_weekly)

Box.test(diff1_log_btc_weekly, lag=14, type="Ljung")
acf(ts(diff1_log_btc_weekly),ylim=c(-0.2,1))
pacf(ts(diff1_log_btc_weekly),ylim=c(-0.2,1))

unitrootTest(diff1_log_btc_weekly,lags=1,type=c("c"))

# conclusion: weekly data is white noise

########## DAILY MODELING ##########

log_btc_daily <-log(Cl(btc_daily))
plot(log_btc_daily)
adf.test(log_btc_daily)

#btc_tscomponents <- decompose(as.ts(log_btc_daily))
#plot(btc_tscomponents, col = "red")

#dex <- decompose.xts(log_btc_daily)
# plot(dex)

diff1_log_btc_daily <- diff(log_btc_daily, differences=1) # same as btc_logrtn_monthly
diff1_log_btc_daily <- na.omit(diff1_log_btc_daily)
plot(diff1_log_btc_daily, col = "red")

adf.test(diff1_log_btc_daily)

Box.test(diff1_log_btc_daily, lag=40, type="Ljung")
acf(ts(diff1_log_btc_daily),ylim=c(-0.2,1))
pacf(ts(diff1_log_btc_daily),ylim=c(-0.2,1))

unitrootTest(diff1_log_btc_daily,lags=1,type=c("c"))


# AR
# auto.arima(log_btc_daily)

acf(ts(diff1_log_btc_daily),lag=60,ylim=c(-0.2,1))
pacf(ts(diff1_log_btc_daily),lag=60,ylim=c(-0.2,1))

resm <- ar(diff1_log_btc_daily, method="ols"); resm
plot(as.numeric(names(resm$aic)), resm$aic, type="h",
     xlab="k", ylab="AIC")

arima(log_btc_daily,order=c(1,1,0))
arima(log_btc_daily,order=c(2,1,0))
arima(log_btc_daily,order=c(3,1,0))
arima(log_btc_daily,order=c(4,1,0))
arima(log_btc_daily,order=c(22,1,0))


daily_AR_model = arima(log_btc_daily,order=c(2,1,0))

Box.test(daily_AR_model$residuals, lag=12, type="Ljung")
forecast::checkresiduals(daily_AR_model)
tsdiag(daily_AR_model)

# predict
daily_AR_model <- arima(log_btc_daily[1:2516], order=c(2,1,0))
daily_AR_pred <- predict(monthly_AR_model, n.ahead=5)
result = cbind(Observed=round(c(log_btc_daily[2517:2521]), 5), 
      Predict=round(c(daily_AR_pred$pred), 5), 
      SE=round(c(daily_AR_pred$se), 5))

plot(log_btc_daily[2500:2521],type="l",lty=1)
lines(result[,2],col="red", lwd=1, lty=2, type="b", pch=2)

lines(result[,2] - 2*result[,3], 
       lwd=1, lty=3, type="l")
lines(result[,2] + 2*result[,3], 
      col="green", lwd=1, lty=3, type="l")

# MA

arima(log_btc_monthly)

#ARMA

TSA::eacf(diff1_log_btc_daily, 12, 12)
resr <- TSA::armasubsets(diff1_log_btc_daily, nar = 12, nma = 12)
plot(resr)

forecast::auto.arima(log_btc_daily, max.p = 12, max.q = 12, max.P = 1, max.Q = 1)
