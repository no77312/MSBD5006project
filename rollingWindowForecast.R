### Ref: https://cran.r-project.org/web/packages/greybox/vignettes/ro.html

library(greybox)
library(Metrics)

x <- rnorm(100,100,10)

ourCall <- "predict(arima(x=data,order=c(0,1,1)),n.ahead=h)"
ourValue <- "pred"

# produce forecasts from the model using rolling origin.
# h: The forecasting horizon.
# o: 	The number of rolling origins.
# ci: if the in-sample window size should be constant. 
# co: whether the holdout sample window size should be constant.
returnedValues1 <- ro(x, h=3, origins=8, call=ourCall, value=ourValue)
plot(returnedValues1)

# Mean Absolute Error
apply(returnedValues1$holdout,1,mae,predicted=returnedValues1$pred)

# Root Mean Square Error
apply(returnedValues1$holdout,1,rmse,predicted=returnedValues1$pred)

# scaled Mean Absolute Error
apply(returnedValues1$holdout,1,mase,predicted=returnedValues1$pred)
#apply(abs(returnedValues1$holdout - returnedValues1$pred),1,mean,na.rm=TRUE) / mean(returnedValues1$actuals)

# want to change the default parameters of RO, we can ask for example, for non-constant holdout and the constant in-sample:
returnedValues2 <- ro(x, h=3, origins=8, call=ourCall, value=ourValue, ci=TRUE, co=FALSE)

plot(returnedValues2)


ourCallETS <- "forecast(ets(data),h=h,level=95)"
ourValueETS <- c("mean","lower","upper")

returnedValues3 <- ro(x, h=3, origins=8, call=ourCallETS, value=ourValueETS)
plot(returnedValues3)