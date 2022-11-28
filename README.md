# Cryptocurrency Data - Time Series Analysis

HKUST MSBD 5006 Quantitative Analysis of Financial Time Series
Team Project

The report aims to use models such as ARIMA and GARCH to complete the time series quantitative analysis of Bitcoin's historical prices and historical returns. This article first starts with three different periods. For the parallel tasks of daily, weekly and monthly sampling methods, through various sequence tests before modeling and various residual tests after fitting the model, the model is completed. The basic selection and evaluation of , the results are as follows:	No suitable model was found for the daily data.
Weekly Data Fits the model gjRGARCH(1,1)：
γ_t=0.009915+a_t,a_t=σ_t ε_t,ε_t~i.i.d.N(0,1)
σ_t^2=0.00004+(0.046134-0.094274N_(t-1))a_t^2+0.999999σ_(t-1)^2
Monthly Data Fits the model eGARCH(1,1)：
γ_t=0.020536+a_t,a_t=σ_t ε_t,ε_t~i.i.d.N(0,1)
〖lnσ〗_t^2=-0.157148+〖0.239008ε〗_(t-1)-0.540046(|ε_(t-1) |-E|ε_(t-1) |)+0.962164〖lnσ〗_(t-1)^2
Finally, the report makes predictions by fitting the model to the data using two methods, 5-step and sliding window.
