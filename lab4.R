# 时间序列分析B LAB4
# PB19010450 和泳毅

# 3.6
library(fBasics)
library(tseries)
library(fGarch)
da = read.table("D:/USTC/时间序列分析/data/m-mrk4608.txt", header = T)[,2]
raw = log(1 + da)

plot(ts(raw))
par(mfrow = c(1, 2))
acf(raw)
pacf(raw,lag.max = 40)

Box.test(raw, lag = 1, type = 'Ljung')
Box.test(raw, lag = 12, type = 'Ljung')

adf.test(raw)
m = arima(raw, order = c(0, 0, 1))
m
tsdiag(m, gof = 10)

Box.test(m$residuals ^ 2, lag = 6, type = 'Ljung')
Box.test(m$residuals ^ 2, lag = 12, type = 'Ljung')

res =m$residuals ^ 2
par(mfrow = c(1, 1))
pacf(res)

model = garchFit(~ arma(0, 1) + garch(3, 0), data = raw, trace = F)
summary(model)

# 3.7
library(fBasics)
require(fGarch)
require(rugarch)
da = read.table("./data/m-3m4608.txt", header = T)[,2]
raw = log(1 + da)
plot(ts(raw))

Box.test(raw,lag = 6,type = 'Ljung')
Box.test(raw,lag = 12,type = 'Ljung')

Box.test(raw^2,lag = 6,type = 'Ljung')
Box.test(raw^2,lag = 12,type = 'Ljung')

pacf(raw^2)
model = garchFit(~ arma(0, 0) + garch(2, 0), data = raw, trace = F)
summary(model)

train = raw[1:750]
model2 = garchFit(~ arma(0, 0) + garch(2, 0), data = train, trace = F)
predict(model2, 5)

model3 = ugarchfit(ugarchspec(variance.model = list(model = "sGARCH",garchOrder= c(2, 0)), 
                              mean.model = list(armaOrder = c(0, 0), archm = T)),train)
show(model3)

model4 = ugarchfit(ugarchspec(variance.model = list(model = "eGARCH",garchOrder= c(1, 1)),
                              mean.model = list(armaOrder = c(0, 0))), train)
show(model4)
ugarchforecast(model4, n.ahead = 5)
