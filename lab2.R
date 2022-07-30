# 时间序列分析B LAB2
# PB19010450 和泳毅

#2.3
library(fBasics)
library(tseries)
library(forecast)
library(fUnitRoots)
da=read.table("D:/USTC/时间序列分析/data/m-unrate.txt",header=T)
rate=da[,4]
plot(ts(rate, start = c(1948, 1), frequency = 12), xlab = 'year', ylab = 'rate')

adf.test(rate)

for(i in 1:2) print(Box.test(rate,type = "Ljung-Box",lag=6*i))

acf(rate)
par(mfrow = c(1, 2))
acf(diff(rate))
pacf(diff(rate))
acf(diff(rate, 12))
pacf(diff(rate, 12))

m=ar(rate, method = 'mle')
m$order

m1=arima(rate, order = c(11, 0, 0))
m1

t1=-0.0741/0.0525
pt(t1,df=12,lower.tail=T)

m1 = arima(rate, order = c(11, 0, 0), fixed = c(NA, NA, 0, 0, 0, NA, 0, 0, 0, NA, NA, NA))
m1

m2 = arima(rate, order = c(2, 1, 1), seasonal = list(order = c(1, 0, 1), period = 12))
m2
tsdiag(m1, gof = 36)
tsdiag(m2, gof = 36)
predict(m1, 4)
predict(m2, 4)

#2.4
library(fBasics)
library(tseries)
library(forecast)
library(fUnitRoots)

da = read.table("D:/USTC/时间序列分析/data/m-deciles08.txt", header = T)
d2  = da[, 3]
d10 = da[, 5]
Box.test(d2,  lag = 12, type = 'Ljung')
Box.test(d10, lag = 12, type = 'Ljung')
plot(ts(d2, start = c(1970, 1), frequency = 12), xlab = 'year', ylab = 'd2')
adf.test(d2)
Box.test(d2,type = "Ljung",lag=12)
par(mfrow = c(1, 2))
acf(d2)
pacf(d2)
m=ar(d2, method = 'mle')
m$order
m1 = arima(d2, order = c(12, 0, 0))
m1
tsdiag(m1, gof = 12)
predict(m1, 1)
predict(m1, 12)

#2.5
da = read.table("D:/USTC/时间序列分析/data/d-ibm3dx7008.txt", header = T)
ibm = da$rtn
acf(abs(ibm), lag = 100)


#2.6
da = read.table("D:/USTC/时间序列分析/data/power6.txt", header = F)
pow = da[, 1]
plot(pow, type = 'o', ylab = 'pow')
adf.test(pow)
Box.test(pow,type='Ljung',lag=12)
par(mfrow = c(1, 2))
acf(pow)
pacf(pow)
acf(diff(pow))
pacf(diff(pow))
acf(diff(pow, 12))
pacf(diff(pow, 12))
acf(diff(diff(pow), 12))
pacf(diff(diff(pow), 12))
# FIXME
m1 = arima(pow, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
m1
tsdiag(m1, gof = 36)
predict(m1, 24)
























