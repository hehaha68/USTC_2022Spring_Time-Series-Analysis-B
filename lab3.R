# 时间序列分析B LAB3
# PB19010450 和泳毅

# 2.10
library(fBasics)

da = read.table("D:/USTC/时间序列分析/data/w-Aaa.txt", header = F)
Aaa = da[, 4]
da = read.table("D:/USTC/时间序列分析/data/w-Baa.txt", header = F)
Baa = da[, 4]
basicStats(Aaa)
basicStats(Baa)

ts = skewness(Aaa) / sqrt(6 / length(Aaa))
ps = (1 - pnorm(ts)) * 2
tk = kurtosis(Aaa) / sqrt(24 / length(Aaa))
pk = (1 - pnorm(tk)) * 2

ts = skewness(Baa) / sqrt(6 / length(Baa))
ps = (1 - pnorm(ts)) * 2
tk = kurtosis(Baa) / sqrt(24 / length(Baa))
pk = (1 - pnorm(tk)) * 2

# 2.11
library(fBasics)
library(xts)
library(tseries)

da = read.table("D:/USTC/时间序列分析/data/w-Aaa.txt", header = F)
Aaa = da[, 4]
plot(ts(Aaa), xlab = 'Time', ylab = 'Aaa')

par(mfrow = c(1, 2))
acf(Aaa)
pacf(Aaa)
acf(diff(Aaa))
pacf(diff(Aaa))

m=ar(diff(Aaa), method = 'mle')
m$order
m1 = arima(Aaa, order = c(9, 1, 0))
m1
tsdiag(m1, gof = 12)

t1=-0.0579/0.0215
pt(t1,df=12,lower.tail=T)

m2 = arima(Aaa, order = c(9, 1, 0), fixed = c(NA, NA, NA, 0, NA, 0, NA, 0, NA))
m2
tsdiag(m2, gof = 12)
# 2.12
library(fBasics)

da = read.table("D:/USTC/时间序列分析/data/w-Aaa.txt", header = F)
Aaa = da[, 4]
da = read.table("D:/USTC/时间序列分析/data/w-Baa.txt", header = F)
Baa = da[, 4]
m1 = lm(diff(Aaa) ~ diff(Baa))
summary(m1)
par(mfrow = c(1, 2))
acf(m1$residuals)
pacf(m1$residuals)

m2 = arima(diff(Aaa), order = c(14, 0, 1), xreg = diff(Baa), include.mean = F)
m2
tsdiag(m2, gof = 12)

# 2.13
library(fBasics)

da = read.table("D:/USTC/时间序列分析/data/m-ew6299.txt", header = F)
rate = da[, 1]
plot(rate, type = 'l', ylab = 'rate')

adf.test(rate)
for(i in 1:3) print(Box.test(rate,type = "Ljung-Box",lag=6*i))
par(mfrow = c(1, 2))
acf(rate)
pacf(rate)

m1 = arima(rate, order = c(1, 0, 0))
m1
tsdiag(m1, gof = 12)

m2 = arima(rate, order = c(0, 0, 1))
m2
tsdiag(m2, gof = 12)
predict(m1, 2)
predict(m2, 2)
