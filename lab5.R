# 时间序列分析B LAB5
# PB19010450 和泳毅

# 8.1
require(MTS)
da = read.table("D:/USTC/时间序列分析/data/m-mrk2vw.txt", header = T)
apply(da[, 2:7], 2, mean)
apply(da[, 2:7], 2, sd)
cov(da[, 2:7])
cor(da[, 2:7])
mq(da[, 2:7], 6)

# 8.2
require(MTS)
da = read.table("D:/USTC/时间序列分析/data/m-gs1n10.txt", header = T)
attach(da)
y = cbind(diff(gs1), diff(gs10))

VARorder(y, 12)
m1 = VAR(y, 6)
m1 = refVAR(m1)
chol(m1$Sigma)
MTSdiag(m1)

VMAorder(y)
m2 = VMAs(y, c(1, 3, 5, 7, 11))
MTSdiag(m2)

# 8.3
require(MTS)
da = read.table("D:/USTC/时间序列分析/data/m-gs1n10.txt", header = T)
plot(ts(log(gs1)))
plot(ts(log(gs10)))
attach(da)
y = cbind(diff(log(gs1)), diff(log(gs10)))
VARorder(y, 12)
m1 = VARMA(y, 1, 9)
refVARMA(m1,thres=2)
MTSdiag(m1)
