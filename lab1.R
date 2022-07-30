# 时间序列分析B LAB1
# PB19010450 和泳毅

# 1.1
#install.packages("fBasics")
library(fBasics)
da=read.table("D:/USTC/时间序列分析/第三版数据/chapter1/d-3stocks9908.txt",header=T)
ibm = da[,2:4]
# (a)
sibm = ibm * 100
sibm
basicStats(sibm)
# (b)
libm=log(1+ibm)*100
# (c)
basicStats(libm)
# (d)
t.test(libm[,1])
t.test(libm[,2])
t.test(libm[,3])

# 1.2
library(fBasics)
da=read.table("D:/USTC/时间序列分析/第三版数据/chapter1/m-gm3dx7508.txt",header=T)
ibm = da[,2:5]
# (a)
sibm = ibm * 100
basicStats(sibm)              
# (b)
libm=log(1+ibm)*100
# (c)
basicStats(libm)
# (d)
t.test(libm[,1])
t.test(libm[,2])
t.test(libm[,3])            
t.test(libm[,4])  

# 1.3
library(fBasics)
da=read.table("D:/USTC/时间序列分析/第三版数据/chapter1/m-gm3dx7508.txt",header=T)
ibm = da[,5]
# (a)
libm=log(1+ibm)
yibm = mean(libm) * 12
yibm
# (b)
exp(yibm * (2008-1975+1))

# 1.4
library(fBasics)
da=read.table("D:/USTC/时间序列分析/第三版数据/chapter1/d-3stocks9908.txt",header=T)
ibm = da[,2]
libm=log(1+ibm)
# (a)
s=skewness(libm)
t1=s/sqrt(6/length(libm))
pv1=2*(1-pnorm(abs(t1)))
# (b)
k=kurtosis(libm)
t2=k/sqrt(24/length(libm))
pv2=2*(1-pnorm(t2))

# 1.5
library(fBasics)
da1=read.table("D:/USTC/时间序列分析/第三版数据/chapter1/d-caus.txt",header=T)
da2=read.table("D:/USTC/时间序列分析/第三版数据/chapter1/d-useu.txt",header=T)
da3=read.table("D:/USTC/时间序列分析/第三版数据/chapter1/d-usuk.txt",header=T)
da4=read.table("D:/USTC/时间序列分析/第三版数据/chapter1/d-jpus.txt",header=T)
da=cbind(da1[,4],da2[,4],da3[,4],da4[,4])
colnames(da) <- c("CA","EU","UK","JP")
# (a)
ibm=da[2:length(da[,1]),]/da[1:length(da[,1])-1,]-1
libm=log(ibm+1)*100
# (b)
basicStats(libm)



