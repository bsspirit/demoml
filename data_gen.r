
library(xts)

n<-3000
dates<-rep(as.Date('2018-01-01')+0:364,20)

date<-sample(dates,n)
y<-rbernoulli(n,0.1)
x1<-rnorm(n,0,1)
x2<-runif(n,100,1000*1000*1000)
x3<-rmvnorm(n=n, mean=c(1,2), sigma=matrix(c(4,2,2,3), ncol=2))[,1]
x4<-rmvnorm(n=n, mean=c(1,2), sigma=matrix(c(4,2,2,3), ncol=2))[,2]
x5<-kmeans(x3,13)$cluster

ids<-rep(1:119,30)
id<-sample(ids,n)

df<-data.frame(id,y,date,x1,x2,x3,x4,x5)
write.csv(df,file = "./data/sample/demo.csv")
