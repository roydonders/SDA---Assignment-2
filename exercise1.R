
setwd("C:/Users/Ka-Ho/Documents/R/R Workspace/SDA")
source("sample2020.txt")
source("functions_Ch3.txt")
par(mfrow=c(2,2), pty="s")
x=seq(0,1,by=0.01)
snormal <- qnorm(x,mean=0,sd=1)
uni<-qunif(x,min=1,max=2)
plot(uni)
plot(snormal)
plot(uni,snormal,
     main = "QQ-Plot Uniform with Standard Normal",
     xlab = "Uniform(1,2)", ylab = "Standard Normal",
     pch=20,
     col="blue",
     cex=1.0)

normal<- qnorm(x,mean=1,sd=sqrt(4))
qt3 <-qt(x,3) 
plot(normal)
plot(qt3)
plot(normal,qt3,
     main = "QQ-Plot Normal with t-distribution",
     xlab = "Normal(1,4)", ylab = "t-distribution with df = 3",
     pch=20,
     col="green",
     cex=1)
plot(qt3,normal)


t10<-qt(x,10)
log<-qlnorm(x,meanlog = 0.1,sdlog = 0.4)
plot(t10)
plot(log)
plot(t10,log,
     main = "QQ-Plot t-distribution with Log normal",
     xlab = "t-distribution with df = 10", ylab = "Log normal with meanlog = 0.1, sdlog = 0.4",
     pch=20,
     col="red",
     cex=1)

qqplot(uni,snormal)
qqplot(seq(0,10,by=0.01),y)

y <- sort(sqrt(sample2020$sample2020a))
yy <- sort(sample2020$sample2020a)
plot(y)
hist(y)
qqlaplace(y)
plot(yy)
hist(yy)
par(mfrow=c(1,3),"s")
qqnorm(y,
       pch=20,
       col="blue",
       cex=1)
qqline(y)
abline(1.871,0.927, col="yellow")
qqt(y,df=3,
    
    pch=20,
    col="blue",
    cex=1)
qqline(y)
abline(1.871,0.927,col="yellow")

qqlaplace(y,
          pch=20,
          col="blue",
          cex=1)
qqline(y)
abline(1.871,0.928,col="yellow")

hist(slope)
qqline(y)
qqnorm(yy)
qqlnorm(y)
qqplot(x,y)
par(mfrow=c(2,2))

qqnorm(y)

qqlaplace(y,
          pch=20,
          col="green",
          cex=1)
qqline(y)
abline(1.879,0.928,col="yellow")


