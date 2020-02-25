setwd("C:/Users/Ka-Ho/Documents/R/R Workspace/SDA")
source("sample2020.txt")
source("functions_Ch3.txt")

z<- c(sample2020$sample2020b)
par(mfrow=c(1,3),"s")
qqnorm(z,pch=20,col="blue",cex=1)
qqline(z)
abline(3.28,1.635, col="yellow")
qqt(z,df=5,pch=20,col="blue",cex=1)
qqline(z)
abline(3.28,1.635, col="yellow")
qqlaplace(z,pch=20,col="blue",cex=1)
qqline(z)
abline(3.28,1.635, col="yellow")

gammsa<-(qgamma(seq(0,1,by=.01),2.1,scale = 1.8))
ks.test(z,qgamma(seq(0,1,by=.01),2.1,scale = 1.8))
plot(z,gammsa)
chisq.test(z,(qgamma(seq(0,0.79,by=.01),2.1,scale = 1.8)))
qqplot(z,(qgamma(seq(0,0.79,by=.01),2.1,scale = 1.8)))

chisq.test(z,(qgamma(seq(0,0.79,by=.01),2.1,scale = 1.8)))
chisq.test(z)


           