setwd("C:/Users/Ka-Ho/Documents/R/R Workspace/SDA")
setwd("C:/Users/timon/SDA---Assignment-2")
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

zlength = length(z)
x_gamma = seq(0,1,length.out = zlength)
y_gamma = qgamma(x_gamma, 2.1, scale = 1.8)
ks.test(z,y_gamma)
plot(z,y_gamma)

chisq.test(z,y_gamma)
qqplot(z,y_gamma)

chisq.test(z,y_gamma)
chisq.test(z)


           