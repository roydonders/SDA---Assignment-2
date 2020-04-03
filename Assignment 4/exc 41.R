custom_qqline <- function(y, datax = FALSE, distribution = qnorm,
                          probs = c(0.25, 0.75), qtype = 7, ...)
{
  stopifnot(length(probs) == 2, is.function(distribution))
  y <- quantile(y, probs, names=FALSE, type=qtype, na.rm = TRUE)
  x <- distribution(probs)
  if (datax) {
    slope <- diff(x)/diff(y)
    int <- x[1L] - slope*y[1L]
  } else {
    slope <- diff(y)/diff(x)
    int <- y[1L]-slope*x[1L]
  }
  abline(int, slope, ...)
}


#bw
setwd("/Users/Ko/Documents/SDA---Assignment-2")
#setwd("C:/Users/timon/SDA---Assignment-2")
source("functions_Ch3.txt")
source("functions_Ch5.txt")
databw = read.table("birthweight.txt")
bw = c(databw$V1)
par(mfrow =c(2,2),pty="s")
summary(bw)
hist(bw,col="blue",main="Histogram of Birthweight",xlab="Birtweight")
boxplot(bw,col="blue",main="Boxplot birtweight",ylab="Sorted Data",xlab="birthweight")
qqnorm(bw,pch=20,col="blue",cex=1,ylab="Sorted Data",main="normal qqplot birtweight")
custom_qqline(bw)
symplot(bw,pch=20,col="blue",cex=1)
qqt(bw,df=5,pch=20,col="blue",cex=1)
qqline(bw)
qqlaplace(bw,pch=20,col="blue",cex=1)
qqline(bw)
qqexp(bw,pch=20,col="blue",cex=1)
qqline(bw)
#unknown distribution so emperical bootstrap
B=1000
ebw_TStar = numeric(B)
for(i in 1:B){
  exstar= sample(bw,replace = TRUE)
  ebw_TStar[i] = median(exstar)
}
hist(ebw_TStar,col="green" ,main="Emperical bootstrap of Birthweight",xlab="birthweight")
sd(ebw_TStar)

#parametric bootstrap
pbw_TStar = numeric(B)
for(i in 1:B){
  pxstar = rexp(bw,rate = (1/mean(bw)))
  pbw_TStar[i] = median(pxstar)
}
hist(pbw_TStar,col="dark red" ,main="Parametric bootstrap of Birthweight",xlab="birthweight")
summary(pbw_TStar)
sd(pbw_TStar) 


#one-line-code for sd
sd(replicate(1000,median(rexp(bw,rate = (1/mean(bw))))))
