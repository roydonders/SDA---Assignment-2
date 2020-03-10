getwd()
setwd("/Users/Ko/Documents/SDA---Assignment-2")
data_t = read.table("t-sample.txt")
source("functions_Ch4.txt")
source("functions_Ch5.txt")
library(e1071)

random_norm = rnorm(10000,mean=0,sd=1)
t_sample = c(data_t$V1,data_t$V2,data_t$V3,data_t$V4,data_t$V5)

par(mfrow=c(1,1),pty="s")
hist(t_sample)
k_t_sample = kurtosis(t_sample,type=1)
kurtosis(random_norm)

n = length(t_sample)
x = t_sample
#kurtosis functie
kurt = function(x) {(n* sum((x-mean(x))^4)/(sum((x-mean(x))^2)^2))-3
}

kurt(x)
kurtosis(x,type=1)

s = var(t_sample) 
#degree of freedom
k = 2*s / (s - 1)

#bootstrap
#B=1000
#empBSx = numeric(B)
#for(i in 1:B){
 # emp=sample(x[], replace = TRUE)
 # empBSx[i] = kurtosis(emp)
#}
#hist(empBSx)

empBS = bootstrap(x,kurtosis,B=1000)

par(mfrow=c(1,1),pty="s")
hist(empBS,main = "Histogram of empBS with excess kurtosis")
abline(v=k_t_sample,col="red")
hist(empBS,main = "Histogram of empBS with true kurtosis = 1")
abline(v=1,col="red")
var(empBS)

parBS = numeric(B)
for(i in 1:B){
  par= rt(x,k)
  parBS[i] = kurtosis(par)
}

hist(parBS)
par(mfrow=c(1,1),pty="s")
hist(parBS,main = "Histogram of parBS with excess kurtosis")
abline(v=k_t_sample,col="red")
hist(parBS,main = "Histogram of parBS with true kurtosis = 1")
abline(v=1,col="red")
var(parBS)
